###############################################################################
# analysis/active/build_repeated_cv_proxy_asinh.R
#
# PURPOSE
#   Repeated cross-fitting for the fuel-supply proxy via elastic net.
#   Runs M repetitions of K-fold CV with random fold assignments, then
#   averages proxy values across repetitions to produce a stable proxy.
#
#   Supports two fold designs:
#     CV_TYPE = "sector"  — K=5, sectors randomly assigned to folds
#     CV_TYPE = "firm"    — K=10, firms randomly assigned to folds
#                           (stratified by primary sector)
#
#   Reference: Fava (2025), "Training and Testing with Multiple Splits"
#   — repeated cross-fitting improves reproducibility and uses all data
#   for both training and evaluation.
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {PROC_DATA}/repeated_cv_proxy_{CV_TYPE}_asinh.RData
#     Contains: repeated_cv_proxy_panel, repeat_diagnostics, syt
#
# RUNS ON: RMD (requires full B2B data)
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(Matrix)
library(glmnet)

# ── Parameters ───────────────────────────────────────────────────────────────
CV_TYPE        <- "sector"   # "sector" or "firm"
M_REPEATS      <- 50L        # number of repeated cross-fitting rounds
K_OUTER        <- if (CV_TYPE == "sector") 5L else 10L
K_INNER        <- 10L        # inner folds for cv.glmnet lambda tuning
MIN_LHS_BUYERS <- 5L
ALPHA          <- 0.5
BASE_SEED      <- 2026L      # each repeat r uses seed = BASE_SEED + r

cat("═══════════════════════════════════════════════════════════════\n")
cat("  REPEATED CROSS-FITTING (", CV_TYPE, " folds)\n")
cat("  M =", M_REPEATS, "repeats, K =", K_OUTER, "folds\n")
cat("═══════════════════════════════════════════════════════════════\n\n")


# =============================================================================
# STEP 1: Load data and prepare LHS panel
# =============================================================================
cat("Loading B2B selected sample...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

lhs <- training_sample %>%
  filter(year >= 2005) %>%
  mutate(
    y = emissions,
    log_revenue = log(pmax(revenue, 1e-12))
  ) %>%
  select(vat, year, y, log_revenue, nace2d, euets) %>%
  arrange(vat, year)
rm(training_sample)

lhs$emit <- as.integer(lhs$y > 0)
cat("LHS panel:", nrow(lhs), "firm-years,",
    length(unique(lhs$vat)), "unique firms\n")
cat("  Emitter firm-years:", sum(lhs$emit), "\n\n")


# =============================================================================
# STEP 2: Assign firms to primary sector
# =============================================================================
firm_sector <- lhs %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

lhs <- lhs %>%
  left_join(firm_sector, by = "vat")

all_sectors <- sort(unique(lhs$primary_nace2d))
all_firms   <- sort(unique(lhs$vat))
cat("Firms:", length(all_firms), "| Sectors:", length(all_sectors), "\n\n")


# =============================================================================
# STEP 3: Filter B2B to LHS buyers
# =============================================================================
cat("Filtering B2B to LHS buyers, years >= 2005...\n")
b2b_lhs <- b2b %>%
  filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)
cat("B2B transactions for LHS buyers:", nrow(b2b_lhs), "\n\n")


# =============================================================================
# STEP 4: Sector-year emission totals
# =============================================================================
syt <- lhs %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# =============================================================================
# STEP 5: Helper functions
# =============================================================================
extract_suppliers <- function(fit, n_ctrl, eligible_sellers, s = "lambda.min") {
  co <- coef(fit, s = s)
  supplier_idx <- (n_ctrl + 2):length(co)
  vals <- co[supplier_idx]
  data.frame(
    vat_i_ano = eligible_sellers,
    coef      = as.numeric(vals),
    stringsAsFactors = FALSE
  ) %>%
    filter(coef != 0) %>%
    arrange(desc(abs(coef)))
}

# Assign folds for one repeat
assign_folds <- function(lhs_df, cv_type, K, seed) {
  set.seed(seed)
  if (cv_type == "sector") {
    # Random sector-to-fold assignment
    sectors <- sort(unique(lhs_df$primary_nace2d))
    sector_folds <- sample(rep(1:K, length.out = length(sectors)))
    sfm <- data.frame(primary_nace2d = sectors, fold_k = sector_folds,
                       stringsAsFactors = FALSE)
    lhs_df <- lhs_df %>%
      select(-any_of("fold_k")) %>%
      left_join(sfm, by = "primary_nace2d")
  } else {
    # Random firm-to-fold assignment, stratified by primary sector
    firm_sector_df <- lhs_df %>%
      distinct(vat, primary_nace2d)
    firm_folds <- integer(nrow(firm_sector_df))
    for (sec in unique(firm_sector_df$primary_nace2d)) {
      idx <- which(firm_sector_df$primary_nace2d == sec)
      firm_folds[idx] <- sample(rep(1:K, length.out = length(idx)))
    }
    firm_sector_df$fold_k <- firm_folds
    lhs_df <- lhs_df %>%
      select(-any_of("fold_k")) %>%
      left_join(firm_sector_df %>% select(vat, fold_k), by = "vat")
  }
  lhs_df
}


# =============================================================================
# STEP 6: Run one complete K-fold CV and return proxy values
# =============================================================================
run_one_cv <- function(lhs_with_folds, b2b_lhs, K, alpha, K_inner, min_buyers, seed) {

  # Pre-assign inner folds for lambda tuning
  set.seed(seed + 1000L)
  all_firms_k <- unique(lhs_with_folds$vat)
  inner_fold_map <- sample(rep(1:K_inner, length.out = length(all_firms_k)))
  names(inner_fold_map) <- all_firms_k

  proxy_pieces <- list()

  for (k in seq_len(K)) {
    train_lhs <- lhs_with_folds[lhs_with_folds$fold_k != k, ]
    test_lhs  <- lhs_with_folds[lhs_with_folds$fold_k == k, ]

    if (nrow(test_lhs) == 0) next

    # Identify eligible sellers from training buyers
    b2b_train_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% train_lhs$vat, ]
    seller_counts_k <- b2b_train_k %>%
      distinct(vat_i_ano, vat_j_ano) %>%
      count(vat_i_ano, name = "n_lhs_buyers")
    eligible_sellers_k <- seller_counts_k %>%
      filter(n_lhs_buyers >= min_buyers) %>%
      pull(vat_i_ano) %>%
      sort()

    if (length(eligible_sellers_k) == 0) {
      proxy_pieces[[k]] <- data.frame(
        vat = test_lhs$vat, year = test_lhs$year,
        proxy = 0, stringsAsFactors = FALSE)
      next
    }

    seller_map_k <- data.frame(
      vat_i_ano = eligible_sellers_k,
      col_idx   = seq_along(eligible_sellers_k),
      stringsAsFactors = FALSE
    )

    # Build sparse design matrix
    train_lhs$row_idx <- seq_len(nrow(train_lhs))

    b2b_agg_k <- b2b_train_k %>%
      filter(vat_i_ano %in% eligible_sellers_k) %>%
      group_by(vat_i_ano, vat_j_ano, year) %>%
      summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
      filter(sales != 0) %>%
      inner_join(train_lhs %>% select(vat, year, row_idx),
                 by = c("vat_j_ano" = "vat", "year" = "year")) %>%
      inner_join(seller_map_k, by = "vat_i_ano")

    n_rows_k <- nrow(train_lhs)
    n_cols_k <- length(eligible_sellers_k)

    X_asinh_k <- sparseMatrix(
      i = b2b_agg_k$row_idx, j = b2b_agg_k$col_idx,
      x = asinh(b2b_agg_k$sales), dims = c(n_rows_k, n_cols_k)
    )
    colnames(X_asinh_k) <- eligible_sellers_k

    rm(b2b_agg_k, b2b_train_k)

    # Controls: log_revenue + year FE + sector FE
    year_dummies_k <- model.matrix(~ factor(year), data = train_lhs)[, -1, drop = FALSE]
    colnames(year_dummies_k) <- paste0("yr_", sort(unique(train_lhs$year))[-1])

    train_sectors <- sort(unique(train_lhs$nace2d))
    sector_dummies_k <- model.matrix(~ factor(nace2d, levels = train_sectors),
                                     data = train_lhs)[, -1, drop = FALSE]
    colnames(sector_dummies_k) <- paste0("sec_", train_sectors[-1])

    X_controls_k <- cbind(log_revenue = train_lhs$log_revenue,
                           year_dummies_k, sector_dummies_k)
    n_controls_k <- ncol(X_controls_k)

    X_full_k <- cbind(Matrix(X_controls_k, sparse = TRUE), X_asinh_k)
    pf_k <- c(rep(0, n_controls_k), rep(1, n_cols_k))

    rm(X_asinh_k, X_controls_k, year_dummies_k, sector_dummies_k)

    # Drop zero-variance columns
    col_means <- colMeans(X_full_k)
    col_vars <- colMeans(X_full_k^2) - col_means^2
    zero_var <- which(col_vars == 0)
    if (length(zero_var) > 0) {
      X_full_k <- X_full_k[, -zero_var]
      pf_k <- pf_k[-zero_var]
      n_controls_k <- sum(pf_k == 0)
      eligible_sellers_k_enet <- colnames(X_full_k)[which(pf_k == 1)]
    } else {
      eligible_sellers_k_enet <- eligible_sellers_k
    }

    # Run cv.glmnet
    inner_foldid_k <- unname(inner_fold_map[train_lhs$vat])

    fit_k <- cv.glmnet(
      x = X_full_k, y = asinh(train_lhs$y),
      family = "gaussian", alpha = alpha,
      penalty.factor = pf_k, foldid = inner_foldid_k,
      standardize = TRUE
    )

    rm(X_full_k)

    # Extract ALL non-zero coefficients
    coef_lookup_k <- extract_suppliers(fit_k, n_controls_k,
                                        eligible_sellers_k_enet, "lambda.min")
    rm(fit_k)

    # Build proxy for held-out firms
    if (nrow(coef_lookup_k) > 0) {
      heldout_vats_k <- unique(test_lhs$vat)
      b2b_heldout_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]

      proxy_k <- b2b_heldout_k %>%
        inner_join(coef_lookup_k, by = "vat_i_ano") %>%
        group_by(vat_j_ano, year) %>%
        summarise(proxy = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                  .groups = "drop") %>%
        rename(vat = vat_j_ano)

      rm(b2b_heldout_k)
    } else {
      proxy_k <- data.frame(vat = character(0), year = integer(0),
                             proxy = numeric(0), stringsAsFactors = FALSE)
    }

    # Merge onto test_lhs
    test_proxy_k <- test_lhs %>%
      select(vat, year) %>%
      left_join(proxy_k, by = c("vat", "year")) %>%
      mutate(proxy = coalesce(proxy, 0))

    proxy_pieces[[k]] <- test_proxy_k %>% select(vat, year, proxy)

    rm(train_lhs, test_lhs, coef_lookup_k, proxy_k, test_proxy_k,
       eligible_sellers_k, eligible_sellers_k_enet, seller_map_k,
       seller_counts_k, pf_k, inner_foldid_k)
  }

  bind_rows(proxy_pieces)
}


# =============================================================================
# STEP 7: Main loop over M repeats
# =============================================================================
cat("\n═══ Starting", M_REPEATS, "repeated cross-fitting rounds ═══\n\n")

t0_all <- Sys.time()
all_repeat_proxies <- list()
repeat_timing <- numeric(M_REPEATS)

for (r in seq_len(M_REPEATS)) {
  t0_r <- Sys.time()
  cat(sprintf("── Repeat %d / %d ", r, M_REPEATS))

  seed_r <- BASE_SEED + r

  # Assign folds
  lhs_r <- assign_folds(lhs, CV_TYPE, K_OUTER, seed_r)

  # Run one complete CV
  proxy_r <- run_one_cv(lhs_r, b2b_lhs, K_OUTER, ALPHA, K_INNER,
                         MIN_LHS_BUYERS, seed_r)

  all_repeat_proxies[[r]] <- proxy_r
  gc()

  elapsed_r <- round(difftime(Sys.time(), t0_r, units = "mins"), 1)
  repeat_timing[r] <- as.numeric(elapsed_r)

  # Estimate remaining time
  avg_time <- mean(repeat_timing[1:r])
  remaining <- round(avg_time * (M_REPEATS - r), 1)

  cat(sprintf("(%.1f min, est. %.0f min remaining) ──\n", elapsed_r, remaining))

  # Progress summary every 10 repeats
  if (r %% 10 == 0) {
    cat(sprintf("\n  Progress: %d/%d complete | Avg %.1f min/repeat | Total %.1f min\n\n",
                r, M_REPEATS, avg_time, sum(repeat_timing[1:r])))
  }
}

total_time <- round(difftime(Sys.time(), t0_all, units = "mins"), 1)
cat(sprintf("\n═══ All %d repeats complete in %.1f min (avg %.1f min/repeat) ═══\n\n",
            M_REPEATS, total_time, mean(repeat_timing)))


# =============================================================================
# STEP 8: Build proxy matrix (N firm-years × M repeats)
# =============================================================================
cat("Building proxy matrix (N x M)...\n")

# Create a firm-year index from lhs (the canonical row order)
firmyear_index <- lhs %>%
  mutate(row_id = row_number()) %>%
  select(row_id, vat, year)

N <- nrow(firmyear_index)

# Initialize N x M matrix
proxy_matrix <- matrix(NA_real_, nrow = N, ncol = M_REPEATS)

for (r in seq_len(M_REPEATS)) {
  # Map repeat r's proxy values onto the canonical row order
  proxy_r <- all_repeat_proxies[[r]] %>%
    inner_join(firmyear_index, by = c("vat", "year"))
  proxy_matrix[proxy_r$row_id, r] <- proxy_r$proxy
}

# Firm-years not appearing in a repeat's held-out set get 0
# (should not happen — every firm-year is held out exactly once per repeat)
proxy_matrix[is.na(proxy_matrix)] <- 0

cat("Proxy matrix:", N, "firm-years x", M_REPEATS, "repeats\n")
cat("  Non-zero entries:", sum(proxy_matrix != 0),
    sprintf("(%.1f%%)\n", 100 * mean(proxy_matrix != 0)))

# Also compute summary stats for diagnostics
proxy_mean <- rowMeans(proxy_matrix)
proxy_sd   <- apply(proxy_matrix, 1, sd)

cat("  Firm-years with mean proxy > 0:", sum(proxy_mean > 0),
    sprintf("(%.1f%%)\n", 100 * mean(proxy_mean > 0)))
cat("  Mean proxy SD across firm-years:", round(mean(proxy_sd, na.rm = TRUE), 4), "\n")
cat("  Median proxy SD:", round(median(proxy_sd, na.rm = TRUE), 4), "\n\n")

# Assemble panel with summary columns (for quick inspection)
repeated_cv_proxy_panel <- lhs %>%
  mutate(
    proxy_mean = proxy_mean,
    proxy_sd   = proxy_sd
  ) %>%
  select(vat, year, nace2d, y, emit, log_revenue, euets,
         primary_nace2d, proxy_mean, proxy_sd)


# =============================================================================
# STEP 9: Diagnostics
# =============================================================================
repeat_diagnostics <- data.frame(
  cv_type     = CV_TYPE,
  M_repeats   = M_REPEATS,
  K_outer     = K_OUTER,
  K_inner     = K_INNER,
  alpha       = ALPHA,
  base_seed   = BASE_SEED,
  total_min   = as.numeric(total_time),
  avg_min     = mean(repeat_timing),
  n_firmyears = N,
  pct_proxy_gt0 = mean(proxy_mean > 0),
  mean_proxy_sd = mean(proxy_sd, na.rm = TRUE),
  stringsAsFactors = FALSE
)

cat("── Repeat timing distribution ──\n")
cat("  Min:", round(min(repeat_timing), 1), "min\n")
cat("  Median:", round(median(repeat_timing), 1), "min\n")
cat("  Max:", round(max(repeat_timing), 1), "min\n")
cat("  Total:", round(sum(repeat_timing), 1), "min\n\n")


# =============================================================================
# STEP 10: Save
# =============================================================================
# Save the full proxy matrix + panel + diagnostics
# proxy_matrix rows correspond to repeated_cv_proxy_panel rows (same order)
OUT_PATH <- file.path(PROC_DATA,
                       paste0("repeated_cv_proxy_", CV_TYPE, "_asinh.RData"))

save(proxy_matrix, repeated_cv_proxy_panel, repeat_diagnostics, syt,
     repeat_timing, firmyear_index,
     file = OUT_PATH)

cat("══════════════════════════════════════════════\n")
cat("Saved to:", OUT_PATH, "\n")
cat("  proxy_matrix:", N, "x", M_REPEATS, "\n")
cat("  repeated_cv_proxy_panel:", nrow(repeated_cv_proxy_panel), "rows x",
    ncol(repeated_cv_proxy_panel), "cols\n")
cat("  CV type:", CV_TYPE, "| M =", M_REPEATS, "| K =", K_OUTER, "\n")
cat("══════════════════════════════════════════════\n")
