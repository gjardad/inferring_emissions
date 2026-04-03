###############################################################################
# analysis/active/build_repeated_cv_proxy_crf.R
#
# PURPOSE
#   Repeated cross-fitting for the fuel-supply proxy via elastic net,
#   with folds defined at the CRF-category level instead of NACE 2-digit.
#
#   CRF groups are defined by the NACE-to-CRF crosswalk in
#   NACE_CRF_CROSSWALK.md. Each CRF group maps to one or more NACE 2-digit
#   sectors; holding out a CRF group means holding out ALL NACE sectors
#   in that group. This mirrors deployment: at deployment, emission totals
#   from the NIR are available at the CRF category level, and the proxy
#   distributes within CRF groups.
#
#   The elastic net specification is identical to build_repeated_cv_proxy_asinh.R:
#     - LHS: asinh(emissions)
#     - RHS: asinh(purchases from eligible sellers), unpenalized log_revenue,
#            year FE, NACE 2d sector FE
#     - Penalty: elastic net with alpha = 0.5
#
#   Only the fold assignment changes: folds are defined over CRF groups
#   (10 groups → K=5 folds), not NACE 2d sectors.
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {PROC_DATA}/repeated_cv_proxy_crf_asinh.RData
#     Contains: proxy_matrix, repeated_cv_proxy_panel, repeat_diagnostics,
#               syt, repeat_timing, firmyear_index, crf_group_map
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
library(doParallel)
library(foreach)

# ── Parameters ───────────────────────────────────────────────────────────────
M_REPEATS      <- 200L
K_OUTER        <- 5L
K_INNER        <- 10L
MIN_LHS_BUYERS <- 5L
ALPHA          <- 0.5
BASE_SEED      <- 2026L
N_CORES        <- 40L

cat("================================================================\n")
cat("  REPEATED CROSS-FITTING (CRF-group folds)\n")
cat("  M =", M_REPEATS, "repeats, K =", K_OUTER, "folds,",
    N_CORES, "cores\n")
cat("================================================================\n\n")


# =============================================================================
# CRF GROUP DEFINITIONS
# =============================================================================
# From NACE_CRF_CROSSWALK.md — each NACE 2d sector maps to exactly one group.
# Transport (49, 50, 51) is excluded from deployment but present in training.
crf_group_map <- data.frame(
  nace2d = c(
    "35",
    "19",
    "24", "25",
    "20", "21",
    "17", "18", "17/18",
    "10", "11", "12",
    "23",
    "05", "06", "07", "08", "09",
    "13", "14", "15", "16", "22",
    "26", "27", "28", "29", "30", "31", "32", "33",
    "41", "42", "43",
    "36", "37", "38", "39",
    "45", "46", "47",
    "52", "53",
    "55", "56",
    "58", "59", "60", "61", "62", "63",
    "64", "65", "66",
    "68", "69", "70", "71", "72", "73", "74", "75",
    "77", "78", "79", "80", "81", "82",
    "84", "85", "86", "87", "88",
    "90", "91", "92", "93", "94", "95", "96",
    "01", "02", "03",
    "49", "50", "51"
  ),
  crf_group = c(
    "energy",
    "refining",
    "metals", "metals",
    "chemicals", "chemicals",
    "paper", "paper", "paper",
    "food", "food", "food",
    "minerals",
    rep("mfg_other", 5),   # 05-09
    rep("mfg_other", 5),   # 13-16, 22
    rep("mfg_other", 8),   # 26-33
    rep("mfg_other", 3),   # 41-43
    rep("commercial", 4),  # 36-39
    rep("commercial", 3),  # 45-47
    rep("commercial", 2),  # 52-53
    rep("commercial", 2),  # 55-56
    rep("commercial", 6),  # 58-63
    rep("commercial", 3),  # 64-66
    rep("commercial", 8),  # 68-75
    rep("commercial", 6),  # 77-82
    rep("commercial", 5),  # 84-88
    rep("commercial", 7),  # 90-96
    "agriculture", "agriculture", "agriculture",
    "transport", "transport", "transport"
  ),
  stringsAsFactors = FALSE
)

CRF_GROUPS <- sort(unique(crf_group_map$crf_group))
cat("CRF groups:", paste(CRF_GROUPS, collapse = ", "), "\n")
cat("Number of CRF groups:", length(CRF_GROUPS), "\n\n")


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
# STEP 2: Assign firms to primary sector and CRF group
# =============================================================================
firm_sector <- lhs %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

lhs <- lhs %>%
  left_join(firm_sector, by = "vat")

# Assign CRF group based on primary NACE 2d
lhs <- lhs %>%
  left_join(crf_group_map, by = c("primary_nace2d" = "nace2d")) %>%
  rename(primary_crf_group = crf_group)

# Check for unmapped sectors
n_unmapped <- sum(is.na(lhs$primary_crf_group))
if (n_unmapped > 0) {
  unmapped_sectors <- unique(lhs$primary_nace2d[is.na(lhs$primary_crf_group)])
  cat("WARNING:", n_unmapped, "firm-years with unmapped NACE 2d sectors:",
      paste(unmapped_sectors, collapse = ", "), "\n")
  cat("  These firms will be excluded from CV.\n\n")
}

all_crf_groups <- sort(unique(lhs$primary_crf_group[!is.na(lhs$primary_crf_group)]))
cat("CRF groups in training sample:", paste(all_crf_groups, collapse = ", "), "\n")
cat("Number of CRF groups in training sample:", length(all_crf_groups), "\n\n")

# Summary
cat("── CRF group summary ──\n")
lhs %>%
  filter(!is.na(primary_crf_group)) %>%
  group_by(primary_crf_group) %>%
  summarise(
    n_nace2d = n_distinct(primary_nace2d),
    n_firms = n_distinct(vat),
    n_fy = n(),
    n_emit = sum(emit),
    n_nonemit = sum(emit == 0),
    .groups = "drop"
  ) %>%
  arrange(desc(n_emit)) %>%
  as.data.frame() %>%
  print()
cat("\n")


# =============================================================================
# STEP 3: Filter B2B to LHS buyers
# =============================================================================
cat("Filtering B2B to LHS buyers, years >= 2005...\n")
b2b_lhs <- b2b %>%
  filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)
cat("B2B transactions for LHS buyers:", nrow(b2b_lhs), "\n\n")


# =============================================================================
# STEP 4: Sector-year emission totals (at NACE 2d level, for reference)
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

# Assign CRF-group-level folds for one repeat
assign_folds_crf <- function(lhs_df, K, seed) {
  set.seed(seed)
  crf_groups <- sort(unique(lhs_df$primary_crf_group[!is.na(lhs_df$primary_crf_group)]))
  group_folds <- sample(rep(1:K, length.out = length(crf_groups)))
  gfm <- data.frame(primary_crf_group = crf_groups, fold_k = group_folds,
                     stringsAsFactors = FALSE)
  lhs_df <- lhs_df %>%
    select(-any_of("fold_k")) %>%
    left_join(gfm, by = "primary_crf_group")
  lhs_df
}


# =============================================================================
# STEP 6: Run one complete K-fold CV and return proxy values
# =============================================================================
run_one_cv <- function(lhs_with_folds, b2b_lhs, K, alpha, K_inner, min_buyers, seed) {

  # Pre-assign inner folds for lambda tuning (at the firm level)
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

    # Controls: log_revenue + year FE + NACE 2d sector FE
    # NOTE: sector FEs remain at NACE 2d level (not CRF), same as the
    # original specification. Only the fold structure changes.
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
# STEP 7: Main loop over M repeats (parallel or sequential)
# =============================================================================
use_parallel <- N_CORES > 1

if (use_parallel) {
  cat("\n=== Starting", M_REPEATS, "repeats in PARALLEL (",
      N_CORES, "cores) ===\n\n")

  cl <- makeCluster(N_CORES)
  registerDoParallel(cl)

  clusterEvalQ(cl, {
    library(dplyr)
    library(Matrix)
    library(glmnet)
  })

  t0_all <- Sys.time()

  all_repeat_results <- foreach(
    r = seq_len(M_REPEATS),
    .packages = c("dplyr", "Matrix", "glmnet"),
    .export = c("lhs", "b2b_lhs", "K_OUTER", "ALPHA",
                "K_INNER", "MIN_LHS_BUYERS", "BASE_SEED",
                "assign_folds_crf", "run_one_cv", "extract_suppliers")
  ) %dopar% {
    t0_r <- Sys.time()
    seed_r <- BASE_SEED + r
    lhs_r <- assign_folds_crf(lhs, K_OUTER, seed_r)
    proxy_r <- run_one_cv(lhs_r, b2b_lhs, K_OUTER, ALPHA, K_INNER,
                           MIN_LHS_BUYERS, seed_r)
    elapsed_r <- as.numeric(difftime(Sys.time(), t0_r, units = "mins"))
    list(proxy = proxy_r, time = elapsed_r)
  }

  stopCluster(cl)

  total_time <- round(difftime(Sys.time(), t0_all, units = "mins"), 1)

  all_repeat_proxies <- lapply(all_repeat_results, `[[`, "proxy")
  repeat_timing <- sapply(all_repeat_results, `[[`, "time")
  rm(all_repeat_results)

  cat(sprintf("=== All %d repeats complete in %.1f min (avg %.1f min/repeat) ===\n\n",
              M_REPEATS, total_time, mean(repeat_timing)))

} else {
  cat("\n=== Starting", M_REPEATS, "repeats SEQUENTIALLY ===\n\n")

  t0_all <- Sys.time()
  all_repeat_proxies <- list()
  repeat_timing <- numeric(M_REPEATS)

  for (r in seq_len(M_REPEATS)) {
    t0_r <- Sys.time()
    cat(sprintf("-- Repeat %d / %d ", r, M_REPEATS))

    seed_r <- BASE_SEED + r
    lhs_r <- assign_folds_crf(lhs, K_OUTER, seed_r)
    proxy_r <- run_one_cv(lhs_r, b2b_lhs, K_OUTER, ALPHA, K_INNER,
                           MIN_LHS_BUYERS, seed_r)

    all_repeat_proxies[[r]] <- proxy_r
    gc()

    elapsed_r <- round(difftime(Sys.time(), t0_r, units = "mins"), 1)
    repeat_timing[r] <- as.numeric(elapsed_r)

    avg_time <- mean(repeat_timing[1:r])
    remaining <- round(avg_time * (M_REPEATS - r), 1)
    cat(sprintf("(%.1f min, est. %.0f min remaining) --\n", elapsed_r, remaining))

    if (r %% 10 == 0) {
      cat(sprintf("\n  Progress: %d/%d complete | Avg %.1f min/repeat | Total %.1f min\n\n",
                  r, M_REPEATS, avg_time, sum(repeat_timing[1:r])))
    }
  }

  total_time <- round(difftime(Sys.time(), t0_all, units = "mins"), 1)
  cat(sprintf("\n=== All %d repeats complete in %.1f min (avg %.1f min/repeat) ===\n\n",
              M_REPEATS, total_time, mean(repeat_timing)))
}


# =============================================================================
# STEP 8: Build proxy matrix (N firm-years x M repeats)
# =============================================================================
cat("Building proxy matrix (N x M)...\n")

firmyear_index <- lhs %>%
  mutate(row_id = row_number()) %>%
  select(row_id, vat, year)

N <- nrow(firmyear_index)
proxy_matrix <- matrix(NA_real_, nrow = N, ncol = M_REPEATS)

for (r in seq_len(M_REPEATS)) {
  proxy_r <- all_repeat_proxies[[r]] %>%
    inner_join(firmyear_index, by = c("vat", "year"))
  proxy_matrix[proxy_r$row_id, r] <- proxy_r$proxy
}

proxy_matrix[is.na(proxy_matrix)] <- 0

cat("Proxy matrix:", N, "firm-years x", M_REPEATS, "repeats\n")
cat("  Non-zero entries:", sum(proxy_matrix != 0),
    sprintf("(%.1f%%)\n", 100 * mean(proxy_matrix != 0)))

proxy_mean <- rowMeans(proxy_matrix)
proxy_sd   <- apply(proxy_matrix, 1, sd)

cat("  Firm-years with mean proxy > 0:", sum(proxy_mean > 0),
    sprintf("(%.1f%%)\n", 100 * mean(proxy_mean > 0)))
cat("  Mean proxy SD across firm-years:", round(mean(proxy_sd, na.rm = TRUE), 4), "\n")
cat("  Median proxy SD:", round(median(proxy_sd, na.rm = TRUE), 4), "\n\n")

repeated_cv_proxy_panel <- lhs %>%
  mutate(
    proxy_mean = proxy_mean,
    proxy_sd   = proxy_sd
  ) %>%
  select(vat, year, nace2d, y, emit, log_revenue, euets,
         primary_nace2d, primary_crf_group, proxy_mean, proxy_sd)


# =============================================================================
# STEP 9: Diagnostics
# =============================================================================
repeat_diagnostics <- data.frame(
  cv_type     = "crf",
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

cat("-- Repeat timing distribution --\n")
cat("  Min:", round(min(repeat_timing), 1), "min\n")
cat("  Median:", round(median(repeat_timing), 1), "min\n")
cat("  Max:", round(max(repeat_timing), 1), "min\n")
cat("  Total:", round(sum(repeat_timing), 1), "min\n\n")


# =============================================================================
# STEP 10: Save
# =============================================================================
OUT_PATH <- file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData")

save(proxy_matrix, repeated_cv_proxy_panel, repeat_diagnostics, syt,
     repeat_timing, firmyear_index, crf_group_map,
     file = OUT_PATH)

cat("==============================================\n")
cat("Saved to:", OUT_PATH, "\n")
cat("  proxy_matrix:", N, "x", M_REPEATS, "\n")
cat("  repeated_cv_proxy_panel:", nrow(repeated_cv_proxy_panel), "rows x",
    ncol(repeated_cv_proxy_panel), "cols\n")
cat("  CV type: CRF-group | M =", M_REPEATS, "| K =", K_OUTER, "\n")
cat("==============================================\n")
