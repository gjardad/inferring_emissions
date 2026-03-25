###############################################################################
# analysis/active/jensen_residual_diagnostics.R
#
# PURPOSE
#   Check whether the variance of asinh-scale residuals differs systematically
#   across firms within sector-year cells. If it does, the Jensen bias from
#   sinh back-transformation is differential and could distort proportional
#   allocation shares.
#
# APPROACH
#   Mirrors build_repeated_cv_proxy_asinh.R (sector CV, M=20 repeats, K=5),
#   but retains the EN model long enough to predict() on the held-out test
#   matrix. For each held-out firm-year we get:
#     fitted_asinh = full EN prediction (FE + log_revenue + suppliers)
#     resid_asinh  = asinh(y) - fitted_asinh
#     proxy_asinh  = supplier-only component (positive coefficients)
#   Then we test whether resid^2 varies with the proxy within sector-year.
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {PROC_DATA}/jensen_residual_diagnostics.RData
#     Contains: bp_regression, binned_variance, binned_variance_all
#
# RUNS ON: RMD
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

# ── Parameters (match build_repeated_cv_proxy_asinh.R except M) ─────────────
CV_TYPE        <- "sector"
M_REPEATS      <- 20L        # reduced from 200 — enough for variance diagnostics
K_OUTER        <- 5L
K_INNER        <- 10L
MIN_LHS_BUYERS <- 5L
ALPHA          <- 0.5
BASE_SEED      <- 2026L

cat("================================================================\n")
cat("  JENSEN RESIDUAL DIAGNOSTICS (", CV_TYPE, " folds)\n")
cat("  M =", M_REPEATS, "repeats, K =", K_OUTER, "folds\n")
cat("================================================================\n\n")


# =============================================================================
# STEP 1: Load data and prepare LHS panel (identical to build script)
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

firm_sector <- lhs %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")
lhs <- lhs %>% left_join(firm_sector, by = "vat")

cat("LHS panel:", nrow(lhs), "firm-years,",
    length(unique(lhs$vat)), "unique firms\n\n")


# =============================================================================
# STEP 2: Filter B2B to LHS buyers
# =============================================================================
b2b_lhs <- b2b %>% filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)
cat("B2B transactions for LHS buyers:", nrow(b2b_lhs), "\n\n")


# =============================================================================
# STEP 3: Helper functions (from build script)
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

assign_folds <- function(lhs_df, cv_type, K, seed) {
  set.seed(seed)
  if (cv_type == "sector") {
    sectors <- sort(unique(lhs_df$primary_nace2d))
    sector_folds <- sample(rep(1:K, length.out = length(sectors)))
    sfm <- data.frame(primary_nace2d = sectors, fold_k = sector_folds,
                       stringsAsFactors = FALSE)
    lhs_df <- lhs_df %>%
      select(-any_of("fold_k")) %>%
      left_join(sfm, by = "primary_nace2d")
  } else {
    stop("This diagnostic script only supports sector CV.")
  }
  lhs_df
}


# =============================================================================
# STEP 4: Modified run_one_cv — returns residuals + proxy
# =============================================================================
run_one_cv_with_residuals <- function(lhs_with_folds, b2b_lhs, K, alpha,
                                       K_inner, min_buyers, seed) {

  set.seed(seed + 1000L)
  all_firms_k <- unique(lhs_with_folds$vat)
  inner_fold_map <- sample(rep(1:K_inner, length.out = length(all_firms_k)))
  names(inner_fold_map) <- all_firms_k

  residual_pieces <- list()

  for (k in seq_len(K)) {
    train_lhs <- lhs_with_folds[lhs_with_folds$fold_k != k, ]
    test_lhs  <- lhs_with_folds[lhs_with_folds$fold_k == k, ]

    if (nrow(test_lhs) == 0) next

    # ── Eligible sellers ──────────────────────────────────────────────────
    b2b_train_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% train_lhs$vat, ]
    seller_counts_k <- b2b_train_k %>%
      distinct(vat_i_ano, vat_j_ano) %>%
      count(vat_i_ano, name = "n_lhs_buyers")
    eligible_sellers_k <- seller_counts_k %>%
      filter(n_lhs_buyers >= min_buyers) %>%
      pull(vat_i_ano) %>%
      sort()

    if (length(eligible_sellers_k) == 0) {
      residual_pieces[[k]] <- data.frame(
        vat = test_lhs$vat, year = test_lhs$year,
        nace2d = test_lhs$nace2d, y = test_lhs$y, emit = test_lhs$emit,
        log_revenue = test_lhs$log_revenue,
        fitted_asinh = NA_real_, resid_asinh = NA_real_, proxy_asinh = 0,
        stringsAsFactors = FALSE)
      next
    }

    seller_map_k <- data.frame(
      vat_i_ano = eligible_sellers_k,
      col_idx   = seq_along(eligible_sellers_k),
      stringsAsFactors = FALSE
    )

    # ── Training design matrix ────────────────────────────────────────────
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

    train_sectors <- sort(unique(train_lhs$nace2d))
    year_dummies_k <- model.matrix(~ factor(year), data = train_lhs)[, -1, drop = FALSE]
    colnames(year_dummies_k) <- paste0("yr_", sort(unique(train_lhs$year))[-1])
    sector_dummies_k <- model.matrix(~ factor(nace2d, levels = train_sectors),
                                     data = train_lhs)[, -1, drop = FALSE]
    colnames(sector_dummies_k) <- paste0("sec_", train_sectors[-1])

    X_controls_k <- cbind(log_revenue = train_lhs$log_revenue,
                           year_dummies_k, sector_dummies_k)
    n_controls_k <- ncol(X_controls_k)

    X_full_train_k <- cbind(Matrix(X_controls_k, sparse = TRUE), X_asinh_k)
    pf_k <- c(rep(0, n_controls_k), rep(1, n_cols_k))

    rm(X_asinh_k, X_controls_k, year_dummies_k, sector_dummies_k)

    # Drop zero-variance columns
    col_vars <- colMeans(X_full_train_k^2) - colMeans(X_full_train_k)^2
    zero_var <- which(col_vars == 0)
    if (length(zero_var) > 0) {
      X_full_train_k <- X_full_train_k[, -zero_var]
      pf_k <- pf_k[-zero_var]
      n_controls_k <- sum(pf_k == 0)
      eligible_sellers_k_enet <- colnames(X_full_train_k)[which(pf_k == 1)]
    } else {
      eligible_sellers_k_enet <- eligible_sellers_k
    }

    # ── Fit EN ────────────────────────────────────────────────────────────
    inner_foldid_k <- unname(inner_fold_map[train_lhs$vat])

    fit_k <- cv.glmnet(
      x = X_full_train_k, y = asinh(train_lhs$y),
      family = "gaussian", alpha = alpha,
      penalty.factor = pf_k, foldid = inner_foldid_k,
      standardize = TRUE
    )
    rm(X_full_train_k)

    # ── Build TEST design matrix ──────────────────────────────────────────
    test_lhs$row_idx <- seq_len(nrow(test_lhs))

    b2b_test_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% test_lhs$vat, ]
    b2b_agg_test_k <- b2b_test_k %>%
      filter(vat_i_ano %in% eligible_sellers_k) %>%
      group_by(vat_i_ano, vat_j_ano, year) %>%
      summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
      filter(sales != 0) %>%
      inner_join(test_lhs %>% select(vat, year, row_idx),
                 by = c("vat_j_ano" = "vat", "year" = "year")) %>%
      inner_join(seller_map_k, by = "vat_i_ano")

    X_asinh_test_k <- sparseMatrix(
      i = b2b_agg_test_k$row_idx, j = b2b_agg_test_k$col_idx,
      x = asinh(b2b_agg_test_k$sales),
      dims = c(nrow(test_lhs), n_cols_k)
    )
    colnames(X_asinh_test_k) <- eligible_sellers_k
    rm(b2b_agg_test_k, b2b_test_k)

    # Test controls — must match training column structure
    # In sector CV, held-out sectors are NOT in train_sectors, so model.matrix
    # would drop rows (na.omit). Build dummies manually instead: held-out
    # sectors get all-zero sector dummies (absorbed into intercept).
    train_years <- sort(unique(train_lhs$year))
    year_dummies_test_k <- matrix(0, nrow = nrow(test_lhs),
                                   ncol = length(train_years) - 1)
    colnames(year_dummies_test_k) <- paste0("yr_", train_years[-1])
    for (i in seq_along(train_years[-1])) {
      year_dummies_test_k[test_lhs$year == train_years[-1][i], i] <- 1
    }

    sector_dummies_test_k <- matrix(0, nrow = nrow(test_lhs),
                                     ncol = length(train_sectors) - 1)
    colnames(sector_dummies_test_k) <- paste0("sec_", train_sectors[-1])
    for (i in seq_along(train_sectors[-1])) {
      sector_dummies_test_k[test_lhs$nace2d == train_sectors[-1][i], i] <- 1
    }

    X_controls_test_k <- cbind(log_revenue = test_lhs$log_revenue,
                                year_dummies_test_k, sector_dummies_test_k)

    X_full_test_k <- cbind(Matrix(X_controls_test_k, sparse = TRUE), X_asinh_test_k)
    rm(X_asinh_test_k, X_controls_test_k, year_dummies_test_k, sector_dummies_test_k)

    # Remove same zero-variance columns as training
    if (length(zero_var) > 0) {
      X_full_test_k <- X_full_test_k[, -zero_var]
    }

    # ── Predict full model on test set ────────────────────────────────────
    fitted_asinh_k <- as.numeric(predict(fit_k, newx = X_full_test_k, s = "lambda.min"))
    rm(X_full_test_k)

    # ── Extract supplier-only proxy (positive coefficients) ───────────────
    coef_lookup_k <- extract_suppliers(fit_k, n_controls_k,
                                        eligible_sellers_k_enet, "lambda.min")
    coef_pos_k <- coef_lookup_k %>% filter(coef > 0)
    rm(fit_k)

    if (nrow(coef_pos_k) > 0) {
      b2b_heldout_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% test_lhs$vat, ]
      proxy_k <- b2b_heldout_k %>%
        inner_join(coef_pos_k, by = "vat_i_ano") %>%
        group_by(vat_j_ano, year) %>%
        summarise(proxy_asinh = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                  .groups = "drop") %>%
        rename(vat = vat_j_ano)
      rm(b2b_heldout_k)
    } else {
      proxy_k <- data.frame(vat = character(0), year = integer(0),
                             proxy_asinh = numeric(0), stringsAsFactors = FALSE)
    }

    # ── Assemble residuals for this fold ──────────────────────────────────
    residual_pieces[[k]] <- test_lhs %>%
      select(vat, year, nace2d, y, emit, log_revenue) %>%
      mutate(fitted_asinh = fitted_asinh_k,
             resid_asinh  = asinh(y) - fitted_asinh_k) %>%
      left_join(proxy_k, by = c("vat", "year")) %>%
      mutate(proxy_asinh = coalesce(proxy_asinh, 0))

    rm(train_lhs, test_lhs, coef_lookup_k, coef_pos_k, proxy_k,
       fitted_asinh_k, eligible_sellers_k, eligible_sellers_k_enet,
       seller_map_k, seller_counts_k, pf_k, inner_foldid_k, train_sectors)
  }

  bind_rows(residual_pieces)
}


# =============================================================================
# STEP 5: Main loop over M repeats (sequential — manageable at M=20)
# =============================================================================
cat("\n=== Starting", M_REPEATS, "repeats SEQUENTIALLY ===\n\n")

t0_all <- Sys.time()
all_residuals <- list()

for (r in seq_len(M_REPEATS)) {
  t0_r <- Sys.time()
  cat(sprintf("── Repeat %d / %d ", r, M_REPEATS))

  seed_r <- BASE_SEED + r
  lhs_r <- assign_folds(lhs, CV_TYPE, K_OUTER, seed_r)
  resid_r <- run_one_cv_with_residuals(lhs_r, b2b_lhs, K_OUTER, ALPHA,
                                        K_INNER, MIN_LHS_BUYERS, seed_r)
  resid_r$repeat_id <- r
  all_residuals[[r]] <- resid_r
  gc()

  elapsed_r <- round(difftime(Sys.time(), t0_r, units = "mins"), 1)
  avg_time <- elapsed_r  # update below
  if (r > 1) {
    times_so_far <- sapply(1:r, function(i) 0)  # placeholder
  }
  cat(sprintf("(%.1f min) ──\n", elapsed_r))
}

total_time <- round(difftime(Sys.time(), t0_all, units = "mins"), 1)
cat(sprintf("\n=== All %d repeats complete in %.1f min ===\n\n", M_REPEATS, total_time))


# =============================================================================
# STEP 6: Pool residuals and run diagnostics
# =============================================================================
residual_panel <- bind_rows(all_residuals)
rm(all_residuals)

# Drop rows where fitted_asinh is NA (e.g., fold had no eligible sellers)
n_before <- nrow(residual_panel)
residual_panel <- residual_panel %>% filter(!is.na(fitted_asinh))
cat("Residual panel:", nrow(residual_panel), "rows (",
    n_before - nrow(residual_panel), "dropped with NA fitted values)\n")

residual_panel$resid_sq <- residual_panel$resid_asinh^2
residual_panel$sy <- paste0(residual_panel$nace2d, "_", residual_panel$year)

cat("Mean residual:", round(mean(residual_panel$resid_asinh), 4), "\n")
cat("SD residual:  ", round(sd(residual_panel$resid_asinh), 4), "\n\n")


# --- Test 1: Regress squared residuals on proxy and log_revenue, with SY FE ---
cat("=== Test 1: resid^2 ~ proxy + log_revenue + sector-year FE ===\n")

bp_fit <- lm(resid_sq ~ proxy_asinh + log_revenue + factor(sy),
             data = residual_panel)
bp_summary <- summary(bp_fit)

cat("  Coef on proxy:       ", signif(coef(bp_fit)["proxy_asinh"], 4),
    " (p =", signif(bp_summary$coefficients["proxy_asinh", 4], 3), ")\n")
cat("  Coef on log_revenue: ", signif(coef(bp_fit)["log_revenue"], 4),
    " (p =", signif(bp_summary$coefficients["log_revenue", 4], 3), ")\n")
cat("  R-squared:", round(bp_summary$r.squared, 4), "\n\n")

bp_regression <- data.frame(
  variable  = c("proxy_asinh", "log_revenue"),
  coef      = coef(bp_fit)[c("proxy_asinh", "log_revenue")],
  se        = bp_summary$coefficients[c("proxy_asinh", "log_revenue"), 2],
  t_stat    = bp_summary$coefficients[c("proxy_asinh", "log_revenue"), 3],
  p_value   = bp_summary$coefficients[c("proxy_asinh", "log_revenue"), 4],
  r_squared = bp_summary$r.squared
)
rm(bp_fit, bp_summary)


# --- Test 2: Binned variance by proxy quartile within SY (emitters only) ---
cat("=== Test 2: Binned variance — emitters with proxy > 0 ===\n")

emitters <- residual_panel %>% filter(emit == 1, proxy_asinh > 0)

binned_variance <- emitters %>%
  group_by(sy) %>%
  filter(n() >= 8) %>%
  mutate(proxy_quartile = ntile(proxy_asinh, 4)) %>%
  ungroup() %>%
  group_by(proxy_quartile) %>%
  summarise(
    n              = n(),
    mean_proxy     = mean(proxy_asinh),
    mean_resid_sq  = mean(resid_sq),
    sd_resid       = sd(resid_asinh),
    median_resid_sq = median(resid_sq),
    .groups = "drop"
  )

print(as.data.frame(binned_variance))

if (nrow(binned_variance) == 4) {
  var_ratio <- binned_variance$mean_resid_sq[4] / binned_variance$mean_resid_sq[1]
  cat(sprintf("\nVariance ratio Q4/Q1: %.2f\n", var_ratio))
  cat("(>1 means high-proxy firms have larger residual variance -> larger Jensen bias)\n\n")
}


# --- Test 3: Binned variance — all firms with proxy > 0 ---
cat("=== Test 3: Binned variance — all firms with proxy > 0 ===\n")

all_with_proxy <- residual_panel %>% filter(proxy_asinh > 0)

binned_variance_all <- all_with_proxy %>%
  group_by(sy) %>%
  filter(n() >= 8) %>%
  mutate(proxy_quartile = ntile(proxy_asinh, 4)) %>%
  ungroup() %>%
  group_by(proxy_quartile) %>%
  summarise(
    n              = n(),
    mean_proxy     = mean(proxy_asinh),
    mean_resid_sq  = mean(resid_sq),
    sd_resid       = sd(resid_asinh),
    .groups = "drop"
  )

print(as.data.frame(binned_variance_all))

if (nrow(binned_variance_all) == 4) {
  var_ratio_all <- binned_variance_all$mean_resid_sq[4] / binned_variance_all$mean_resid_sq[1]
  cat(sprintf("\nVariance ratio Q4/Q1 (all firms): %.2f\n", var_ratio_all))
}


# =============================================================================
# STEP 7: Save (small file — just diagnostics)
# =============================================================================
OUT_PATH <- file.path(PROC_DATA, "jensen_residual_diagnostics.RData")

save(bp_regression, binned_variance, binned_variance_all,
     file = OUT_PATH)

cat("\n================================================================\n")
cat("Saved to:", OUT_PATH, "\n")
cat("================================================================\n")
