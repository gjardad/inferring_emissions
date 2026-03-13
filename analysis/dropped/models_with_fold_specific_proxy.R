###############################################################################
# analysis/nested_cv/models_with_fold_specific_proxy.R
#
# PURPOSE
#   Produce the main results table (5 rows) evaluating prediction models for
#   firm-level emissions. All rows use the K=5 sector-fold structure from
#   build_fold_specific_proxy.R for consistency.
#
#   Row 1: Revenue-proportional allocation (Trucost/EEIO baseline)
#   Row 2: Elastic net on annual accounts covariates (ML literature benchmark)
#   Row 3: Proxy-proportional allocation (no hurdle)
#   Row 4: Proxy-proportional + cross-sector percentile hurdle (sectors 19/24)
#   Row 5: Row 4 + clipping (calibrate_with_cap)
#
# INPUT
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/main_results_table.csv
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
REPO_DIR <- "c:/Users/jota_/Documents/inferring_emissions"
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(glmnet)
library(Matrix)

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading firm-year panel with proxies...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

panel <- training_sample %>%
  rename(
    fold_specific_proxy     = fold_specific_proxy_asinh,
    fold_specific_proxy_all = fold_specific_proxy_all_asinh
  )
rm(training_sample)

# Revenue: prefer turnover_VAT (raw), fallback to exp(log_revenue)
if ("turnover_VAT" %in% names(panel)) {
  panel$revenue <- coalesce(panel$turnover_VAT, exp(panel$log_revenue))
} else {
  panel$revenue <- exp(panel$log_revenue)
}

# Recreate syt if not loaded
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

# Check fold assignment
n_no_fold <- sum(is.na(panel$fold_k))
if (n_no_fold > 0) {
  warning(sprintf("%d firm-years have no fold assignment. Dropping from Row 2.", n_no_fold))
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("fold_specific_proxy > 0:", sum(panel$fold_specific_proxy > 0),
    sprintf("(%.1f%%)\n\n", 100 * mean(panel$fold_specific_proxy > 0)))


# ── Helpers ──────────────────────────────────────────────────────────────────
make_result_row <- function(model_name, m) {
  data.frame(
    model                = model_name,
    nrmse_sd             = m$nrmse_sd,
    median_apd           = m$median_apd,
    apd_q25              = m$apd_q25,
    apd_q75              = m$apd_q75,
    rho_pooled_global    = m$rho_pooled_global,
    rho_pooled           = m$rho_pooled,
    rho_pooled_min       = m$rho_pooled_min,
    rho_pooled_max       = m$rho_pooled_max,
    fpr_nonemitters      = m$fpr_nonemitters,
    tpr_emitters         = m$tpr_emitters,
    avg_nonemit_p50_rank = m$avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = m$avg_nonemit_p99_rank,
    stringsAsFactors     = FALSE
  )
}

print_metrics <- function(label, m) {
  cat(sprintf("\n── %s ──\n", label))
  cat(sprintf("  nRMSE:    %.3f\n", m$nrmse_sd))
  cat(sprintf("  Med APD:  %.3f  [IQR: %.3f – %.3f]\n", m$median_apd, m$apd_q25, m$apd_q75))
  cat(sprintf("  rho_g:    %.3f\n", m$rho_pooled_global))
  cat(sprintf("  rho_s:    %.3f  [%.3f – %.3f]\n", m$rho_pooled, m$rho_pooled_min, m$rho_pooled_max))
  cat(sprintf("  FPR:      %.3f   TPR: %.3f\n", m$fpr_nonemitters, m$tpr_emitters))
  cat(sprintf("  FP p50:   %.3f   FP p99: %.3f\n", m$avg_nonemit_p50_rank, m$avg_nonemit_p99_rank))
}


# =============================================================================
# ROW 1: Revenue-proportional allocation (Trucost/EEIO baseline)
# =============================================================================
cat("═══ ROW 1: Revenue-proportional allocation ═══\n")

yhat_row1 <- calibrate_predictions(
  pmax(panel$revenue, 0, na.rm = TRUE),
  panel$nace2d,
  panel$year,
  syt
)

m1 <- calc_metrics(panel$y, yhat_row1, nace2d = panel$nace2d, year = panel$year)
row1 <- make_result_row("revenue_proportional", m1)
print_metrics("Row 1: Revenue-proportional", m1)


# =============================================================================
# ROW 2: Elastic net on financials (ML literature benchmark, NO calibration)
# =============================================================================
cat("\n═══ ROW 2: Elastic net on annual accounts covariates ═══\n")

ALPHA   <- 0.5
K_INNER <- 10L
SEED    <- 42L

# Identify financial covariates
v_cols <- grep("^v_[0-9]", names(panel), value = TRUE)
extra_fin <- intersect(c("turnover_VAT", "inputs_VAT", "investment_VAT"), names(panel))
fin_cols <- c(v_cols, extra_fin)
cat(sprintf("Financial covariates: %d columns\n", length(fin_cols)))

# Asinh-transform financial covariates
X_fin <- as.matrix(panel[, fin_cols])
X_fin[is.na(X_fin)] <- 0
X_fin <- asinh(X_fin)
colnames(X_fin) <- paste0("fin_", fin_cols)

# Year dummies
year_f <- factor(panel$year)
X_year <- model.matrix(~ year_f - 1)

# NACE 2-digit dummies
nace2d_f <- factor(panel$nace2d)
X_nace <- model.matrix(~ nace2d_f - 1)

cat(sprintf("Feature matrix: %d financial + %d year + %d sector = %d total\n",
            ncol(X_fin), ncol(X_year), ncol(X_nace),
            ncol(X_fin) + ncol(X_year) + ncol(X_nace)))

# Penalty factor
penalty_factor <- c(rep(1, ncol(X_fin)), rep(0, ncol(X_year)), rep(0, ncol(X_nace)))

# Inner CV fold assignment (firm-grouped)
set.seed(SEED)
unique_firms <- unique(panel$vat)
firm_inner_folds <- sample(rep(1:K_INNER, length.out = length(unique_firms)))
names(firm_inner_folds) <- unique_firms
inner_foldid <- unname(firm_inner_folds[panel$vat])

# K=5 sector-fold loop
panel$yhat_en <- NA_real_
t0_total <- Sys.time()

for (k in sort(unique(na.omit(panel$fold_k)))) {
  held_out_sectors <- sector_fold_map$nace2d[sector_fold_map$fold_k == k]
  cat(sprintf("  Fold %d (sectors: %s) ...", k, paste(held_out_sectors, collapse = ", ")))
  t0 <- Sys.time()

  train_idx <- which(!(panel$primary_nace2d %in% held_out_sectors) & !is.na(panel$fold_k))
  test_idx  <- which(panel$primary_nace2d %in% held_out_sectors & !is.na(panel$fold_k))

  # Build design matrix
  X_train_full <- cbind(X_fin[train_idx, ], X_year[train_idx, ], X_nace[train_idx, ])
  X_test_full  <- cbind(X_fin[test_idx, ],  X_year[test_idx, ],  X_nace[test_idx, ])

  # Drop zero-variance columns
  col_var <- apply(X_train_full, 2, var)
  keep <- which(col_var > 0)
  X_train <- X_train_full[, keep, drop = FALSE]
  X_test  <- X_test_full[, keep, drop = FALSE]
  pf_fold <- penalty_factor[keep]

  inner_fid <- inner_foldid[train_idx]

  # Fit: Gaussian EN on asinh(y)
  fit <- tryCatch(
    cv.glmnet(
      x = X_train,
      y = asinh(panel$y[train_idx]),
      family = "gaussian",
      alpha = ALPHA,
      penalty.factor = pf_fold,
      foldid = inner_fid,
      standardize = TRUE
    ),
    error = function(e) {
      cat(sprintf(" ERROR: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(fit)) {
    raw_preds <- as.numeric(predict(fit, newx = X_test, s = "lambda.min"))
    panel$yhat_en[test_idx] <- pmax(sinh(raw_preds), 0)
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" %d train, %d test, %.1fs\n", length(train_idx), length(test_idx), elapsed))
}

elapsed_total <- round(difftime(Sys.time(), t0_total, units = "mins"), 1)
cat(sprintf("EN complete (%.1f min)\n", elapsed_total))

# Clean up large matrices
rm(X_fin, X_year, X_nace, X_train_full, X_test_full, X_train, X_test)

# Metrics: raw predictions, NO calibration
ok_en <- !is.na(panel$yhat_en)
m2 <- calc_metrics(panel$y[ok_en], panel$yhat_en[ok_en],
                   nace2d = panel$nace2d[ok_en], year = panel$year[ok_en])
row2 <- make_result_row("enet_financials", m2)
print_metrics("Row 2: EN on financials (raw, no calibration)", m2)


# =============================================================================
# ROW 3: Proxy-proportional allocation (no hurdle)
# =============================================================================
cat("\n═══ ROW 3: Proxy-proportional allocation ═══\n")

yhat_row3 <- calibrate_predictions(
  panel$fold_specific_proxy,
  panel$nace2d,
  panel$year,
  syt
)

m3 <- calc_metrics(panel$y, yhat_row3, nace2d = panel$nace2d, year = panel$year)
row3 <- make_result_row("proxy_proportional", m3)
print_metrics("Row 3: Proxy-proportional", m3)


# =============================================================================
# ROW 4: Proxy-proportional + cross-sector percentile hurdle (sectors 19/24)
# =============================================================================
cat("\n═══ ROW 4: Proxy-proportional + cross-sector percentile hurdle ═══\n")

# Helper: find percentile threshold that maximizes Youden's J (TPR - FPR)
learn_percentile_threshold <- function(proxy_vals, emit_vals) {
  pctile_ranks <- ecdf(proxy_vals)(proxy_vals)

  cutoffs <- seq(0.01, 0.99, by = 0.01)
  best_youden <- -Inf
  best_p      <- NA_real_
  best_tpr    <- NA_real_
  best_fpr    <- NA_real_

  n_emit    <- sum(emit_vals == 1)
  n_nonemit <- sum(emit_vals == 0)

  for (p in cutoffs) {
    pred_emit <- as.integer(pctile_ranks >= p)
    tpr <- if (n_emit > 0)    sum(pred_emit == 1 & emit_vals == 1) / n_emit    else NA_real_
    fpr <- if (n_nonemit > 0) sum(pred_emit == 1 & emit_vals == 0) / n_nonemit else NA_real_

    youden <- tpr - fpr
    if (!is.na(youden) && youden > best_youden) {
      best_youden <- youden
      best_p      <- p
      best_tpr    <- tpr
      best_fpr    <- fpr
    }
  }

  list(threshold = best_p, youden = best_youden, tpr = best_tpr, fpr = best_fpr)
}

# Learn thresholds from sectors 24 and 19
sec19_idx <- which(panel$nace2d == "19")
sec24_idx <- which(panel$nace2d == "24")

tau_from_24 <- learn_percentile_threshold(panel$fold_specific_proxy[sec24_idx],
                                           panel$emit[sec24_idx])
tau_from_19 <- learn_percentile_threshold(panel$fold_specific_proxy[sec19_idx],
                                           panel$emit[sec19_idx])

cat(sprintf("  Threshold from sector 24: p* = %.2f (Youden = %.3f, TPR = %.3f, FPR = %.3f)\n",
            tau_from_24$threshold, tau_from_24$youden, tau_from_24$tpr, tau_from_24$fpr))
cat(sprintf("  Threshold from sector 19: p* = %.2f (Youden = %.3f, TPR = %.3f, FPR = %.3f)\n",
            tau_from_19$threshold, tau_from_19$youden, tau_from_19$tpr, tau_from_19$fpr))

# Cross-sector validation: apply each threshold to the other sector
cross_19 <- {
  pctile_19 <- ecdf(panel$fold_specific_proxy[sec19_idx])(panel$fold_specific_proxy[sec19_idx])
  pred_emit_19 <- as.integer(pctile_19 >= tau_from_24$threshold)
  tpr_19 <- sum(pred_emit_19 == 1 & panel$emit[sec19_idx] == 1) / max(sum(panel$emit[sec19_idx] == 1), 1)
  fpr_19 <- sum(pred_emit_19 == 1 & panel$emit[sec19_idx] == 0) / max(sum(panel$emit[sec19_idx] == 0), 1)
  list(tpr = tpr_19, fpr = fpr_19)
}

cross_24 <- {
  pctile_24 <- ecdf(panel$fold_specific_proxy[sec24_idx])(panel$fold_specific_proxy[sec24_idx])
  pred_emit_24 <- as.integer(pctile_24 >= tau_from_19$threshold)
  tpr_24 <- sum(pred_emit_24 == 1 & panel$emit[sec24_idx] == 1) / max(sum(panel$emit[sec24_idx] == 1), 1)
  fpr_24 <- sum(pred_emit_24 == 1 & panel$emit[sec24_idx] == 0) / max(sum(panel$emit[sec24_idx] == 0), 1)
  list(tpr = tpr_24, fpr = fpr_24)
}

cat(sprintf("  Cross-sector: tau_from_24 on sector 19 → TPR = %.3f, FPR = %.3f\n",
            cross_19$tpr, cross_19$fpr))
cat(sprintf("  Cross-sector: tau_from_19 on sector 24 → TPR = %.3f, FPR = %.3f\n",
            cross_24$tpr, cross_24$fpr))

# Apply thresholds
panel$thresholded_proxy <- panel$fold_specific_proxy

# Sector 19: apply threshold learned from sector 24
if (length(sec19_idx) > 0) {
  pctile_19 <- ecdf(panel$fold_specific_proxy[sec19_idx])(panel$fold_specific_proxy[sec19_idx])
  panel$thresholded_proxy[sec19_idx] <- ifelse(
    pctile_19 >= tau_from_24$threshold,
    panel$fold_specific_proxy[sec19_idx],
    0
  )
}

# Sector 24: apply threshold learned from sector 19
if (length(sec24_idx) > 0) {
  pctile_24 <- ecdf(panel$fold_specific_proxy[sec24_idx])(panel$fold_specific_proxy[sec24_idx])
  panel$thresholded_proxy[sec24_idx] <- ifelse(
    pctile_24 >= tau_from_19$threshold,
    panel$fold_specific_proxy[sec24_idx],
    0
  )
}

# Other sectors: no hurdle (proxy > 0 is natural condition)

# Calibrate
yhat_row4 <- calibrate_predictions(
  panel$thresholded_proxy,
  panel$nace2d,
  panel$year,
  syt
)

m4 <- calc_metrics(panel$y, yhat_row4, nace2d = panel$nace2d, year = panel$year)
row4 <- make_result_row("proxy_hurdle", m4)
print_metrics("Row 4: Proxy + cross-sector hurdle", m4)


# =============================================================================
# ROW 5: Row 4 + clipping (calibrate_with_cap)
# =============================================================================
cat("\n═══ ROW 5: Proxy + hurdle + cap ═══\n")

yhat_row5 <- calibrate_with_cap(
  panel$thresholded_proxy,
  panel$emit,
  panel$y,
  panel$nace2d,
  panel$year,
  syt
)

m5 <- calc_metrics(panel$y, yhat_row5, nace2d = panel$nace2d, year = panel$year)
row5 <- make_result_row("proxy_hurdle_cap", m5)
print_metrics("Row 5: Proxy + hurdle + cap", m5)


# =============================================================================
# ROWS 3a–5a: Repeat rows 3–5 with fold_specific_proxy_all (pos + neg coeffs)
# =============================================================================

# ── ROW 3a: Proxy-proportional (all coefficients) ──
cat("\n═══ ROW 3a: Proxy-proportional (all coefficients) ═══\n")

yhat_row3a <- calibrate_predictions(
  panel$fold_specific_proxy_all,
  panel$nace2d,
  panel$year,
  syt
)

m3a <- calc_metrics(panel$y, yhat_row3a, nace2d = panel$nace2d, year = panel$year)
row3a <- make_result_row("proxy_proportional_all", m3a)
print_metrics("Row 3a: Proxy-proportional (all)", m3a)


# ── ROW 4a: Proxy (all) + cross-sector percentile hurdle ──
cat("\n═══ ROW 4a: Proxy (all) + cross-sector percentile hurdle ═══\n")

tau_all_from_24 <- learn_percentile_threshold(panel$fold_specific_proxy_all[sec24_idx],
                                               panel$emit[sec24_idx])
tau_all_from_19 <- learn_percentile_threshold(panel$fold_specific_proxy_all[sec19_idx],
                                               panel$emit[sec19_idx])

cat(sprintf("  Threshold from sector 24: p* = %.2f (Youden = %.3f, TPR = %.3f, FPR = %.3f)\n",
            tau_all_from_24$threshold, tau_all_from_24$youden, tau_all_from_24$tpr, tau_all_from_24$fpr))
cat(sprintf("  Threshold from sector 19: p* = %.2f (Youden = %.3f, TPR = %.3f, FPR = %.3f)\n",
            tau_all_from_19$threshold, tau_all_from_19$youden, tau_all_from_19$tpr, tau_all_from_19$fpr))

panel$thresholded_proxy_all <- panel$fold_specific_proxy_all

if (length(sec19_idx) > 0) {
  pctile_19a <- ecdf(panel$fold_specific_proxy_all[sec19_idx])(panel$fold_specific_proxy_all[sec19_idx])
  panel$thresholded_proxy_all[sec19_idx] <- ifelse(
    pctile_19a >= tau_all_from_24$threshold,
    panel$fold_specific_proxy_all[sec19_idx],
    0
  )
}

if (length(sec24_idx) > 0) {
  pctile_24a <- ecdf(panel$fold_specific_proxy_all[sec24_idx])(panel$fold_specific_proxy_all[sec24_idx])
  panel$thresholded_proxy_all[sec24_idx] <- ifelse(
    pctile_24a >= tau_all_from_19$threshold,
    panel$fold_specific_proxy_all[sec24_idx],
    0
  )
}

yhat_row4a <- calibrate_predictions(
  panel$thresholded_proxy_all,
  panel$nace2d,
  panel$year,
  syt
)

m4a <- calc_metrics(panel$y, yhat_row4a, nace2d = panel$nace2d, year = panel$year)
row4a <- make_result_row("proxy_hurdle_all", m4a)
print_metrics("Row 4a: Proxy (all) + cross-sector hurdle", m4a)


# ── ROW 5a: Proxy (all) + hurdle + cap ──
cat("\n═══ ROW 5a: Proxy (all) + hurdle + cap ═══\n")

yhat_row5a <- calibrate_with_cap(
  panel$thresholded_proxy_all,
  panel$emit,
  panel$y,
  panel$nace2d,
  panel$year,
  syt
)

m5a <- calc_metrics(panel$y, yhat_row5a, nace2d = panel$nace2d, year = panel$year)
row5a <- make_result_row("proxy_hurdle_cap_all", m5a)
print_metrics("Row 5a: Proxy (all) + hurdle + cap", m5a)


# =============================================================================
# ASSEMBLE AND SAVE
# =============================================================================
results <- bind_rows(row1, row2, row3, row4, row5, row3a, row4a, row5a)
results$row <- seq_len(nrow(results))

cat("\n\n══════════════ MAIN RESULTS TABLE ══════════════\n")
print(results %>%
        select(row, model, nrmse_sd, median_apd, rho_pooled_global,
               fpr_nonemitters, tpr_emitters, avg_nonemit_p50_rank),
      row.names = FALSE)
cat("════════════════════════════════════════════════\n")

# Save
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, "main_results_table.csv")
write.csv(results, out_path, row.names = FALSE)
cat("\nResults saved to:", out_path, "\n")
