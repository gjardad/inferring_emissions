###############################################################################
# analysis/model_selection/losocv/06_hurdle_proxy.R
#
# PURPOSE
#   Row 5 of the model selection table: hurdle × proportional allocation
#   by proxy (calibrated, no cap).
#
#   Within each sector-year cell, E_total is distributed proportionally
#   to proxy_weighted among predicted emitters (phat > tau). Firms with
#   phat <= tau get yhat = 0. This automatically satisfies calibration
#   to sector-year totals.
#
# INPUT
#   {OUTPUT_DIR}/model_selection/losocv/classifier_phat_{CLASSIFIER}.csv
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/row5_preds.csv
#   {OUTPUT_DIR}/model_selection/losocv/row5_metrics.csv
#   {OUTPUT_DIR}/model_selection/losocv/row5_threshold_sweep.csv
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
  REPO_DIR <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))

OUT_DIR <- file.path(OUTPUT_DIR, "model_selection", "losocv")


# ── User choice: which classifier? ──────────────────────────────────────────
CLASSIFIER <- "gam_enriched"


# ── Load inputs ──────────────────────────────────────────────────────────────
cat(sprintf("Loading classifier: %s\n", CLASSIFIER))
phat_df <- read.csv(file.path(OUT_DIR, sprintf("classifier_phat_%s.csv", CLASSIFIER)),
                    stringsAsFactors = FALSE)

cat("Loading training sample (for proxy_weighted)...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
proxy_df <- training_sample %>%
  transmute(vat, year, proxy_weighted, emit = as.integer(y > 0))
rm(training_sample)

# Merge
df <- inner_join(
  phat_df %>% select(vat, nace2d, year, y, emit, phat),
  proxy_df %>% select(vat, year, proxy_weighted),
  by = c("vat", "year")
)
cat(sprintf("Merged: %d firm-year observations\n", nrow(df)))


# ── Sector-year totals ──────────────────────────────────────────────────────
syt <- df %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# ── Threshold search ─────────────────────────────────────────────────────────
THRESHOLDS <- seq(0.01, 0.99, by = 0.01)

cat("Searching over thresholds (with proportional allocation)...\n")
sweep <- data.frame(
  threshold = THRESHOLDS,
  rmse      = NA_real_,
  nrmse_sd  = NA_real_,
  fpr       = NA_real_,
  tpr       = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(THRESHOLDS)) {
  tau <- THRESHOLDS[i]

  # Gate proxy by hurdle threshold
  yhat_raw <- pmax(as.numeric(df$phat > tau) * df$proxy_weighted, 0)

  # Proportional allocation: calibrate_predictions distributes E_total
  # proportionally to yhat_raw within each (nace2d, year) cell
  yhat_cal <- calibrate_predictions(yhat_raw, df$nace2d, df$year, syt)

  err <- df$y - yhat_cal
  sweep$rmse[i] <- sqrt(mean(err^2))
  sweep$nrmse_sd[i] <- sweep$rmse[i] / sd(df$y)

  is_emit <- (df$y > 0)
  pred_pos <- (yhat_cal > 0)
  n_non <- sum(!is_emit)
  n_emi <- sum(is_emit)
  sweep$fpr[i] <- if (n_non > 0) sum(pred_pos & !is_emit) / n_non else NA_real_
  sweep$tpr[i] <- if (n_emi > 0) sum(pred_pos & is_emit) / n_emi else NA_real_
}

best_idx <- which.min(sweep$rmse)
best_tau <- sweep$threshold[best_idx]
cat(sprintf("Best threshold: %.2f (RMSE=%.2f, nRMSE=%.3f)\n",
            best_tau, sweep$rmse[best_idx], sweep$nrmse_sd[best_idx]))


# ── Compute full metrics at best threshold ───────────────────────────────────
yhat_raw_best <- pmax(as.numeric(df$phat > best_tau) * df$proxy_weighted, 0)
yhat_cal_best <- calibrate_predictions(yhat_raw_best, df$nace2d, df$year, syt)

m <- calc_metrics(
  y      = df$y,
  yhat   = yhat_cal_best,
  nace2d = df$nace2d,
  year   = df$year
)

metrics_df <- data.frame(
  model      = sprintf("hurdle_proxy_%s", CLASSIFIER),
  row        = 5L,
  cv_scheme  = "losocv",
  classifier = CLASSIFIER,
  best_tau   = best_tau,
  n          = m$n,
  nrmse_sd   = m$nrmse_sd,
  median_apd = m$median_apd,
  apd_q25    = m$apd_q25,
  apd_q75    = m$apd_q75,
  rho_pooled_global = m$rho_pooled_global,
  rho_pooled        = m$rho_pooled,
  rho_pooled_min    = m$rho_pooled_min,
  rho_pooled_max    = m$rho_pooled_max,
  fpr_nonemitters   = m$fpr_nonemitters,
  tpr_emitters      = m$tpr_emitters,
  avg_nonemit_p50_rank = m$avg_nonemit_p50_rank,
  avg_nonemit_p99_rank = m$avg_nonemit_p99_rank,
  stringsAsFactors = FALSE
)


# ── Save ─────────────────────────────────────────────────────────────────────
# Save predictions with all components (needed by script 07)
preds_df <- df %>%
  transmute(
    vat, nace2d, year, y, emit, phat, proxy_weighted,
    yhat_raw = yhat_raw_best,
    yhat     = yhat_cal_best,
    best_tau = best_tau
  )

write.csv(preds_df, file.path(OUT_DIR, "row5_preds.csv"), row.names = FALSE)
write.csv(metrics_df, file.path(OUT_DIR, "row5_metrics.csv"), row.names = FALSE)
write.csv(sweep, file.path(OUT_DIR, "row5_threshold_sweep.csv"), row.names = FALSE)

cat("\n── Row 5: Hurdle × proxy (proportional allocation, no cap) ──\n")
cat(sprintf("  Classifier: %s  |  Threshold: %.2f\n", CLASSIFIER, best_tau))
cat(sprintf("  nRMSE:    %.3f\n", m$nrmse_sd))
cat(sprintf("  Med APD:  %.3f  [IQR: %.3f – %.3f]\n", m$median_apd, m$apd_q25, m$apd_q75))
cat(sprintf("  rho_g:    %.3f\n", m$rho_pooled_global))
cat(sprintf("  rho_s:    %.3f  [%.3f – %.3f]\n", m$rho_pooled, m$rho_pooled_min, m$rho_pooled_max))
cat(sprintf("  FPR:      %.3f   TPR: %.3f\n", m$fpr_nonemitters, m$tpr_emitters))
cat(sprintf("  p50 rank: %.3f   p99 rank: %.3f\n", m$avg_nonemit_p50_rank, m$avg_nonemit_p99_rank))
cat(sprintf("\nSaved to: %s\n", OUT_DIR))
