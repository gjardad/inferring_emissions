###############################################################################
# analysis/model_selection/losocv/07_calibrate_cap.R
#
# PURPOSE
#   Row 6 of the model selection table: take row 5's predictions (at row 5's
#   best threshold) and apply calibrate_with_cap() instead of plain
#   calibrate_predictions(). Isolates the value of capping false positives.
#
# INPUT
#   {OUTPUT_DIR}/model_selection/losocv/row5_preds.csv
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/row6_metrics.csv
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


# ── Load row 5 predictions ──────────────────────────────────────────────────
cat("Loading row 5 predictions...\n")
df <- read.csv(file.path(OUT_DIR, "row5_preds.csv"), stringsAsFactors = FALSE)
best_tau <- df$best_tau[1]

cat(sprintf("Loaded: %d observations, best_tau = %.2f\n", nrow(df), best_tau))


# ── Sector-year totals ──────────────────────────────────────────────────────
syt <- df %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# ── Apply calibrate_with_cap ─────────────────────────────────────────────────
cat("Applying calibrate_with_cap...\n")
yhat_cap <- calibrate_with_cap(
  yhat   = df$yhat_raw,
  emit   = df$emit,
  y      = df$y,
  nace2d = df$nace2d,
  year   = df$year,
  syt    = syt
)


# ── Compute metrics ──────────────────────────────────────────────────────────
m <- calc_metrics(
  y      = df$y,
  yhat   = yhat_cap,
  nace2d = df$nace2d,
  year   = df$year
)

metrics_df <- data.frame(
  model      = "hurdle_proxy_calibrate_cap",
  row        = 6L,
  cv_scheme  = "losocv",
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
write.csv(metrics_df, file.path(OUT_DIR, "row6_metrics.csv"), row.names = FALSE)

cat("\n── Row 6: Hurdle × proxy + calibrate_with_cap ──\n")
cat(sprintf("  Threshold: %.2f (inherited from row 5)\n", best_tau))
cat(sprintf("  nRMSE:    %.3f\n", m$nrmse_sd))
cat(sprintf("  Med APD:  %.3f  [IQR: %.3f – %.3f]\n", m$median_apd, m$apd_q25, m$apd_q75))
cat(sprintf("  rho_g:    %.3f\n", m$rho_pooled_global))
cat(sprintf("  rho_s:    %.3f  [%.3f – %.3f]\n", m$rho_pooled, m$rho_pooled_min, m$rho_pooled_max))
cat(sprintf("  FPR:      %.3f   TPR: %.3f\n", m$fpr_nonemitters, m$tpr_emitters))
cat(sprintf("  p50 rank: %.3f   p99 rank: %.3f\n", m$avg_nonemit_p50_rank, m$avg_nonemit_p99_rank))
cat(sprintf("\nSaved to: %s\n", OUT_DIR))
