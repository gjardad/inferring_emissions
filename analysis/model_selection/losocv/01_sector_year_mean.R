###############################################################################
# analysis/model_selection/losocv/01_sector_year_mean.R
#
# PURPOSE
#   Row 1 of the model selection table: naive baseline.
#   Assign each firm its sector-year mean emission (E_total / n_firms).
#   No CV needed — no model is learned.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/row1_preds.csv
#   {OUTPUT_DIR}/model_selection/losocv/row1_metrics.csv
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

OUT_DIR <- file.path(OUTPUT_DIR, "model_selection", "losocv")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

cat(sprintf("Panel: %d rows, %d firms\n", nrow(panel), n_distinct(panel$vat)))


# ── Compute sector-year totals ───────────────────────────────────────────────
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# ── Assign sector-year mean as prediction ────────────────────────────────────
panel <- panel %>%
  left_join(syt, by = c("nace2d", "year")) %>%
  mutate(yhat = E_total / n_full)


# ── Compute metrics ──────────────────────────────────────────────────────────
cat("Computing metrics...\n")
m <- calc_metrics(
  y     = panel$y,
  yhat  = panel$yhat,
  nace2d = panel$nace2d,
  year   = panel$year
)

metrics_df <- data.frame(
  model      = "sector_year_mean",
  row        = 1L,
  cv_scheme  = "losocv",
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
write.csv(
  panel %>% select(vat, nace2d, year, y, yhat),
  file.path(OUT_DIR, "row1_preds.csv"),
  row.names = FALSE
)

write.csv(metrics_df, file.path(OUT_DIR, "row1_metrics.csv"), row.names = FALSE)

cat("\n── Row 1: Sector-year mean ──\n")
cat(sprintf("  nRMSE:    %.3f\n", m$nrmse_sd))
cat(sprintf("  Med APD:  %.3f  [IQR: %.3f – %.3f]\n", m$median_apd, m$apd_q25, m$apd_q75))
cat(sprintf("  rho_g:    %.3f\n", m$rho_pooled_global))
cat(sprintf("  rho_s:    %.3f  [%.3f – %.3f]\n", m$rho_pooled, m$rho_pooled_min, m$rho_pooled_max))
cat(sprintf("  FPR:      %.3f   TPR: %.3f\n", m$fpr_nonemitters, m$tpr_emitters))
cat(sprintf("  p50 rank: %.3f   p99 rank: %.3f\n", m$avg_nonemit_p50_rank, m$avg_nonemit_p99_rank))
cat(sprintf("\nSaved to: %s\n", OUT_DIR))
