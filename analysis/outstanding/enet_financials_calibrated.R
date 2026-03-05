###############################################################################
# analysis/nested_cv/enet_financials_calibrated.R
#
# PURPOSE
#   Exercise (e): Does calibrating the EN-on-financials predictions to
#   sector-year totals improve level accuracy?
#
#   Row 2 in models_with_fold_specific_proxy.R evaluates the EN raw predictions
#   without calibration. This script adds a calibrated version (Row 2b) using
#   calibrate_predictions(), making the comparison with proxy-based rows fairer
#   since those rows all use calibration.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/fold_specific_proxy.RData
#
# OUTPUT
#   Prints metrics for Row 2 (raw) and Row 2b (calibrated) side by side.
#   {OUTPUT_DIR}/nested_cv/enet_financials_calibrated.csv
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
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

cat("Loading fold-specific proxy...\n")
load(file.path(PROC_DATA, "fold_specific_proxy.RData"))

# Merge fold assignments
panel <- training_sample %>%
  left_join(fs_proxy_panel %>% select(vat, year, fold_k, primary_nace2d),
            by = c("vat", "year")) %>%
  mutate(emit = as.integer(y > 0))
rm(training_sample, fs_proxy_panel)

# Sector-year totals
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n\n")


# ── Helpers ──────────────────────────────────────────────────────────────────
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
# FIT EN ON FINANCIALS (same as Row 2 in models_with_fold_specific_proxy.R)
# =============================================================================
cat("═══ Fitting EN on financials (K=5 sector-fold CV) ═══\n")

ALPHA   <- 0.5
K_INNER <- 10L
SEED    <- 42L

# Financial covariates
v_cols <- grep("^v_[0-9]", names(panel), value = TRUE)
extra_fin <- intersect(c("turnover_VAT", "inputs_VAT", "investment_VAT"), names(panel))
fin_cols <- c(v_cols, extra_fin)
cat(sprintf("Financial covariates: %d columns\n", length(fin_cols)))

# Asinh-transform
X_fin <- as.matrix(panel[, fin_cols])
X_fin[is.na(X_fin)] <- 0
X_fin <- asinh(X_fin)
colnames(X_fin) <- paste0("fin_", fin_cols)

# Year and sector dummies
year_f <- factor(panel$year)
X_year <- model.matrix(~ year_f - 1)

nace2d_f <- factor(panel$nace2d)
X_nace <- model.matrix(~ nace2d_f - 1)

# Penalty factor: financials penalized, year/sector unpenalized
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

  X_train_full <- cbind(X_fin[train_idx, ], X_year[train_idx, ], X_nace[train_idx, ])
  X_test_full  <- cbind(X_fin[test_idx, ],  X_year[test_idx, ],  X_nace[test_idx, ])

  # Drop zero-variance columns
  col_var <- apply(X_train_full, 2, var)
  keep <- which(col_var > 0)
  X_train <- X_train_full[, keep, drop = FALSE]
  X_test  <- X_test_full[, keep, drop = FALSE]
  pf_fold <- penalty_factor[keep]

  inner_fid <- inner_foldid[train_idx]

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

rm(X_fin, X_year, X_nace, X_train_full, X_test_full, X_train, X_test)


# =============================================================================
# EVALUATE: Row 2 (raw) vs Row 2b (calibrated)
# =============================================================================
ok <- !is.na(panel$yhat_en)
cat(sprintf("\nEvaluating on %d firm-years with valid EN predictions\n", sum(ok)))

# Row 2: raw EN predictions (no calibration)
m2_raw <- calc_metrics(panel$y[ok], panel$yhat_en[ok],
                       nace2d = panel$nace2d[ok], year = panel$year[ok])
print_metrics("Row 2: EN on financials (raw, no calibration)", m2_raw)

# Row 2b: calibrated EN predictions
yhat_en_cal <- calibrate_predictions(
  panel$yhat_en[ok],
  panel$nace2d[ok],
  panel$year[ok],
  syt
)

m2_cal <- calc_metrics(panel$y[ok], yhat_en_cal,
                       nace2d = panel$nace2d[ok], year = panel$year[ok])
print_metrics("Row 2b: EN on financials (calibrated)", m2_cal)


# ── Comparison table ─────────────────────────────────────────────────────────
cat("\n\n══════════════ COMPARISON ══════════════\n")
comp <- data.frame(
  model = c("enet_raw", "enet_calibrated"),
  nrmse_sd = c(m2_raw$nrmse_sd, m2_cal$nrmse_sd),
  median_apd = c(m2_raw$median_apd, m2_cal$median_apd),
  rho_pooled_global = c(m2_raw$rho_pooled_global, m2_cal$rho_pooled_global),
  rho_pooled = c(m2_raw$rho_pooled, m2_cal$rho_pooled),
  fpr = c(m2_raw$fpr_nonemitters, m2_cal$fpr_nonemitters),
  tpr = c(m2_raw$tpr_emitters, m2_cal$tpr_emitters),
  stringsAsFactors = FALSE
)
print(comp, row.names = FALSE)
cat("════════════════════════════════════════\n")

# ── Save ─────────────────────────────────────────────────────────────────────
out_dir <- file.path(OUTPUT_DIR, "nested_cv")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(comp, file.path(out_dir, "enet_financials_calibrated.csv"), row.names = FALSE)
cat("\nSaved to:", file.path(out_dir, "enet_financials_calibrated.csv"), "\n")
