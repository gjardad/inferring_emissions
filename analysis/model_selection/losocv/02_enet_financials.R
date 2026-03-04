###############################################################################
# analysis/model_selection/losocv/02_enet_financials.R
#
# PURPOSE
#   Row 2 of the model selection table: elastic net predicting emissions
#   from the full set of ~220 annual accounts covariates (Trucost benchmark).
#   Evaluated under LOSOCV: hold out one NACE 2-digit sector at a time.
#
#   The EN uses family = "poisson" (log link, non-negative predictions),
#   consistent with the existing PPML approach. Year and sector dummies
#   are unpenalized; financial covariates are penalized.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/row2_preds.csv
#   {OUTPUT_DIR}/model_selection/losocv/row2_metrics.csv
#   {OUTPUT_DIR}/model_selection/losocv/row2_enet_summary.csv  (lambda, # coefs)
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
library(glmnet)
library(Matrix)
source(file.path(UTILS_DIR, "calc_metrics.R"))

OUT_DIR <- file.path(OUTPUT_DIR, "model_selection", "losocv")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)


# ── Hyperparameters ──────────────────────────────────────────────────────────
ALPHA      <- 0.5           # elastic net mixing (0 = ridge, 1 = lasso)
K_INNER    <- 10L           # inner CV folds for lambda selection
SEED       <- 42L


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

cat(sprintf("Panel: %d rows, %d firms\n", nrow(panel), n_distinct(panel$vat)))


# ── Identify financial covariates ────────────────────────────────────────────
# NBB annual accounts columns: v_XXXX_XX pattern + named financial vars
v_cols <- grep("^v_[0-9]", names(panel), value = TRUE)
extra_fin <- intersect(c("turnover_VAT", "inputs_VAT", "investment_VAT"), names(panel))
fin_cols <- c(v_cols, extra_fin)
cat(sprintf("Financial covariates: %d columns\n", length(fin_cols)))


# ── Build feature matrix components ──────────────────────────────────────────
# Financial covariates: asinh(coalesce(x, 0))
X_fin <- as.matrix(panel[, fin_cols])
X_fin[is.na(X_fin)] <- 0
X_fin <- asinh(X_fin)
colnames(X_fin) <- paste0("fin_", fin_cols)

# Year dummies
year_f <- factor(panel$year)
X_year <- model.matrix(~ year_f - 1)  # no intercept, glmnet adds its own

# NACE 2-digit dummies
nace2d_f <- factor(panel$nace2d)
X_nace <- model.matrix(~ nace2d_f - 1)

cat(sprintf("Feature matrix: %d financial + %d year + %d sector = %d total\n",
            ncol(X_fin), ncol(X_year), ncol(X_nace),
            ncol(X_fin) + ncol(X_year) + ncol(X_nace)))


# ── Penalty factor: financials penalized, year/sector unpenalized ────────────
penalty_factor <- c(
  rep(1, ncol(X_fin)),    # penalize financials
  rep(0, ncol(X_year)),   # unpenalize year dummies
  rep(0, ncol(X_nace))    # unpenalize sector dummies
)


# ── LOSOCV: assign firms to sectors ─────────────────────────────────────────
# Primary sector = most frequent NACE 2-digit across all years (same as fit_cv_losocv.R)
firm_sector <- panel %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

panel <- panel %>%
  left_join(firm_sector, by = "vat")

sector_levels <- sort(unique(firm_sector$primary_nace2d))
n_folds <- length(sector_levels)
cat(sprintf("LOSOCV: %d sector folds\n", n_folds))


# ── Inner CV fold assignment (firm-grouped, for cv.glmnet lambda selection) ──
set.seed(SEED)
unique_firms <- unique(panel$vat)
firm_inner_folds <- sample(rep(1:K_INNER, length.out = length(unique_firms)))
names(firm_inner_folds) <- unique_firms
inner_foldid <- unname(firm_inner_folds[panel$vat])


# ── Pre-allocate predictions ─────────────────────────────────────────────────
panel$yhat <- NA_real_


# ── Sector-year totals (for metrics) ────────────────────────────────────────
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# ── LOSOCV loop ──────────────────────────────────────────────────────────────
cat("\nRunning LOSOCV elastic net...\n")
t0_total <- Sys.time()
fold_summary <- list()

for (s in seq_along(sector_levels)) {
  sec <- sector_levels[s]
  cat(sprintf("  Fold %d/%d (sector %s) ...", s, n_folds, sec))
  t0 <- Sys.time()

  train_idx <- which(panel$primary_nace2d != sec)
  test_idx  <- which(panel$primary_nace2d == sec)

  # Build design matrix for this fold
  X_train <- cbind(X_fin[train_idx, ], X_year[train_idx, ], X_nace[train_idx, ])
  X_test  <- cbind(X_fin[test_idx, ],  X_year[test_idx, ],  X_nace[test_idx, ])
  y_train <- panel$y[train_idx]

  # Inner foldid for cv.glmnet (subset to training observations)
  inner_fid <- inner_foldid[train_idx]

  # Fit cv.glmnet with Poisson family
  fit <- tryCatch(
    cv.glmnet(
      x = X_train,
      y = y_train,
      family = "poisson",
      alpha = ALPHA,
      penalty.factor = penalty_factor,
      foldid = inner_fid,
      standardize = TRUE
    ),
    error = function(e) {
      cat(sprintf(" ERROR: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(fit)) {
    # Predict on held-out sector
    preds <- as.numeric(predict(fit, newx = X_test, s = "lambda.min", type = "response"))
    panel$yhat[test_idx] <- preds

    # Summary for this fold
    n_coef <- sum(coef(fit, s = "lambda.min")[-1] != 0)  # exclude intercept
    fold_summary[[s]] <- data.frame(
      sector = sec,
      n_train = length(train_idx),
      n_test = length(test_idx),
      lambda_min = fit$lambda.min,
      n_nonzero_coef = n_coef,
      stringsAsFactors = FALSE
    )
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" %d obs, %.1fs\n", length(test_idx), elapsed))
}

elapsed_total <- round(difftime(Sys.time(), t0_total, units = "mins"), 1)
cat(sprintf("\nLOSOCV complete (%.1f min)\n", elapsed_total))


# ── Compute metrics ──────────────────────────────────────────────────────────
cat("Computing metrics...\n")
ok <- !is.na(panel$yhat)
m <- calc_metrics(
  y      = panel$y[ok],
  yhat   = panel$yhat[ok],
  nace2d = panel$nace2d[ok],
  year   = panel$year[ok]
)

metrics_df <- data.frame(
  model      = "enet_financials",
  row        = 2L,
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
  panel %>% filter(ok) %>% select(vat, nace2d, year, y, yhat),
  file.path(OUT_DIR, "row2_preds.csv"),
  row.names = FALSE
)

write.csv(metrics_df, file.path(OUT_DIR, "row2_metrics.csv"), row.names = FALSE)

if (length(fold_summary) > 0) {
  write.csv(
    bind_rows(fold_summary),
    file.path(OUT_DIR, "row2_enet_summary.csv"),
    row.names = FALSE
  )
}

cat("\n── Row 2: Elastic net on financials (Trucost benchmark) ──\n")
cat(sprintf("  nRMSE:    %.3f\n", m$nrmse_sd))
cat(sprintf("  Med APD:  %.3f  [IQR: %.3f – %.3f]\n", m$median_apd, m$apd_q25, m$apd_q75))
cat(sprintf("  rho_g:    %.3f\n", m$rho_pooled_global))
cat(sprintf("  rho_s:    %.3f  [%.3f – %.3f]\n", m$rho_pooled, m$rho_pooled_min, m$rho_pooled_max))
cat(sprintf("  FPR:      %.3f   TPR: %.3f\n", m$fpr_nonemitters, m$tpr_emitters))
cat(sprintf("  p50 rank: %.3f   p99 rank: %.3f\n", m$avg_nonemit_p50_rank, m$avg_nonemit_p99_rank))
cat(sprintf("\nSaved to: %s\n", OUT_DIR))
