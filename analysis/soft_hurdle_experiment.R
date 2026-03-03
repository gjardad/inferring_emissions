###############################################################################
# analysis/soft_hurdle_experiment.R
#
# PURPOSE
#   Compare the current hard-threshold hurdle (yhat = I(phat > tau) * muhat)
#   against a soft version (yhat = phat * muhat) that preserves continuous
#   ranking information. Both use calibrate_with_cap post-hoc.
#
#   Reuses the same LOFOCV out-of-fold phat and muhat from the hard-threshold
#   specification, so the only difference is the aggregation step.
#
# INPUT
#   {PROC_DATA}/training_sample.RData  (must include proxy_weighted)
#
# OUTPUT
#   Console comparison of metrics
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(mgcv)

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

panel <- panel %>%
  mutate(
    year_f   = factor(year),
    nace2d_f = factor(nace2d),
    emit     = as.integer(y > 0),
    log_revenue = ifelse(is.na(log_revenue), 0, log_revenue)
  )

# Recreate foldid if absent
if (!exists("K_FOLDS")) K_FOLDS <- 10L
if (!exists("foldid")) {
  set.seed(42)
  unique_firms <- unique(panel$vat)
  firm_folds <- sample(rep(1:K_FOLDS, length.out = length(unique_firms)))
  names(firm_folds) <- unique_firms
  foldid <- unname(firm_folds[panel$vat])
  cat("(foldid recreated from seed)\n")
}
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n\n")


# ===========================================================================
# LOFOCV: get out-of-fold phat and muhat (same model as main spec)
# ===========================================================================
re_base <- "year_f + s(nace2d_f, bs = 're')"

panel$phat  <- NA_real_
panel$muhat <- NA_real_

cat("Running LOFOCV for hurdle (K =", K_FOLDS, ")...\n")
for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)
  train_emit <- train[train$emit == 1, ]

  # Extensive margin: logit
  fit_ext <- tryCatch(
    gam(as.formula(paste("emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +", re_base)),
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit_ext)) {
    phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
    panel$phat[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  # Intensive margin: Poisson on emitters
  if (nrow(train_emit) > 0) {
    fit_int <- tryCatch(
      gam(as.formula(paste("y ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +", re_base)),
          data = train_emit, family = poisson(link = "log"), method = "REML"),
      error = function(e) NULL
    )
    if (!is.null(fit_int)) {
      muhat <- pmax(as.numeric(predict(fit_int, newdata = test, type = "response")), 0)
      panel$muhat[test_idx] <- muhat
    }
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" done (%.1fs)\n", elapsed))
}

ok <- !is.na(panel$phat) & !is.na(panel$muhat) & !is.na(panel$y)
cat("\nValid obs:", sum(ok), "/", nrow(panel), "\n")


# ===========================================================================
# Version A: Hard threshold (current main spec, threshold = 0.48)
# ===========================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("Version A: Hard threshold (tau = 0.48)\n")
cat("══════════════════════════════════════════════════════════\n")

yhat_hard_raw <- ifelse(panel$phat > 0.48, 1, 0) * panel$muhat
yhat_hard <- rep(NA_real_, nrow(panel))
yhat_hard[ok] <- calibrate_with_cap(yhat_hard_raw[ok], panel$emit[ok], panel$y[ok],
                                     panel$nace2d[ok], panel$year[ok], syt)
m_hard <- calc_metrics(panel$y[ok], yhat_hard[ok],
                       nace2d = panel$nace2d[ok], year = panel$year[ok])

cat("  nRMSE:    ", round(m_hard$nrmse_sd, 4), "\n")
cat("  Med. APD: ", round(m_hard$median_apd, 4), "\n")
cat("  Spearman: ", round(m_hard$spearman, 4), "\n")
cat("  rho_s:    ", round(m_hard$rho_pooled, 4),
    sprintf(" [min: %.3f, max: %.3f]\n", m_hard$rho_pooled_min, m_hard$rho_pooled_max))
cat("  FPR:      ", round(m_hard$fpr_nonemitters, 4), "\n")
cat("  TPR:      ", round(m_hard$tpr_emitters, 4), "\n")


# ===========================================================================
# Version B: Soft hurdle (yhat = phat * muhat, no threshold)
# ===========================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("Version B: Soft hurdle (yhat = phat * muhat)\n")
cat("══════════════════════════════════════════════════════════\n")

yhat_soft_raw <- panel$phat * panel$muhat
yhat_soft <- rep(NA_real_, nrow(panel))
yhat_soft[ok] <- calibrate_with_cap(yhat_soft_raw[ok], panel$emit[ok], panel$y[ok],
                                     panel$nace2d[ok], panel$year[ok], syt)
m_soft <- calc_metrics(panel$y[ok], yhat_soft[ok],
                       nace2d = panel$nace2d[ok], year = panel$year[ok])

cat("  nRMSE:    ", round(m_soft$nrmse_sd, 4), "\n")
cat("  Med. APD: ", round(m_soft$median_apd, 4), "\n")
cat("  Spearman: ", round(m_soft$spearman, 4), "\n")
cat("  rho_s:    ", round(m_soft$rho_pooled, 4), "\n")
cat("  FPR:      ", round(m_soft$fpr_nonemitters, 4), "\n")
cat("  TPR:      ", round(m_soft$tpr_emitters, 4), "\n")


# ===========================================================================
# Version C: Sector-specific proxy slope (random slope by NACE 2d)
# ===========================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("Version C: Sector-specific proxy slope + hard threshold\n")
cat("══════════════════════════════════════════════════════════\n")

re_slope <- "year_f + s(nace2d_f, bs = 're') + s(nace2d_f, by = asinh(proxy_weighted), bs = 're')"

panel$phat_slope  <- NA_real_
panel$muhat_slope <- NA_real_

cat("Running LOFOCV for sector-slope hurdle (K =", K_FOLDS, ")...\n")
for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)
  train_emit <- train[train$emit == 1, ]

  # Extensive margin: logit with sector-specific proxy slope
  fit_ext <- tryCatch(
    gam(as.formula(paste("emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +", re_slope)),
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit_ext)) {
    phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
    panel$phat_slope[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  # Intensive margin: Poisson with sector-specific proxy slope
  if (nrow(train_emit) > 0) {
    fit_int <- tryCatch(
      gam(as.formula(paste("y ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +", re_slope)),
          data = train_emit, family = poisson(link = "log"), method = "REML"),
      error = function(e) NULL
    )
    if (!is.null(fit_int)) {
      muhat <- pmax(as.numeric(predict(fit_int, newdata = test, type = "response")), 0)
      panel$muhat_slope[test_idx] <- muhat
    }
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" done (%.1fs)\n", elapsed))
}

# Threshold search
cat("Searching optimal threshold...\n")
THRESHOLDS <- seq(0.01, 0.60, by = 0.01)
best_thr_slope <- 0.5
best_nrmse_slope <- Inf

for (thr in THRESHOLDS) {
  yhat_raw <- ifelse(panel$phat_slope > thr, 1, 0) * panel$muhat_slope
  ok_s <- !is.na(yhat_raw) & !is.na(panel$y)
  if (sum(ok_s) == 0) next
  yhat_cal <- calibrate_with_cap(yhat_raw[ok_s], panel$emit[ok_s], panel$y[ok_s],
                                  panel$nace2d[ok_s], panel$year[ok_s], syt)
  nrmse <- sqrt(mean((panel$y[ok_s] - yhat_cal)^2)) / sd(panel$y[ok_s])
  if (nrmse < best_nrmse_slope) {
    best_nrmse_slope <- nrmse
    best_thr_slope <- thr
  }
}
cat("  Best threshold:", best_thr_slope, "-> nRMSE:", round(best_nrmse_slope, 4), "\n")

yhat_slope_raw <- ifelse(panel$phat_slope > best_thr_slope, 1, 0) * panel$muhat_slope
ok_slope <- !is.na(yhat_slope_raw) & !is.na(panel$y)
yhat_slope <- rep(NA_real_, nrow(panel))
yhat_slope[ok_slope] <- calibrate_with_cap(yhat_slope_raw[ok_slope], panel$emit[ok_slope],
                                            panel$y[ok_slope], panel$nace2d[ok_slope],
                                            panel$year[ok_slope], syt)
m_slope <- calc_metrics(panel$y[ok_slope], yhat_slope[ok_slope],
                        nace2d = panel$nace2d[ok_slope], year = panel$year[ok_slope])

cat("  nRMSE:    ", round(m_slope$nrmse_sd, 4), "\n")
cat("  Med. APD: ", round(m_slope$median_apd, 4), "\n")
cat("  Spearman: ", round(m_slope$spearman, 4), "\n")
cat("  rho_s:    ", round(m_slope$rho_pooled, 4), "\n")
cat("  FPR:      ", round(m_slope$fpr_nonemitters, 4), "\n")
cat("  TPR:      ", round(m_slope$tpr_emitters, 4), "\n")


# ===========================================================================
# Version D1: Hybrid — base hurdle classifies, proxy ranks
#   Uses phat from Version A (base model) for classification.
#   Among predicted emitters, allocate proportional to proxy_weighted
#   instead of muhat. calibrate_with_cap handles the rest.
# ===========================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("Version D1: Hybrid (base hurdle classifies, proxy ranks)\n")
cat("══════════════════════════════════════════════════════════\n")

# Threshold search (optimal tau may differ when ranking signal changes)
cat("Searching optimal threshold for D1...\n")
best_thr_d1 <- 0.48
best_nrmse_d1 <- Inf

for (thr in THRESHOLDS) {
  yhat_raw <- ifelse(panel$phat > thr, 1, 0) * panel$proxy_weighted
  ok_d <- !is.na(yhat_raw) & !is.na(panel$y) & ok
  if (sum(ok_d) == 0) next
  yhat_cal <- calibrate_with_cap(yhat_raw[ok_d], panel$emit[ok_d], panel$y[ok_d],
                                  panel$nace2d[ok_d], panel$year[ok_d], syt)
  nrmse <- sqrt(mean((panel$y[ok_d] - yhat_cal)^2)) / sd(panel$y[ok_d])
  if (nrmse < best_nrmse_d1) {
    best_nrmse_d1 <- nrmse
    best_thr_d1 <- thr
  }
}
cat("  Best threshold:", best_thr_d1, "-> nRMSE:", round(best_nrmse_d1, 4), "\n")

yhat_d1_raw <- ifelse(panel$phat > best_thr_d1, 1, 0) * panel$proxy_weighted
yhat_d1 <- rep(NA_real_, nrow(panel))
yhat_d1[ok] <- calibrate_with_cap(yhat_d1_raw[ok], panel$emit[ok], panel$y[ok],
                                   panel$nace2d[ok], panel$year[ok], syt)
m_d1 <- calc_metrics(panel$y[ok], yhat_d1[ok],
                     nace2d = panel$nace2d[ok], year = panel$year[ok])

cat("  nRMSE:    ", round(m_d1$nrmse_sd, 4), "\n")
cat("  Med. APD: ", round(m_d1$median_apd, 4), "\n")
cat("  Spearman: ", round(m_d1$spearman, 4), "\n")
cat("  rho_s:    ", round(m_d1$rho_pooled, 4),
    sprintf(" [min: %.3f, max: %.3f]\n", m_d1$rho_pooled_min, m_d1$rho_pooled_max))
cat("  FPR:      ", round(m_d1$fpr_nonemitters, 4), "\n")
cat("  TPR:      ", round(m_d1$tpr_emitters, 4), "\n")


# ===========================================================================
# Version D2: Hybrid — sector-slope hurdle classifies, proxy ranks
#   Uses phat_slope from Version C (sector-specific slope) for classification.
#   Among predicted emitters, allocate proportional to proxy_weighted.
# ===========================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("Version D2: Hybrid (sector-slope hurdle classifies, proxy ranks)\n")
cat("══════════════════════════════════════════════════════════\n")

# Threshold search
cat("Searching optimal threshold for D2...\n")
ok_slope_base <- !is.na(panel$phat_slope) & !is.na(panel$y)
best_thr_d2 <- 0.5
best_nrmse_d2 <- Inf

for (thr in THRESHOLDS) {
  yhat_raw <- ifelse(panel$phat_slope > thr, 1, 0) * panel$proxy_weighted
  ok_d <- ok_slope_base & !is.na(yhat_raw)
  if (sum(ok_d) == 0) next
  yhat_cal <- calibrate_with_cap(yhat_raw[ok_d], panel$emit[ok_d], panel$y[ok_d],
                                  panel$nace2d[ok_d], panel$year[ok_d], syt)
  nrmse <- sqrt(mean((panel$y[ok_d] - yhat_cal)^2)) / sd(panel$y[ok_d])
  if (nrmse < best_nrmse_d2) {
    best_nrmse_d2 <- nrmse
    best_thr_d2 <- thr
  }
}
cat("  Best threshold:", best_thr_d2, "-> nRMSE:", round(best_nrmse_d2, 4), "\n")

yhat_d2_raw <- ifelse(panel$phat_slope > best_thr_d2, 1, 0) * panel$proxy_weighted
yhat_d2 <- rep(NA_real_, nrow(panel))
yhat_d2[ok_slope] <- calibrate_with_cap(yhat_d2_raw[ok_slope], panel$emit[ok_slope],
                                         panel$y[ok_slope], panel$nace2d[ok_slope],
                                         panel$year[ok_slope], syt)
m_d2 <- calc_metrics(panel$y[ok_slope], yhat_d2[ok_slope],
                     nace2d = panel$nace2d[ok_slope], year = panel$year[ok_slope])

cat("  nRMSE:    ", round(m_d2$nrmse_sd, 4), "\n")
cat("  Med. APD: ", round(m_d2$median_apd, 4), "\n")
cat("  Spearman: ", round(m_d2$spearman, 4), "\n")
cat("  rho_s:    ", round(m_d2$rho_pooled, 4), "\n")
cat("  FPR:      ", round(m_d2$fpr_nonemitters, 4), "\n")
cat("  TPR:      ", round(m_d2$tpr_emitters, 4), "\n")


# ===========================================================================
# Summary comparison
# ===========================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("Side-by-side comparison\n")
cat("══════════════════════════════════════════════════════════\n")

comp <- data.frame(
  metric = c("nRMSE", "Med. APD", "Spearman rho", "rho_s", "FPR", "TPR"),
  hard   = c(m_hard$nrmse_sd, m_hard$median_apd, m_hard$spearman,
             m_hard$rho_pooled, m_hard$fpr_nonemitters, m_hard$tpr_emitters),
  soft   = c(m_soft$nrmse_sd, m_soft$median_apd, m_soft$spearman,
             m_soft$rho_pooled, m_soft$fpr_nonemitters, m_soft$tpr_emitters),
  slope  = c(m_slope$nrmse_sd, m_slope$median_apd, m_slope$spearman,
             m_slope$rho_pooled, m_slope$fpr_nonemitters, m_slope$tpr_emitters),
  hyb_base = c(m_d1$nrmse_sd, m_d1$median_apd, m_d1$spearman,
               m_d1$rho_pooled, m_d1$fpr_nonemitters, m_d1$tpr_emitters),
  hyb_slope = c(m_d2$nrmse_sd, m_d2$median_apd, m_d2$spearman,
                m_d2$rho_pooled, m_d2$fpr_nonemitters, m_d2$tpr_emitters)
)
comp$d_soft      <- round(comp$soft - comp$hard, 4)
comp$d_slope     <- round(comp$slope - comp$hard, 4)
comp$d_hyb_base  <- round(comp$hyb_base - comp$hard, 4)
comp$d_hyb_slope <- round(comp$hyb_slope - comp$hard, 4)
comp$hard      <- round(comp$hard, 4)
comp$soft      <- round(comp$soft, 4)
comp$slope     <- round(comp$slope, 4)
comp$hyb_base  <- round(comp$hyb_base, 4)
comp$hyb_slope <- round(comp$hyb_slope, 4)

cat("\n  A = Hard threshold (tau=0.48)\n")
cat("  B = Soft hurdle (phat*muhat)\n")
cat("  C = Sector-slope + hard threshold\n")
cat("  D1 = Hybrid: base hurdle classifies, proxy ranks\n")
cat("  D2 = Hybrid: sector-slope hurdle classifies, proxy ranks\n\n")

print(comp[, c("metric", "hard", "hyb_base", "d_hyb_base", "hyb_slope", "d_hyb_slope")], row.names = FALSE)
cat("\n")
print(comp[, c("metric", "hard", "soft", "d_soft", "slope", "d_slope")], row.names = FALSE)
