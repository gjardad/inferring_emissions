###############################################################################
# analysis/diagnostic_rho_by_threshold.R
#
# PURPOSE
#   Diagnostic: show per-sector rho_pooled distribution at tau = 0.01 vs 0.60
#   for the hybrid_proxy_weighted_ind_base model.
#
#   Re-runs only the extensive margin (logit GAM) from the CV to recover
#   cross-validated phat, then computes per-sector rho at both thresholds.
###############################################################################

REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
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
    nace4d_f = factor(substr(nace5d, 1, 4)),
    nace5d_f = factor(nace5d),
    emit     = as.integer(y > 0),
    log_revenue = ifelse(is.na(log_revenue), 0, log_revenue)
  )

# Recreate foldid (same seed as fit_cv_lofocv.R)
K_FOLDS <- 10L
set.seed(42)
unique_firms <- unique(panel$vat)
firm_folds <- sample(rep(1:K_FOLDS, length.out = length(unique_firms)))
names(firm_folds) <- unique_firms
foldid <- unname(firm_folds[panel$vat])

# Recreate syt
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n\n")

# ── Re-run extensive margin only to recover phat ─────────────────────────────
ext_formula <- emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +
  year_f + s(nace2d_f, bs = "re")

panel$phat <- NA_real_

cat("Re-running extensive margin (logit GAM) for each fold...\n")
for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)

  fit_ext <- gam(ext_formula, data = train,
                 family = binomial(link = "logit"), method = "REML")
  phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
  phat <- pmin(pmax(phat, 0), 1)
  panel$phat[test_idx] <- phat

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" done (%.1fs)\n", elapsed))
}

# ── Helper: compute per-sector rho_pooled for a given threshold ──────────────
compute_sector_rho <- function(panel, phat, proxy, y, emit, nace2d, year,
                               syt, threshold) {
  ok <- !is.na(phat) & !is.na(proxy) & !is.na(y)

  yhat_hard <- pmax(as.numeric(phat[ok] > threshold) * proxy[ok], 0)

  yhat_cap <- calibrate_with_cap(
    yhat_hard, emit[ok], y[ok],
    nace2d[ok], year[ok], syt
  )

  # Demean by (sector, year)
  df <- data.frame(
    y = y[ok], yhat = yhat_cap,
    nace2d = nace2d[ok], year = year[ok]
  )
  df <- df %>%
    group_by(nace2d, year) %>%
    mutate(y_dm = y - mean(y), yhat_dm = yhat - mean(yhat)) %>%
    ungroup()

  # Per-sector Spearman rho (pooling years within sector)
  sector_rho <- df %>%
    group_by(nace2d) %>%
    summarise(
      n_firmyears = n(),
      rho = suppressWarnings(
        cor(y_dm, yhat_dm, method = "spearman", use = "complete.obs")
      ),
      .groups = "drop"
    ) %>%
    filter(!is.na(rho), n_firmyears >= 5) %>%
    arrange(nace2d)

  sector_rho
}

# ── Compute at tau = 0.01 and tau = 0.60 ────────────────────────────────────
cat("\nComputing per-sector rho at tau = 0.01...\n")
rho_low <- compute_sector_rho(
  panel, panel$phat, panel$proxy_weighted, panel$y, panel$emit,
  panel$nace2d, panel$year, syt, threshold = 0.01
)

cat("Computing per-sector rho at tau = 0.60...\n")
rho_high <- compute_sector_rho(
  panel, panel$phat, panel$proxy_weighted, panel$y, panel$emit,
  panel$nace2d, panel$year, syt, threshold = 0.60
)

# ── Display side-by-side ────────────────────────────────────────────────────
comp <- merge(rho_low, rho_high, by = "nace2d", suffixes = c("_t01", "_t60"))

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("Per-sector rho_pooled: tau = 0.01 vs tau = 0.60\n")
cat("(hybrid_proxy_weighted_ind_base, calibrated_clipped)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

cat(sprintf("%-6s  %8s  %10s  %10s  %10s\n",
            "NACE", "FirmYrs", "rho(0.01)", "rho(0.60)", "Delta"))
cat(strrep("-", 52), "\n")
for (i in seq_len(nrow(comp))) {
  r <- comp[i, ]
  cat(sprintf("%-6s  %8d  %10.4f  %10.4f  %+10.4f\n",
              r$nace2d, r$n_firmyears_t01,
              r$rho_t01, r$rho_t60,
              r$rho_t60 - r$rho_t01))
}

cat(sprintf("\n%-6s  %8s  %10.4f  %10.4f  %+10.4f\n",
            "MEDIAN", "",
            median(comp$rho_t01), median(comp$rho_t60),
            median(comp$rho_t60) - median(comp$rho_t01)))

cat(sprintf("%-6s  %8s  %10.4f  %10.4f  %+10.4f\n",
            "MEAN", "",
            mean(comp$rho_t01), mean(comp$rho_t60),
            mean(comp$rho_t60) - mean(comp$rho_t01)))

# ── Also show full distribution summaries ────────────────────────────────────
cat("\n── Distribution of per-sector rho ──\n")
cat("tau = 0.01:\n")
print(summary(rho_low$rho))
cat("\ntau = 0.60:\n")
print(summary(rho_high$rho))

# ── Show which sectors changed the most ──────────────────────────────────────
comp$delta <- comp$rho_t60 - comp$rho_t01
cat("\n── Sectors with largest |delta| ──\n")
top_delta <- comp %>% arrange(desc(abs(delta))) %>% head(10)
cat(sprintf("%-6s  %10s  %10s  %10s\n", "NACE", "rho(0.01)", "rho(0.60)", "Delta"))
cat(strrep("-", 42), "\n")
for (i in seq_len(nrow(top_delta))) {
  r <- top_delta[i, ]
  cat(sprintf("%-6s  %10.4f  %10.4f  %+10.4f\n",
              r$nace2d, r$rho_t01, r$rho_t60, r$delta))
}
