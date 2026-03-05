###############################################################################
# analysis/nested_cv/models_with_fold_specific_proxy.R
#
# PURPOSE
#   Evaluate the nested CV proxy via proportional allocation of sector-year
#   emission totals. Each firm's fold_specific_proxy was computed from an elastic net
#   that never saw that firm's sector, so evaluation on the full sample is
#   honest.
#
#   Compares:
#     1. fold_specific_proxy  (nested CV — leakage-free)
#     2. proxy_weighted (full-sample elastic net — leaked)
#     3. proxy_tabachova (fixed NACE-code rule — no leakage by construction)
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#   {INT_DATA}/fold_specific_proxy.RData
#
# OUTPUT
#   {OUTPUT_DIR}/nested_cv_proportional_performance.csv
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
REPO_DIR <- "c:/Users/jota_/Documents/inferring_emissions"
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

cat("Loading nested CV proxy...\n")
load(file.path(INT_DATA, "fold_specific_proxy.RData"))

# Merge fold_specific_proxy and fold_k into training_sample
panel <- training_sample %>%
  left_join(fs_proxy_panel %>% select(vat, year, fold_k, fold_specific_proxy, primary_nace2d),
            by = c("vat", "year")) %>%
  mutate(
    fold_specific_proxy = coalesce(fold_specific_proxy, 0),
    emit = as.integer(y > 0)
  )
rm(training_sample, fs_proxy_panel)

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("fold_specific_proxy > 0:", sum(panel$fold_specific_proxy > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$fold_specific_proxy > 0)))
cat("proxy_weighted > 0:", sum(panel$proxy_weighted > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_weighted > 0)))
cat("proxy_tabachova > 0:", sum(panel$proxy_tabachova > 0),
    sprintf("(%.1f%%)\n\n", 100 * mean(panel$proxy_tabachova > 0)))

# Recreate syt if not loaded
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}


# ── Proportional allocation for each proxy variant ───────────────────────────
proxy_variants <- list(
  list(name = "fold_specific_proxy",    col = "fold_specific_proxy"),
  list(name = "proxy_weighted",  col = "proxy_weighted"),
  list(name = "proxy_tabachova", col = "proxy_tabachova")
)

results <- list()

for (pv in proxy_variants) {
  cat(sprintf("═══ %s ═══\n", pv$name))

  proxy_vals <- panel[[pv$col]]
  ok <- !is.na(proxy_vals) & !is.na(panel$y)

  # --- Proportional allocation (no cap) ---
  yhat_cal <- calibrate_predictions(
    proxy_vals[ok], panel$nace2d[ok], panel$year[ok], syt
  )

  m_cal <- calc_metrics(panel$y[ok], yhat_cal,
                        nace2d = panel$nace2d[ok], year = panel$year[ok])

  cat("  calibrate_predictions (no cap):\n")
  cat(sprintf("    nRMSE = %.3f, Spearman = %.3f, FPR = %.3f, TPR = %.3f\n",
              m_cal$nrmse_sd, m_cal$spearman,
              m_cal$fpr_nonemitters, m_cal$tpr_emitters))
  cat(sprintf("    within_sy_rho_med = %.3f, rho_pooled = %.3f, rho_pooled_global = %.3f\n",
              m_cal$within_sy_rho_med, m_cal$rho_pooled, m_cal$rho_pooled_global))

  results[[paste0(pv$name, "_cal")]] <- data.frame(
    model = pv$name, variant = "calibrated",
    nRMSE = m_cal$nrmse_sd, rmse = m_cal$rmse,
    spearman = m_cal$spearman,
    fpr_nonemitters = m_cal$fpr_nonemitters,
    tpr_emitters = m_cal$tpr_emitters,
    emitter_mass_captured = m_cal$emitter_mass_captured,
    mapd_emitters = m_cal$mapd_emitters,
    within_sy_rho_med = m_cal$within_sy_rho_med,
    rho_pooled = m_cal$rho_pooled,
    rho_pooled_global = m_cal$rho_pooled_global,
    stringsAsFactors = FALSE
  )

  # --- Proportional allocation with cap ---
  yhat_cap <- calibrate_with_cap(
    proxy_vals[ok], panel$emit[ok], panel$y[ok],
    panel$nace2d[ok], panel$year[ok], syt
  )

  m_cap <- calc_metrics(panel$y[ok], yhat_cap,
                        nace2d = panel$nace2d[ok], year = panel$year[ok])

  cat("  calibrate_with_cap:\n")
  cat(sprintf("    nRMSE = %.3f, Spearman = %.3f, FPR = %.3f, TPR = %.3f\n",
              m_cap$nrmse_sd, m_cap$spearman,
              m_cap$fpr_nonemitters, m_cap$tpr_emitters))
  cat(sprintf("    within_sy_rho_med = %.3f, rho_pooled = %.3f, rho_pooled_global = %.3f\n",
              m_cap$within_sy_rho_med, m_cap$rho_pooled, m_cap$rho_pooled_global))

  results[[paste0(pv$name, "_cap")]] <- data.frame(
    model = pv$name, variant = "calibrated_capped",
    nRMSE = m_cap$nrmse_sd, rmse = m_cap$rmse,
    spearman = m_cap$spearman,
    fpr_nonemitters = m_cap$fpr_nonemitters,
    tpr_emitters = m_cap$tpr_emitters,
    emitter_mass_captured = m_cap$emitter_mass_captured,
    mapd_emitters = m_cap$mapd_emitters,
    within_sy_rho_med = m_cap$within_sy_rho_med,
    rho_pooled = m_cap$rho_pooled,
    rho_pooled_global = m_cap$rho_pooled_global,
    stringsAsFactors = FALSE
  )

  cat("\n")
}


# ── Summary: proportional allocation ─────────────────────────────────────────
cv_proportional <- bind_rows(results)

cat("═══ Proportional allocation summary ═══\n")
print(cv_proportional %>%
        select(model, variant, nRMSE, spearman, fpr_nonemitters,
               tpr_emitters, within_sy_rho_med, rho_pooled_global),
      row.names = FALSE)
cat("\n")


# =============================================================================
# PART 2: HYBRID — GAM classifier + proxy ranking + calibrate_with_cap
# =============================================================================
library(mgcv)

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("  HYBRID: GAM classifier + proxy ranking (K=5 sector folds)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Factor variables for mgcv
panel <- panel %>%
  mutate(
    year_f   = factor(year),
    nace2d_f = factor(nace2d),
    nace5d_f = factor(nace5d),
    log_revenue = ifelse(is.na(log_revenue), 0, log_revenue)
  )

all_nace2d_levels <- levels(panel$nace2d_f)
all_nace5d_levels <- levels(panel$nace5d_f)
all_year_levels   <- levels(panel$year_f)

# Threshold grid
THRESHOLDS <- seq(0.01, 0.60, by = 0.01)

# Helper: make result row
make_result_row <- function(nm, variant, thr, m) {
  data.frame(
    model = nm, variant = variant, threshold = thr,
    nRMSE = m$nrmse_sd, rmse = m$rmse,
    spearman = m$spearman,
    fpr_nonemitters = m$fpr_nonemitters,
    tpr_emitters = m$tpr_emitters,
    emitter_mass_captured = m$emitter_mass_captured,
    mapd_emitters = m$mapd_emitters,
    within_sy_rho_med = m$within_sy_rho_med,
    rho_pooled = m$rho_pooled,
    rho_pooled_global = m$rho_pooled_global,
    stringsAsFactors = FALSE
  )
}

# Define hybrid specs:
#   ext_formula: GAM for emitter classification (binomial)
#   proxy_col: raw proxy used for within-sector ranking after thresholding
hybrid_specs <- list(
  # Benchmark: no proxy in classifier, no proxy for ranking → revenue-based
  list(name = "ncv_benchmark",
       ext_formula = emit ~ log_revenue + year_f + s(nace2d_f, bs = "re"),
       proxy_col = "log_revenue"),

  # Nested proxy (leakage-free)
  list(name = "ncv_hybrid_nested",
       ext_formula = emit ~ log_revenue + I(fold_specific_proxy > 0) + asinh(fold_specific_proxy) +
         year_f + s(nace2d_f, bs = "re"),
       proxy_col = "fold_specific_proxy"),

  # Leaked proxy (full-sample, for comparison)
  list(name = "ncv_hybrid_leaked",
       ext_formula = emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +
         year_f + s(nace2d_f, bs = "re"),
       proxy_col = "proxy_weighted"),

  # Tabachova benchmark
  list(name = "ncv_hybrid_tabachova",
       ext_formula = emit ~ log_revenue + I(proxy_tabachova > 0) + asinh(proxy_tabachova) +
         year_f + s(nace2d_f, bs = "re"),
       proxy_col = "proxy_tabachova")
)

hybrid_results <- list()
hybrid_sweep   <- list()

for (hsp in hybrid_specs) {
  h_nm <- hsp$name
  cat(sprintf("\n── %s ──\n", h_nm))

  # Collect phat across folds
  panel$ncv_phat <- NA_real_

  t0 <- Sys.time()

  for (k in sort(unique(panel$fold_k))) {
    held_out_sectors <- sector_fold_map$nace2d[sector_fold_map$fold_k == k]

    train_idx <- which(!(panel$primary_nace2d %in% held_out_sectors))
    test_idx  <- which(panel$primary_nace2d %in% held_out_sectors)

    train <- panel[train_idx, ]
    test  <- panel[test_idx, ]

    # Ensure full factor levels for mgcv RE
    train$nace2d_f <- factor(train$nace2d, levels = all_nace2d_levels)
    test$nace2d_f  <- factor(test$nace2d,  levels = all_nace2d_levels)
    train$nace5d_f <- factor(train$nace5d, levels = all_nace5d_levels)
    test$nace5d_f  <- factor(test$nace5d,  levels = all_nace5d_levels)
    train$year_f   <- factor(train$year,   levels = all_year_levels)
    test$year_f    <- factor(test$year,    levels = all_year_levels)

    # Fit extensive margin GAM
    fit_ext <- tryCatch(
      gam(hsp$ext_formula, data = train,
          family = binomial(link = "logit"), method = "REML"),
      error = function(e) { cat("  GAM error fold", k, ":", e$message, "\n"); NULL }
    )

    if (!is.null(fit_ext)) {
      phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
      phat <- pmin(pmax(phat, 0), 1)
      panel$ncv_phat[test_idx] <- phat
    }

    cat(sprintf("  Fold %d: %d train, %d test\n", k, nrow(train), nrow(test)))
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf("  GAM CV done (%.1fs)\n", elapsed))

  # Combine phat + proxy for threshold sweep
  proxy_vals <- panel[[hsp$proxy_col]]
  ok <- !is.na(panel$ncv_phat) & !is.na(proxy_vals) & !is.na(panel$y)

  if (sum(ok) == 0) {
    cat("  WARNING: No valid predictions. Skipping.\n")
    panel$ncv_phat <- NULL
    next
  }

  cat("  Threshold sweep (", length(THRESHOLDS), " points)...\n", sep = "")

  best_m   <- NULL
  best_thr <- NA_real_
  best_rmse <- Inf

  for (thr in THRESHOLDS) {
    yhat_thr <- pmax(as.numeric(panel$ncv_phat[ok] > thr) * proxy_vals[ok], 0)

    yhat_cap <- calibrate_with_cap(
      yhat_thr, panel$emit[ok], panel$y[ok],
      panel$nace2d[ok], panel$year[ok], syt
    )

    m <- calc_metrics(panel$y[ok], yhat_cap,
                      nace2d = panel$nace2d[ok], year = panel$year[ok])

    if (!is.na(m$rmse) && m$rmse < best_rmse) {
      best_m   <- m
      best_thr <- thr
      best_rmse <- m$rmse
    }

    hybrid_sweep[[length(hybrid_sweep) + 1]] <- data.frame(
      model = h_nm, threshold = thr,
      nRMSE = m$nrmse_sd, rmse = m$rmse,
      fpr_nonemitters = m$fpr_nonemitters,
      tpr_emitters = m$tpr_emitters,
      within_sy_rho_med = m$within_sy_rho_med,
      rho_pooled_global = m$rho_pooled_global,
      spearman = m$spearman,
      stringsAsFactors = FALSE
    )
  }

  cat(sprintf("  Best threshold: %.2f\n", best_thr))
  cat(sprintf("  nRMSE = %.3f, Spearman = %.3f, FPR = %.3f, TPR = %.3f\n",
              best_m$nrmse_sd, best_m$spearman,
              best_m$fpr_nonemitters, best_m$tpr_emitters))
  cat(sprintf("  within_sy_rho_med = %.3f, rho_pooled_global = %.3f\n",
              best_m$within_sy_rho_med, best_m$rho_pooled_global))

  hybrid_results[[h_nm]] <- make_result_row(h_nm, "hybrid_capped", best_thr, best_m)

  panel$ncv_phat <- NULL
}


# ── Combined summary ────────────────────────────────────────────────────────
cv_hybrid <- bind_rows(hybrid_results)

cat("\n═══ Hybrid model summary ═══\n")
print(cv_hybrid %>%
        select(model, threshold, nRMSE, spearman, fpr_nonemitters,
               tpr_emitters, within_sy_rho_med, rho_pooled_global),
      row.names = FALSE)

# Combine proportional + hybrid results
cv_all <- bind_rows(cv_proportional, cv_hybrid)

# Save
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "nested_cv_performance.csv")
write.csv(cv_all, out_path, row.names = FALSE)
cat("\nAll results saved to:", out_path, "\n")

if (length(hybrid_sweep) > 0) {
  sweep_path <- file.path(OUTPUT_DIR, "nested_cv_threshold_sweep.csv")
  write.csv(bind_rows(hybrid_sweep), sweep_path, row.names = FALSE)
  cat("Threshold sweep saved to:", sweep_path, "\n")
}
