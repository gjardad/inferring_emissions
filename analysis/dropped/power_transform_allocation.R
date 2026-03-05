###############################################################################
# analysis/nested_cv/power_transform_allocation.R
#
# PURPOSE
#   Exercise (b): Does allocating E_total proportionally to proxy^gamma
#   (instead of proxy^1) improve within-sector ranking?
#
#   If the proxy-emissions relationship is concave, gamma < 1 compresses
#   extreme proxy values and may improve Spearman rho. If convex, gamma > 1
#   helps.
#
# CROSS-FITTING
#   Gamma is a data-dependent choice, so it must be cross-fitted. For each
#   outer fold k (K=5 sector folds):
#     1. Training folds: sweep gamma, pick the one that minimizes RMSE
#     2. Apply that gamma to the held-out fold k
#   Final metrics are computed on the concatenated held-out predictions only.
#
#   We test Row 3-style (no hurdle), Row 4-style (hurdle), and Row 5-style
#   (hurdle + cap).
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/fold_specific_proxy.RData
#
# OUTPUT
#   {OUTPUT_DIR}/nested_cv/power_transform_sweep.csv
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

cat("Loading fold-specific proxy...\n")
load(file.path(PROC_DATA, "fold_specific_proxy.RData"))

panel <- training_sample %>%
  left_join(fs_proxy_panel %>% select(vat, year, fold_k, fold_specific_proxy,
                                       primary_nace2d),
            by = c("vat", "year")) %>%
  mutate(
    fold_specific_proxy = coalesce(fold_specific_proxy, 0),
    emit = as.integer(y > 0)
  )
rm(training_sample, fs_proxy_panel)

# Sector-year totals (calibration targets from NIR; not a learned parameter)
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n\n")


# ── Hurdle threshold helper ─────────────────────────────────────────────────
learn_percentile_threshold <- function(proxy_vals, emit_vals) {
  pctile_ranks <- ecdf(proxy_vals)(proxy_vals)
  cutoffs <- seq(0.01, 0.99, by = 0.01)
  best_youden <- -Inf
  best_p <- NA_real_
  n_emit <- sum(emit_vals == 1)
  n_nonemit <- sum(emit_vals == 0)

  for (p in cutoffs) {
    pred_emit <- as.integer(pctile_ranks >= p)
    tpr <- if (n_emit > 0) sum(pred_emit == 1 & emit_vals == 1) / n_emit else NA_real_
    fpr <- if (n_nonemit > 0) sum(pred_emit == 1 & emit_vals == 0) / n_nonemit else NA_real_
    youden <- tpr - fpr
    if (!is.na(youden) && youden > best_youden) {
      best_youden <- youden
      best_p <- p
    }
  }
  best_p
}

# Learn hurdle thresholds from sectors 19/24 (cross-sector, same as Row 4)
sec19_idx <- which(panel$nace2d == "19")
sec24_idx <- which(panel$nace2d == "24")
tau_from_24 <- learn_percentile_threshold(panel$fold_specific_proxy[sec24_idx], panel$emit[sec24_idx])
tau_from_19 <- learn_percentile_threshold(panel$fold_specific_proxy[sec19_idx], panel$emit[sec19_idx])

# Apply cross-sector hurdle
panel$thresholded_proxy <- panel$fold_specific_proxy
if (length(sec19_idx) > 0) {
  pctile_19 <- ecdf(panel$fold_specific_proxy[sec19_idx])(panel$fold_specific_proxy[sec19_idx])
  panel$thresholded_proxy[sec19_idx] <- ifelse(
    pctile_19 >= tau_from_24, panel$fold_specific_proxy[sec19_idx], 0
  )
}
if (length(sec24_idx) > 0) {
  pctile_24 <- ecdf(panel$fold_specific_proxy[sec24_idx])(panel$fold_specific_proxy[sec24_idx])
  panel$thresholded_proxy[sec24_idx] <- ifelse(
    pctile_24 >= tau_from_19, panel$fold_specific_proxy[sec24_idx], 0
  )
}

cat(sprintf("Hurdle thresholds: tau_from_24 = %.2f, tau_from_19 = %.2f\n\n", tau_from_24, tau_from_19))


# ── Gamma grid ───────────────────────────────────────────────────────────────
GAMMAS <- c(0.25, 0.5, 0.75, 1.0, 1.25, 1.5)
folds <- sort(unique(na.omit(panel$fold_k)))
K <- length(folds)


# =============================================================================
# CROSS-FITTED GAMMA SELECTION
# =============================================================================
cat("═══ Cross-fitting gamma (K=5 sector folds) ═══\n\n")

# Storage for held-out predictions (one column per row-style)
panel$yhat_r3 <- NA_real_
panel$yhat_r4 <- NA_real_
panel$yhat_r5 <- NA_real_

# Track which gamma was selected per fold
fold_gammas <- data.frame(
  fold_k = integer(0),
  best_gamma_r3 = numeric(0),
  best_gamma_r4 = numeric(0),
  best_gamma_r5 = numeric(0),
  stringsAsFactors = FALSE
)

for (k in folds) {
  held_out_sectors <- sector_fold_map$nace2d[sector_fold_map$fold_k == k]
  train_idx <- which(!(panel$primary_nace2d %in% held_out_sectors) & !is.na(panel$fold_k))
  test_idx  <- which(panel$primary_nace2d %in% held_out_sectors & !is.na(panel$fold_k))

  cat(sprintf("Fold %d (held-out: %s) — %d train, %d test\n",
              k, paste(held_out_sectors, collapse = ", "),
              length(train_idx), length(test_idx)))

  # Sweep gamma on training data, pick best by RMSE for each row-style
  best_rmse_r3 <- Inf; best_g_r3 <- 1.0
  best_rmse_r4 <- Inf; best_g_r4 <- 1.0
  best_rmse_r5 <- Inf; best_g_r5 <- 1.0

  for (gamma in GAMMAS) {
    # Row 3: no hurdle
    proxy_g <- panel$fold_specific_proxy[train_idx]^gamma
    yhat <- calibrate_predictions(proxy_g, panel$nace2d[train_idx],
                                   panel$year[train_idx], syt)
    rmse <- sqrt(mean((panel$y[train_idx] - yhat)^2))
    if (rmse < best_rmse_r3) { best_rmse_r3 <- rmse; best_g_r3 <- gamma }

    # Row 4: hurdle
    proxy_h_g <- panel$thresholded_proxy[train_idx]^gamma
    yhat <- calibrate_predictions(proxy_h_g, panel$nace2d[train_idx],
                                   panel$year[train_idx], syt)
    rmse <- sqrt(mean((panel$y[train_idx] - yhat)^2))
    if (rmse < best_rmse_r4) { best_rmse_r4 <- rmse; best_g_r4 <- gamma }

    # Row 5: hurdle + cap
    yhat <- calibrate_with_cap(proxy_h_g, panel$emit[train_idx], panel$y[train_idx],
                                panel$nace2d[train_idx], panel$year[train_idx], syt)
    rmse <- sqrt(mean((panel$y[train_idx] - yhat)^2))
    if (rmse < best_rmse_r5) { best_rmse_r5 <- rmse; best_g_r5 <- gamma }
  }

  cat(sprintf("  Selected: gamma_r3=%.2f, gamma_r4=%.2f, gamma_r5=%.2f\n",
              best_g_r3, best_g_r4, best_g_r5))

  fold_gammas <- rbind(fold_gammas, data.frame(
    fold_k = k, best_gamma_r3 = best_g_r3,
    best_gamma_r4 = best_g_r4, best_gamma_r5 = best_g_r5
  ))

  # Apply selected gammas to held-out fold
  panel$yhat_r3[test_idx] <- calibrate_predictions(
    panel$fold_specific_proxy[test_idx]^best_g_r3,
    panel$nace2d[test_idx], panel$year[test_idx], syt
  )

  panel$yhat_r4[test_idx] <- calibrate_predictions(
    panel$thresholded_proxy[test_idx]^best_g_r4,
    panel$nace2d[test_idx], panel$year[test_idx], syt
  )

  panel$yhat_r5[test_idx] <- calibrate_with_cap(
    panel$thresholded_proxy[test_idx]^best_g_r5,
    panel$emit[test_idx], panel$y[test_idx],
    panel$nace2d[test_idx], panel$year[test_idx], syt
  )
}


# =============================================================================
# EVALUATE ON HELD-OUT PREDICTIONS
# =============================================================================
cat("\n\n═══ Cross-fitted results (held-out predictions only) ═══\n")

ok <- !is.na(panel$yhat_r3)

print_metrics <- function(label, m) {
  cat(sprintf("\n── %s ──\n", label))
  cat(sprintf("  nRMSE:    %.3f\n", m$nrmse_sd))
  cat(sprintf("  Med APD:  %.3f  [IQR: %.3f – %.3f]\n", m$median_apd, m$apd_q25, m$apd_q75))
  cat(sprintf("  rho_g:    %.3f\n", m$rho_pooled_global))
  cat(sprintf("  rho_s:    %.3f  [%.3f – %.3f]\n", m$rho_pooled, m$rho_pooled_min, m$rho_pooled_max))
  cat(sprintf("  FPR:      %.3f   TPR: %.3f\n", m$fpr_nonemitters, m$tpr_emitters))
  cat(sprintf("  FP p50:   %.3f   FP p99: %.3f\n", m$avg_nonemit_p50_rank, m$avg_nonemit_p99_rank))
}

m3 <- calc_metrics(panel$y[ok], panel$yhat_r3[ok],
                   nace2d = panel$nace2d[ok], year = panel$year[ok])
m4 <- calc_metrics(panel$y[ok], panel$yhat_r4[ok],
                   nace2d = panel$nace2d[ok], year = panel$year[ok])
m5 <- calc_metrics(panel$y[ok], panel$yhat_r5[ok],
                   nace2d = panel$nace2d[ok], year = panel$year[ok])

print_metrics("Row 3 (proxy-proportional, cross-fitted gamma)", m3)
print_metrics("Row 4 (proxy + hurdle, cross-fitted gamma)", m4)
print_metrics("Row 5 (proxy + hurdle + cap, cross-fitted gamma)", m5)


# ── Gamma selection summary ──────────────────────────────────────────────────
cat("\n\n── Gamma selected per fold ──\n")
print(fold_gammas, row.names = FALSE)

# Check stability: if gamma is the same across all folds, it's robust
for (col in c("best_gamma_r3", "best_gamma_r4", "best_gamma_r5")) {
  vals <- fold_gammas[[col]]
  if (length(unique(vals)) == 1) {
    cat(sprintf("  %s: stable at %.2f across all folds\n", col, vals[1]))
  } else {
    cat(sprintf("  %s: varies across folds (%.2f – %.2f)\n", col, min(vals), max(vals)))
  }
}


# ── Comparison table ─────────────────────────────────────────────────────────
cat("\n\n══════════════ COMPARISON ══════════════\n")
results <- data.frame(
  model = c("row3_gamma", "row4_gamma", "row5_gamma"),
  nrmse_sd = c(m3$nrmse_sd, m4$nrmse_sd, m5$nrmse_sd),
  median_apd = c(m3$median_apd, m4$median_apd, m5$median_apd),
  rho_pooled_global = c(m3$rho_pooled_global, m4$rho_pooled_global, m5$rho_pooled_global),
  rho_pooled = c(m3$rho_pooled, m4$rho_pooled, m5$rho_pooled),
  fpr = c(m3$fpr_nonemitters, m4$fpr_nonemitters, m5$fpr_nonemitters),
  tpr = c(m3$tpr_emitters, m4$tpr_emitters, m5$tpr_emitters),
  stringsAsFactors = FALSE
)
print(results, row.names = FALSE)
cat("════════════════════════════════════════\n")
cat("\nCompare against gamma=1 baselines from models_with_fold_specific_proxy.R\n")

# ── Save ─────────────────────────────────────────────────────────────────────
out_dir <- file.path(OUTPUT_DIR, "nested_cv")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(results, file.path(out_dir, "power_transform_sweep.csv"), row.names = FALSE)
write.csv(fold_gammas, file.path(out_dir, "power_transform_fold_gammas.csv"), row.names = FALSE)
cat("\nSaved to:", out_dir, "\n")
