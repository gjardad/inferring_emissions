###############################################################################
# analysis/nested_cv/ensemble_proxy_enet.R
#
# PURPOSE
#   Exercise (a): Does combining the fuel-supply proxy with EN-on-financials
#   predictions improve within-sector ranking (rho_s)?
#
#   The proxy captures fuel-supplier relationships (B2B signal), while the EN
#   on financials captures emission-relevant variation from annual accounts.
#   After applying the hurdle threshold, allocate E_total proportionally to
#   a composite signal: w * proxy_rank + (1-w) * en_rank, then calibrate.
#
# CROSS-FITTING
#   Both the EN predictions and the ensemble weight w are cross-fitted:
#     1. EN is fitted using the K=5 sector-fold structure (identical to Row 2)
#     2. For each outer fold k:
#        a. Training folds: sweep w, pick the one that minimizes RMSE
#        b. Apply that w to the held-out fold k
#   Final metrics are computed on concatenated held-out predictions only.
#
# DESIGN NOTE
#   The EN predictions and proxy values live on different scales. To make
#   the weight w meaningful, we rank-normalize both within each sector-year
#   cell before combining.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/fold_specific_proxy.RData
#
# OUTPUT
#   {OUTPUT_DIR}/nested_cv/ensemble_proxy_enet_sweep.csv
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

panel <- training_sample %>%
  left_join(fs_proxy_panel %>% select(vat, year, fold_k, fold_specific_proxy,
                                       primary_nace2d),
            by = c("vat", "year")) %>%
  mutate(
    fold_specific_proxy = coalesce(fold_specific_proxy, 0),
    emit = as.integer(y > 0)
  )
rm(training_sample, fs_proxy_panel)

if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n\n")


# =============================================================================
# STEP 1: Fit EN on financials (K=5 sector-fold CV, identical to Row 2)
# =============================================================================
cat("═══ Fitting EN on financials (K=5 sector-fold CV) ═══\n")

ALPHA   <- 0.5
K_INNER <- 10L
SEED    <- 42L

v_cols <- grep("^v_[0-9]", names(panel), value = TRUE)
extra_fin <- intersect(c("turnover_VAT", "inputs_VAT", "investment_VAT"), names(panel))
fin_cols <- c(v_cols, extra_fin)

X_fin <- as.matrix(panel[, fin_cols])
X_fin[is.na(X_fin)] <- 0
X_fin <- asinh(X_fin)
colnames(X_fin) <- paste0("fin_", fin_cols)

year_f <- factor(panel$year)
X_year <- model.matrix(~ year_f - 1)

nace2d_f <- factor(panel$nace2d)
X_nace <- model.matrix(~ nace2d_f - 1)

penalty_factor <- c(rep(1, ncol(X_fin)), rep(0, ncol(X_year)), rep(0, ncol(X_nace)))

set.seed(SEED)
unique_firms <- unique(panel$vat)
firm_inner_folds <- sample(rep(1:K_INNER, length.out = length(unique_firms)))
names(firm_inner_folds) <- unique_firms
inner_foldid <- unname(firm_inner_folds[panel$vat])

folds <- sort(unique(na.omit(panel$fold_k)))

panel$yhat_en <- NA_real_
t0_total <- Sys.time()

for (k in folds) {
  held_out_sectors <- sector_fold_map$nace2d[sector_fold_map$fold_k == k]
  cat(sprintf("  Fold %d (sectors: %s) ...", k, paste(held_out_sectors, collapse = ", ")))
  t0 <- Sys.time()

  train_idx <- which(!(panel$primary_nace2d %in% held_out_sectors) & !is.na(panel$fold_k))
  test_idx  <- which(panel$primary_nace2d %in% held_out_sectors & !is.na(panel$fold_k))

  X_train_full <- cbind(X_fin[train_idx, ], X_year[train_idx, ], X_nace[train_idx, ])
  X_test_full  <- cbind(X_fin[test_idx, ],  X_year[test_idx, ],  X_nace[test_idx, ])

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
cat(sprintf("EN complete (%.1f min)\n\n", elapsed_total))

rm(X_fin, X_year, X_nace, X_train_full, X_test_full, X_train, X_test)


# =============================================================================
# STEP 2: Apply hurdle (same as Row 4 in models_with_fold_specific_proxy.R)
# =============================================================================
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

sec19_idx <- which(panel$nace2d == "19")
sec24_idx <- which(panel$nace2d == "24")
tau_from_24 <- learn_percentile_threshold(panel$fold_specific_proxy[sec24_idx], panel$emit[sec24_idx])
tau_from_19 <- learn_percentile_threshold(panel$fold_specific_proxy[sec19_idx], panel$emit[sec19_idx])

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


# =============================================================================
# STEP 3: Cross-fit ensemble weight w
# =============================================================================
WEIGHTS <- seq(0, 1, by = 0.1)

cat("═══ Cross-fitting ensemble weight w (K=5 sector folds) ═══\n")
cat("w=1: pure proxy ranking | w=0: pure EN ranking (with proxy hurdle)\n\n")

# Storage for held-out predictions
panel$yhat_ens_r4 <- NA_real_
panel$yhat_ens_r5 <- NA_real_

fold_weights <- data.frame(
  fold_k = integer(0),
  best_w_r4 = numeric(0),
  best_w_r5 = numeric(0),
  stringsAsFactors = FALSE
)

for (k in folds) {
  held_out_sectors <- sector_fold_map$nace2d[sector_fold_map$fold_k == k]
  train_idx <- which(!(panel$primary_nace2d %in% held_out_sectors) & !is.na(panel$fold_k))
  test_idx  <- which(panel$primary_nace2d %in% held_out_sectors & !is.na(panel$fold_k))

  # Skip firms without EN predictions
  train_idx <- train_idx[!is.na(panel$yhat_en[train_idx])]
  test_idx  <- test_idx[!is.na(panel$yhat_en[test_idx])]

  cat(sprintf("Fold %d (held-out: %s) — %d train, %d test\n",
              k, paste(held_out_sectors, collapse = ", "),
              length(train_idx), length(test_idx)))

  # Rank-normalize proxy and EN within sector-year cells on TRAINING data
  train_df <- panel[train_idx, ] %>%
    group_by(nace2d, year) %>%
    mutate(
      proxy_rank = rank(thresholded_proxy, ties.method = "average") / n(),
      en_rank    = rank(yhat_en, ties.method = "average") / n()
    ) %>%
    ungroup()
  train_df$proxy_rank[train_df$thresholded_proxy == 0] <- 0

  # Sweep w on training data
  best_rmse_r4 <- Inf; best_w_r4 <- 1.0
  best_rmse_r5 <- Inf; best_w_r5 <- 1.0

  for (w in WEIGHTS) {
    composite <- w * train_df$proxy_rank + (1 - w) * train_df$en_rank

    # Row 4-style
    yhat <- calibrate_predictions(composite, train_df$nace2d, train_df$year, syt)
    rmse <- sqrt(mean((train_df$y - yhat)^2))
    if (rmse < best_rmse_r4) { best_rmse_r4 <- rmse; best_w_r4 <- w }

    # Row 5-style
    yhat <- calibrate_with_cap(composite, train_df$emit, train_df$y,
                                train_df$nace2d, train_df$year, syt)
    rmse <- sqrt(mean((train_df$y - yhat)^2))
    if (rmse < best_rmse_r5) { best_rmse_r5 <- rmse; best_w_r5 <- w }
  }

  cat(sprintf("  Selected: w_r4=%.1f, w_r5=%.1f\n", best_w_r4, best_w_r5))

  fold_weights <- rbind(fold_weights, data.frame(
    fold_k = k, best_w_r4 = best_w_r4, best_w_r5 = best_w_r5
  ))

  # Rank-normalize on TEST data (within test sector-year cells)
  test_df <- panel[test_idx, ] %>%
    group_by(nace2d, year) %>%
    mutate(
      proxy_rank = rank(thresholded_proxy, ties.method = "average") / n(),
      en_rank    = rank(yhat_en, ties.method = "average") / n()
    ) %>%
    ungroup()
  test_df$proxy_rank[test_df$thresholded_proxy == 0] <- 0

  # Apply selected w to test fold
  composite_r4 <- best_w_r4 * test_df$proxy_rank + (1 - best_w_r4) * test_df$en_rank
  composite_r5 <- best_w_r5 * test_df$proxy_rank + (1 - best_w_r5) * test_df$en_rank

  panel$yhat_ens_r4[test_idx] <- calibrate_predictions(
    composite_r4, test_df$nace2d, test_df$year, syt
  )

  panel$yhat_ens_r5[test_idx] <- calibrate_with_cap(
    composite_r5, test_df$emit, test_df$y,
    test_df$nace2d, test_df$year, syt
  )
}


# =============================================================================
# EVALUATE ON HELD-OUT PREDICTIONS
# =============================================================================
cat("\n\n═══ Cross-fitted results (held-out predictions only) ═══\n")

print_metrics <- function(label, m) {
  cat(sprintf("\n── %s ──\n", label))
  cat(sprintf("  nRMSE:    %.3f\n", m$nrmse_sd))
  cat(sprintf("  Med APD:  %.3f  [IQR: %.3f – %.3f]\n", m$median_apd, m$apd_q25, m$apd_q75))
  cat(sprintf("  rho_g:    %.3f\n", m$rho_pooled_global))
  cat(sprintf("  rho_s:    %.3f  [%.3f – %.3f]\n", m$rho_pooled, m$rho_pooled_min, m$rho_pooled_max))
  cat(sprintf("  FPR:      %.3f   TPR: %.3f\n", m$fpr_nonemitters, m$tpr_emitters))
  cat(sprintf("  FP p50:   %.3f   FP p99: %.3f\n", m$avg_nonemit_p50_rank, m$avg_nonemit_p99_rank))
}

ok <- !is.na(panel$yhat_ens_r4)

m4 <- calc_metrics(panel$y[ok], panel$yhat_ens_r4[ok],
                   nace2d = panel$nace2d[ok], year = panel$year[ok])
m5 <- calc_metrics(panel$y[ok], panel$yhat_ens_r5[ok],
                   nace2d = panel$nace2d[ok], year = panel$year[ok])

print_metrics("Row 4 ensemble (proxy + EN, cross-fitted w)", m4)
print_metrics("Row 5 ensemble (proxy + EN + cap, cross-fitted w)", m5)


# ── Weight selection summary ─────────────────────────────────────────────────
cat("\n\n── Weight w selected per fold ──\n")
print(fold_weights, row.names = FALSE)

for (col in c("best_w_r4", "best_w_r5")) {
  vals <- fold_weights[[col]]
  if (length(unique(vals)) == 1) {
    cat(sprintf("  %s: stable at %.1f across all folds\n", col, vals[1]))
  } else {
    cat(sprintf("  %s: varies across folds (%.1f – %.1f)\n", col, min(vals), max(vals)))
  }
}


# ── Comparison table ─────────────────────────────────────────────────────────
cat("\n\n══════════════ COMPARISON ══════════════\n")
results <- data.frame(
  model = c("ensemble_row4", "ensemble_row5"),
  nrmse_sd = c(m4$nrmse_sd, m5$nrmse_sd),
  median_apd = c(m4$median_apd, m5$median_apd),
  rho_pooled_global = c(m4$rho_pooled_global, m5$rho_pooled_global),
  rho_pooled = c(m4$rho_pooled, m5$rho_pooled),
  fpr = c(m4$fpr_nonemitters, m5$fpr_nonemitters),
  tpr = c(m4$tpr_emitters, m5$tpr_emitters),
  stringsAsFactors = FALSE
)
print(results, row.names = FALSE)
cat("════════════════════════════════════════\n")
cat("\nCompare against w=1 baselines (pure proxy) from models_with_fold_specific_proxy.R\n")

# ── Save ─────────────────────────────────────────────────────────────────────
out_dir <- file.path(OUTPUT_DIR, "nested_cv")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(results, file.path(out_dir, "ensemble_proxy_enet_sweep.csv"), row.names = FALSE)
write.csv(fold_weights, file.path(out_dir, "ensemble_proxy_enet_fold_weights.csv"), row.names = FALSE)
cat("\nSaved to:", out_dir, "\n")
