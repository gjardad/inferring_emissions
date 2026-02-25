###############################################################################
# utils/calc_metrics.R
#
# PURPOSE
#   Compute performance metrics for emissions predictions yhat vs y.
#
# DESIGN
#   - Continuous error metrics on full evaluation sample:
#       rmse, mae, mape, spearman
#       nrmse_mean = rmse / mean(y)
#       nrmse_sd   = rmse / sd(y)     (recommended)
#   - Emitter / non-emitter diagnostics using y>0 as "true positive":
#       mapd_emitters (emitters only)
#   - Threshold-based classification-style diagnostics using yhat > fp_threshold
#       fpr_nonemitters, tpr_emitters, ppv_precision, f1,
#       predicted_positive_rate
#       emitter_mass_captured = sum(y among predicted positives) / sum(y among true emitters)
#   - Summaries of predicted values for true non-emitters:
#       mean_pred_nonemit, p50_pred_nonemit, p90_pred_nonemit, p95_pred_nonemit, p99_pred_nonemit
#   - Sector-specific false-positive severity (optional, when nace2d provided):
#       For NACE 19 and 24 (sectors with confirmed zeros in the training sample),
#       take the p50/p90/p99 of predicted emissions among true non-emitters and
#       express each as a percentile rank within the sector's emitter distribution.
#       nonemit_p50_rank_19, nonemit_p90_rank_19, nonemit_p99_rank_19
#       nonemit_p50_rank_24, nonemit_p90_rank_24, nonemit_p99_rank_24
#   - Within-sector-year averaged FP severity (optional, when nace2d AND year
#     provided): for each (sector, year) cell in NACE 19 and 24, find the
#     percentile rank (within the cell's emitter ecdf) of the median and p99
#     of predicted emissions among the cell's non-emitters. Average these
#     percentile ranks across all valid cells.
#       avg_nonemit_p50_rank, avg_nonemit_p99_rank
#
# INPUTS
#   y, yhat : numeric vectors (same length)
#   fp_threshold : numeric threshold; predicted positive iff yhat > fp_threshold
#   nace2d : optional character vector (same length as y); when provided,
#            computes sector-specific false-positive severity for NACE 19/24
#   year : optional vector (same length as y); when provided together with
#          nace2d, computes within-sector-year averaged FP severity
#
# OUTPUT
#   A named list with fields used by ppml.R and hurdle.R.
###############################################################################

calc_metrics <- function(y, yhat, fp_threshold = 0, nace2d = NULL, year = NULL) {
  y <- as.numeric(y)
  yhat <- as.numeric(yhat)

  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  if (!is.null(nace2d)) nace2d <- nace2d[ok]
  if (!is.null(year))   year   <- year[ok]
  
  n <- length(y)
  if (n == 0) {
    return(list(
      n = 0,
      rmse = NA_real_, nrmse_mean = NA_real_, nrmse_sd = NA_real_,
      mae = NA_real_, mape = NA_real_, spearman = NA_real_,
      fp_threshold = fp_threshold,
      fpr_nonemitters = NA_real_, tpr_emitters = NA_real_, ppv_precision = NA_real_,
      f1 = NA_real_, predicted_positive_rate = NA_real_, emitter_mass_captured = NA_real_,
      mean_pred_nonemit = NA_real_, p50_pred_nonemit = NA_real_, p90_pred_nonemit = NA_real_,
      p95_pred_nonemit = NA_real_, p99_pred_nonemit = NA_real_,
      mapd_emitters = NA_real_,
      TP = NA_integer_, FP = NA_integer_, TN = NA_integer_, FN = NA_integer_,
      nonemit_p50_rank_19 = NA_real_, nonemit_p90_rank_19 = NA_real_,
      nonemit_p99_rank_19 = NA_real_,
      nonemit_p50_rank_24 = NA_real_, nonemit_p90_rank_24 = NA_real_,
      nonemit_p99_rank_24 = NA_real_,
      avg_nonemit_p50_rank = NA_real_, avg_nonemit_p99_rank = NA_real_,
      within_sy_rho_med = NA_real_, within_sy_rho_min = NA_real_,
      within_sy_rho_max = NA_real_
    ))
  }
  
  # -----------------------------
  # Continuous metrics
  # -----------------------------
  err <- y - yhat
  rmse <- sqrt(mean(err^2))
  mae  <- mean(abs(err))
  
  y_mean <- mean(y)
  y_sd   <- stats::sd(y)
  nrmse_mean <- if (is.finite(y_mean) && y_mean != 0) rmse / y_mean else NA_real_
  nrmse_sd   <- if (is.finite(y_sd)   && y_sd   != 0) rmse / y_sd   else NA_real_
  
  # MAPE: define as mean(|err| / y) over y>0 (avoid divide-by-zero)
  idx_pos_y <- (y > 0)
  mape <- if (any(idx_pos_y)) mean(abs(err[idx_pos_y]) / y[idx_pos_y]) else NA_real_
  
  spearman <- suppressWarnings(stats::cor(y, yhat, method = "spearman", use = "complete.obs"))
  
  # -----------------------------
  # Emitter / non-emitter splits
  # -----------------------------
  is_emit_true <- (y > 0)
  is_nonemit_true <- !is_emit_true
  
  # MAPD among emitters only: mean(|y - yhat| / y)
  mapd_emitters <- if (any(is_emit_true)) {
    mean(abs(y[is_emit_true] - yhat[is_emit_true]) / y[is_emit_true])
  } else {
    NA_real_
  }
  
  # -----------------------------
  # Threshold-based classification metrics
  # predicted positive iff yhat > fp_threshold
  # -----------------------------
  pred_pos <- (yhat > fp_threshold)
  
  TP <- sum(pred_pos & is_emit_true)
  FP <- sum(pred_pos & is_nonemit_true)
  TN <- sum(!pred_pos & is_nonemit_true)
  FN <- sum(!pred_pos & is_emit_true)
  
  denom_fpr <- (FP + TN)
  denom_tpr <- (TP + FN)
  denom_ppv <- (TP + FP)
  
  fpr_nonemitters <- if (denom_fpr > 0) FP / denom_fpr else NA_real_
  tpr_emitters    <- if (denom_tpr > 0) TP / denom_tpr else NA_real_
  ppv_precision   <- if (denom_ppv > 0) TP / denom_ppv else NA_real_
  
  f1 <- if (is.finite(ppv_precision) && is.finite(tpr_emitters) &&
            (ppv_precision + tpr_emitters) > 0) {
    2 * ppv_precision * tpr_emitters / (ppv_precision + tpr_emitters)
  } else {
    NA_real_
  }
  
  predicted_positive_rate <- mean(pred_pos)
  
  # emitter mass captured: share of true emitter mass that lies in predicted positives
  denom_mass <- sum(y[is_emit_true])
  emitter_mass_captured <- if (is.finite(denom_mass) && denom_mass > 0) {
    sum(y[pred_pos & is_emit_true]) / denom_mass
  } else {
    NA_real_
  }
  
  # -----------------------------
  # Non-emitter prediction summaries (true non-emitters)
  # -----------------------------
  yhat_nonemit <- yhat[is_nonemit_true]
  if (length(yhat_nonemit) > 0) {
    q_nonemit <- as.numeric(stats::quantile(
      yhat_nonemit,
      probs = c(0.50, 0.90, 0.95, 0.99),
      na.rm = TRUE,
      names = FALSE,
      type = 7
    ))
    mean_pred_nonemit <- mean(yhat_nonemit, na.rm = TRUE)
  } else {
    q_nonemit <- c(NA_real_, NA_real_, NA_real_, NA_real_)
    mean_pred_nonemit <- NA_real_
  }
  
  # -----------------------------
  # Sector-specific false-positive severity (NACE 19 and 24)
  # -----------------------------
  nonemit_p50_rank_19 <- NA_real_
  nonemit_p90_rank_19 <- NA_real_
  nonemit_p99_rank_19 <- NA_real_
  nonemit_p50_rank_24 <- NA_real_
  nonemit_p90_rank_24 <- NA_real_
  nonemit_p99_rank_24 <- NA_real_

  if (!is.null(nace2d)) {
    for (sec in c("19", "24")) {
      in_sec        <- (nace2d == sec)
      sec_emitters  <- (in_sec & is_emit_true)
      sec_nonemit   <- (in_sec & is_nonemit_true)

      if (sum(sec_emitters) >= 3 && sum(sec_nonemit) >= 1) {
        emitter_ecdf <- stats::ecdf(y[sec_emitters])
        q_ne <- stats::quantile(yhat[sec_nonemit],
                                probs = c(0.50, 0.90, 0.99),
                                na.rm = TRUE, names = FALSE, type = 7)
        ranks <- emitter_ecdf(q_ne)

        if (sec == "19") {
          nonemit_p50_rank_19 <- ranks[1]
          nonemit_p90_rank_19 <- ranks[2]
          nonemit_p99_rank_19 <- ranks[3]
        } else {
          nonemit_p50_rank_24 <- ranks[1]
          nonemit_p90_rank_24 <- ranks[2]
          nonemit_p99_rank_24 <- ranks[3]
        }
      }
    }
  }

  # -----------------------------
  # Within-sector-year averaged FP severity (NACE 19 and 24)
  # For each (sector, year) cell: rank the median and p99 of non-emitter
  # predictions in the cell's emitter ecdf. Average across all valid cells.
  # -----------------------------
  avg_nonemit_p50_rank <- NA_real_
  avg_nonemit_p99_rank <- NA_real_

  if (!is.null(nace2d) && !is.null(year)) {
    cell_p50_ranks <- numeric(0)
    cell_p99_ranks <- numeric(0)
    cell_sectors   <- character(0)
    cell_years     <- character(0)

    for (sec in c("19", "24")) {
      in_sec <- (nace2d == sec)
      yrs <- sort(unique(year[in_sec]))

      for (yr in yrs) {
        in_cell      <- (in_sec & year == yr)
        cell_emit    <- (in_cell & is_emit_true)
        cell_nonemit <- (in_cell & is_nonemit_true)

        if (sum(cell_emit) < 3 || sum(cell_nonemit) < 1) next

        cell_ecdf <- stats::ecdf(y[cell_emit])
        ne_preds  <- yhat[cell_nonemit]

        q_ne <- stats::quantile(ne_preds, probs = c(0.50, 0.99),
                                na.rm = TRUE, names = FALSE, type = 7)

        cell_p50_ranks <- c(cell_p50_ranks, cell_ecdf(q_ne[1]))
        cell_p99_ranks <- c(cell_p99_ranks, cell_ecdf(q_ne[2]))
        cell_sectors   <- c(cell_sectors, sec)
        cell_years     <- c(cell_years, as.character(yr))
      }
    }

    if (length(cell_p50_ranks) > 0) {
      avg_nonemit_p50_rank <- mean(cell_p50_ranks)
      avg_nonemit_p99_rank <- mean(cell_p99_ranks)
    }
  }

  # -----------------------------
  # Within-sector-year Spearman rho
  # For each (sector, year) cell across ALL sectors, compute rank correlation
  # between y and yhat. Requires >=5 firms with variation in y.
  # Returns median, min, max across cells.
  # -----------------------------
  within_sy_rho_med <- NA_real_
  within_sy_rho_min <- NA_real_
  within_sy_rho_max <- NA_real_

  if (!is.null(nace2d) && !is.null(year)) {
    sy_rhos <- numeric(0)

    all_secs <- sort(unique(nace2d))
    for (sec in all_secs) {
      in_sec <- (nace2d == sec)
      yrs <- sort(unique(year[in_sec]))

      for (yr in yrs) {
        in_cell <- (in_sec & year == yr)
        y_cell    <- y[in_cell]
        yhat_cell <- yhat[in_cell]

        # Need >=5 firms and variation in both y and yhat
        if (sum(in_cell) < 5) next
        if (sd(y_cell) == 0 || sd(yhat_cell) == 0) next

        rho_cell <- suppressWarnings(
          stats::cor(y_cell, yhat_cell, method = "spearman", use = "complete.obs")
        )
        if (is.finite(rho_cell)) sy_rhos <- c(sy_rhos, rho_cell)
      }
    }

    if (length(sy_rhos) > 0) {
      within_sy_rho_med <- median(sy_rhos)
      within_sy_rho_min <- min(sy_rhos)
      within_sy_rho_max <- max(sy_rhos)
    }
  }

  # Build per-cell detail data.frame (empty if no cells computed)
  if (length(cell_p50_ranks) > 0) {
    cell_fp_severity <- data.frame(
      nace2d   = cell_sectors,
      year     = cell_years,
      p50_rank = cell_p50_ranks,
      p99_rank = cell_p99_ranks,
      stringsAsFactors = FALSE
    )
  } else {
    cell_fp_severity <- data.frame(
      nace2d   = character(0),
      year     = character(0),
      p50_rank = numeric(0),
      p99_rank = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  # -----------------------------
  # Return named list
  # -----------------------------
  list(
    n = n,

    rmse = rmse,
    nrmse_mean = nrmse_mean,
    nrmse_sd = nrmse_sd,
    mae = mae,
    mape = mape,
    spearman = spearman,

    # classification-style (threshold-based)
    fp_threshold = fp_threshold,
    fpr_nonemitters = fpr_nonemitters,
    tpr_emitters = tpr_emitters,
    ppv_precision = ppv_precision,
    f1 = f1,
    predicted_positive_rate = predicted_positive_rate,

    # recommended extra diagnostic
    emitter_mass_captured = emitter_mass_captured,

    # non-emitter prediction summaries
    mean_pred_nonemit = mean_pred_nonemit,
    p50_pred_nonemit  = q_nonemit[1],
    p90_pred_nonemit  = q_nonemit[2],
    p95_pred_nonemit  = q_nonemit[3],
    p99_pred_nonemit  = q_nonemit[4],

    # sector-specific false-positive severity (NACE 19 and 24)
    nonemit_p50_rank_19 = nonemit_p50_rank_19,
    nonemit_p90_rank_19 = nonemit_p90_rank_19,
    nonemit_p99_rank_19 = nonemit_p99_rank_19,
    nonemit_p50_rank_24 = nonemit_p50_rank_24,
    nonemit_p90_rank_24 = nonemit_p90_rank_24,
    nonemit_p99_rank_24 = nonemit_p99_rank_24,

    # within-sector-year averaged FP severity (NACE 19 and 24)
    avg_nonemit_p50_rank = avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = avg_nonemit_p99_rank,

    # within-sector-year Spearman rho (all sectors)
    within_sy_rho_med = within_sy_rho_med,
    within_sy_rho_min = within_sy_rho_min,
    within_sy_rho_max = within_sy_rho_max,

    # per-cell detail (data.frame with nace2d, year, p50_rank, p99_rank)
    cell_fp_severity = cell_fp_severity,

    # emitters-only intensity error summary
    mapd_emitters = mapd_emitters,

    # confusion matrix counts
    TP = as.integer(TP),
    FP = as.integer(FP),
    TN = as.integer(TN),
    FN = as.integer(FN)
  )
}
