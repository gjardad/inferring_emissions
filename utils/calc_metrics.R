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
#
# INPUTS
#   y, yhat : numeric vectors (same length)
#   fp_threshold : numeric threshold; predicted positive iff yhat > fp_threshold
#
# OUTPUT
#   A named list with fields used by ppml.R and hurdle.R.
###############################################################################

calc_metrics <- function(y, yhat, fp_threshold = 0) {
  y <- as.numeric(y)

  yhat <- as.numeric(yhat)
  
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  
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
      TP = NA_integer_, FP = NA_integer_, TN = NA_integer_, FN = NA_integer_
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
    
    # emitters-only intensity error summary
    mapd_emitters = mapd_emitters,
    
    # confusion matrix counts
    TP = as.integer(TP),
    FP = as.integer(FP),
    TN = as.integer(TN),
    FN = as.integer(FN)
  if (length(y) == 0) {
    return(list(
      nRMSE = NA_real_,
      MAPD_emitters = NA_real_,
      FPR_nonemitters = NA_real_,
      R2_LOO = NA_real_,
      spearman = NA_real_
    ))
  }
  
  # -----------------------
  # RMSE and nRMSE
  # -----------------------
  rmse  <- sqrt(mean((y - yhat)^2))
  nrmse <- rmse / sd(y)
  
  # -----------------------
  # R2 (LOO-style)
  # -----------------------
  denom <- sum((y - mean(y))^2)
  r2loo <- if (denom > 0) 1 - sum((y - yhat)^2) / denom else NA_real_
  
  # ----------------------------------------
  # MAPD for emitters only (y > 0)
  # ----------------------------------------
  emitters <- y > 0
  if (any(emitters)) {
    mapd_emitters <- mean(abs(yhat[emitters] - y[emitters]) / y[emitters])
  } else {
    mapd_emitters <- NA_real_
  }
  
  # --------------------------------------------------
  # False Positive Rate: Pr(ŷ > threshold | y = 0)
  # --------------------------------------------------
  non_emitters <- y == 0
  if (any(non_emitters)) {
    fpr <- mean(yhat[non_emitters] > fp_threshold)
  } else {
    fpr <- NA_real_
  }
  
  # --------------------------------------------------
  # Median, 90th pct and max for non-emitters
  # --------------------------------------------------
  non_emitters <- y == 0
  
  if (any(non_emitters)) {
    yhat_nonemit <- yhat[non_emitters]
    
    # Severity: median and max predicted emissions among true zeros
    med_yhat_nonemit <- median(yhat_nonemit, na.rm = TRUE)
    max_yhat_nonemit <- max(yhat_nonemit, na.rm = TRUE)
    
    # 90th pct among true zeros
    p90_yhat_nonemit <- as.numeric(
      quantile(yhat_nonemit, probs = 0.9, na.rm = TRUE)
    )
    
    # Percentile rank of these values in the *overall* yhat distribution
    # (share of all predictions <= that value). Multiply by 100 if you prefer.
    Fy <- stats::ecdf(yhat)  # yhat already filtered to finite via ok
    
    pctile_all_yhat_at_med_nonemit <- as.numeric(Fy(med_yhat_nonemit))*100
    pctile_all_yhat_at_max_nonemit <- as.numeric(Fy(max_yhat_nonemit))*100
    
  } else {
    med_yhat_nonemit <- NA_real_
    p90_yhat_nonemit <- NA_real_
    max_yhat_nonemit <- NA_real_
    pctile_all_yhat_at_med_nonemit <- NA_real_
    pctile_all_yhat_at_max_nonemit <- NA_real_
  }
  
  # -----------------------
  # Spearman rank correlation
  # -----------------------
  spear <- suppressWarnings(cor(y, yhat, method = "spearman"))
  
  list(
    nRMSE = nrmse,
    RMSE = rmse,
    R2_LOO = r2loo,
    MAPD_emitters = mapd_emitters,
    FPR_nonemitters = fpr,
    med_yhat_nonemit = med_yhat_nonemit,
    p90_yhat_nonemit = p90_yhat_nonemit,
    max_yhat_nonemit = max_yhat_nonemit,
    pctile_all_yhat_at_med_nonemit = pctile_all_yhat_at_med_nonemit,
    pctile_all_yhat_at_max_nonemit = pctile_all_yhat_at_max_nonemit,
    spearman = spear
  )
}
