###############################################################################
# calc_metrics.R
#
# PURPOSE:
#   Compute LOOCV performance metrics for emissions imputation, including:
#     - Continuous metrics (RMSE, nRMSE, MAE, etc.)
#     - Rank correlation (Spearman)
#     - Classification-style metrics based on a positive threshold:
#         FPR, TPR, PPV (precision), F1, predicted_positive_rate
#     - Summary stats for predicted values among true non-emitters
###############################################################################

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required.")
  }
})

# -----------------------------
# Helper: safe quantile
# -----------------------------
.safe_quantile <- function(x, probs) {
  if (length(x) == 0L) return(rep(NA_real_, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
}

# -----------------------------
# Main function
# -----------------------------
calc_metrics <- function(
    data = NULL,
    y = NULL,
    yhat = NULL,
    y_var = NULL,
    yhat_var = NULL,
    # Threshold used to decide whether a prediction is "positive"
    # (use 0 if you literally want >0; use small >0 to avoid floating-point noise)
    fp_threshold = 0,
    # Which observations count as true emitters?
    # Default: y > 0
    emitter_def = function(y) y > 0,
    # Extra options
    compute_spearman = TRUE
) {
  # -----------------------------
  # Input handling
  # -----------------------------
  if (!is.null(data)) {
    DT <- data.table::as.data.table(data)
    if (is.null(y_var) || is.null(yhat_var)) stop("If data is provided, supply y_var and yhat_var.")
    y    <- DT[[y_var]]
    yhat <- DT[[yhat_var]]
  } else {
    if (is.null(y) || is.null(yhat)) stop("Provide either (data + y_var/yhat_var) or vectors y and yhat.")
  }
  
  # Make numeric
  y    <- as.numeric(y)
  yhat <- as.numeric(yhat)
  
  # Drop rows where either missing
  ok <- stats::complete.cases(y, yhat)
  y    <- y[ok]
  yhat <- yhat[ok]
  
  n <- length(y)
  if (n == 0L) {
    return(list(
      n = 0L,
      rmse = NA_real_, nrmse_mean = NA_real_, nrmse_sd = NA_real_,
      mae = NA_real_, mape = NA_real_,
      spearman = NA_real_,
      # classification
      fp_threshold = fp_threshold,
      fpr_nonemitters = NA_real_, tpr_emitters = NA_real_,
      ppv_precision = NA_real_, f1 = NA_real_,
      predicted_positive_rate = NA_real_,
      # non-emitter prediction summaries
      mean_pred_nonemit = NA_real_, p50_pred_nonemit = NA_real_,
      p90_pred_nonemit = NA_real_, p95_pred_nonemit = NA_real_, p99_pred_nonemit = NA_real_
    ))
  }
  
  # -----------------------------
  # Core continuous metrics
  # -----------------------------
  err  <- yhat - y
  rmse <- sqrt(mean(err^2))
  mae  <- mean(abs(err))
  
  y_mean <- mean(y)
  y_sd   <- stats::sd(y)
  
  nrmse_mean <- if (is.finite(y_mean) && y_mean != 0) rmse / y_mean else NA_real_
  nrmse_sd   <- if (is.finite(y_sd)   && y_sd   != 0) rmse / y_sd   else NA_real_
  
  # MAPE (only defined when y>0)
  idx_pos_y <- which(y > 0)
  mape <- if (length(idx_pos_y) > 0L) {
    mean(abs(err[idx_pos_y]) / y[idx_pos_y])
  } else {
    NA_real_
  }
  
  # Spearman correlation
  spearman <- NA_real_
  if (isTRUE(compute_spearman)) {
    # Need at least 2 non-missing and some variation
    if (n >= 2L && stats::sd(y) > 0 && stats::sd(yhat) > 0) {
      spearman <- suppressWarnings(stats::cor(y, yhat, method = "spearman"))
    }
  }
  
  # -----------------------------
  # Classification-style metrics
  # -----------------------------
  is_emitter <- emitter_def(y)          # TRUE if y is an emitter
  pred_pos   <- (yhat > fp_threshold)   # TRUE if predicted positive
  
  # Confusion components
  TP <- sum(pred_pos &  is_emitter)
  FP <- sum(pred_pos & !is_emitter)
  TN <- sum(!pred_pos & !is_emitter)
  FN <- sum(!pred_pos &  is_emitter)
  
  # Rates
  # FPR = FP / (FP+TN) among true non-emitters
  denom_nonemit <- FP + TN
  fpr_nonemitters <- if (denom_nonemit > 0L) FP / denom_nonemit else NA_real_
  
  # TPR (recall) = TP / (TP+FN) among true emitters
  denom_emit <- TP + FN
  tpr_emitters <- if (denom_emit > 0L) TP / denom_emit else NA_real_
  
  # Precision / PPV = TP / (TP+FP) among predicted positives
  denom_predpos <- TP + FP
  ppv_precision <- if (denom_predpos > 0L) TP / denom_predpos else NA_real_
  
  # F1 = 2 * precision * recall / (precision + recall)
  f1 <- if (is.finite(ppv_precision) && is.finite(tpr_emitters) &&
            (ppv_precision + tpr_emitters) > 0) {
    2 * ppv_precision * tpr_emitters / (ppv_precision + tpr_emitters)
  } else {
    NA_real_
  }
  
  predicted_positive_rate <- mean(pred_pos)
  
  # -----------------------------
  # Non-emitter prediction summaries (severity / leakage)
  # -----------------------------
  yhat_nonemit <- yhat[!is_emitter]
  q_nonemit <- .safe_quantile(yhat_nonemit, c(0.50, 0.90, 0.95, 0.99))
  
  mean_pred_nonemit <- if (length(yhat_nonemit) > 0L) mean(yhat_nonemit) else NA_real_
  
  # -----------------------------
  # Optional additional stats I recommend
  # -----------------------------
  # 1) “Emitter mass captured” by predicted positives:
  #    Share of true emissions coming from obs with yhat > threshold.
  emitter_mass_captured <- NA_real_
  total_emissions <- sum(y)
  if (is.finite(total_emissions) && total_emissions > 0) {
    emitter_mass_captured <- sum(y[pred_pos]) / total_emissions
  }
  
  # 2) For emitters only: MAPD (mean abs percent deviation) on emitters
  mapd_emitters <- NA_real_
  if (length(idx_pos_y) > 0L) {
    mapd_emitters <- mean(abs(err[idx_pos_y]) / y[idx_pos_y])
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
    
    # confusion matrix counts (often useful for debugging)
    TP = TP, FP = FP, TN = TN, FN = FN
  )
}
