###############################################################################
# 06_utils/calc_metrics.R
#
# PURPOSE
#   Metric computation helpers used by the metrics table builder (new system).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/06_utils/calc_metrics.R
###############################################################################

# ============================================================
# File: calc_metrics.R
#
# Purpose:
#   Defines calc_metrics(), a function to evaluate performance
#   models to predict firm-year emissions
#
# Metrics returned:
#   - nRMSE: RMSE normalized by mean(y), computed on all obs
#   - MAPD_emitters: Mean Absolute Percentage Deviation computed
#       ONLY on emitters (y > 0), i.e. mean(|yhat - y| / y | y>0)
#   - FPR_nonemitters: False Positive Rate among non-emitters,
#       Pr(yhat > fp_threshold | y == 0)
#   - R2_LOO: LOO-style R2 using the held-out sample mean as baseline
#   - spearman: Spearman rank correlation between y and yhat
#
# Notes:
#   - Automatically drops non-finite y/yhat pairs.
#   - fp_threshold defaults to 0 but can be set > 0 if desired
#     to ignore tiny positive predictions.
# ============================================================

calc_metrics <- function(y, yhat, fp_threshold = 0) {
  y    <- as.numeric(y)
  yhat <- as.numeric(yhat)
  
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]
  yhat <- yhat[ok]
  
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
