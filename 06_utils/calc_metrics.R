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
  # nRMSE (all observations)
  # -----------------------
  rmse  <- sqrt(mean((y - yhat)^2))
  nrmse <- rmse / mean(y)
  
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
  
  # -----------------------
  # R2 (LOO-style)
  # -----------------------
  denom <- sum((y - mean(y))^2)
  r2loo <- if (denom > 0) 1 - sum((y - yhat)^2) / denom else NA_real_
  
  # -----------------------
  # Spearman rank correlation
  # -----------------------
  spear <- suppressWarnings(cor(y, yhat, method = "spearman"))
  
  list(
    nRMSE = nrmse,
    MAPD_emitters = mapd_emitters,
    FPR_nonemitters = fpr,
    R2_LOO = r2loo,
    spearman = spear
  )
}
