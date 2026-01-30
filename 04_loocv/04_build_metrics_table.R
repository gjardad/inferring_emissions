###############################################################################
# 04_loocv/04_build_metrics_table.R
#
# PURPOSE
#   Build consolidated LOOCV metrics table (new system).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/04_loocv/04_build_metrics_table.R
###############################################################################

# ============================================================
# File: build_metrics_table.R
#
# Purpose:
#   Defines build_metrics_table(), a helper to standardize and
#   tag model performance metrics produced by LOOCV runs.
#
# Adds metadata:
#   - n_obs_est: number of observations used to estimate the model
#   - n_firms_est: number of unique firms used in estimation
#
# Intended workflow:
#   - A LOOCV function returns out$metrics (one row per variant:
#     raw vs calibrated).
#   - build_metrics_table() adds identifying metadata and sample
#     size information, returning a tidy table ready to append
#     to a master performance log.
# ============================================================

build_metrics_table <- function(out,
                                model_family,
                                partial_pooling,
                                fuel_proxy,
                                sample_tag = NA_character_,
                                n_obs_est = NA_integer_,
                                n_firms_est = NA_integer_) {
  
  if (is.null(out$metrics)) stop("out$metrics is missing.")
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for build_metrics_table().")
  }
  DTm <- data.table::as.data.table(out$metrics)
  
  # Add metadata tags + estimation sample sizes
  DTm[, `:=`(
    model_family    = as.character(model_family),
    partial_pooling = as.character(partial_pooling),
    fuel_proxy      = as.character(fuel_proxy),
    sample_tag      = as.character(sample_tag),
    n_obs_est       = as.integer(n_obs_est),
    n_firms_est     = as.integer(n_firms_est)
  )]
  
  # Preferred column order (only reorder those that exist)
  preferred <- c(
    "model_family", "partial_pooling", "fuel_proxy", "model",
    "n_obs_est", "n_firms_est",
    "nRMSE", "MAPD_emitters", "FPR_nonemitters",
    "R2_LOO", "spearman",
    "eval_dropped_singletons",
    "sample_tag"
  )
  existing <- preferred[preferred %in% names(DTm)]
  rest <- setdiff(names(DTm), existing)
  data.table::setcolorder(DTm, c(existing, rest))
  
  DTm[]
}
