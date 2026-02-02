###############################################################################
# 04_build_metrics_table.R
#
# PURPOSE
#   Build a standardized performance-metrics table from a LOOCV/LOFOCV run,
#   tagging each row with model metadata and (optionally) splitting metrics
#   by prediction variant (raw vs calibrated).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/04_loocv/04_build_metrics_table.R
#
# EXPECTED INPUT
#   - out: a list produced by a LOOCV runner that contains:
#       out$metrics : a data.frame / data.table with one or more rows of metrics.
#     Typically contains two rows: raw and calibrated.
#
# WHAT THIS RETURNS
#   A data.table with:
#     - metadata columns: model, step, step_effective, sample, sectorfe, fuel_proxy
#     - estimation sample sizes: n_obs_est, n_firms_est
#     - metric columns preserved from out$metrics (e.g., nRMSE, R2_LOO, spearman, ...)
#
# DESIGN NOTES
#   - 'variant' is used to distinguish raw vs calibrated rows. If out$metrics has
#     a column like 'variant'/'model'/'type', it will be normalized to:
#       "raw" or "calibrated"
#   - 'step_effective' maps:
#       raw        -> step
#       calibrated -> step + "+3" (unless step already includes 3)
###############################################################################

build_metrics_table <- function(out,
                                model,
                                step,
                                sample,
                                sectorfe,
                                fuel_proxy,
                                n_obs_est   = NA_integer_,
                                n_firms_est = NA_integer_,
                                # how to detect raw vs calibrated inside out$metrics
                                variant_col_candidates = c("variant", "model", "type"),
                                calibrated_values = c("cal", "calibrated", "yhat_cal", "calibrated_pred"),
                                raw_values        = c("raw", "uncal", "uncalibrated", "yhat_raw", "raw_pred")) {

  if (is.null(out$metrics)) stop("out$metrics is missing.")

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for build_metrics_table().")
  }

  DTm <- data.table::as.data.table(out$metrics)

  # ----------------------------
  # 1) Identify variant (raw vs calibrated)
  # ----------------------------
  variant_col <- intersect(variant_col_candidates, names(DTm))
  if (length(variant_col) == 0L) {
    # If out$metrics is a single-row table with no label, assume "raw"
    DTm[, variant := "raw"]
  } else {
    variant_col <- variant_col[1L]
    DTm[, variant := as.character(get(variant_col))]

    # Normalize common spellings to raw/calibrated
    vlow <- tolower(DTm$variant)
    DTm[, variant := data.table::fifelse(
      vlow %in% tolower(calibrated_values), "calibrated",
      data.table::fifelse(vlow %in% tolower(raw_values), "raw", DTm$variant)
    )]
  }

  # ----------------------------
  # 2) Compute "step_effective"
  #    raw -> step
  #    calibrated -> step + "+3" unless 3 already included
  # ----------------------------
  step_chr <- as.character(step)

  add_3_if_missing <- function(s) {
    # If it already contains a 3 as a standalone step, don't add.
    # Handles "3", "1+3", "2+3", "1+2+3".
    if (grepl("(^|\\+)3(\\+|$)", s)) return(s)
    paste0(s, "+3")
  }

  DTm[, step_effective := data.table::fifelse(
    tolower(variant) %in% c("calibrated", "cal"),
    vapply(step_chr, add_3_if_missing, character(1)),
    step_chr
  )]

  # ----------------------------
  # 3) Add metadata tags + estimation sample sizes
  # ----------------------------
  DTm[, `:=`(
    model       = as.character(model),
    step        = step_chr,                 # what you passed
    sample      = as.character(sample),
    sectorfe    = as.character(sectorfe),
    fuel_proxy  = as.character(fuel_proxy),
    n_obs_est   = as.integer(n_obs_est),
    n_firms_est = as.integer(n_firms_est)
  )]
  
  # If fuel_proxy and proxy both exist, drop the legacy one from out$metrics
  if ("proxy" %in% names(DTm) && "fuel_proxy" %in% names(DTm)) {
    DTm[, proxy := NULL]
  }

  # ----------------------------
  # 4) Preferred column order (reorder only those that exist)
  # ----------------------------
  preferred <- c(
    "model", "variant", "step", "step_effective",
    "sample", "sectorfe", "fuel_proxy",
    "n_obs_est", "n_firms_est",
    "nRMSE", "RMSE", "R2_LOO",
    "MAPD_emitters",
    "FPR_nonemitters", "med_yhat_nonemit",
    "p90_yhat_nonemit", "max_yhat_nonemit",
    "pctile_all_yhat_at_med_nonemit",
    "pctile_all_yhat_at_max_nonemit",
    "spearman",
    "eval_dropped_singletons"
  )

  existing <- preferred[preferred %in% names(DTm)]
  rest     <- setdiff(names(DTm), existing)
  data.table::setcolorder(DTm, c(existing, rest))

  DTm[]
}
