###############################################################################
# 04_build_metrics_table.R
#
# PURPOSE
#   Build a standardized performance-metrics table from a LOOCV/LOFOCV run,
#   tagging each row with model metadata and (optionally) splitting metrics
#   by prediction variant (raw vs calibrated).
#
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

suppressPackageStartupMessages({
  library(data.table)
})

build_metrics_table <- function(
    out,
    model_family,
    partial_pooling,
    step_tag = NA_character_,      # e.g. "2", "1+2", "1+2+3"
    sample_tag = NA_character_,    # e.g. "all", "subsample", etc.
    proxy_tag = NA_character_,     # legacy single proxy label
    proxy_tag_ext = NA_character_, # for hurdle: extensive proxy label
    proxy_tag_int = NA_character_, # for hurdle: intensive proxy label
    extra_id_cols = list()         # named list of additional identifiers to include
) {
  # ---------------------------
  # Extract metrics payload
  # ---------------------------
  metrics_payload <- NULL
  
  if (is.list(out) && !is.null(out$metrics)) {
    metrics_payload <- out$metrics
  } else {
    # assume `out` itself is a metrics list or 1-row df/dt
    metrics_payload <- out
  }
  
  # If it's a data.frame/data.table with multiple rows (e.g. raw vs calibrated),
  # keep all rows, but still attach identifiers.
  if (is.data.frame(metrics_payload) || is.data.table(metrics_payload)) {
    metrics_dt <- as.data.table(metrics_payload)
  } else if (is.list(metrics_payload)) {
    metrics_dt <- as.data.table(as.list(metrics_payload))
  } else {
    stop("build_metrics_table: can't interpret metrics payload.")
  }
  
  # ---------------------------
  # Standard identifiers
  # ---------------------------
  id_dt <- data.table(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    model_family = as.character(model_family),
    partial_pooling = as.character(partial_pooling),
    step_tag = as.character(step_tag),
    sample_tag = as.character(sample_tag)
  )
  
  # ---------------------------
  # Proxy tags (support single or pairs)
  # ---------------------------
  # We prefer explicit ext/int tags when present; otherwise fall back to proxy_tag.
  if (!is.na(proxy_tag_ext) || !is.na(proxy_tag_int)) {
    id_dt[, fuel_proxy_ext := as.character(proxy_tag_ext)]
    id_dt[, fuel_proxy_int := as.character(proxy_tag_int)]
    # Backward compatible composite label
    id_dt[, fuel_proxy := paste0("ext=", fuel_proxy_ext, "|int=", fuel_proxy_int)]
  } else {
    id_dt[, fuel_proxy := as.character(proxy_tag)]
    # Keep columns existing in older logs if you want
    id_dt[, fuel_proxy_ext := NA_character_]
    id_dt[, fuel_proxy_int := NA_character_]
  }
  
  # ---------------------------
  # Extra identifiers (optional)
  # ---------------------------
  if (length(extra_id_cols) > 0) {
    for (nm in names(extra_id_cols)) {
      id_dt[[nm]] <- extra_id_cols[[nm]]
    }
  }
  
  # ---------------------------
  # Bind identifiers + metrics
  # ---------------------------
  out_dt <- cbind(id_dt, metrics_dt)
  
  # ---------------------------
  # Optional: stable column ordering (keep what exists)
  # ---------------------------
  preferred_front <- c(
    "timestamp","model_family","partial_pooling","step_tag","sample_tag",
    "fuel_proxy","fuel_proxy_ext","fuel_proxy_int",
    "variant" # if your model outputs raw/calibrated variants
  )
  existing_front <- preferred_front[preferred_front %in% names(out_dt)]
  remaining <- setdiff(names(out_dt), existing_front)
  setcolorder(out_dt, c(existing_front, remaining))
  
  out_dt
}

