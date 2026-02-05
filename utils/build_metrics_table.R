###############################################################################
# utils/build_metrics_table.R
#
# PURPOSE
#   Convert an "out" object produced by model estimators (ppml.R, hurdle.R, etc.)
#   into a standardized, log-ready metrics table with stable run identifiers.
#
# EXPECTED INPUT (out)
#   out$metrics : data.table with at least:
#       variant  ("raw" / "calibrated")
#       proxy    (string tag for proxy spec; can be "none" or combo tags)
#       nRMSE, RMSE, ... (any metrics; extra columns are fine)
#
# INTERFACE
#   build_metrics_table(
#     out,
#     model_family,        e.g. "ppml" or "hurdle"
#     partial_pooling,     "yes" or "no" (string to keep stable in logs)
#     step_tag,            e.g. "2" or "12"
#     sample_tag,          e.g. "all" or "subsample"
#     proxy_tag,           e.g. "none" or "ext=...|int=..."
#     extra_id_cols=list() additional scalar identifiers to bind as columns
#   )
#
# OUTPUT
#   A data.table with one row per variant in out$metrics, plus identifier columns.
###############################################################################

build_metrics_table <- function(out,
                                model_family,
                                partial_pooling,
                                step_tag,
                                sample_tag,
                                proxy_tag,
                                extra_id_cols = list()) {
  
  suppressPackageStartupMessages({
    library(data.table)
  })
  
  if (is.null(out$metrics)) stop("out$metrics is NULL. Estimator must return out$metrics.")
  M <- as.data.table(out$metrics)
  
  if (!("variant" %in% names(M))) stop("out$metrics must contain column 'variant'.")
  
  # Ensure proxy is present (estimator may already set it)
  if (!("proxy" %in% names(M))) {
    M[, proxy := proxy_tag]
  } else {
    M[is.na(proxy) | proxy == "", proxy := proxy_tag]
  }
  
  # -----------------------
  # Split proxy tag into ext/int (with overrides)
  # -----------------------
  proxy_tag_ext <- ""
  proxy_tag_int <- ""
  
  # 1) Explicit override via extra_id_cols (preferred)
  if (length(extra_id_cols) > 0) {
    if (!is.null(extra_id_cols$proxy_tag_ext)) proxy_tag_ext <- as.character(extra_id_cols$proxy_tag_ext)
    if (!is.null(extra_id_cols$proxy_tag_int)) proxy_tag_int <- as.character(extra_id_cols$proxy_tag_int)
  }
  
  # 2) If not explicitly set, infer from proxy_tag
  #    - If proxy_tag looks like "ext=...|int=..." parse it
  #    - Else treat it as "int" (models without step 1)
  if ((proxy_tag_ext == "" && proxy_tag_int == "") && !is.null(proxy_tag) && is.character(proxy_tag)) {
    if (grepl("^ext=.*\\|int=.*$", proxy_tag)) {
      proxy_tag_ext <- sub("^ext=([^|]+)\\|int=.*$", "\\1", proxy_tag)
      proxy_tag_int <- sub("^ext=.*\\|int=([^|]+)$", "\\1", proxy_tag)
    } else {
      proxy_tag_ext <- ""
      proxy_tag_int <- as.character(proxy_tag)
    }
  }
  
  # -----------------------
  # Add stable identifiers
  # -----------------------
  M[, model_family    := as.character(model_family)]
  M[, partial_pooling := as.character(partial_pooling)]
  M[, step_tag        := as.character(step_tag)]
  M[, sample_tag      := as.character(sample_tag)]
  M[, proxy_tag       := as.character(proxy_tag)]
  
  # NEW: split proxy tags as stable identifiers
  M[, proxy_tag_ext := as.character(proxy_tag_ext)]
  M[, proxy_tag_int := as.character(proxy_tag_int)]
  
  # Timestamp (useful even if you dedup)
  M[, run_ts := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]
  
  # Bind extra ID columns (must be scalars)
  if (length(extra_id_cols) > 0) {
    for (nm in names(extra_id_cols)) {
      val <- extra_id_cols[[nm]]
      if (length(val) != 1) stop("extra_id_cols[[", nm, "]] must be length-1 (scalar).")
      M[, (nm) := val]
    }
  }
  
  # Preferred column order: identifiers first, then metrics
  id_cols <- c(
    "run_ts", "model_family", "partial_pooling", "step_tag", "sample_tag",
    "proxy_tag_ext", "proxy_tag_int", "proxy_tag",
    "variant", "proxy"
  )
  keep_ids <- id_cols[id_cols %in% names(M)]
  metric_cols <- setdiff(names(M), keep_ids)
  
  M <- M[, c(keep_ids, metric_cols), with = FALSE]
  M[]
}
