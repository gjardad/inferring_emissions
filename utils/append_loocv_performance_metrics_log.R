###############################################################################
# utils/append_loocv_performance_metrics_log.R
#
# PURPOSE
#   Append new metrics rows to a central performance log stored as .rds and .csv,
#   with optional de-duplication.
#
# INTERFACE
#   append_metrics_log(new_tbl, rds_path, csv_path, dedup=TRUE)
#
# DEDUP LOGIC
#   When dedup=TRUE, we drop duplicates based on a stable key:
#     model_family, partial_pooling, step_tag, sample_tag, proxy_tag, variant
#   keeping the last row (most recent run_ts).
###############################################################################

append_metrics_log <- function(new_tbl,
                               rds_path,
                               csv_path,
                               dedup = TRUE) {
  
  suppressPackageStartupMessages({
    library(data.table)
  })
  
  new_tbl <- as.data.table(new_tbl)
  
  # Basic sanity: expected ID cols
  required <- c("model_family", "partial_pooling", "step_tag", "sample_tag", "proxy_tag", "variant")
  missing_req <- setdiff(required, names(new_tbl))
  if (length(missing_req) > 0) {
    stop("new_tbl missing required columns: ", paste(missing_req, collapse = ", "))
  }
  
  # Load existing if present
  if (file.exists(rds_path)) {
    old <- readRDS(rds_path)
    old <- as.data.table(old)
    all_tbl <- rbindlist(list(old, new_tbl), fill = TRUE)
  } else {
    all_tbl <- copy(new_tbl)
  }
  
  # Deduplicate
  if (isTRUE(dedup)) {
    key_cols <- required
    # ensure run_ts exists for ordering (if absent, create)
    if (!("run_ts" %in% names(all_tbl))) {
      all_tbl[, run_ts := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]
    }
    # keep last by run_ts within key
    setorderv(all_tbl, c(key_cols, "run_ts"))
    all_tbl <- all_tbl[, .SD[.N], by = key_cols]
  }
  
  # Save
  dir.create(dirname(rds_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(all_tbl, rds_path)
  
  if (!missing(csv_path) && !is.null(csv_path)) {
    dir.create(dirname(csv_path), showWarnings = FALSE, recursive = TRUE)
    data.table::fwrite(all_tbl, csv_path)
  }
  
  invisible(all_tbl)
}
