###############################################################################
# 04_loocv/05_append_loocv_performance_metrics_log.R
#
# PURPOSE
#   Append/update LOOCV performance metrics log (new system).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/04_loocv/05_append_loocv_performance_metrics_log.R
###############################################################################

# ============================================================
# File: append_loocv_performance_log.R
#
# Purpose:
#   Defines append_metrics_log(), a helper to append standardized
#   metrics tables (e.g., from build_metrics_table()) to a
#   persistent on-disk log for later summary tables/figures.
#
# Behavior:
#   - Appends to an RDS file (authoritative storage; preserves types)
#   - Optionally also writes a CSV snapshot for quick inspection
#   - Optionally de-duplicates rows based on a user-specified key
#
# Typical usage:
#   source(".../calc_metrics.R")
#   source(".../build_metrics_table.R")
#   source(".../append_metrics_log.R")
#
#   metrics_tbl <- build_metrics_table(out, "benchmark", "FE", "none", "sampleA")
#   metrics_all <- append_metrics_log(metrics_tbl, rds_path, csv_path)
#
# Requirements:
#   - data.table
# ============================================================

append_metrics_log <- function(metrics_tbl,
                               rds_path,
                               csv_path = NULL,
                               dedup = TRUE,
                               dedup_cols = c("model_family", "partial_pooling", "fuel_proxy", "model", "sample_tag"),
                               sort_cols = c("model_family", "partial_pooling", "fuel_proxy", "model", "sample_tag")) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for append_metrics_log().")
  }
  DTnew <- data.table::as.data.table(metrics_tbl)
  
  # Basic checks
  if (!file.exists(dirname(rds_path))) {
    dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
  }
  
  if (file.exists(rds_path)) {
    DTold <- data.table::as.data.table(readRDS(rds_path))
    DTall <- data.table::rbindlist(list(DTold, DTnew), use.names = TRUE, fill = TRUE)
  } else {
    DTall <- DTnew
  }
  
  # De-duplicate if requested
  if (dedup) {
    missing_cols <- setdiff(dedup_cols, names(DTall))
    if (length(missing_cols) > 0) {
      stop("Cannot de-duplicate: missing columns in metrics table: ",
           paste(missing_cols, collapse = ", "))
    }
    data.table::setorderv(DTall, sort_cols[sort_cols %in% names(DTall)])
    DTall <- unique(DTall, by = dedup_cols)
  }
  
  # Save authoritative log
  saveRDS(DTall, rds_path)
  
  # Optional CSV snapshot
  if (!is.null(csv_path)) {
    if (!file.exists(dirname(csv_path))) {
      dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
    }
    data.table::fwrite(DTall, file = csv_path)
  }
  
  DTall[]
}
