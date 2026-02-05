###############################################################################
<<<<<<< HEAD
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
=======
# 05_append_loocv_performance_metrics_log.R
#
# PURPOSE
#   Append/update the on-disk log of LOOCV/LOFOCV performance metrics.
#   This log is used later to build summary tables/figures across runs.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/04_loocv/05_append_loocv_performance_metrics_log.R
#
# BEHAVIOR
#   - Appends new rows to an authoritative RDS log (preserves column types)
#   - Optionally writes a CSV snapshot for quick inspection
#   - Optionally de-duplicates by a key that *distinguishes raw vs calibrated*
#     (via 'variant' and 'step_effective').
#
# DESIGN NOTES
#   - De-dup keeps the *latest appended* row for each dedup key.
#     This makes re-running the same spec overwrite prior entries (in effect).
###############################################################################

append_metrics_log <- function(metrics_tbl,
                               rds_path,
                               csv_path = NULL,
                               dedup = TRUE,
                               # New default keys (distinguish raw vs calibrated)
                               dedup_cols = c("model", "variant", "step_effective",
                                              "sample", "sectorfe", "fuel_proxy"),
                               sort_cols  = c("model", "sample", "sectorfe", "fuel_proxy",
                                              "step_effective", "variant")) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for append_metrics_log().")
  }

  DTnew <- data.table::as.data.table(metrics_tbl)

  # ---- basic checks: ensure directories exist ----
  if (!file.exists(dirname(rds_path))) {
    dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
  }

  # ---- read + append ----
  if (file.exists(rds_path)) {
    DTold <- data.table::as.data.table(readRDS(rds_path))
    DTall <- data.table::rbindlist(list(DTold, DTnew), use.names = TRUE, fill = TRUE)
  } else {
    DTall <- DTnew
  }

  # ---- de-duplicate ----
  if (dedup) {
    # Use only dedup columns that exist (allows older logs/rows to coexist)
    dedup_use <- dedup_cols[dedup_cols %in% names(DTall)]
    if (length(dedup_use) == 0L) {
      stop("Cannot de-duplicate: none of dedup_cols exist in the metrics table.")
    }

    # Sort by available sort columns (for stable behavior)
    sort_use <- sort_cols[sort_cols %in% names(DTall)]
    if (length(sort_use) > 0L) data.table::setorderv(DTall, sort_use)

    # Keep the last row per key (latest appended wins)
    DTall <- DTall[, .SD[.N], by = dedup_use]
  }

  # ---- save authoritative log ----
  saveRDS(DTall, rds_path)

  # ---- optional CSV snapshot ----
  if (!is.null(csv_path)) {
    if (!file.exists(dirname(csv_path))) {
      dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
    }
    data.table::fwrite(DTall, file = csv_path)
  }

  DTall[]
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
}
