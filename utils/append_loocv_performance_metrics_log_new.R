###############################################################################
# append_loocv_performance_metrics_log.R
#
# PURPOSE
#   Append/update the on-disk log of LOOCV/LOFOCV performance metrics.
#   This log is used later to build summary tables/figures across runs.
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

suppressPackageStartupMessages({
  library(data.table)
})

append_metrics_log <- function(
    new_rows,
    rds_path,
    csv_path = NULL,
    dedup = TRUE,
    # If NULL, we'll auto-choose a good dedup key based on columns present
    dedup_cols = NULL,
    sort_cols = NULL
) {
  new_dt <- as.data.table(new_rows)
  
  # ---------------------------
  # Load old log if exists
  # ---------------------------
  old_dt <- NULL
  if (!is.null(rds_path) && file.exists(rds_path)) {
    old_dt <- readRDS(rds_path)
    old_dt <- as.data.table(old_dt)
  }
  
  # ---------------------------
  # Combine
  # ---------------------------
  if (is.null(old_dt)) {
    combined <- copy(new_dt)
  } else {
    # align columns (fill missing with NA)
    combined <- rbindlist(list(old_dt, new_dt), fill = TRUE, use.names = TRUE)
  }
  
  # ---------------------------
  # Dedup key
  # ---------------------------
  if (isTRUE(dedup)) {
    if (is.null(dedup_cols)) {
      # prefer proxy pairs when present
      candidates <- c(
        "model_family",
        "partial_pooling",
        "step_tag",
        "sample_tag",
        "variant",
        "fuel_proxy_ext",
        "fuel_proxy_int",
        "fuel_proxy"
      )
      dedup_cols <- candidates[candidates %in% names(combined)]
    } else {
      dedup_cols <- dedup_cols[dedup_cols %in% names(combined)]
    }
    
    if (length(dedup_cols) > 0) {
      # keep most recent row per dedup key
      # ensure timestamp exists; if not, create a monotone index
      if (!("timestamp" %in% names(combined))) {
        combined[, timestamp := NA_character_]
        combined[, .row_id__ := .I]
        combined <- combined[order(.row_id__)]
        combined <- combined[, .SD[.N], by = dedup_cols]
        combined[, .row_id__ := NULL]
      } else {
        # order by timestamp then keep last per key
        suppressWarnings({
          combined[, timestamp__ := as.POSIXct(timestamp, tz = "UTC")]
        })
        combined <- combined[order(timestamp__, na.last = TRUE)]
        combined <- combined[, .SD[.N], by = dedup_cols]
        combined[, timestamp__ := NULL]
      }
    }
  }
  
  # ---------------------------
  # Sorting
  # ---------------------------
  if (is.null(sort_cols)) {
    sort_candidates <- c("model_family","partial_pooling","step_tag","sample_tag","variant","fuel_proxy_ext","fuel_proxy_int","fuel_proxy")
    sort_cols <- sort_candidates[sort_candidates %in% names(combined)]
  } else {
    sort_cols <- sort_cols[sort_cols %in% names(combined)]
  }
  
  if (length(sort_cols) > 0) {
    setorderv(combined, cols = sort_cols)
  }
  
  # ---------------------------
  # Save
  # ---------------------------
  if (!is.null(rds_path)) {
    dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(combined, rds_path)
  }
  
  if (!is.null(csv_path)) {
    dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
    fwrite(combined, csv_path)
  }
  
  combined
}
