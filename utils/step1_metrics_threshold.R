# ============== BEGIN SETTING UP PATHS ============= #
suppressPackageStartupMessages({
  library(data.table)
})

# ========================
# Define data paths ------
# =========================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

# ===========================
# Define paths for code -----
# ===========================

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}

repo_dir <- paste0(getwd(), "/inferring_emissions")
utils_dir <- file.path(repo_dir, "utils")
loocv_dir <- file.path(repo_dir, "loocv")

#================== END SETTING UP PATHS ================ #

###############################################################################
# step1_metrics_threshold.R
#
# PURPOSE
#   Compute **Step-1-only** (extensive margin) performance metrics for each
#   (proxy, threshold) pair. This is meant to be fast and simple:
#     - phat_dt is precomputed and cached for each proxy
#     - we only evaluate classification on emit__ using calc_metrics()
#     - thresholds are supplied explicitly (e.g., seq(0, 0.45, by = 0.05))
#
# WHY THIS EXISTS
#   The full hurdle evaluation (pairing step1 + step2) is expensive. If you want
#   to rank (proxy, threshold) pairs for Step 1 only, this lightweight routine
#   avoids running the full hurdle grid.
#
# INPUTS
#   base : list
#       Output from prep_hurdle_base_DT(). Must contain base$DT with columns:
#       id__/year__/emit__.
#
#   phat_paths : character vector
#       Each path is an .rds file containing:
#         list(proxy_name = <string>, phat_dt = <data.table>)
#
#   thresholds : numeric vector
#       Thresholds to evaluate. calc_metrics() will treat predicted positive as
#       yhat > fp_threshold, so we pass each threshold through fp_threshold.
#
#   progress_every : integer
#       Print a progress message every N proxies.
#
# OUTPUT
#   data.table with one row per (proxy, threshold) and columns:
#     - proxy_tag_ext, threshold, n, FPR_nonemitters, TPR_emitters, PPV_precision,
#       F1, and additional calc_metrics outputs if present.
###############################################################################

step1_threshold_metrics <- function(base,
                                    phat_paths,
                                    thresholds,
                                    progress_every = 50) {
  suppressPackageStartupMessages({
    library(data.table)
  })

  if (!is.list(base) || is.null(base$DT)) stop("base must be a list with base$DT.")
  DT0 <- as.data.table(base$DT)

  req_cols <- c("id__", "year__", "emit__")
  if (!all(req_cols %in% names(DT0))) {
    stop("base$DT is missing required columns: ", paste(setdiff(req_cols, names(DT0)), collapse = ", "))
  }

  if (length(phat_paths) == 0 || length(thresholds) == 0) {
    return(data.table())
  }

  # Standardize join keys once
  DT_join <- DT0[, .(id = id__, year = year__, emit__ = emit__)]

  metrics_list <- vector("list", length(phat_paths))

  for (i in seq_along(phat_paths)) {
    p <- phat_paths[[i]]
    if (i %% progress_every == 0) {
      message(sprintf("[step1-threshold-metrics] %d / %d : %s", i, length(phat_paths), basename(p)))
    }

    obj <- readRDS(p)
    if (is.null(obj$proxy_name) || is.null(obj$phat_dt)) next

    ph <- as.data.table(obj$phat_dt)
    if (!all(c("id", "year", "phat_raw") %in% names(ph))) {
      stop("phat_dt must contain columns id, year, phat_raw. Missing in: ", p)
    }

    dt <- merge(DT_join, ph[, .(id, year, phat_raw)], by = c("id", "year"), all.x = TRUE)
    dt[is.na(phat_raw), phat_raw := 0]

    rows <- vector("list", length(thresholds))
    for (j in seq_along(thresholds)) {
      thr <- thresholds[[j]]
      m <- calc_metrics(dt$emit__, dt$phat_raw, fp_threshold = thr)

      rows[[j]] <- data.table(
        proxy_tag_ext = as.character(obj$proxy_name),
        threshold = as.numeric(thr),
        n = m[["n"]],
        FPR_nonemitters = m[["fpr_nonemitters"]],
        TPR_emitters = m[["tpr_emitters"]],
        PPV_precision = m[["ppv_precision"]],
        F1 = m[["f1"]],
        predicted_positive_rate = m[["predicted_positive_rate"]],
        emitter_mass_captured = m[["emitter_mass_captured"]]
      )
    }

    metrics_list[[i]] <- rbindlist(rows, fill = TRUE)
  }

  rbindlist(metrics_list, fill = TRUE)
}
