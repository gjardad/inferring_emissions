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

# ============================================================================ #
# prescreen_proxies.R
# ============================================================================ #
# PURPOSE
#   Fast "prescreening" stage for a large proxy library.
#
#   - Run a lightweight evaluation for EACH proxy within:
#       (i)  step 1 (extensive margin proxy set), and
#       (ii) step 2 (intensive margin proxy set)
#   - Rank proxies by a user-chosen metric and keep only the top-k for each step.
#
# DESIGN PRINCIPLES
#   - Model-agnostic: prescreening does not know whether you use PPML, logit,
#     GAM, etc. You pass a user-supplied model_runner_stepX().
#   - The step-level model runner MUST return a standardized metrics table
#     (typically created by build_metrics_table()).
#   - This script only orchestrates:
#       1) enumerating proxies,
#       2) running step-level model runners,
#       3) ranking and selecting top-k,
#       4) writing results to disk.
#
# INPUTS (via function args)
#   proxy_files_step1, proxy_files_step2 : character vectors
#       Paths to proxy_*.rds files for step 1 and step 2.
#
#   df_run, syt_run : training data + auxiliary objects
#       Passed through to the model_runner functions unchanged.
#
#   model_runner_step1(proxy_file, df_run, syt_run, ...) -> list(metrics=dt)
#   model_runner_step2(proxy_file, df_run, syt_run, ...) -> list(metrics=dt)
#
#   k : integer
#       Number of top proxies to keep per step.
#
#   rank_metric : character
#       Column in the returned metrics table used to rank proxies (default: "nRMSE").
#
#   smaller_is_better : logical
#       TRUE => lower rank_metric is better (e.g., nRMSE, RMSE, MAPD).
#       FALSE => higher rank_metric is better (e.g., spearman, R2).
#
#   out_dir : character
#       Directory to save:
#         - prescreen_metrics_step1.rds
#         - prescreen_metrics_step2.rds
#         - topk_step1.rds
#         - topk_step2.rds
#
# OUTPUTS
#   Returns a list with:
#     $metrics_step1, $metrics_step2 : data.table
#     $topk_step1, $topk_step2       : data.table
#
# NOTES
#   - If you want prescreening to be cheaper than the full LOFOCV, implement the
#     speedups inside model_runner_stepX(): e.g. use a LOFOCV subsample, fewer
#     folds, fewer iterations, etc.
# ============================================================================ #

suppressPackageStartupMessages({
  library(data.table)
})

#' Safe row-bind list of data.tables (ignores NULLs)
rbindlist_safe <- function(x) {
  x <- Filter(Negate(is.null), x)
  if (length(x) == 0) return(data.table())
  rbindlist(x, fill = TRUE, use.names = TRUE)
}

#' Rank proxies within a step by a chosen metric and keep top-k
#'
#' @param metrics_dt data.table with at least columns proxy_tag and rank_metric
#' @param k integer
#' @param rank_metric character
#' @param smaller_is_better logical
#' @param ties_method passed to data.table::frank (default "min")
#' @return data.table subset ordered best->worst with an added column rank
select_topk <- function(metrics_dt,
                        k,
                        rank_metric = "nRMSE",
                        smaller_is_better = TRUE,
                        ties_method = "min",
                        proxy_id_col = "proxy_tag") {
  stopifnot(is.data.table(metrics_dt))
  stopifnot(rank_metric %in% names(metrics_dt))
  stopifnot(proxy_id_col %in% names(metrics_dt))
  
  dt <- copy(metrics_dt)
  dt <- dt[is.finite(get(rank_metric))]
  
  if (nrow(dt) == 0) {
    warning("No finite values for rank_metric; returning empty top-k.")
    return(dt[0])
  }
  
  dt[, rank := frank(get(rank_metric),
                     ties.method = ties_method,
                     na.last = "keep",
                     decreasing = !smaller_is_better)]
  
  dt <- dt[order(rank, get(rank_metric), get(proxy_id_col))]
  dt <- dt[rank <= k]
  dt <- dt[order(rank, get(rank_metric), get(proxy_id_col))]
  
  dt
}

#' Prescreen a set of proxies for a given step
#'
#' @param proxy_files character vector of .rds proxy paths
#' @param model_runner function(proxy_file, df_run, syt_run, ...) -> list(metrics=dt)
#' @param step_name character ("step1" or "step2") used only for messages
#' @param ... passed to model_runner
#' @return data.table of stacked metrics
prescreen_one_step <- function(proxy_files,
                               model_runner,
                               df_run,
                               syt_run,
                               step_name = "step",
                               progress_every = 25,
                               ...) {
  if (length(proxy_files) == 0) return(data.table())

  out_list <- vector("list", length(proxy_files))

  for (i in seq_along(proxy_files)) {
    pf <- proxy_files[[i]]

    if (i %% progress_every == 0) {
      message(sprintf("[prescreen:%s] %d / %d : %s",
                      step_name, i, length(proxy_files), basename(pf)))
    }

    res <- tryCatch(
      model_runner(proxy_file = pf, df_run = df_run, syt_run = syt_run, ...),
      error = function(e) {
        warning(sprintf("[prescreen:%s] FAILED on %s : %s",
                        step_name, basename(pf), e$message))
        NULL
      }
    )

    if (!is.null(res) && !is.null(res$metrics)) {
      out_list[[i]] <- as.data.table(res$metrics)
    } else {
      out_list[[i]] <- NULL
    }
  }

  rbindlist_safe(out_list)
}

#' Main prescreen orchestrator: run step 1 + step 2, select top-k for each
prescreen_proxies <- function(proxy_files_step1,
                              proxy_files_step2,
                              df_run,
                              syt_run,
                              model_runner_step1,
                              model_runner_step2,
                              k = 10L,
                              rank_metric = "nRMSE",
                              smaller_is_better = TRUE,
                              out_dir = NULL,
                              rank_variant = "raw",
                              ...) {
  stopifnot(is.function(model_runner_step1), is.function(model_runner_step2))
  stopifnot(is.numeric(k), k >= 1)

  metrics_step1 <- prescreen_one_step(
    proxy_files = proxy_files_step1,
    model_runner = model_runner_step1,
    df_run = df_run, syt_run = syt_run,
    step_name = "step1",
    ...
  )

  metrics_step2 <- prescreen_one_step(
    proxy_files = proxy_files_step2,
    model_runner = model_runner_step2,
    df_run = df_run, syt_run = syt_run,
    step_name = "step2",
    ...
  )
  
  if ("variant" %in% names(metrics_step1)) metrics_step1 <- metrics_step1[variant == rank_variant]
  if ("variant" %in% names(metrics_step2)) metrics_step2 <- metrics_step2[variant == rank_variant]

  # Sanity: ensure proxy_tag exists
  if (nrow(metrics_step1) > 0 && !"proxy_tag_ext" %in% names(metrics_step1)) {
    stop("metrics_step1 lacks proxy_tag_ext. Ensure build_metrics_table() outputs it, or pass it via extra_id_cols.")
  }
  if (nrow(metrics_step2) > 0 && !"proxy_tag_int" %in% names(metrics_step2)) {
    stop("metrics_step2 lacks proxy_tag_int. Ensure build_metrics_table() outputs it, or pass it via extra_id_cols.")
  }
  

  topk_step1 <- if (nrow(metrics_step1) > 0) select_topk(metrics_step1, k, rank_metric, smaller_is_better, proxy_id_col="proxy_tag_ext") else data.table()
  topk_step2 <- if (nrow(metrics_step2) > 0) select_topk(metrics_step2, k, rank_metric, smaller_is_better, proxy_id_col="proxy_tag_int") else data.table()

  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(metrics_step1, file.path(out_dir, "prescreen_metrics_step1.rds"))
    saveRDS(metrics_step2, file.path(out_dir, "prescreen_metrics_step2.rds"))
    saveRDS(topk_step1,    file.path(out_dir, "topk_step1.rds"))
    saveRDS(topk_step2,    file.path(out_dir, "topk_step2.rds"))
  }

  list(
    metrics_step1 = metrics_step1,
    metrics_step2 = metrics_step2,
    topk_step1 = topk_step1,
    topk_step2 = topk_step2
  )
}

# ---------------------------------------------------------------------------- #
# Example usage (edit paths + runners to match your pipeline)
# ---------------------------------------------------------------------------- #
# proxy_files_step1 <- list.files("loocv_pipeline/02_proxies/cache_step1",
#                                 pattern = "\\.rds$", full.names = TRUE)
# proxy_files_step2 <- list.files("loocv_pipeline/02_proxies/cache_step2",
#                                 pattern = "\\.rds$", full.names = TRUE)
#
# # model_runner_step1 <- function(proxy_file, df_run, syt_run, ...) { ... }
# # model_runner_step2 <- function(proxy_file, df_run, syt_run, ...) { ... }
#
# out <- prescreen_proxies(
#   proxy_files_step1, proxy_files_step2,
#   df_run = df_run, syt_run = syt_run,
#   model_runner_step1 = model_runner_step1,
#   model_runner_step2 = model_runner_step2,
#   k = 10,
#   rank_metric = "nRMSE",
#   smaller_is_better = TRUE,
#   out_dir = "loocv_pipeline/04_loocv/cache_prescreen"
# )
