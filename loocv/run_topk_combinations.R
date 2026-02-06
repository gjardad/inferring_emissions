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
# run_topk_combinations.R
# ============================================================================ #
# PURPOSE
#   After prescreening, evaluate only the KxK combinations of the top-k step-1
#   proxies and top-k step-2 proxies with your FULL two-step (hurdle) model.
#
#   - Inputs are the outputs from prescreen_proxies.R (topk_step1/topk_step2),
#     or any two tables that list proxies you want to combine.
#   - The hurdle model runner is user-supplied and model-agnostic.
#
# DESIGN
#   - This script only builds a combination grid and calls hurdle_runner().
#   - hurdle_runner() MUST return a standardized metrics table (via
#     build_metrics_table()) and include BOTH proxy identifiers (step1 + step2)
#     inside that metrics table (e.g., proxy_tag_step1 / proxy_tag_step2).
#
# INPUTS (via function args)
#   topk_step1, topk_step2 : data.table
#       Must contain at least a proxy identifier column. By default we look for:
#         - step 1: "proxy_file" or "proxy_path" or "proxy_file_step1"
#         - step 2: "proxy_file" or "proxy_path" or "proxy_file_step2"
#       If your tables instead contain only proxy_tag, you can map to paths
#       before calling this script.
#
#   hurdle_runner(step1_proxy_file, step2_proxy_file, df_run, syt_run, ...) -> list(metrics=dt)
#
#   out_dir : character
#       Saves:
#         - combo_grid.rds
#         - hurdle_metrics_topk_combos.rds
#
# OUTPUTS
#   Returns list:
#     $combo_grid    : data.table (KxK)
#     $combo_metrics : data.table (stacked)
# ============================================================================ #

suppressPackageStartupMessages({
  library(data.table)
})

rbindlist_safe <- function(x) {
  x <- Filter(Negate(is.null), x)
  if (length(x) == 0) return(data.table())
  rbindlist(x, fill = TRUE, use.names = TRUE)
}

#' Extract a proxy path column from a table (robust to naming)
get_proxy_path_col <- function(dt, step = c("step1","step2")) {
  step <- match.arg(step)
  candidates <- c(
    sprintf("proxy_file_%s", step),
    sprintf("proxy_path_%s", step),
    "proxy_file", "proxy_path", "proxy_rds", "proxy_file_path"
  )
  hit <- candidates[candidates %in% names(dt)]
  if (length(hit) == 0) {
    stop(sprintf("Could not find a proxy path column for %s. Tried: %s",
                 step, paste(candidates, collapse = ", ")))
  }
  hit[[1]]
}

#' Build the KxK grid of combinations
build_combo_grid <- function(topk_step1, topk_step2) {
  stopifnot(is.data.table(topk_step1), is.data.table(topk_step2))

  c1 <- get_proxy_path_col(topk_step1, "step1")
  c2 <- get_proxy_path_col(topk_step2, "step2")

  dt1 <- unique(topk_step1[, .(step1_proxy_file = get(c1))])
  dt2 <- unique(topk_step2[, .(step2_proxy_file = get(c2))])

  CJ(step1_proxy_file = dt1$step1_proxy_file,
     step2_proxy_file = dt2$step2_proxy_file,
     unique = TRUE)
}

#' Evaluate the hurdle model on each (step1, step2) combo
run_hurdle_on_grid <- function(combo_grid,
                               hurdle_runner,
                               df_run,
                               syt_run,
                               progress_every = 10,
                               ...) {
  stopifnot(is.data.table(combo_grid))
  stopifnot(is.function(hurdle_runner))
  stopifnot(all(c("step1_proxy_file","step2_proxy_file") %in% names(combo_grid)))

  out_list <- vector("list", nrow(combo_grid))

  for (i in seq_len(nrow(combo_grid))) {
    p1 <- combo_grid$step1_proxy_file[[i]]
    p2 <- combo_grid$step2_proxy_file[[i]]

    if (i %% progress_every == 0) {
      message(sprintf("[topk-combos] %d / %d : (%s) x (%s)",
                      i, nrow(combo_grid), basename(p1), basename(p2)))
    }

    res <- tryCatch(
      hurdle_runner(step1_proxy_file = p1,
                    step2_proxy_file = p2,
                    df_run = df_run,
                    syt_run = syt_run,
                    ...),
      error = function(e) {
        warning(sprintf("[topk-combos] FAILED on (%s, %s): %s",
                        basename(p1), basename(p2), e$message))
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

#' Main orchestrator
run_topk_combinations <- function(topk_step1,
                                  topk_step2,
                                  df_run,
                                  syt_run,
                                  hurdle_runner,
                                  out_dir = NULL,
                                  ...) {
  combo_grid <- build_combo_grid(as.data.table(topk_step1),
                                 as.data.table(topk_step2))

  combo_metrics <- run_hurdle_on_grid(
    combo_grid = combo_grid,
    hurdle_runner = hurdle_runner,
    df_run = df_run,
    syt_run = syt_run,
    ...
  )

  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(combo_grid,   file.path(out_dir, "combo_grid.rds"))
    saveRDS(combo_metrics,file.path(out_dir, "hurdle_metrics_topk_combos.rds"))
  }

  list(combo_grid = combo_grid,
       combo_metrics = combo_metrics)
}

# ---------------------------------------------------------------------------- #
# Example usage
# ---------------------------------------------------------------------------- #
# topk_step1 <- readRDS("loocv_pipeline/04_loocv/cache_prescreen/topk_step1.rds")
# topk_step2 <- readRDS("loocv_pipeline/04_loocv/cache_prescreen/topk_step2.rds")
#
# # hurdle_runner <- function(step1_proxy_file, step2_proxy_file, df_run, syt_run, ...) { ... }
#
# out <- run_topk_combinations(
#   topk_step1, topk_step2,
#   df_run = df_run, syt_run = syt_run,
#   hurdle_runner = hurdle_runner,
#   out_dir = "loocv_pipeline/04_loocv/cache_topk_combos"
# )
