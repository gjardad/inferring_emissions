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
# progress_utils.R
#
# PURPOSE:
#   Utilities for:
#   - logging progress with timestamps (log_step)
#   - tracking runtime for scripts/steps (tic/toc)
#   - printing progress + ETA in loops (progress_eta)
#
# EXPECTED LOCATION:
#   /code/loocv/functions/progress_utils.R
###############################################################################

if (!exists(".timers_env", inherits = FALSE)) {
  .timers_env <- new.env(parent = emptyenv())
  .timers_env$stack <- list()
}

log_step <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(paste0("[", timestamp, "] ", msg))
}

tic <- function(label = "timer") {
  .timers_env$stack[[length(.timers_env$stack) + 1]] <- list(label = label, start = Sys.time())
  log_step(paste0("START: ", label))
}

toc <- function() {
  if (length(.timers_env$stack) == 0) stop("toc() called without matching tic().")
  t <- .timers_env$stack[[length(.timers_env$stack)]]
  .timers_env$stack <- .timers_env$stack[-length(.timers_env$stack)]
  elapsed <- difftime(Sys.time(), t$start, units = "secs")
  log_step(paste0("END: ", t$label, " | elapsed = ", round(as.numeric(elapsed)/60, 2), " min"))
  invisible(elapsed)
}

progress_eta <- function(i, n, start_time, every = 50, prefix = "") {
  if (i == 1 || i == n || (i %% every == 0)) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    rate <- elapsed / i
    remaining <- rate * (n - i)
    log_step(paste0(prefix, "Progress ", i, "/", n,
                    " | elapsed ", round(elapsed/60, 2), " min",
                    " | ETA ", round(remaining/60, 2), " min"))
  }
}
