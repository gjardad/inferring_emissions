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

# 00_config/paths.R

# -----------------------
# User-specific root
# -----------------------
if (tolower(Sys.info()[["user"]]) == "jardang") {
  BASE_DIR <- "X:/Documents/JARDANG"
} else {
  stop("Define BASE_DIR for this user.")
}

# -----------------------
# Project roots
# -----------------------
PROJECT_DIR <- file.path(BASE_DIR, "carbon_policy_networks")

DATA_RAW  <- file.path(PROJECT_DIR, "data", "raw")
DATA_INT  <- file.path(PROJECT_DIR, "data", "intermediate")
DATA_PROC <- file.path(PROJECT_DIR, "data", "processed")
OUTPUT    <- file.path(PROJECT_DIR, "output")

CODE_DIR  <- file.path(PROJECT_DIR, "code")
LOOCV_ROOT <- file.path(CODE_DIR, "inferring_emissions")

# -----------------------
# Pipeline-specific paths
# -----------------------
PROXY_CACHE_DIR <- file.path(LOOCV_ROOT, "02_proxies", "cache")
LOOCV_RESULTS   <- file.path(LOOCV_ROOT, "04_loocv", "results")
FIG_DIR         <- file.path(LOOCV_ROOT, "05_analysis", "figures")

# -----------------------
# Backwards-compatible aliases (old scripts expect these)
# -----------------------
raw_data  <- DATA_RAW
int_data  <- DATA_INT
proc_data <- DATA_PROC
out_data  <- OUTPUT

CACHE_DIR <- PROXY_CACHE_DIR   # if older code uses CACHE_DIR