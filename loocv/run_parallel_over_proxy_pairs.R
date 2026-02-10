###############################################################################
# run_parallel_over_proxy_pairs.R
#
# PURPOSE
#   Run hurdle LOFOCV models over MANY (proxy_ext, proxy_int) pairs, typically
#   the full Cartesian product of cached proxy definitions, and append results
#   to the central performance metrics log.
#
# DESIGN
#   - Loads LOOCV training sample and builds sector-year totals
#   - Optional TEST_MODE: take LOFO-consistent subsample for fast iteration
#   - Reads proxy_*.rds files from cache directories (ext + int)
#   - Constructs Cartesian product of proxy files:
#        (ext file 1..N_ext) x (int file 1..N_int)
#   - Optional slicing via PAIR_START/PAIR_END (for batching)
#   - Parallel execution via future.apply::future_lapply
#   - Each task:
#        * reads ext proxy file and int proxy file
#        * runs hurdle via run_one_proxy_pair()
#        * returns standardized metrics_tbl (2 rows: raw + calibrated)
#   - Appends all rows to output/model_performance_metrics.(rds/csv)
#
# INPUTS (expected in repo structure)
#   - data/processed/loocv_training_sample.RData (object: loocv_training_sample)
#   - root/02_proxies/cache (or other) containing proxy_*.rds files
#   - code/utils/{calc_metrics.R, build_metrics_table.R, append_loocv_performance_metrics_log.R,
#                make_lofo_subsample.R (optional for TEST_MODE)}
#   - code/loocv/{hurdle.R} and this script + run_one_proxy_pair.R
#
# OUTPUTS
#   - Appends hurdle results to:
#       output/model_performance_metrics.rds
#       output/model_performance_metrics.csv
#
###############################################################################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  DATA_DIR <- "C:/Users/jota_/Documents/NBB_projects/data"
  REPO_DIR <- "C:/Users/jota_/Documents/NBB_projects/inferring_emissions"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

UTILS_DIR <- file.path(REPO_DIR, "utils")
LOOCV_DIR <- file.path(REPO_DIR, "loocv")

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}



#rm(list = ls())

# ===============
# User paths ----
# ===============
if (tolower(Sys.info()[["user"]]) == "jardang") {
  folder <- "X:/Documents/JARDANG"
} else {
  stop("Define 'folder' for this user.")
}

proc_data <- file.path(folder, "carbon_policy_networks", "data", "processed")
output    <- file.path(folder, "carbon_policy_networks", "output")

code  <- file.path(folder, "carbon_policy_networks", "code")
root  <- file.path(code, "inferring_emissions")
utils <- file.path(root, "utils")
loocv <- file.path(root, "loocv")

# ===============
# Proxy cache dirs
# ===============
# You can use the same dir for both (common case), or separate dirs.
cache_dir_ext <- file.path(root, "02_proxies", "cache")
cache_dir_int <- file.path(root, "02_proxies", "cache")

# ===============
# Config --------
# ===============
PARTIAL_POOLING <- TRUE
FALLBACK_EQUAL_SPLIT <- TRUE
DROP_SINGLETON_CELLS_IN_METRICS <- TRUE
FP_THRESHOLD <- 0
PROGRESS_EVERY <- 50

# Pair slicing for batching
PAIR_START <- 1
PAIR_END   <- Inf

# Parallelism
N_WORKERS <- max(1L, parallel::detectCores() - 1L)
FUTURE_PLAN <- "multisession"  # "multicore" may be faster on Linux, but Windows => multisession

# TEST_MODE (LOFO-consistent subsample)
TEST_MODE <- FALSE
TEST_FRAC <- 0.20
TEST_SEED <- 123

# ===============
# Packages ------
# ===============
suppressPackageStartupMessages({
  library(data.table)
  library(future.apply)
})

# ===============
# Helpers -------
# ===============
source(file.path(utils, "calc_metrics.R"))
source(file.path(utils, "build_metrics_table.R"))
source(file.path(utils, "append_loocv_performance_metrics_log.R"))

source(file.path(loocv, "hurdle.R"))
source(file.path(loocv, "run_one_proxy_pair.R"))

# Optional (only needed if TEST_MODE=TRUE)
if (isTRUE(TEST_MODE)) {
  source(file.path(utils, "make_lofo_subsample.R"))
}

# ===============
# Utility: read proxy file robustly
# ===============
read_proxy_rds <- function(path) {
  obj <- readRDS(path)
  proxy_tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name))  obj$name  else tools::file_path_sans_ext(basename(path))
  proxy_tbl <- as.data.table(proxy_tbl)
  
  # cached proxies use buyer_id; df uses vat
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }
  
  # Ensure required columns exist
  if (!all(c("vat", "year") %in% names(proxy_tbl))) {
    stop("Proxy table missing vat/year in: ", path)
  }
  
  # Ensure proxy value column exists; most caches use fuel_proxy
  if (!("fuel_proxy" %in% names(proxy_tbl))) {
    stop("Proxy table missing fuel_proxy in: ", path)
  }
  
  list(tbl = proxy_tbl, name = proxy_name)
}

# ===============
# Load data -----
# ===============
load(file.path(proc_data, "loocv_training_sample.RData"))
df_full <- loocv_training_sample

sector_year_totals_full <- as.data.table(df_full)[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

df_run  <- df_full
syt_run <- sector_year_totals_full
sample_tag <- "all"

if (isTRUE(TEST_MODE)) {
  sub <- make_lofo_subsample(df = df_full, frac = TEST_FRAC, seed = TEST_SEED)
  df_run  <- sub$df_sub
  syt_run <- sub$sector_year_totals
  sample_tag <- "subsample"
}

message("Sample: ", sample_tag,
        " | Nobs=", nrow(df_run),
        " | Nfirms=", uniqueN(df_run$vat),
        " | partial_pooling=", if (PARTIAL_POOLING) "re" else "fe")

# ===============
# Proxy files ---
# ===============
if (!dir.exists(cache_dir_ext)) stop("cache_dir_ext does not exist: ", cache_dir_ext)
if (!dir.exists(cache_dir_int)) stop("cache_dir_int does not exist: ", cache_dir_int)

ext_files <- list.files(cache_dir_ext, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
int_files <- list.files(cache_dir_int, pattern = "^proxy_.*\\.rds$", full.names = TRUE)

if (length(ext_files) == 0) stop("No ext proxy .rds files found in: ", cache_dir_ext)
if (length(int_files) == 0) stop("No int proxy .rds files found in: ", cache_dir_int)

message("Found ", length(ext_files), " ext proxies and ", length(int_files), " int proxies.")

# ===============
# Build Cartesian product of pairs
# ===============
pairs <- CJ(ext_idx = seq_along(ext_files), int_idx = seq_along(int_files))
pairs[, ext_path := ext_files[ext_idx]]
pairs[, int_path := int_files[int_idx]]

# Apply slicing for batching
pairs <- pairs[seq.int(PAIR_START, min(nrow(pairs), PAIR_END))]

message("Running ", nrow(pairs), " proxy pairs (after slicing).")
if (nrow(pairs) == 0) stop("No pairs to run after slicing. Check PAIR_START/PAIR_END.")

# ===============
# Parallel run
# ===============
future::plan(strategy = FUTURE_PLAN, workers = N_WORKERS)

# One task per pair
task_fun <- function(k) {
  ext_path <- pairs$ext_path[k]
  int_path <- pairs$int_path[k]
  
  ext <- read_proxy_rds(ext_path)
  int <- read_proxy_rds(int_path)
  
  # Run one pair
  res <- run_one_proxy_pair(
    df_run = df_run,
    syt_run = syt_run,
    proxy_ext_tbl = ext$tbl,
    proxy_int_tbl = int$tbl,
    proxy_name_ext = ext$name,
    proxy_name_int = int$name,
    
    partial_pooling = PARTIAL_POOLING,
    fallback_equal_split = FALLBACK_EQUAL_SPLIT,
    drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
    fp_threshold = FP_THRESHOLD,
    progress_every = PROGRESS_EVERY,
    
    sample_tag = sample_tag,
    step_tag = "12"
  )
  
  # return only metrics (keeps parallel objects smaller)
  res$metrics_tbl
}

metrics_list <- future.apply::future_lapply(
  X = seq_len(nrow(pairs)),
  FUN = task_fun,
  future.seed = TRUE
)

metrics_tbl_all <- data.table::rbindlist(metrics_list, fill = TRUE)

# ===============
# Append log ----
# ===============
metrics_path_rds <- file.path(output, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl_all,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

message("Done. Appended ", nrow(metrics_tbl_all), " rows to metrics log.")
