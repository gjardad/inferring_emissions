###############################################################################
# execute_loocv.R
#
# PURPOSE
#   One-shot driver to run and log:
#     (1) ppml_woutfuelproxy_sectorFE  (Step 2 only, no proxy, sector FE)
#     (2) ppml_woutfuelproxy_sectorRE  (Step 2 only, no proxy, sector RE)
#     (3) parallel PPML step-2-only with fuel proxy (over proxy_*.rds)
#     (4) parallel hurdle model (over proxy pairs ext x int)
#
# THIS SCRIPT ASSUMES YOUR FOLDER STRUCTURE (per screenshots)
#   code/inferring_emissions/
#     loocv/
#       ppml/ppml.R
#       hurdle/hurdle.R
#       run_one_proxy.R
#       run_one_proxy_pair.R
#       run_parallel_over_proxies.R         (not sourced; we implement here)
#       run_parallel_over_proxy_pairs.R     (not sourced; we implement here)
#       ppml_woutfuelproxy_sectorRE.R       (not sourced; we call functions)
#       ppml_woutfuelproxy_sectorFE.R       (not sourced; we call functions)
#     utils/
#       calc_metrics.R
#       build_metrics_table.R
#       append_loocv_performance_metrics_log.R
#       make_lofo_subsample.R
#     proxies/cache/proxy_*.rds
#
# TIMING
#   Prints wall-clock timestamps + elapsed minutes at each stage.
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



# -----------------------
# 0) Small logger
# -----------------------
t_start <- Sys.time()

ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
elapsed_min <- function() round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 2)

log_step <- function(msg) {
  message(sprintf("[%s | +%s min] %s", ts(), elapsed_min(), msg))
}

# -----------------------
# 1) Paths
# -----------------------
if (tolower(Sys.info()[["user"]]) == "jardang") {
  folder <- "X:/Documents/JARDANG"
} else {
  stop("Define 'folder' for this user.")
}

PROJECT_DIR <- file.path(folder, "carbon_policy_networks")
proc_data   <- file.path(PROJECT_DIR, "data", "processed")
output_dir  <- file.path(PROJECT_DIR, "output")

code_root <- file.path(PROJECT_DIR, "code", "inferring_emissions")
UTILS_DIR <- file.path(code_root, "utils")
LOOCV_DIR <- file.path(code_root, "loocv")

# Proxies live here per your tree
proxy_cache_dir <- file.path(code_root, "proxies", "cache")

# Metrics log
metrics_path_rds <- file.path(output_dir, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output_dir, "model_performance_metrics.csv")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# log file path
LOG_FILE_HURDLE <- file.path(output_dir, "hurdle_progress.log")
LOG_FILE_PPML   <- file.path(output_dir, "ppml_progress.log")

# start fresh each run (optional)
if (file.exists(LOG_FILE_HURDLE)) file.remove(LOG_FILE_HURDLE)
if (file.exists(LOG_FILE_PPML))   file.remove(LOG_FILE_PPML)

# -----------------------
# 2) Run config
# -----------------------
TEST_MODE <- TRUE
TEST_FRAC <- 0.20
TEST_SEED <- 123

# Common options
FP_THRESHOLD <- 0
DROP_SINGLETON_CELLS_IN_METRICS <- TRUE
FALLBACK_EQUAL_SPLIT <- TRUE
PROGRESS_EVERY <- 50

# Parallel
N_WORKERS <- max(1L, parallel::detectCores() - 1L)
FUTURE_PLAN <- "multisession"  # Windows-safe

# Optional slicing
PROXY_START <- 1
PROXY_END   <- 50

# For hurdle pair runs (cartesian product)
PAIR_START <- 1
PAIR_END   <- 50

# -----------------------
# 3) Libraries
# -----------------------
suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(future.apply)
})

# -----------------------
# 4) Robust sourcer (handles hidden .R extensions)
# -----------------------
source_try <- function(dir, fname_no_ext) {
  p1 <- file.path(dir, fname_no_ext)
  p2 <- paste0(p1, ".R")
  if (file.exists(p1)) return(source(p1, local = FALSE))
  if (file.exists(p2)) return(source(p2, local = FALSE))
  stop("Could not find file to source: ", p1, " or ", p2)
}

# -----------------------
# 5) Source utils (single source of truth)
# -----------------------
log_step("Sourcing utils...")
source_try(UTILS_DIR, "calc_metrics")
source_try(UTILS_DIR, "build_metrics_table")
source_try(UTILS_DIR, "append_loocv_performance_metrics_log")
if (isTRUE(TEST_MODE)) source_try(UTILS_DIR, "make_lofo_subsample")

# -----------------------
# 6) Source model functions
# -----------------------
log_step("Sourcing model functions...")
source_try(LOOCV_DIR, "ppml")
source_try(LOOCV_DIR, "hurdle")

# Driver helpers (thin wrappers)
source_try(LOOCV_DIR, "run_one_proxy")
source_try(LOOCV_DIR, "run_one_proxy_pair")

# -----------------------
# 7) Load data once + build sector-year totals once
# -----------------------
log_step("Loading LOOCV training sample...")
load(file.path(proc_data, "loocv_training_sample.RData"))
df_full <- as.data.table(loocv_training_sample)

sector_year_totals_full <- df_full[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

df_run  <- df_full
syt_run <- sector_year_totals_full
sample_tag <- "all"

if (isTRUE(TEST_MODE)) {
  log_step(sprintf("TEST_MODE=TRUE: subsampling frac=%.2f seed=%d", TEST_FRAC, TEST_SEED))
  sub <- make_lofo_subsample(df = df_full, frac = TEST_FRAC, seed = TEST_SEED)
  df_run  <- as.data.table(sub$df_sub)
  syt_run <- as.data.table(sub$sector_year_totals)
  sample_tag <- "subsample"
}

log_step(sprintf("Data ready: sample_tag=%s | Nobs=%d | Nfirms=%d",
                 sample_tag, nrow(df_run), uniqueN(df_run$vat)))

# -----------------------
# 8) Append helper
# -----------------------
append_and_message <- function(metrics_tbl) {
  metrics_all <- append_metrics_log(
    as.data.table(metrics_tbl),
    rds_path = metrics_path_rds,
    csv_path = metrics_path_csv,
    dedup = TRUE
  )
  log_step(sprintf("Appended %d rows. Log now has %d rows.", nrow(metrics_tbl), nrow(metrics_all)))
  invisible(metrics_all)
}

# --------------------
# Log helper
# --------------------

ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

log_line <- function(path, ...) {
  msg <- paste0("[", ts(), "] ", paste0(..., collapse = ""))
  # Use file lock so multiple workers don't interleave lines
  if (!requireNamespace("filelock", quietly = TRUE)) {
    # fallback: append without locking (usually ok, but can interleave)
    cat(msg, "\n", file = path, append = TRUE)
    return(invisible(NULL))
  }
  lock_path <- paste0(path, ".lock")
  lck <- filelock::lock(lock_path, timeout = 60000)  # wait up to 60s
  on.exit(filelock::unlock(lck), add = TRUE)
  cat(msg, "\n", file = path, append = TRUE)
  invisible(NULL)
}

# ============================================================================
# (1) PPML benchmark | sector FE | no proxy
# ============================================================================
log_step("[1/4] Running PPML benchmark | sector FE | no proxy")

out_ppml_fe <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  proxy_df = NULL,
  proxy_keys = c("vat","year"),
  proxy_var = "fuel_proxy",
  proxy_tag = "none",
  coalesce_proxy_to_zero = TRUE,
  partial_pooling = FALSE,
  fallback_equal_split = FALLBACK_EQUAL_SPLIT,
  drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
  fp_threshold = FP_THRESHOLD,
  progress_every = PROGRESS_EVERY
)

metrics_ppml_fe <- build_metrics_table(
  out             = out_ppml_fe,
  model_family    = "ppml",
  partial_pooling = "no",
  step_tag        = "2",
  sample_tag      = sample_tag,
  proxy_tag       = "none",
  extra_id_cols   = list(
    model_name   = "ppml_woutfuelproxy_sectorFE",
    n_obs_est    = nrow(df_run),
    n_firms_est  = uniqueN(df_run$vat)
  )
)

append_and_message(metrics_ppml_fe)

# ============================================================================
# (2) PPML benchmark | sector RE | no proxy
# ============================================================================
log_step("[2/4] Running PPML benchmark | sector RE | no proxy")

out_ppml_re <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  proxy_df = NULL,
  proxy_keys = c("vat","year"),
  proxy_var = "fuel_proxy",
  proxy_tag = "none",
  coalesce_proxy_to_zero = TRUE,
  partial_pooling = TRUE,
  fallback_equal_split = FALLBACK_EQUAL_SPLIT,
  drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
  fp_threshold = FP_THRESHOLD,
  progress_every = PROGRESS_EVERY
)

metrics_ppml_re <- build_metrics_table(
  out             = out_ppml_re,
  model_family    = "ppml",
  partial_pooling = "yes",
  step_tag        = "2",
  sample_tag      = sample_tag,
  proxy_tag       = "none",
  extra_id_cols   = list(
    model_name   = "ppml_woutfuelproxy_sectorRE",
    n_obs_est    = nrow(df_run),
    n_firms_est  = uniqueN(df_run$vat)
  )
)

append_and_message(metrics_ppml_re)

# ============================================================================
# (3) Parallel PPML step-2 with proxy over proxy_*.rds
# ============================================================================
log_step("[3/4] Parallel PPML step-2 w/ proxy: listing proxy files...")

if (!dir.exists(proxy_cache_dir)) stop("proxy_cache_dir does not exist: ", proxy_cache_dir)
proxy_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(proxy_files) == 0) stop("No proxy_*.rds files found in: ", proxy_cache_dir)

proxy_files <- proxy_files[seq.int(PROXY_START, min(length(proxy_files), PROXY_END))]
log_step(sprintf("PPML proxy runs: %d proxy files", length(proxy_files)))

# Define model_runner that returns standardized metrics
ppml_proxy_runner <- function(df_run, syt_run, proxy_tbl, proxy_name,
                              id_var, year_var, sector_var, y_var, revenue_var,
                              proxy_keys, proxy_var, coalesce_proxy_to_zero,
                              partial_pooling, step_tag, sample_tag, model_name, ...) {
  
  out <- poissonPP_lofo(
    df = df_run,
    sector_year_totals = syt_run,
    id_var = id_var,
    year_var = year_var,
    sector_var = sector_var,
    y_var = y_var,
    revenue_var = revenue_var,
    proxy_df   = proxy_tbl,
    proxy_keys = proxy_keys,
    proxy_var  = proxy_var,
    proxy_tag  = proxy_name,
    coalesce_proxy_to_zero = coalesce_proxy_to_zero,
    partial_pooling = partial_pooling,
    fallback_equal_split = FALLBACK_EQUAL_SPLIT,
    drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
    fp_threshold = FP_THRESHOLD,
    progress_every = PROGRESS_EVERY
  )
  
  metrics <- build_metrics_table(
    out             = out,
    model_family    = "ppml",
    partial_pooling = if (isTRUE(partial_pooling)) "yes" else "no",
    step_tag        = step_tag,
    sample_tag      = sample_tag,
    proxy_tag       = proxy_name,
    extra_id_cols   = list(
      model_name   = model_name,
      n_obs_est    = nrow(df_run),
      n_firms_est  = uniqueN(df_run[[id_var]])
    )
  )
  
  list(metrics = metrics, out = out)
}

future::plan(strategy = FUTURE_PLAN, workers = N_WORKERS)

log_step(sprintf("Starting parallel PPML proxy runs (workers=%d, plan=%s)", N_WORKERS, FUTURE_PLAN))

metrics_list_ppml_proxy <- future_lapply(
  seq_along(proxy_files),
  function(j) {
    pf <- proxy_files[[j]]
    
    log_line(
      LOG_FILE_PPML,
      sprintf("[PPML proxy %d/%d] %s", j, length(proxy_files), basename(pf))
    )
    
    run_one_proxy(
      proxy_file = pf,
      df_run = df_run,
      syt_run = syt_run,
      model_runner = ppml_proxy_runner,
      # tags forwarded to model_runner
      model_name = "ppml_step2_proxy",
      step_tag = "2",
      sample_tag = sample_tag,
      partial_pooling = TRUE,  # set FALSE if you want FE in this batch
      id_var = "vat",
      year_var = "year",
      sector_var = "nace2d",
      y_var = "emissions",
      revenue_var = "revenue",
      proxy_keys = c("vat","year"),
      proxy_var = "fuel_proxy",
      coalesce_proxy_to_zero = TRUE
    )
  },
  future.seed = TRUE
)

metrics_ppml_proxy <- rbindlist(metrics_list_ppml_proxy, fill = TRUE)
append_and_message(metrics_ppml_proxy)

# ============================================================================
# (4) Parallel hurdle model over proxy pairs
# ============================================================================
log_step("[4/4] Parallel hurdle runs: building proxy pair grid...")

# Use the SAME cache dir for ext/int unless you later split them
ext_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
int_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)

if (length(ext_files) == 0 || length(int_files) == 0) stop("No proxies found for hurdle in: ", proxy_cache_dir)

pairs <- CJ(ext_idx = seq_along(ext_files), int_idx = seq_along(int_files))
pairs[, ext_path := ext_files[ext_idx]]
pairs[, int_path := int_files[int_idx]]

pairs <- pairs[seq.int(PAIR_START, min(nrow(pairs), PAIR_END))]
log_step(sprintf("Hurdle runs: %d pairs (ext=%d x int=%d)", nrow(pairs), length(ext_files), length(int_files)))

# helper to read proxy RDS in same way everywhere
read_proxy_rds_simple <- function(path) {
  obj <- readRDS(path)
  tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  name <- if (is.list(obj) && !is.null(obj$name))  obj$name  else tools::file_path_sans_ext(basename(path))
  tbl <- as.data.table(tbl)
  if ("buyer_id" %in% names(tbl) && !"vat" %in% names(tbl)) setnames(tbl, "buyer_id", "vat")
  if (!all(c("vat","year","fuel_proxy") %in% names(tbl))) stop("Proxy missing vat/year/fuel_proxy: ", path)
  list(tbl = tbl, name = name)
}

log_step(sprintf("Starting parallel hurdle proxy-pair runs (workers=%d, plan=%s)", N_WORKERS, FUTURE_PLAN))

metrics_list_hurdle <- future_lapply(
  seq_len(nrow(pairs)),
  function(k) {
    ext <- read_proxy_rds_simple(pairs$ext_path[k])
    int <- read_proxy_rds_simple(pairs$int_path[k])
    
    log_line(
      LOG_FILE_HURDLE,
      sprintf("[Hurdle %d/%d] ext=%s | int=%s",
              k, nrow(pairs), ext$name, int$name)
    )
    
    res <- run_one_proxy_pair(
      df_run = df_run,
      syt_run = syt_run,
      proxy_ext_tbl = ext$tbl,
      proxy_int_tbl = int$tbl,
      proxy_name_ext = ext$name,
      proxy_name_int = int$name,
      partial_pooling = TRUE,
      fallback_equal_split = FALLBACK_EQUAL_SPLIT,
      drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
      fp_threshold = FP_THRESHOLD,
      progress_every = PROGRESS_EVERY,
      sample_tag = sample_tag,
      step_tag = "12"  # "step 1+2"
    )
    
    res$metrics_tbl
  },
  future.seed = TRUE
)

metrics_hurdle <- rbindlist(metrics_list_hurdle, fill = TRUE)
append_and_message(metrics_hurdle)

plan(sequential)
log_step("ALL DONE.")
