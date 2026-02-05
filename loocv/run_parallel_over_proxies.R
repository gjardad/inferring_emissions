# ============================================================================ #
# run_parallel_over_proxies.R
# ============================================================================ #
# PURPOSE
#   Parallel driver to evaluate many proxy specifications (each stored as an
#   .rds file) using a chosen model runner (PPML today, hurdle tomorrow).
#
# LOGIC (high-level)
#   1) Configure project paths, model choice, and parallel settings.
#   2) Load the training sample once in the main session.
#   3) Build auxiliary objects once (e.g. sector-year totals).
#   4) Build a list of proxy files to run (optionally slice start/end).
#   5) Choose a model runner from `model_registry` via MODEL_CHOICE.
#   6) Use future.apply::future_lapply() to run `run_one_proxy()` over proxies.
#   7) Row-bind results into a single metrics table.
#   8) Append results to your metrics log (RDS + CSV) with de-duplication.
#
# INPUTS (configured in this script)
#   Paths:
#     - processed/loocv_training_sample.RData  (must contain loocv_training_sample)
#     - cache_dir containing proxy_*.rds files
#
#   Key toggles:
#     - MODEL_CHOICE: selects which entry in model_registry to run
#     - PARTIAL_POOLING: controls sector FE vs RE in the PPML example
#     - TEST_MODE: run a small subsample (optional)
#     - N_WORKERS / FUTURE_PLAN: parallel settings (Windows-safe default)
#
# OUTPUTS
#   - A metrics table appended to:
#       output/model_performance_metrics.rds
#       output/model_performance_metrics.csv
#   - Prints progress to console.
#
# EXTENDING (future hurdle model)
#   Add your hurdle source() file and create another `model_registry` entry.
#   No changes required to the parallelization infrastructure.
#
# NOTES / GOTCHAS
#   - Each worker is a separate R session under multisession.
#     Make sure ALL needed functions are available via source() before launching.
#   - If you use random numbers in your model, keep future.seed=TRUE.
# ============================================================================ #

rm(list = ls())

# =========================
# User paths (adapt as needed)
# =========================
if (tolower(Sys.info()[["user"]]) == "jardang") {
  folder <- "X:/Documents/JARDANG"
} else {
  stop("Define 'folder' for this user.")
}

PROJECT_DIR <- file.path(folder, "carbon_policy_networks")
proc_data   <- file.path(PROJECT_DIR, "data", "processed")
output      <- file.path(PROJECT_DIR, "output")

code_dir <- file.path(PROJECT_DIR, "code", "inferring_emissions")
utils    <- file.path(code_dir, "utils")
loocv    <- file.path(code_dir, "loocv")  # adjust if your models live elsewhere

# Proxies cache dir (adjust to your actual structure)
cache_dir <- file.path(code_dir, "proxies", "cache")

# =========================
# Config
# =========================
MODEL_CHOICE <- "ppml_step2"   # later: "hurdle_step1_step2"

TEST_MODE <- FALSE
TEST_FRAC <- 0.20
TEST_SEED <- 123

PARTIAL_POOLING <- TRUE

PROXY_START <- 1
PROXY_END   <- Inf

# Parallel settings
N_WORKERS <- max(1, parallel::detectCores() - 1)
FUTURE_PLAN <- "multisession" # Windows-safe. Use "multicore" on Linux/Mac.

# Metrics log output
metrics_path_rds <- file.path(output, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output, "model_performance_metrics.csv")

# =========================
# Packages
# =========================
suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(future.apply)
})

# =========================
# Sources (must be available to workers)
# =========================
source(file.path(utils, "calc_metrics.R"))
source(file.path(utils, "build_metrics_table.R"))
source(file.path(utils, "append_loocv_performance_metrics_log.R"))
source(file.path(loocv, "run_one_proxy.R"))

# Model implementations (example: your existing PPML code)
# Adjust the path/filename to match your repo.
source(file.path(loocv, "ppml.R"))

# Later you’ll add:
# source(file.path(loocv, "hurdle.R"))

# =========================
# Load data
# =========================
load(file.path(proc_data, "loocv_training_sample.RData"))
df_full <- as.data.table(loocv_training_sample)

# Build auxiliary sector-year totals once
sector_year_totals_full <- df_full[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

df_run  <- df_full
syt_run <- sector_year_totals_full
sample_tag <- "all"

# Optional test subsample
if (isTRUE(TEST_MODE)) {
  source(file.path(utils, "make_lofo_subsample.R"))
  sub <- make_lofo_subsample(df = df_full, frac = TEST_FRAC, seed = TEST_SEED)
  df_run  <- as.data.table(sub$df_sub)
  syt_run <- as.data.table(sub$sector_year_totals)
  sample_tag <- "subsample"
}

# =========================
# Proxy files
# =========================
if (!dir.exists(cache_dir)) stop("cache_dir does not exist: ", cache_dir)

proxy_files <- list.files(cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(proxy_files) == 0) stop("No proxy .rds files found in: ", cache_dir)

proxy_files <- proxy_files[seq.int(PROXY_START, min(length(proxy_files), PROXY_END))]

message("Found ", length(proxy_files), " proxy files")
message("Sample_tag=", sample_tag, " | Nobs=", nrow(df_run), " | Nfirms=", uniqueN(df_run$vat))
message("partial_pooling=", PARTIAL_POOLING)
message("MODEL_CHOICE=", MODEL_CHOICE)
message("workers=", N_WORKERS, " | plan=", FUTURE_PLAN)

# =========================
# Model registry (add new models here)
# =========================
model_registry <- list(

  # --- PPML Step 2 only (example) ---
  ppml_step2 = function(df_run, syt_run, proxy_tbl, proxy_name,
                       id_var, year_var, sector_var, y_var, revenue_var,
                       proxy_keys, proxy_var, coalesce_proxy_to_zero,
                       partial_pooling,
                       step_tag, sample_tag, model_name, ...) {

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
      fallback_equal_split = TRUE,
      progress_every = 50,
      drop_singleton_cells_in_metrics = TRUE
    )

    metrics <- build_metrics_table(
      out             = out,
      model_family    = "ppml",
      partial_pooling = if (isTRUE(partial_pooling)) "yes" else "no",
      step_tag        = step_tag,
      sample_tag      = sample_tag,
      proxy_tag       = proxy_name,
      extra_id_cols   = list(
        model_name  = model_name,
        n_obs_est   = nrow(df_run),
        n_firms_est = data.table::uniqueN(df_run[[id_var]])
      )
    )

    list(metrics = metrics, out = out)
  }

  # --- placeholder for your future hurdle model ---
  # hurdle_step1_step2 = function(...) {
  #   out <- hurdle_lofo(...)
  #   metrics <- build_metrics_table(out = out, model_family = "hurdle", ...)
  #   list(metrics = metrics, out = out)
  # }
)

if (!MODEL_CHOICE %in% names(model_registry)) {
  stop("Unknown MODEL_CHOICE: ", MODEL_CHOICE, "\nAvailable: ", paste(names(model_registry), collapse = ", "))
}
model_runner <- model_registry[[MODEL_CHOICE]]

# =========================
# Parallel execution
# =========================
plan(strategy = FUTURE_PLAN, workers = N_WORKERS)

metrics_list <- future_lapply(
  seq_along(proxy_files),
  function(j) {
    pf <- proxy_files[[j]]
    message(sprintf("[%d/%d] %s", j, length(proxy_files), basename(pf)))

    run_one_proxy(
      proxy_file = pf,
      df_run = df_run,
      syt_run = syt_run,
      model_runner = model_runner,
      model_name = MODEL_CHOICE,
      step_tag = if (MODEL_CHOICE == "ppml_step2") "2" else "custom",
      sample_tag = sample_tag,
      partial_pooling = PARTIAL_POOLING
    )
  },
  future.seed = TRUE
)

metrics_tbl_proxy <- rbindlist(metrics_list, fill = TRUE)

# =========================
# Append log
# =========================
metrics_all <- append_metrics_log(
  metrics_tbl_proxy,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

message("Done. Appended ", nrow(metrics_tbl_proxy), " rows.")
