###############################################################################
# ppml_loop_over_proxies_sectorRE.R
#
# PURPOSE
#   Loop over cached fuel consumption proxies and run LOFOCV PPML (Step 2 only)
#   with sector effects specified as partially pooled random effects:
#       s(nace2d, bs = "re")

#   For each proxy definition (cached as an .rds), fit a PPML model:
#       E[y_it|X] = exp(log(revenue) + I(proxy>0) + asinh(proxy)
#                       + year FE + sector effect )
#       where the sector effect is partially pooled sector
#       random effects via s(sector, bs="re")
#
#   The LOFOCV design holds out one firm (all years) per fold. Predictions for
#   the held-out firm are optionally calibrated to match known sector-year
#   totals (nace2d × year)
#
# INPUTS
#   - data/processed/loocv_training_sample.RData  (object: loocv_training_sample)
#   - Proxy cache directory containing proxy_*.rds files.
#
# OUTPUTS
#   - Appends one metrics row per proxy to:
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
OUTPUT_DIR <- file.path(DATA_DIR, "output")

UTILS_DIR <- file.path(REPO_DIR, "utils")
LOOCV_DIR <- file.path(REPO_DIR, "loocv")
CACHE_DIR <- file.path(REPO_DIR, "proxies", "cache")

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}

# ===============
# Config --------
# ===============
# Run on full sample or on a subsample for quick testing
TEST_MODE <- TRUE
TEST_FRAC <- 0.20   # 0.20 used because very small subsamples can cause RE/REML instability in some folds
TEST_SEED <- 123

# Sector effect specification (this script is meant for RE runs)
PARTIAL_POOLING <- TRUE  # TRUE => s(sector, bs="re"); FALSE => sector FE

# PPML/LOFOCV options
PROGRESS_EVERY <- 50
DROP_SINGLETON_CELLS_IN_METRICS <- TRUE
FALLBACK_EQUAL_SPLIT <- TRUE

# Optional: limit proxies (useful for batching without parallelization)
PROXY_START <- 1
PROXY_END   <- Inf

# ===============
# Packages ------
# ===============
suppressPackageStartupMessages({
  library(data.table)
  library(mgcv)
})

# ===============
# Helpers -------
# ===============
source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "build_metrics_table.R"))
source(file.path(UTILS_DIR, "append_loocv_performance_metrics_log.R"))
source(file.path(LOOCV_DIR, "ppml.R"))

# ===============
# Load data -----
# ===============
load(file.path(PROC_DATA, "loocv_training_sample.RData"))
df_full <- loocv_training_sample

sector_year_totals_full <- as.data.table(df_full)[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

# Optionally subsample for testing (single code path downstream)
df_run  <- df_full
syt_run <- sector_year_totals_full
sample_tag <- "all"

if (isTRUE(TEST_MODE)) {
  source(file.path(UTILS_DIR, "make_lofo_subsample.R"))

  # NOTE ON NUMERICAL STABILITY (LOFOCV + PARTIAL POOLING):
  # With very aggressive downsampling (≈10%), some LOFO folds leave too little
  # information to estimate the sector random-effect variance via REML. In those
  # cases the RE smoothing parameter can become numerically ill-conditioned and
  # mgcv may fail (gam.fit3 arg 3 error). This is a fold-specific small-sample
  # artifact that typically disappears as the sample size increases (e.g. at 20%+).
  sub <- make_lofo_subsample(
    df   = df_full,
    frac = TEST_FRAC,
    seed = TEST_SEED
  )
  df_run  <- sub$df_sub
  syt_run <- sub$sector_year_totals
  sample_tag <- "subsample"
}

# ===============
# Proxy files ---
# ===============
if (!dir.exists(cache_dir)) stop("cache_dir does not exist: ", cache_dir)

proxy_files <- list.files(cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(proxy_files) == 0) stop("No proxy .rds files found in cache_dir: ", cache_dir)

# Apply optional slicing for batching
proxy_files <- proxy_files[seq.int(PROXY_START, min(length(proxy_files), PROXY_END))]

message("Found ", length(proxy_files), " proxy files in: ", cache_dir)
message("Running sample_tag = ", sample_tag, " | Nobs = ", nrow(df_run),
        " | Nfirms = ", uniqueN(df_run$vat))
message("partial_pooling = ", PARTIAL_POOLING)

# ===============
# Run loop ------
# ===============
metrics_list <- vector("list", length(proxy_files))

for (j in seq_along(proxy_files)) {

  pf <- proxy_files[[j]]
  obj <- readRDS(pf)

  proxy_tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(pf))

  proxy_tbl <- as.data.table(proxy_tbl)

  # cached proxies use buyer_id; model df uses vat
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }

  # checks
  if (!all(c("vat", "year", "fuel_proxy") %in% names(proxy_tbl))) {
    stop("Proxy file does not contain vat/year/fuel_proxy: ", pf)
  }

  message(sprintf("[%d/%d] Running proxy = %s", j, length(proxy_files), proxy_name))

  out <- poissonPP_lofo(
    df = df_run,
    sector_year_totals = syt_run,
    id_var = "vat",
    year_var = "year",
    sector_var = "nace2d",
    y_var = "emissions",
    revenue_var = "revenue",
    # ---- proxy ----
    proxy_df   = proxy_tbl,
    proxy_keys = c("vat", "year"),
    proxy_var  = "fuel_proxy",
    proxy_tag  = proxy_name,
    coalesce_proxy_to_zero = TRUE,
    # ---- sector effects ----
    partial_pooling = PARTIAL_POOLING,
    # ---- calibration / evaluation ----
    fallback_equal_split = FALLBACK_EQUAL_SPLIT,
    progress_every = PROGRESS_EVERY,
    drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS
  )

  sectorfe_tag <- if (isTRUE(PARTIAL_POOLING)) "re" else "fe"

  metrics_list[[j]] <- build_metrics_table(
    out         = out,
    model       = "ppml",
    step        = "2",
    sample      = sample_tag,
    sectorfe    = sectorfe_tag,
    fuel_proxy  = proxy_name,
    n_obs_est   = nrow(df_run),
    n_firms_est = uniqueN(df_run$vat)
  )
}

metrics_tbl_proxy <- rbindlist(metrics_list, fill = TRUE)

# ===============
# Append log ----
# ===============
metrics_path_rds <- file.path(OUTPUT_DIR, "model_performance_metrics.rds")
metrics_path_csv <- file.path(OUTPUT_DIR, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl_proxy,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

message("Done. Appended ", nrow(metrics_tbl_proxy), " rows to metrics log.")
