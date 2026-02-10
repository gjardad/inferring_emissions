###############################################################################
# ppml_woutfuelproxy_sectorRE.R
#
# PURPOSE
#   Run Leave-One-Firm-Out cross-validation (LOFOCV) for the PPML benchmark
#   model (Step 2 only) on the LOOCV training sample, WITHOUT fuel proxies,
#   and with partially pooled sector random effects:
#     s(nace2d, bs = "re")
#
#   This script:
#     1) Loads the LOOCV training sample.
#     2) Builds sector-year totals (E_total) for calibration.
#     3) Runs poissonPP_lofo() with proxy_df = NULL.
#     4) Logs raw + calibrated metrics using the standardized build_metrics_table()
#        + append_metrics_log() interface.
#
# MODEL SPECIFICATION (BENCHMARK PPML, NO PROXY)
#   E[y_it | X_it] = exp( β log(revenue_it) + year FE + sector effect )
#   with sector effect as random effect: s(sector, bs="re")
#
# OUTPUT
#   - Appends metrics rows to:
#       output/model_performance_metrics.rds
#       output/model_performance_metrics.csv
###############################################################################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "config", "paths.R"))


# ===============
# Setup paths ----
# ===============


# ===============
# Config ---------
# ===============
PARTIAL_POOLING <- TRUE
PROGRESS_EVERY  <- 50
DROP_SINGLETON_CELLS_IN_METRICS <- TRUE
FALLBACK_EQUAL_SPLIT <- TRUE

# For quick local test
TEST_MODE <- FALSE
TEST_FRAC <- 0.10
TEST_SEED <- 123

# ===============
# Packages -------
# ===============
suppressPackageStartupMessages({
  library(data.table)
})

# ===============
# Helpers --------
# ===============
source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "build_metrics_table.R"))
source(file.path(UTILS_DIR, "append_loocv_performance_metrics_log.R"))
source(file.path(LOOCV_DIR, "ppml.R"))

# ============================================================
# Load data
# ============================================================
load(file.path(PROC_DATA, "loocv_training_sample.RData"))
df_full <- loocv_training_sample

sector_year_totals_full <- as.data.table(df_full)[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

df_run  <- df_full
syt_run <- sector_year_totals_full
sample_tag <- "all"

if (isTRUE(TEST_MODE)) {
  source(file.path(UTILS_DIR, "make_lofo_subsample.R"))
  
  sub <- make_lofo_subsample(
    df   = df_full,
    frac = TEST_FRAC,
    seed = TEST_SEED
  )
  
  df_run  <- sub$df_sub
  syt_run <- sub$sector_year_totals
  sample_tag <- "subsample"
}

# ============================================================
# Run PPML benchmark (no proxy)
# ============================================================
out <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  # ---- proxy (disabled) ----
  proxy_df     = NULL,
  proxy_keys   = c("buyer_id","year"),  # unused when proxy_df=NULL (kept for signature consistency)
  proxy_var    = "fuel_proxy",
  proxy_tag    = "none",
  coalesce_proxy_to_zero = TRUE,
  # ---- sector effects ----
  partial_pooling = PARTIAL_POOLING,
  # ---- calibration / evaluation ----
  fallback_equal_split = FALLBACK_EQUAL_SPLIT,
  progress_every = PROGRESS_EVERY,
  drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS
)

# Optional inspection
print(out$metrics)
print(head(out$predictions))

# ============================================================
# Log metrics (NEW build_metrics_table interface)
# ============================================================
metrics_tbl <- build_metrics_table(
  out             = out,
  model_family    = "ppml",
  partial_pooling = if (isTRUE(PARTIAL_POOLING)) "yes" else "no",
  step_tag        = "2",
  sample_tag      = sample_tag,
  proxy_tag       = "none",
  extra_id_cols   = list(
    n_obs_est   = nrow(df_run),
    n_firms_est = data.table::uniqueN(df_run$vat)
  )
)

metrics_path_rds <- file.path(OUTPUT_DIR, "model_performance_metrics.rds")
metrics_path_csv <- file.path(OUTPUT_DIR, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

message("Done. Appended ", nrow(metrics_tbl), " rows to metrics log.")
