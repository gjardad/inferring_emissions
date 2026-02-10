###############################################################################
# ppml_woutfuelproxy_sectorFE.R
#
# PURPOSE
#   Run a Leave-One-Firm-Out cross-validation (LOFOCV) exercise for the
#   single-equation PPML benchmark model with *sector fixed effects* (no
#   partial pooling) on the LOOCV training sample.
#
#   This script estimates the most basic benchmark specification used in the
#   emissions-imputation exercise: a PPML model with log(revenue), year fixed
#   effects, and sector fixed effects, without any fuel consumption proxy
#   and without shrinkage across sectors.
#
# MODEL SPECIFICATION (THIS SCRIPT)
#   Benchmark PPML with sector fixed effects:
#
#     E[y_it | X_it] = exp(
#         β * log(revenue_it)
#       + year FE
#       + sector FE
#     )
#
#   Sector effects are included as a full set of fixed effects (factor nace2d),
#   with no partial pooling or shrinkage.
#
# CROSS-VALIDATION DESIGN
#   LOFOCV: all observations of one firm (identified by vat) are held out in each
#   fold. The model is estimated on remaining firms and used to predict emissions
#   for the held-out firm across all its observed years.
#
# CALIBRATION
#   Predictions are calibrated so that, within each sector × year cell:
#
#       sum_i yhat_{i,s,t} = E_total_{s,t},
#
#   where E_total_{s,t} is the observed aggregate emissions in the training data.
#   Calibration preserves relative firm-level weights within each cell and uses
#   full-sample sector-year totals, mimicking deployment conditions.
#
# OUTPUTS
#   - Appends metrics rows (raw + calibrated) to:
#       output/model_performance_metrics.rds
#       output/model_performance_metrics.csv
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
# Config --------
# ===============
PARTIAL_POOLING <- FALSE
PROGRESS_EVERY  <- 50
DROP_SINGLETON_CELLS_IN_METRICS <- TRUE
FALLBACK_EQUAL_SPLIT <- TRUE

TEST_MODE <- FALSE
TEST_FRAC <- 0.10
TEST_SEED <- 123

# ===============
# Packages ------
# ===============
suppressPackageStartupMessages({
  library(data.table)
})

# ===============
# Helpers -------
# ===============
source(file.path(utils, "calc_metrics.R"))
source(file.path(utils, "build_metrics_table.R"))
source(file.path(utils, "append_loocv_performance_metrics_log.R"))
source(file.path(loocv, "ppml.R"))

# ============================================================
# Load data
# ============================================================
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
  source(file.path(utils, "make_lofo_subsample.R"))
  
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
# Run PPML benchmark (no proxy) with sector FE
# ============================================================
out <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  # ---- proxy disabled ----
  proxy_df     = NULL,
  proxy_keys   = c("buyer_id","year"),  # unused when proxy_df=NULL
  proxy_var    = "fuel_proxy",
  proxy_tag    = "none",
  coalesce_proxy_to_zero = TRUE,
  # ---- sector effects: FE ----
  partial_pooling = PARTIAL_POOLING,
  # ---- calibration / evaluation ----
  fallback_equal_split = FALLBACK_EQUAL_SPLIT,
  progress_every = PROGRESS_EVERY,
  drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS
)

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

metrics_path_rds <- file.path(output, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

message("Done. Appended ", nrow(metrics_tbl), " rows to metrics log.")
