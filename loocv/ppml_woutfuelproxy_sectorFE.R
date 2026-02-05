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
<<<<<<< HEAD
#   effects, and sector fixed effects, without any fuel consumption proxy
=======
#   effects, and sector fixed effects, but without any fuel consumption proxy
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
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
<<<<<<< HEAD
# OUTPUTS
#   - Appends metrics rows (raw + calibrated) to:
#       output/model_performance_metrics.rds
#       output/model_performance_metrics.csv
###############################################################################

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
=======
# EVALUATION
#   Out-of-sample performance is evaluated using metrics computed by
#   calc_metrics(), including:
#     - nRMSE = RMSE / sd(y)
#     - MAPD among emitters
#     - False positive rate among non-emitters
#     - R²_LOO
#     - Spearman rank correlation
#
#   Sector-year cells with a single firm in the full sample may be excluded from
#   evaluation to avoid mechanically perfect predictions after calibration.
#
# ROLE IN THE PIPELINE
#   This script produces the *simplest* benchmark against which:
#     - PPML models with partial pooling,
#     - PPML models augmented with fuel consumption proxies, and
#     - hurdle-based models with explicit extensive-margin modeling,
#   are compared in subsequent LOOCV analyses.
#
###############################################################################

# ===============
# Setup ---------
# ===============

rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")
root      <- file.path(code, "inferring_emissions")
utils     <- file.path(root, "utils")
loocv     <- file.path(root, "loocv")

# Load packages
suppressPackageStartupMessages({
  library(data.table)
  library(mgcv)
})

# Load helpers
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
source(file.path(utils, "calc_metrics.R"))
source(file.path(utils, "build_metrics_table.R"))
source(file.path(utils, "append_loocv_performance_metrics_log.R"))
source(file.path(loocv, "ppml.R"))

# ============================================================
<<<<<<< HEAD
# Load data
# ============================================================
load(file.path(proc_data, "loocv_training_sample.RData"))
df_full <- loocv_training_sample

sector_year_totals_full <- as.data.table(df_full)[
=======
# Perform LOOCV with partial pooling on training sample ------
# ============================================================

load(paste0(proc_data, "/loocv_training_sample.RData"))
df <- loocv_training_sample

sector_year_totals <- as.data.table(df)[
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

<<<<<<< HEAD
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
=======
out <- poissonPP_lofo(
  df = df,
  sector_year_totals = sector_year_totals,
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
<<<<<<< HEAD
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
=======
  # ---- proxy (single spec per call) ----
  proxy_df     = NULL,         # NULL => no proxy; else must have buyer_id, year, fuel_proxy
  proxy_keys   = c("buyer_id","year"),
  proxy_var    = "fuel_proxy",
  proxy_tag    = "none",
  coalesce_proxy_to_zero = TRUE,
  # ---- sector effects specification ----
  partial_pooling = FALSE,      # TRUE => s(sector, bs="re"); FALSE => sector FE
  # ---- calibration / evaluation ----
  fallback_equal_split = TRUE,
  progress_every = 50,
  drop_singleton_cells_in_metrics = TRUE
)

out$metrics
head(out$predictions)

# =======================
# Save it --------------
# =======================

metrics_tbl <- build_metrics_table(
  out             = out,
  model           = "ppml",
  step            = "2",
  sample          = "all",
  sectorfe        = "fe",
  fuel_proxy      = "none",
  n_obs_est       = nrow(df),
  n_firms_est     = data.table::uniqueN(df$vat)
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
)

metrics_path_rds <- file.path(output, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

<<<<<<< HEAD
message("Done. Appended ", nrow(metrics_tbl), " rows to metrics log.")
=======
# =======================
# When testing: run for random subset of training sample
# =======================

test <- F
if(test == TRUE){
  source(file.path(utils, "make_lofo_subsample.R"))
  
  load(paste0(proc_data, "/loocv_training_sample.RData"))
  
  sub <- make_lofo_subsample(
    df   = loocv_training_sample,
    frac = 0.10,
    seed = 123
  )
  
  df_small <- sub$df_sub
  sector_year_totals_small <- sub$sector_year_totals
  
  out_test <- poissonPP_lofo(
    df = df_small,
    sector_year_totals = sector_year_totals_small,
    id_var = "vat",
    year_var = "year",
    sector_var = "nace2d",
    y_var = "emissions",
    revenue_var = "revenue",
    # ---- proxy (single spec per call) ----
    proxy_df     = NULL,         # NULL => no proxy; else must have buyer_id, year, fuel_proxy
    proxy_keys   = c("buyer_id","year"),
    proxy_var    = "fuel_proxy",
    proxy_tag    = "none",
    coalesce_proxy_to_zero = TRUE,
    # ---- sector effects specification ----
    partial_pooling = FALSE,      # TRUE => s(sector, bs="re"); FALSE => sector FE
    # ---- calibration / evaluation ----
    fallback_equal_split = TRUE,
    progress_every = 50,
    drop_singleton_cells_in_metrics = TRUE
  )
  
  out_test$metrics
  head(out_test$predictions)
}
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
