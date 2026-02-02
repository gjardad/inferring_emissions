###############################################################################
# ppml_loop_over_proxies_sectorRE.R
#
# PURPOSE
#   Loop over all cached fuel consumption proxies and run LOFOCV PPML models
#   (single-equation) for each proxy specification, optionally with or without
#   partial pooling of sector effects.
#
#   This script is intended to generate the "PPML+Proxy" comparator family:
#     - PPML benchmark (no proxy) can be run separately (recommended), and
#       this script focuses on the proxy-augmented PPML runs.
#     - For each proxy definition (cached as an .rds), fit a PPML model:
#          E[y_it|X] = exp( log(revenue) + I(proxy>0) + asinh(proxy)
#                           + year FE + sector effect )
#       where the sector effect is either:
#         (i) sector fixed effects (partial_pooling = FALSE), or
#         (ii) partially pooled sector random effects via s(sector, bs="re")
#              (partial_pooling = TRUE).
#
#   The LOFOCV design holds out one firm (all years) per fold. Predictions for
#   the held-out firm are optionally calibrated to match known sector-year
#   totals (nace2d × year), mimicking deployment conditions where aggregates
#   are known.
#
# INPUTS
#   - Processed training sample:  data/processed/loocv_training_sample.RData
#   - Cached proxies:             output/.../02_proxies/cache/*.rds
#     Each proxy file is expected to contain either:
#       (a) list(name=..., proxy=data.frame(buyer_id, year, fuel_proxy), mods=...)
#       (b) or directly a data.frame with buyer_id, year, fuel_proxy
#
# OUTPUTS
#   - A combined metrics table (in-memory) with one row per (proxy × model_variant)
#   - Optionally appends results to your metrics log via append_metrics_log()
#
# NOTES
#   - This script intentionally does NOT embed any proxy loop inside the model
#     function. It calls poissonPP_lofo() once per proxy.
#   - Running 96 proxies with LOFOCV can be time-consuming. Consider splitting
#     the proxy list into batches if needed.
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
cache_dir <- file.path(root, "proxies", "cache")
# Load packages
suppressPackageStartupMessages({
  library(data.table)
  library(mgcv)
})

# Load helpers
source(file.path(utils, "calc_metrics.R"))
source(file.path(utils, "build_metrics_table.R"))
source(file.path(utils, "append_loocv_performance_metrics_log.R"))
source(file.path(loocv, "ppml.R"))

# ============================================================
# Load training sample and build calibration targets
# ============================================================

load(file.path(proc_data, "loocv_training_sample.RData"))
df <- loocv_training_sample

sector_year_totals <- as.data.table(df)[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

# ============================================================
# Discover cached proxy files
# ============================================================

proxy_files <- list.files(cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(proxy_files) == 0) stop("No proxy .rds files found in cache_dir: ", cache_dir)

# ============================================================
# Config: choose sector effects variants to run
# ============================================================

pooling_grid <- data.table(
  partial_pooling = TRUE
)

# ============================================================
# Run LOFOCV for each proxy (and pooling option)
# ============================================================

metrics_list <- list()
k <- 0L

for (pf in proxy_files) {
  
  obj <- readRDS(pf)
  
  proxy_tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(pf))
  
  # Safety checks (optional but recommended)
  proxy_tbl <- as.data.table(proxy_tbl)

  # rename buyer_id -> vat (in-place)
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }
  
  # safety check
  stopifnot(all(c("vat","year","fuel_proxy") %in% names(proxy_tbl)))
  
  if (!all(c("vat", "year", "fuel_proxy") %in% names(proxy_tbl))) {
    stop("Proxy file does not contain vat/year/fuel_proxy: ", pf)
  }
  
  for (pp in pooling_grid$partial_pooling) {
    
    message("Running proxy = ", proxy_name, " | partial_pooling = ", pp)
    
    out <- poissonPP_lofo(
      df = df,
      sector_year_totals = sector_year_totals,
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
      partial_pooling = pp,
      # ---- calibration / evaluation ----
      fallback_equal_split = TRUE,
      progress_every = 50,
      drop_singleton_cells_in_metrics = TRUE
    )
    
    k <- k + 1L
    sectorfe_tag <- if (isTRUE(pp)) "re" else "fe"
    
    metrics_list[[k]] <- build_metrics_table(
      out             = out,
      model           = "ppml",
      step            = "2",
      sample          = "all",              # or "subsample" when testing
      sectorfe        = sectorfe_tag,
      fuel_proxy      = proxy_name,         # <-- key change vs "none"
      n_obs_est       = nrow(df),
      n_firms_est     = data.table::uniqueN(df$vat)
    )
  }
}

# ===============
# Save it -------
# ===============

metrics_tbl_proxy <- data.table::rbindlist(metrics_list, fill = TRUE)

metrics_path_rds <- file.path(output, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl_proxy,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

# =======================
# When testing: run for random subset of training sample
# =======================

test <- F
if(test == TRUE){
  source(file.path(utils, "make_lofo_subsample.R"))
  
  load(paste0(proc_data, "/loocv_training_sample.RData"))
  

  sub <- make_lofo_subsample(
    df   = loocv_training_sample,
    frac = 0.20,
    
    # WHY AM i USING 0.2 INSTEAD OF 0.1 HERE?
    
    # When running LOFOCV on a very small subsample (e.g. 10% of the training data),
    # some folds can produce training sets with too little information to reliably
    # estimate the variance component of the sector random effects
    # (s(nace2d, bs = "re")) under Poisson REML. In those folds, the likelihood becomes
    # nearly flat or boundary-valued in the smoothing-parameter (variance) direction,
    # causing mgcv's REML optimization to step into NA/Inf values and fail
    # (gam.fit3 arg 3 error). This is a small-sample / fold-composition issue rather
    # than a data or model-specification problem, and it disappears as the training
    # sample grows (e.g. at 20% or full sample), where sector-level information is
    # sufficient to stabilize variance-component estimation.
    
    seed = 123
  )
  
  df_small <- sub$df_sub
  sector_year_totals_small <- sub$sector_year_totals
  
  
  metrics_list <- list()
  k <- 0L
  
  for (pf in proxy_files) {
    
    obj <- readRDS(pf)
    
    proxy_tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
    proxy_name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(pf))
    
    # Safety checks (optional but recommended)
    proxy_tbl <- as.data.table(proxy_tbl)
    
    # rename buyer_id -> vat (in-place)
    if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
      setnames(proxy_tbl, "buyer_id", "vat")
    }
    
    # safety check
    stopifnot(all(c("vat","year","fuel_proxy") %in% names(proxy_tbl)))
    
    if (!all(c("vat", "year", "fuel_proxy") %in% names(proxy_tbl))) {
      stop("Proxy file does not contain vat/year/fuel_proxy: ", pf)
    }
    
    for (pp in pooling_grid$partial_pooling) {
      
      message("Running proxy = ", proxy_name, " | partial_pooling = ", pp)
      
      out_test <- poissonPP_lofo(
        df = df_small,
        sector_year_totals = sector_year_totals_small,
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
        partial_pooling = pp,
        # ---- calibration / evaluation ----
        fallback_equal_split = TRUE,
        progress_every = 50,
        drop_singleton_cells_in_metrics = TRUE
      )
      
      k <- k + 1L
      sectorfe_tag <- if (isTRUE(pp)) "re" else "fe"
      
      metrics_list[[k]] <- build_metrics_table(
        out             = out_test,
        model           = "ppml",
        step            = "2",
        sample          = "all",              # or "subsample" when testing
        sectorfe        = sectorfe_tag,
        fuel_proxy      = proxy_name,         # <-- key change vs "none"
        n_obs_est       = nrow(df_small),
        n_firms_est     = data.table::uniqueN(df_small$vat)
      )
    }
  }
  
  metrics_tbl_proxy <- data.table::rbindlist(metrics_list, fill = TRUE)
  
  metrics_path_rds <- file.path(output, "model_performance_metrics.rds")
  metrics_path_csv <- file.path(output, "model_performance_metrics.csv")
  
  metrics_all <- append_metrics_log(
    metrics_tbl_proxy,
    rds_path = metrics_path_rds,
    csv_path = metrics_path_csv,
    dedup = TRUE
  )
}
