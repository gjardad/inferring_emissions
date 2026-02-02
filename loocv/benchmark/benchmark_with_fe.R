###############################################################################
# 03_models/benchmark/01_fit_benchmark.R
#
# PURPOSE
#   Benchmark model estimation code (new system).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/03_models/benchmark/01_fit_benchmark.R
###############################################################################

# ============================================================
# Leave-One-Firm-Out CV (LOFOCV) for benchmark model
# with fully saturated sector fixed effects
#
# Model (benchmark):
#   emissions ~ log(revenue) + year FE + sector FE
#   where sector effects are estimated as unrestricted
#   fixed effects (no shrinkage).
#
# Estimation:
#   Poisson Pseudo-Maximum Likelihood (PPML) with:
#     - year fixed effects
#     - sector fixed effects
#
# Two-step procedure:
#   1. Fit the PPML model on training firms and predict
#      firm-year-level emissions.
#   2. Calibrate firm-year predictions so that within each
#      sector-year:
#        sum_i pred_cal_{i,s,t} = E_total_{s,t}   (known aggregate),
#      while preserving relative weights implied by the raw
#      PPML predictions.
#
# Cross-validation design:
#   LOFOCV: hold out one firm (all its years), fit on the
#           remaining firms, predict the held-out firm, and
#           then calibrate predictions using FULL known
#           sector-year totals.
#
# Evaluation:
#   Compute nRMSE, sMAPE, R2_LOO, and Spearman rank correlation
#   for both:
#     (i) raw PPML predictions, and
#     (ii) predictions calibrated to sector-year aggregates.
#
# Notes:
#   - Sector fixed effects are identified only from firms
#     present in the training fold.
#   - In sector-year cells with a single firm in the full
#     sample, calibration mechanically assigns the entire
#     sector-year total to that firm; such observations can
#     optionally be excluded from evaluation to avoid
#     artificially inflated performance metrics.
# ============================================================

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

# Load packages
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(dplyr)
})

# Load helpers
source(file.path(code, "calc_metrics.R"))
source(file.path(code, "build_metrics_table.R"))
source(file.path(code, "append_loocv_performance_metrics_log.R"))

# =========================
# LOFOCV function ---------
# =========================
ppml_lofo_fullcal <- function(df,
                              sector_year_totals,            # must include: sector, year, E_total
                              id_var       = "vat",
                              year_var     = "year",
                              sector_var   = "nace2d",
                              y_var        = "emissions",
                              revenue_var  = "revenue",
                              fallback_equal_split = TRUE,   # if all weights in a cell are zero
                              progress_every = 50) {
  
  DT <- as.data.table(df)
  
  # Keep all emissions >= 0, including zeros
  DT <- DT[
    !is.na(get(id_var)) & !is.na(get(year_var)) & !is.na(get(sector_var)) &
      is.finite(get(revenue_var)) & is.finite(get(y_var)) &
      get(y_var) >= 0
  ]
  
  # Types for merges/FE stability
  DT[, (year_var)   := as.integer(get(year_var))]
  DT[, (sector_var) := as.character(get(sector_var))]
  
  # Regressor
  DT[, x_logrev := log(pmax(get(revenue_var), 1e-12))]
  
  # Known totals
  SYT <- as.data.table(sector_year_totals)
  if (!all(c(sector_var, year_var, "E_total") %in% names(SYT))) {
    stop("sector_year_totals must contain columns: ", sector_var, ", ", year_var, ", E_total")
  }
  SYT <- SYT[, .(
    sector_key = as.character(get(sector_var)),
    year_key   = as.integer(get(year_var)),
    E_total    = as.numeric(E_total)
  )]
  setkey(SYT, sector_key, year_key)
  
  # Precompute unique ids
  ids <- unique(DT[[id_var]])
  nF  <- length(ids)
  
  preds_list <- vector("list", nF)
  t0 <- Sys.time()
  
  # For speed, create local aliases to avoid repeated get() inside formulas
  DT[, id__     := get(id_var)]
  DT[, year__   := get(year_var)]
  DT[, sector__ := get(sector_var)]
  DT[, y__      := get(y_var)]
  
  for (i in seq_along(ids)) {
    heldout_id <- ids[i]
    
    if (i %% progress_every == 0) {
      dt <- difftime(Sys.time(), t0, units = "mins")
      message(sprintf("LOFOCV %d / %d (elapsed %.1f min)", i, nF, as.numeric(dt)))
    }
    
    train <- DT[id__ != heldout_id]
    test  <- DT[id__ == heldout_id]
    
    # ---- PPML with FE ----
    mod <- fepois(
      y__ ~ x_logrev | year__ + sector__,
      data  = train,
      notes = FALSE, warn = FALSE
    )
    
    # Raw predictions (levels)
    train[, yhat_raw := pmax(as.numeric(predict(mod, newdata = train, type = "response")), 0)]
    test[,  yhat_raw := pmax(as.numeric(predict(mod, newdata = test,  type = "response")), 0)]
    
    # ---- Calibrate firm-year predictions to match sector-year aggregates ----
    
    denom_train <- train[, .(
      denom_train = sum(yhat_raw, na.rm = TRUE),
      n_train     = .N
    ), by = .(sector__ , year__)]
    
    # Attach known totals
    setnames(denom_train, c("sector__", "year__"), c("sector_key", "year_key"))
    cell_info <- merge(denom_train, SYT, by = c("sector_key", "year_key"), all.x = TRUE)
    
    # Merge cell_info to test
    test2 <- copy(test)
    test2[, sector_key := sector__]
    test2[, year_key   := year__]
    test2 <- merge(test2, cell_info, by = c("sector_key", "year_key"), all.x = TRUE)
    
    test2[is.na(denom_train), denom_train := 0]
    test2[is.na(n_train),     n_train := 0L]
    
    # denom_full includes heldout firm's own weight
    test2[, denom_full := denom_train + yhat_raw]
    
    # Case 1: known total is exactly zero -> calibrated must be zero
    test2[is.finite(E_total) & E_total == 0, yhat_cal := 0]
    
    # Case 2: known total > 0 and denom_full > 0 -> proportional allocation
    test2[is.finite(E_total) & E_total > 0 & denom_full > 0,
         yhat_cal := E_total * (yhat_raw / denom_full)]
    
    # ---- Fallback only when total > 0 but denom_full == 0 ----
    if (fallback_equal_split) {
      test2[, n_full := n_train + 1L]
      test2[is.finite(E_total) & E_total > 0 &
              (is.na(yhat_cal) | !is.finite(yhat_cal)) & n_full > 0,
            yhat_cal := E_total / n_full]
    }
    
    preds_list[[i]] <- test2[, .(
      id     = id__,
      year   = year__,
      sector = sector__,
      y_true = y__,
      yhat_raw,
      yhat_cal
    )]
  }
  
  P <- rbindlist(preds_list, use.names = TRUE, fill = TRUE)
  
  # ---- metrics on held-out observations ----
  m_raw <- calc_metrics(P$y_true, P$yhat_raw)
  m_cal <- calc_metrics(P$y_true, P$yhat_cal)
  
  metrics <- rbind(
    data.table(
      model = "raw",
      nRMSE = m_raw$nRMSE,
      MAPD_emitters = m_raw$MAPD_emitters,
      FPR_nonemitters = m_raw$FPR_nonemitters,
      R2_LOO = m_raw$R2_LOO,
      spearman = m_raw$spearman
    ),
    data.table(
      model = "calibrated_to_match_sectoral_agg",
      nRMSE = m_cal$nRMSE,
      MAPD_emitters = m_cal$MAPD_emitters,
      FPR_nonemitters = m_cal$FPR_nonemitters,
      R2_LOO = m_cal$R2_LOO,
      spearman = m_cal$spearman
    )
  )
  
  list(predictions = P, metrics = metrics)
}

# ============================================
# Perform LOOCV on training sample -----------
# ============================================

# 0) Load data
load(paste0(proc_data, "/loocv_training_sample.RData"))
df <- loocv_training_sample

# 1) Build sector-year totals
sector_year_totals <- as.data.table(df)[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

# 2) Run LOFOCV with PPML + calibration to full known totals
out <- ppml_lofo_fullcal(
  df = df,
  sector_year_totals = sector_year_totals,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  progress_every = 50
)

out$metrics
head(out$predictions)

# Save it ------
metrics_tbl <- build_metrics_table(
  out             = out,
  model_family    = "benchmark",
  partial_pooling = "no",
  fuel_proxy      = "none",
  sample_tag      = "loocv_training_sample",
  n_obs_est       = nrow(df),
  n_firms_est     = data.table::uniqueN(df$vat)
)

metrics_path_rds <- file.path(output, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)

# Obs: some firms have yhat_cal NA because they are the only firms within sector-year.
# In that case, the calibrated yhat would simply allocate the known sector-year aggregate
# to that one firm, and yhat_cal woudl be exactly equal to y_true.
# Doing this would significantly increase performance metrics, but this would be artificial:
# in deployment data there is no sector-year for which there is only one firm, and thus
# it will never be the case in deployment data that I'd get such a precise firm-year
# impute as the ones I'd obtain in the training sample.
