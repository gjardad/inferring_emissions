###############################################################################
# fit_partial_pooling_loggaussian.R
#
# PURPOSE
#   LOFOCV benchmark model that matches the Step-2 intensive specification
#   (log-Gaussian on emitters only with partial pooling sector effects),
#   but WITHOUT any fuel proxy covariates.
#
# MODEL (Benchmark comparable to Step 2):
#   Intensive model (fit on emitters in training fold):
#     log(emissions) ~ log(revenue) + year FE + sector RE (partially pooled)
#
#   Predictions (applied to everyone in the held-out firm):
#     yhat_raw = exp(eta_hat) * smear
#   where smear is computed on training emitters only:
#     smear = mean( exp(residual) )
#
#   Calibration (optional, same as before):
#     Calibrate to match known totals within sector-year (nace2d × year):
#       sum_i yhat_cal_{i,sy} = E_total_{sy}
#     using full totals (deployment-style target).
#
# EVALUATION
#   Metrics computed on held-out firm-year observations, with nRMSE defined
#   in calc_metrics.R as RMSE / sd(y). (You said you already updated calc_metrics.)
#   Note: This benchmark’s "raw" predictions correspond to the Step-2-only
#   baseline that applies the intensive model to everyone (i.e., p=1).
###############################################################################


suppressPackageStartupMessages({
  library(data.table)
  library(mgcv)
})

# Helpers (you already have these in your system)
source(file.path(LOOCV_DIR, "calc_metrics.R"))
source(file.path(LOOCV_DIR, "build_metrics_table.R"))
source(file.path(LOOCV_DIR, "append_loocv_performance_metrics_log.R"))

# ============================================================
# LOFOCV: log-Gaussian (emitters only) + sector RE + calibration
# ============================================================
logGaussianPP_lofo_fullcal <- function(df,
                                       sector_year_totals,   # cols: sector_var, year_var, E_total
                                       id_var       = "vat",
                                       year_var     = "year",
                                       sector_var   = "nace2d",
                                       y_var        = "emissions",
                                       revenue_var  = "revenue",
                                       fallback_equal_split = TRUE,
                                       drop_singleton_cells_in_metrics = TRUE,
                                       min_train_emitters = 20,
                                       progress_every = 50) {
  
  DT <- as.data.table(df)
  
  # Basic cleaning: keep non-missing ids, sector/year, finite revenue and y >= 0
  DT <- DT[
    !is.na(get(id_var)) & !is.na(get(year_var)) & !is.na(get(sector_var)) &
      is.finite(get(revenue_var)) & is.finite(get(y_var)) & get(y_var) >= 0
  ]
  
  # Types
  DT[, (year_var)   := as.integer(get(year_var))]
  DT[, (sector_var) := as.character(get(sector_var))]
  
  # Regressor
  DT[, x_logrev := log(pmax(get(revenue_var), 1e-12))]
  
  # Aliases
  DT[, id__     := get(id_var)]
  DT[, year__   := get(year_var)]
  DT[, sector__ := get(sector_var)]
  DT[, y__      := as.numeric(get(y_var))]
  DT[, D__      := as.integer(y__ > 0)]
  DT[, logy__   := ifelse(D__ == 1, log(y__), NA_real_)]
  
  all_years   <- sort(unique(DT[["year__"]]))
  all_sectors <- sort(unique(DT[["sector__"]]))
  
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
  
  # Full-sample cell counts (for singleton filtering)
  full_cellN <- DT[, .N, by = .(sector__, year__)]
  setnames(full_cellN,
           c("sector__", "year__", "N"),
           c("sector_key", "year_key", "N_full"))
  
  ids <- unique(DT$id__)
  nF  <- length(ids)
  
  preds_list <- vector("list", nF)
  t0 <- Sys.time()
  
  for (i in seq_along(ids)) {
    heldout_id <- ids[i]
    
    if (i %% progress_every == 0) {
      dtm <- difftime(Sys.time(), t0, units = "mins")
      message(sprintf("LOFOCV %d / %d (elapsed %.1f min)", i, nF, as.numeric(dtm)))
    }
    
    train <- copy(DT[id__ != heldout_id])
    test  <- copy(DT[id__ == heldout_id])
    
    train_df <- as.data.frame(train)
    test_df  <- as.data.frame(test)
    
    # Factor levels fixed across folds (important!)
    train_df$year__   <- factor(train_df$year__,   levels = all_years)
    test_df$year__    <- factor(test_df$year__,    levels = all_years)
    
    train_df$sector__ <- factor(train_df$sector__, levels = all_sectors)
    test_df$sector__  <- factor(test_df$sector__,  levels = all_sectors)
    
    # --- Fit intensive model on emitters only ---
    train_emit <- train_df[train_df$D__ == 1 & is.finite(train_df$logy__), , drop = FALSE]
    
    # If a fold has too few emitters, fall back to something safe.
    # Here: predict 0 raw; calibration can still allocate totals if you enable fallback.
    if (nrow(train_emit) < min_train_emitters) {
      train$yhat_raw <- 0
      test$yhat_raw  <- 0
    } else {
      
      mod <- mgcv::gam(
        logy__ ~ x_logrev + year__ + s(sector__, bs = "re"),
        data   = train_emit,
        family = gaussian(),
        method = "REML"
      )
      
      # Smearing correction computed on training emitters only
      eta_train <- as.numeric(predict(mod, newdata = train_emit, type = "link"))
      resid_train <- train_emit$logy__ - eta_train
      smear <- mean(exp(resid_train), na.rm = TRUE)
      if (!is.finite(smear) || smear <= 0) smear <- 1
      
      # Predict for ALL observations (this is the "apply Step 2 to everyone" baseline)
      eta_train_all <- as.numeric(predict(mod, newdata = train_df, type = "link"))
      eta_test_all  <- as.numeric(predict(mod, newdata = test_df,  type = "link"))
      
      train$yhat_raw <- pmax(exp(eta_train_all) * smear, 0)
      test$yhat_raw  <- pmax(exp(eta_test_all)  * smear, 0)
    }
    
    # --- Calibrate to sector-year totals (same logic as your PPML script) ---
    denom_train <- train[, .(
      denom_train = sum(yhat_raw, na.rm = TRUE),
      n_train     = .N
    ), by = .(sector__, year__)]
    setnames(denom_train, c("sector__", "year__"), c("sector_key", "year_key"))
    
    cell_info <- merge(denom_train, SYT,      by = c("sector_key", "year_key"), all.x = TRUE)
    cell_info <- merge(cell_info,  full_cellN, by = c("sector_key", "year_key"), all.x = TRUE)
    
    test2 <- copy(test)
    test2[, sector_key := sector__]
    test2[, year_key   := year__]
    test2 <- merge(test2, cell_info, by = c("sector_key", "year_key"), all.x = TRUE)
    
    test2[is.na(denom_train), denom_train := 0]
    test2[is.na(n_train),     n_train := 0L]
    test2[is.na(N_full),      N_full := 1L]
    
    # "full" denominator includes test firm's own raw prediction
    test2[, denom_full := denom_train + yhat_raw]
    
    test2[is.finite(E_total) & E_total == 0, yhat_cal := 0]
    test2[is.finite(E_total) & E_total > 0 & denom_full > 0,
          yhat_cal := E_total * (yhat_raw / denom_full)]
    
    if (fallback_equal_split) {
      test2[, n_full_trainplus := n_train + 1L]
      test2[is.finite(E_total) & E_total > 0 &
              (is.na(yhat_cal) | !is.finite(yhat_cal)) & n_full_trainplus > 0,
            yhat_cal := E_total / n_full_trainplus]
    }
    
    preds_list[[i]] <- test2[, .(
      id     = id__,
      year   = year__,
      sector = sector__,
      y_true = y__,
      yhat_raw,
      yhat_cal,
      N_full
    )]
  }
  
  P <- rbindlist(preds_list, use.names = TRUE, fill = TRUE)
  
  # --- Metrics ---
  Pm_raw <- P
  Pm_cal <- P
  
  if (drop_singleton_cells_in_metrics) {
    Pm_raw <- P[N_full > 1]
    Pm_cal <- P[N_full > 1]
  }
  
  m_raw <- calc_metrics(Pm_raw$y_true, Pm_raw$yhat_raw)
  m_cal <- calc_metrics(Pm_cal$y_true, Pm_cal$yhat_cal)
  
  metrics <- rbind(
    data.table(
      model = "raw_partial_pooling_loggaussian_intensive_only",
      nRMSE = m_raw$nRMSE,
      MAPD_emitters = m_raw$MAPD_emitters,
      FPR_nonemitters = m_raw$FPR_nonemitters,
      R2_LOO = m_raw$R2_LOO,
      spearman = m_raw$spearman
    ),
    data.table(
      model = "calibrated_to_match_sectoral_agg_partial_pooling_loggaussian_intensive_only",
      nRMSE = m_cal$nRMSE,
      MAPD_emitters = m_cal$MAPD_emitters,
      FPR_nonemitters = m_cal$FPR_nonemitters,
      R2_LOO = m_cal$R2_LOO,
      spearman = m_cal$spearman
    )
  )
  
  list(predictions = P, metrics = metrics)
}

# ============================================================
# Run benchmark on your LOOCV training sample
# ============================================================

load(file.path(PROC_DATA, "loocv_training_sample.RData"))
df <- loocv_training_sample

sector_year_totals <- as.data.table(df)[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

out_gaussian_pp <- logGaussianPP_lofo_fullcal(
  df = df,
  sector_year_totals = sector_year_totals,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  progress_every = 50,
  drop_singleton_cells_in_metrics = TRUE
)

print(out_gaussian_pp$metrics)
print(head(out_gaussian_pp$predictions))

# Save metrics log entry (same pattern as your old scripts)
metrics_tbl <- build_metrics_table(
  out             = out_gaussian_pp,
  model_family    = "benchmark_gaussian",
  partial_pooling = "yes",
  fuel_proxy      = "none",
  sample_tag      = "loocv_training_sample",
  n_obs_est       = nrow(df),
  n_firms_est     = data.table::uniqueN(df$vat)
)

metrics_path_rds <- file.path(OUTPUT_DIR, "model_performance_metrics.rds")
metrics_path_csv <- file.path(OUTPUT_DIR, "model_performance_metrics.csv")

metrics_all <- append_metrics_log(
  metrics_tbl,
  rds_path = metrics_path_rds,
  csv_path = metrics_path_csv,
  dedup = TRUE
)
