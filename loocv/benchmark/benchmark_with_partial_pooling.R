###############################################################################
# benchmark_with_partial_pooling.R
#
# PURPOSE
#   Partial pooling model estimation code
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/03_models/benchmark/03_fit_partial_pooling.R
###############################################################################

# ============================================================
# Leave-One-Firm-Out CV (LOFOCV) for benchmark model
# with partially pooled sector effects
#
# Model (benchmark, partial pooling):
#   emissions ~ log(revenue) + year FE + sector RE
#   where sector effects are estimated with shrinkage
#   (Poisson model with log link; sector treated as a random effect)
#
# Estimation:
#   Poisson quasi-ML (PPML-style) with:
#     - year fixed effects (unpenalized)
#     - sector random effects (penalized / partially pooled)
#
# Two-step procedure:
#   1. Fit model on training firms and predict firm-year-level emissions
#      using partially pooled sector effects.
#   2. Calibrate firm-year predictions so that within each sector-year:
#        sum_i pred_cal_{i,s,t} = E_total_{s,t}   (known aggregate),
#      while preserving relative weights implied by the raw predictions.
#
# Cross-validation design:
#   LOFOCV: hold out one firm (all its years), fit on remaining firms,
#           predict the held-out firm, then calibrate using FULL
#           known sector-year totals.
#
# Evaluation:
#   Compute nRMSE, sMAPE, R2_LOO, and Spearman rank correlation
#   for both:
#     (i) raw model predictions, and
#     (ii) predictions calibrated to sector-year aggregates.
#
# Notes:
#   - Sector effects are shrunk toward the global mean based on
#     information in the training fold.
#   - Sector-year cells with a single firm in the full sample
#     can optionally be excluded from evaluation to avoid
#     mechanically perfect predictions after calibration.
# ============================================================

# ===============
# Setup ---------
# ===============


# Load packages
suppressPackageStartupMessages({
  library(data.table)
  library(mgcv)
})

# Load helpers
source(file.path(LOOCV_DIR, "calc_metrics.R"))
source(file.path(LOOCV_DIR, "build_metrics_table.R"))
source(file.path(LOOCV_DIR, "append_loocv_performance_metrics_log.R"))

# ============================================================
# LOFOCV with Poisson + partial pooling sector RE + calibration
# ============================================================
poissonPP_lofo_fullcal <- function(df,
                                   sector_year_totals,   # cols: sector_var, year_var, E_total
                                   id_var       = "vat",
                                   year_var     = "year",
                                   sector_var   = "nace2d",
                                   y_var        = "emissions",
                                   revenue_var  = "revenue",
                                   fallback_equal_split = TRUE,
                                   drop_singleton_cells_in_metrics = TRUE,
                                   progress_every = 50) {
  
  DT <- as.data.table(df)
  
  DT <- DT[
    !is.na(get(id_var)) & !is.na(get(year_var)) & !is.na(get(sector_var)) &
      is.finite(get(revenue_var)) & is.finite(get(y_var)) & get(y_var) >= 0
  ]
  
  # types
  DT[, (year_var)   := as.integer(get(year_var))]
  DT[, (sector_var) := as.character(get(sector_var))]
  
  # regressor
  DT[, x_logrev := log(pmax(get(revenue_var), 1e-12))]
  
  # aliases
  DT[, id__     := get(id_var)]
  DT[, year__   := get(year_var)]
  DT[, sector__ := get(sector_var)]
  DT[, y__      := get(y_var)]
  
  all_years <- sort(unique(DT[[ "year__" ]]))
  all_sectors <- sort(unique(DT[[ "sector__" ]]))
  
  # known totals
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
  
  # for your “singleton sector-year” concern: compute full-sample cell counts once
  full_cellN <- DT[, .N, by = .(sector__ , year__)]
  setnames(full_cellN, c("sector__", "year__", "N"), c("sector_key", "year_key", "N_full"))
  
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
    
    train_df$year__   <- factor(train_df$year__,   levels = all_years)
    test_df$year__    <- factor(test_df$year__,    levels = all_years)
    
    train_df$sector__ <- factor(train_df$sector__, levels = all_sectors)
    test_df$sector__  <- factor(test_df$sector__,  levels = all_sectors)
    
    # ---- Poisson with year FE + partially pooled sector effects ----
    # s(sector__, bs="re") is the shrinkage term
    mod <- mgcv::gam(
      y__ ~ x_logrev + year__ + s(sector__, bs = "re"),
      data   = train_df,
      family = poisson(link = "log"),
      method = "REML"
    )
    
    # raw predictions (levels)
    train$yhat_raw <- pmax(as.numeric(predict(mod, newdata = train_df, type = "response")), 0)
    test$yhat_raw  <- pmax(as.numeric(predict(mod, newdata = test_df,  type = "response")), 0)
    
    # ---- Calibrate to sector-year totals (your same logic) ----
    denom_train <- train[, .(
      denom_train = sum(yhat_raw, na.rm = TRUE),
      n_train     = .N
    ), by = .(sector__ , year__)]
    
    setnames(denom_train, c("sector__", "year__"), c("sector_key", "year_key"))
    cell_info <- merge(denom_train, SYT, by = c("sector_key", "year_key"), all.x = TRUE)
    cell_info <- merge(cell_info, full_cellN, by = c("sector_key", "year_key"), all.x = TRUE)
    
    test2 <- copy(test)
    test2[, sector_key := sector__]
    test2[, year_key   := year__]
    test2 <- merge(test2, cell_info, by = c("sector_key", "year_key"), all.x = TRUE)
    
    test2[is.na(denom_train), denom_train := 0]
    test2[is.na(n_train),     n_train := 0L]
    test2[is.na(N_full),      N_full := 1L]
    
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
  
  # ---- metrics ----
  Pm_raw <- P
  Pm_cal <- P
  
  if (drop_singleton_cells_in_metrics) {
    # removes the "only firm in sector-year" evaluation cases entirely
    Pm_raw <- P[N_full > 1]
    Pm_cal <- P[N_full > 1]
  }
  
  m_raw <- calc_metrics(Pm_raw$y_true, Pm_raw$yhat_raw)
  m_cal <- calc_metrics(Pm_cal$y_true, Pm_cal$yhat_cal)
  
  metrics <- rbind(
    data.table(
      model = "raw_partial_pooling",
      nRMSE = m_raw$nRMSE,
      MAPD_emitters = m_raw$MAPD_emitters,
      FPR_nonemitters = m_raw$FPR_nonemitters,
      R2_LOO = m_raw$R2_LOO,
      spearman = m_raw$spearman
    ),
    data.table(
      model = "calibrated_to_match_sectoral_agg_partial_pooling",
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
# Perform LOOCV with partial pooling on training sample ------
# ============================================================

load(paste0(PROC_DATA, "/loocv_training_sample.RData"))
df <- loocv_training_sample

sector_year_totals <- as.data.table(df)[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

out_pp <- poissonPP_lofo_fullcal(
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

out_pp$metrics
head(out_pp$predictions)

# Save it ------
metrics_tbl <- build_metrics_table(
  out             = out_pp,
  model_family    = "benchmark",
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

