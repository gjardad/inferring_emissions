###############################################################################
# 03_models/ppml.R
#
# PURPOSE
#   Leave-One-Firm-Out cross-validation (LOFOCV) for Poisson Pseudo–Maximum
#   Likelihood (PPML) models used as single-equation baselines in the
#   emissions-imputation exercise.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/03_models/ppml.R
#
# DESIGN
#   - Estimates ONE PPML specification per call (benchmark or with proxy)
#   - LOFOCV: hold out one firm (all years), train on remaining firms, predict
#     the held-out firm in all its observed years.
#   - Optional calibration to sector-year totals (deployment-style)
#   - Metrics computed via calc_metrics() and returned in a standardized
#     out$metrics table compatible with build_metrics_table().
#
# METRICS (via calc_metrics)
#   - nRMSE = RMSE / sd(y) (computed on evaluation sample)
#   - RMSE
#   - MAPD_emitters
#   - FPR_nonemitters (uses fp_threshold)
#   - med_yhat_nonemit, p90_yhat_nonemit
#   - R2_LOO
#   - spearman
###############################################################################

poissonPP_lofo <- function(df,
                           sector_year_totals,          # cols: sector_var, year_var, E_total
                           id_var       = "vat",
                           year_var     = "year",
                           sector_var   = "nace2d",
                           y_var        = "emissions",
                           revenue_var  = "revenue",
                           # ---- proxy (single spec per call) ----
                           proxy_df     = NULL,         # NULL => no proxy; else must have proxy_keys + proxy_var
                           proxy_keys   = c("buyer_id","year"),
                           proxy_var    = "fuel_proxy",
                           proxy_tag    = "none",
                           coalesce_proxy_to_zero = TRUE,
                           # ---- sector effects specification ----
                           partial_pooling = TRUE,      # TRUE => s(sector, bs="re"); FALSE => sector FE
                           # ---- calibration / evaluation ----
                           fallback_equal_split = TRUE,
                           drop_singleton_cells_in_metrics = TRUE,
                           fp_threshold = 0,            # forwarded to calc_metrics()
                           progress_every = 50) {
  
  suppressPackageStartupMessages({
    library(data.table)
    library(mgcv)
  })
  
  DT <- as.data.table(df)
  
  # -----------------------
  # Basic cleaning / types
  # -----------------------
  DT <- DT[
    !is.na(get(id_var)) &
      !is.na(get(year_var)) &
      !is.na(get(sector_var)) &
      is.finite(get(revenue_var)) &
      is.finite(get(y_var)) &
      get(y_var) >= 0
  ]
  
  DT[, (year_var)   := as.integer(get(year_var))]
  DT[, (sector_var) := as.character(get(sector_var))]
  
  # regressors / aliases used downstream
  DT[, x_logrev := log(pmax(get(revenue_var), 1e-12))]
  
  DT[, id__     := get(id_var)]
  DT[, year__   := get(year_var)]
  DT[, sector__ := get(sector_var)]
  DT[, y__      := as.numeric(get(y_var))]
  
  include_proxy <- !is.null(proxy_df)
  
  # -----------------------
  # Attach proxy (single spec)
  # -----------------------
  if (include_proxy) {
    PXY <- as.data.table(proxy_df)
    
    if (!all(c(proxy_keys, proxy_var) %in% names(PXY))) {
      stop("proxy_df must contain: ", paste(c(proxy_keys, proxy_var), collapse = ", "))
    }
    
    if ("year" %in% proxy_keys) PXY[, year := as.integer(year)]
    
    if (!all(proxy_keys %in% names(DT))) {
      stop("df must contain proxy_keys to merge proxy_df: ", paste(proxy_keys, collapse = ", "))
    }
    
    setkeyv(DT, proxy_keys)
    setkeyv(PXY, proxy_keys)
    DT <- PXY[DT]  # left join into DT rows
    
    if (coalesce_proxy_to_zero) {
      DT[is.na(get(proxy_var)), (proxy_var) := 0]
    }
    
    DT[, I_pos__   := as.integer(get(proxy_var) > 0)]
    DT[, w_asinh__ := asinh(as.numeric(get(proxy_var)))]
  } else {
    DT[, I_pos__   := 0L]
    DT[, w_asinh__ := 0]
  }
  
  # fixed levels across folds (avoid new-level issues in prediction)
  all_years   <- sort(unique(DT[["year__"]]))
  all_sectors <- sort(unique(DT[["sector__"]]))
  
  # -----------------------
  # Known totals (sector-year)
  # -----------------------
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
  
  # Full-sample cell counts once (for singleton filtering)
  full_cellN <- DT[, .N, by = .(sector__, year__)]
  setnames(full_cellN, c("sector__", "year__", "N"),
           c("sector_key", "year_key", "N_full"))
  
  ids <- unique(DT$id__)
  nF  <- length(ids)
  
  preds_list <- vector("list", nF)
  t0 <- Sys.time()
  
  # -----------------------
  # Formula builder
  # -----------------------
  sector_term <- if (partial_pooling) {
    "s(sector__, bs = 're')"
  } else {
    "sector__"
  }
  
  rhs <- if (include_proxy) {
    paste("x_logrev + I_pos__ + w_asinh__ + year__ +", sector_term)
  } else {
    paste("x_logrev + year__ +", sector_term)
  }
  
  fml <- as.formula(paste("y__ ~", rhs))
  
  # -----------------------
  # LOFOCV loop
  # -----------------------
  for (i in seq_along(ids)) {
    heldout_id <- ids[i]
    
    if (i %% progress_every == 0) {
      dtm <- difftime(Sys.time(), t0, units = "mins")
      message(sprintf(
        "LOFOCV %d / %d (elapsed %.1f min) | proxy=%s | sector=%s",
        i, nF, as.numeric(dtm), proxy_tag,
        if (partial_pooling) "re" else "fe"
      ))
    }
    
    train <- copy(DT[id__ != heldout_id])
    test  <- copy(DT[id__ == heldout_id])
    
    train_df <- as.data.frame(train)
    test_df  <- as.data.frame(test)
    
    # lock factor levels
    train_df$year__   <- factor(train_df$year__,   levels = all_years)
    test_df$year__    <- factor(test_df$year__,    levels = all_years)
    
    train_df$sector__ <- factor(train_df$sector__, levels = all_sectors)
    test_df$sector__  <- factor(test_df$sector__,  levels = all_sectors)
    
    # ------------------------------------------------------------------
    # FE fallback for unseen sectors in test fold (singleton-sector case)
    # If sector is absent from training, set its sector effect to 0 by
    # mapping test sector to the training reference sector.
    # ------------------------------------------------------------------
    if (!partial_pooling) {
      # sectors actually present in THIS training fold
      train_sectors_present <- unique(as.character(train$sector__))
      
      # choose a reference sector that is guaranteed to be in training
      ref_sector <- train_sectors_present[1]
      
      # make that sector the reference level in BOTH train and test
      train_df$sector__ <- stats::relevel(train_df$sector__, ref = ref_sector)
      test_df$sector__  <- stats::relevel(test_df$sector__,  ref = ref_sector)
      
      # map any test sectors not present in training to the reference
      test_sector_chr <- as.character(test_df$sector__)
      unseen_sector   <- !(test_sector_chr %in% train_sectors_present)
      
      if (any(unseen_sector)) {
        test_sector_chr[unseen_sector] <- ref_sector
        test_df$sector__ <- factor(test_sector_chr, levels = levels(train_df$sector__))
      }
    }
    
    mod <- mgcv::gam(
      formula = fml,
      data    = train_df,
      family  = poisson(link = "log"),
      method  = "REML"
    )
    
    train$yhat_raw <- pmax(as.numeric(predict(mod, newdata = train_df, type = "response")), 0)
    test$yhat_raw  <- pmax(as.numeric(predict(mod, newdata = test_df,  type = "response")), 0)
    
    # -----------------------
    # Calibrate to sector-year totals (deployment-style)
    # -----------------------
    denom_train <- train[, .(
      denom_train = sum(yhat_raw, na.rm = TRUE),
      n_train     = .N
    ), by = .(sector__, year__)]
    setnames(denom_train, c("sector__", "year__"), c("sector_key", "year_key"))
    
    cell_info <- merge(denom_train, SYT,       by = c("sector_key", "year_key"), all.x = TRUE)
    cell_info <- merge(cell_info,  full_cellN, by = c("sector_key", "year_key"), all.x = TRUE)
    
    test2 <- copy(test)
    test2[, sector_key := sector__]
    test2[, year_key   := year__]
    test2 <- merge(test2, cell_info, by = c("sector_key", "year_key"), all.x = TRUE)
    
    test2[is.na(denom_train), denom_train := 0]
    test2[is.na(n_train),     n_train := 0L]
    test2[is.na(N_full),      N_full := 1L]
    
    # include test firm's own raw prediction in denom (deployment-style)
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
  
  P <- data.table::rbindlist(preds_list, use.names = TRUE, fill = TRUE)
  
  # -----------------------
  # Metrics (standardized)
  # -----------------------
  eval_dropped_singletons <- isTRUE(drop_singleton_cells_in_metrics)
  
  Pm_raw <- P
  Pm_cal <- P
  if (eval_dropped_singletons) {
    Pm_raw <- P[N_full > 1]
    Pm_cal <- P[N_full > 1]
  }
  
  m_raw <- calc_metrics(Pm_raw$y_true, Pm_raw$yhat_raw, fp_threshold = fp_threshold)
  m_cal <- calc_metrics(Pm_cal$y_true, Pm_cal$yhat_cal, fp_threshold = fp_threshold)
  
  # ---- out$metrics format expected by build_metrics_table() ----
  metrics <- data.table::rbindlist(list(
    data.table::data.table(
      variant = "raw",
      proxy   = proxy_tag,
      eval_dropped_singletons = eval_dropped_singletons,
      nRMSE = m_raw$nRMSE,
      RMSE  = m_raw$RMSE,
      MAPD_emitters   = m_raw$MAPD_emitters,
      FPR_nonemitters = m_raw$FPR_nonemitters,
      med_yhat_nonemit = m_raw$med_yhat_nonemit,
      p90_yhat_nonemit = m_raw$p90_yhat_nonemit,
      max_yhat_nonemit = m_raw$max_yhat_nonemit,
      pctile_all_yhat_at_med_nonemit = m_raw$pctile_all_yhat_at_med_nonemit,
      pctile_all_yhat_at_max_nonemit = m_raw$pctile_all_yhat_at_max_nonemit,
      R2_LOO   = m_raw$R2_LOO,
      spearman = m_raw$spearman
    ),
    data.table::data.table(
      variant = "calibrated",
      proxy   = proxy_tag,
      eval_dropped_singletons = eval_dropped_singletons,
      nRMSE = m_cal$nRMSE,
      RMSE  = m_cal$RMSE,
      MAPD_emitters   = m_cal$MAPD_emitters,
      FPR_nonemitters = m_cal$FPR_nonemitters,
      med_yhat_nonemit = m_cal$med_yhat_nonemit,
      p90_yhat_nonemit = m_cal$p90_yhat_nonemit,
      max_yhat_nonemit = m_cal$max_yhat_nonemit,
      pctile_all_yhat_at_med_nonemit = m_cal$pctile_all_yhat_at_med_nonemit,
      pctile_all_yhat_at_max_nonemit = m_cal$pctile_all_yhat_at_max_nonemit,
      R2_LOO   = m_cal$R2_LOO,
      spearman = m_cal$spearman
    )
  ))
  
  list(
    predictions     = P,
    metrics         = metrics,
    proxy           = proxy_tag,
    include_proxy   = include_proxy,
    partial_pooling = partial_pooling
  )
}
