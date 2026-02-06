# ============== BEGIN SETTING UP PATHS ============= #
suppressPackageStartupMessages({
  library(data.table)
})

# ========================
# Define data paths ------
# =========================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

# ===========================
# Define paths for code -----
# ===========================

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}

repo_dir <- paste0(getwd(), "/inferring_emissions")
utils_dir <- file.path(repo_dir, "utils")
loocv_dir <- file.path(repo_dir, "loocv")

#================== END SETTING UP PATHS ================ #

###############################################################################
# hurdle.R
#
# PURPOSE
#   Leave-One-Firm-Out cross-validation (LOFOCV) for a two-part "hurdle" model
#   used in the emissions-imputation exercise.
#
# DESIGN
#   - Estimates ONE hurdle specification per call:
#       * Step 1: extensive margin (probability of being an emitter)
#       * Step 2: intensive margin (emissions level conditional on emitting)
#     with possibly different proxies for Step 1 and Step 2.
#   - LOFOCV: hold out one firm (all years), train on remaining firms, predict
#     the held-out firm in all its observed years.
#   - Optional calibration to sector-year totals (deployment-style)
#   - Metrics computed via calc_metrics() and returned in a standardized
#     out$metrics table compatible with build_metrics_table().
#
# MODEL EQUATIONS
#   Let y_it be emissions, and D_it = 1{y_it > 0}.
#
#   Step 1 (Extensive margin, Logit):
#     Pr(D_it = 1 | X_it) = logit^{-1}(
#         α_t + β1 log(revenue_it)
#             + β2 I(proxy_ext_it > 0) + β3 asinh(proxy_ext_it)
#             + sector effect
#     )
#
#   Step 2 (Intensive margin, Poisson log-link on emitters only):
#     E[y_it | D_it=1, X_it] = exp(
#         γ_t + δ1 log(revenue_it)
#             + δ2 I(proxy_int_it > 0) + δ3 asinh(proxy_int_it)
#             + sector effect
#     )
#
#   Combined (unconditional) prediction:
#     yhat_raw_it = phat_it * muhat_it
#
#   Calibration (deployment-style, same spirit as ppml.R):
#     Within each sector-year cell (s,t), with target total E_total_{s,t},
#     define denom_train_{s,t} = sum_{train in cell} yhat_raw,
#     and include the held-out firm's own prediction in denom:
#       denom_full = denom_train + yhat_raw_it
#     then:
#       yhat_cal_it = E_total_{s,t} * (yhat_raw_it / denom_full)
#     with fallback equal-split if denom_full == 0.
#
# METRICS (via calc_metrics; NEW NAMING)
#   calc_metrics returns a list including:
#     rmse, nrmse_mean, nrmse_sd, mae, mape, spearman,
#     fpr_nonemitters, tpr_emitters, ppv_precision, f1,
#     predicted_positive_rate, emitter_mass_captured,
#     mean_pred_nonemit, p50_pred_nonemit, p90_pred_nonemit, p95_pred_nonemit, p99_pred_nonemit,
#     mapd_emitters, and TP/FP/TN/FN.
#
#   We map these into stable out$metrics columns (same style as ppml.R):
#     nRMSE, RMSE, MAE, MAPE, MAPD_emitters, FPR_nonemitters,
#     TPR_emitters, PPV_precision, F1, predicted_positive_rate, emitter_mass_captured,
#     mean_yhat_nonemit, med_yhat_nonemit, p90_yhat_nonemit, p95_yhat_nonemit, p99_yhat_nonemit,
#     TP/FP/TN/FN, spearman, plus n and fp_threshold.
###############################################################################

hurdle_lofo <- function(df,
                        sector_year_totals,          # cols: sector_var, year_var, E_total
                        id_var       = "vat",
                        year_var     = "year",
                        sector_var   = "nace2d",
                        y_var        = "emissions",
                        revenue_var  = "revenue",
                        # ---- proxy for Step 1 (extensive margin) ----
                        proxy_df_ext = NULL,         # NULL => no ext proxy; else must have proxy_keys_ext + proxy_var_ext
                        proxy_keys_ext = c("buyer_id","year"),
                        proxy_var_ext  = "fuel_proxy",
                        proxy_tag_ext  = "none",
                        # ---- proxy for Step 2 (intensive margin) ----
                        proxy_df_int = NULL,         # NULL => no int proxy; else must have proxy_keys_int + proxy_var_int
                        proxy_keys_int = c("buyer_id","year"),
                        proxy_var_int  = "fuel_proxy",
                        proxy_tag_int  = "none",
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
  
  if (!exists("calc_metrics")) {
    stop("calc_metrics() not found in scope. Source your utils/calc_metrics.R before calling hurdle_lofo().")
  }
  
  # --------------------------------------------------------------------------
  # Helper: robustly retrieve metric names (keeps compatibility if you ever
  #         compare against older objects).
  # --------------------------------------------------------------------------
  getm <- function(m, candidates, default = NA_real_) {
    for (nm in candidates) {
      if (!is.null(m[[nm]])) return(m[[nm]])
    }
    default
  }
  
  DT <- as.data.table(df)
  
  # -----------------------
  # Basic cleaning / types (match ppml.R)
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
  DT[, emit__   := as.integer(y__ > 0)]
  
  include_proxy_ext <- !is.null(proxy_df_ext)
  include_proxy_int <- !is.null(proxy_df_int)
  
  # -----------------------
  # Attach Step 1 proxy (extensive)
  # -----------------------
  if (include_proxy_ext) {
    PXYE <- as.data.table(proxy_df_ext)
    
    if (!all(c(proxy_keys_ext, proxy_var_ext) %in% names(PXYE))) {
      stop("proxy_df_ext must contain: ", paste(c(proxy_keys_ext, proxy_var_ext), collapse = ", "))
    }
    if ("year" %in% proxy_keys_ext) PXYE[, year := as.integer(year)]
    if (!all(proxy_keys_ext %in% names(DT))) {
      stop("df must contain proxy_keys_ext to merge proxy_df_ext: ", paste(proxy_keys_ext, collapse = ", "))
    }
    
    setkeyv(DT,   proxy_keys_ext)
    setkeyv(PXYE, proxy_keys_ext)
    DT <- PXYE[DT]  # left join into DT rows
    
    if (coalesce_proxy_to_zero) {
      DT[is.na(get(proxy_var_ext)), (proxy_var_ext) := 0]
    }
    
    DT[, I_pos_ext__   := as.integer(get(proxy_var_ext) > 0)]
    DT[, w_asinh_ext__ := asinh(as.numeric(get(proxy_var_ext)))]
  } else {
    DT[, I_pos_ext__   := 0L]
    DT[, w_asinh_ext__ := 0]
  }
  
  # -----------------------
  # Attach Step 2 proxy (intensive)
  # NOTE: we may have already joined PXYE; now join PXYI.
  # -----------------------
  if (include_proxy_int) {
    PXYI <- as.data.table(proxy_df_int)
    
    if (!all(c(proxy_keys_int, proxy_var_int) %in% names(PXYI))) {
      stop("proxy_df_int must contain: ", paste(c(proxy_keys_int, proxy_var_int), collapse = ", "))
    }
    if ("year" %in% proxy_keys_int) PXYI[, year := as.integer(year)]
    if (!all(proxy_keys_int %in% names(DT))) {
      stop("df must contain proxy_keys_int to merge proxy_df_int: ", paste(proxy_keys_int, collapse = ", "))
    }
    
    setkeyv(DT,   proxy_keys_int)
    setkeyv(PXYI, proxy_keys_int)
    DT <- PXYI[DT]  # left join into DT rows
    
    if (coalesce_proxy_to_zero) {
      DT[is.na(get(proxy_var_int)), (proxy_var_int) := 0]
    }
    
    DT[, I_pos_int__   := as.integer(get(proxy_var_int) > 0)]
    DT[, w_asinh_int__ := asinh(as.numeric(get(proxy_var_int)))]
  } else {
    DT[, I_pos_int__   := 0L]
    DT[, w_asinh_int__ := 0]
  }
  
  # fixed levels across folds (avoid new-level issues in prediction)
  all_years   <- sort(unique(DT[["year__"]]))
  all_sectors <- sort(unique(DT[["sector__"]]))
  
  # -----------------------
  # Known totals (sector-year) (match ppml.R)
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
  # Formula builders (match ppml.R)
  # -----------------------
  sector_term <- if (partial_pooling) "s(sector__, bs = 're')" else "sector__"
  
  rhs_ext <- if (include_proxy_ext) {
    paste("x_logrev + I_pos_ext__ + w_asinh_ext__ + year__ +", sector_term)
  } else {
    paste("x_logrev + year__ +", sector_term)
  }
  fml_ext <- as.formula(paste("emit__ ~", rhs_ext))
  
  rhs_int <- if (include_proxy_int) {
    paste("x_logrev + I_pos_int__ + w_asinh_int__ + year__ +", sector_term)
  } else {
    paste("x_logrev + year__ +", sector_term)
  }
  fml_int <- as.formula(paste("y__ ~", rhs_int))
  
  # -----------------------
  # LOFOCV loop
  # -----------------------
  for (i in seq_along(ids)) {
    heldout_id <- ids[i]
    
    if (i %% progress_every == 0) {
      dtm <- difftime(Sys.time(), t0, units = "mins")
      message(sprintf(
        "LOFOCV %d / %d (elapsed %.1f min) | ext=%s | int=%s | sector=%s",
        i, nF, as.numeric(dtm), proxy_tag_ext, proxy_tag_int,
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
    
    # FE fallback for unseen sectors in test fold (exactly as ppml.R)
    if (!partial_pooling) {
      train_sectors_present <- unique(as.character(train$sector__))
      ref_sector <- train_sectors_present[1]
      
      train_df$sector__ <- stats::relevel(train_df$sector__, ref = ref_sector)
      test_df$sector__  <- stats::relevel(test_df$sector__,  ref = ref_sector)
      
      test_sector_chr <- as.character(test_df$sector__)
      unseen_sector   <- !(test_sector_chr %in% train_sectors_present)
      
      if (any(unseen_sector)) {
        test_sector_chr[unseen_sector] <- ref_sector
        test_df$sector__ <- factor(test_sector_chr, levels = levels(train_df$sector__))
      }
    }
    
    # ----- Step 1: logit (emitter probability) -----
    mod_ext <- mgcv::gam(
      formula = fml_ext,
      data    = train_df,
      family  = binomial(link = "logit"),
      method  = "REML"
    )
    
    train$phat_raw <- pmin(pmax(as.numeric(predict(mod_ext, newdata = train_df, type = "response")), 0), 1)
    test$phat_raw  <- pmin(pmax(as.numeric(predict(mod_ext, newdata = test_df,  type = "response")), 0), 1)
    
    # ----- Step 2: poisson on emitters only (intensive mean) -----
    train_emit <- train[emit__ == 1]
    
    if (nrow(train_emit) == 0) {
      train$muhat_int_raw <- 0
      test$muhat_int_raw  <- 0
    } else {
      
      train_emit_df <- as.data.frame(train_emit)
      train_emit_df$year__   <- factor(train_emit_df$year__,   levels = all_years)
      train_emit_df$sector__ <- factor(train_emit_df$sector__, levels = all_sectors)
      
      mod_int <- mgcv::gam(
        formula = fml_int,
        data    = as.data.frame(train_emit_df),
        family  = poisson(link = "log"),
        method  = "REML"
      )
      
      train$muhat_int_raw <- pmax(as.numeric(predict(mod_int, newdata = train_df, type = "response")), 0)
      test$muhat_int_raw  <- pmax(as.numeric(predict(mod_int, newdata = test_df,  type = "response")), 0)
    }
    
    # Combined raw predictions
    train$yhat_raw <- pmax(train$phat_raw * train$muhat_int_raw, 0)
    test$yhat_raw  <- pmax(test$phat_raw  * test$muhat_int_raw,  0)
    
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
      phat_raw,
      muhat_int_raw,
      yhat_raw,
      yhat_cal,
      N_full
    )]
  }
  
  P <- data.table::rbindlist(preds_list, use.names = TRUE, fill = TRUE)
  
  # -----------------------
  # Metrics (standardized; consistent with ppml.R)
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
  
  # Standardize metrics table (raw + calibrated)
  combo_proxy <- paste0("ext=", proxy_tag_ext, "|int=", proxy_tag_int)
  
  metrics <- data.table::rbindlist(list(
    data.table::data.table(
      variant = "raw",
      proxy   = combo_proxy,
      proxy_ext = proxy_tag_ext,
      proxy_int = proxy_tag_int,
      eval_dropped_singletons = eval_dropped_singletons,
      
      n = getm(m_raw, c("n")),
      fp_threshold = getm(m_raw, c("fp_threshold")),
      
      nRMSE = getm(m_raw, c("nrmse_sd", "nrmse_mean", "nRMSE")),
      RMSE  = getm(m_raw, c("rmse", "RMSE")),
      MAE   = getm(m_raw, c("mae")),
      MAPE  = getm(m_raw, c("mape")),
      
      MAPD_emitters   = getm(m_raw, c("mapd_emitters", "MAPD_emitters")),
      FPR_nonemitters = getm(m_raw, c("fpr_nonemitters", "FPR_nonemitters")),
      
      TPR_emitters            = getm(m_raw, c("tpr_emitters", "TPR_emitters")),
      PPV_precision           = getm(m_raw, c("ppv_precision", "PPV_precision")),
      F1                      = getm(m_raw, c("f1", "F1")),
      predicted_positive_rate = getm(m_raw, c("predicted_positive_rate")),
      emitter_mass_captured   = getm(m_raw, c("emitter_mass_captured")),
      
      mean_yhat_nonemit = getm(m_raw, c("mean_pred_nonemit", "mean_yhat_nonemit")),
      med_yhat_nonemit  = getm(m_raw, c("p50_pred_nonemit", "med_yhat_nonemit")),
      p90_yhat_nonemit  = getm(m_raw, c("p90_pred_nonemit", "p90_yhat_nonemit")),
      p95_yhat_nonemit  = getm(m_raw, c("p95_pred_nonemit")),
      p99_yhat_nonemit  = getm(m_raw, c("p99_pred_nonemit")),
      
      TP = getm(m_raw, c("TP")),
      FP = getm(m_raw, c("FP")),
      TN = getm(m_raw, c("TN")),
      FN = getm(m_raw, c("FN")),
      
      spearman = getm(m_raw, c("spearman"))
    ),
    data.table::data.table(
      variant = "calibrated",
      proxy   = combo_proxy,
      proxy_ext = proxy_tag_ext,
      proxy_int = proxy_tag_int,
      eval_dropped_singletons = eval_dropped_singletons,
      
      n = getm(m_cal, c("n")),
      fp_threshold = getm(m_cal, c("fp_threshold")),
      
      nRMSE = getm(m_cal, c("nrmse_sd", "nrmse_mean", "nRMSE")),
      RMSE  = getm(m_cal, c("rmse", "RMSE")),
      MAE   = getm(m_cal, c("mae")),
      MAPE  = getm(m_cal, c("mape")),
      
      MAPD_emitters   = getm(m_cal, c("mapd_emitters", "MAPD_emitters")),
      FPR_nonemitters = getm(m_cal, c("fpr_nonemitters", "FPR_nonemitters")),
      
      TPR_emitters            = getm(m_cal, c("tpr_emitters", "TPR_emitters")),
      PPV_precision           = getm(m_cal, c("ppv_precision", "PPV_precision")),
      F1                      = getm(m_cal, c("f1", "F1")),
      predicted_positive_rate = getm(m_cal, c("predicted_positive_rate")),
      emitter_mass_captured   = getm(m_cal, c("emitter_mass_captured")),
      
      mean_yhat_nonemit = getm(m_cal, c("mean_pred_nonemit", "mean_yhat_nonemit")),
      med_yhat_nonemit  = getm(m_cal, c("p50_pred_nonemit", "med_yhat_nonemit")),
      p90_yhat_nonemit  = getm(m_cal, c("p90_pred_nonemit", "p90_yhat_nonemit")),
      p95_yhat_nonemit  = getm(m_cal, c("p95_pred_nonemit")),
      p99_yhat_nonemit  = getm(m_cal, c("p99_pred_nonemit")),
      
      TP = getm(m_cal, c("TP")),
      FP = getm(m_cal, c("FP")),
      TN = getm(m_cal, c("TN")),
      FN = getm(m_cal, c("FN")),
      
      spearman = getm(m_cal, c("spearman"))
    )
  ), fill = TRUE)
  
  list(
    predictions       = P,
    metrics           = metrics,
    proxy_ext         = proxy_tag_ext,
    proxy_int         = proxy_tag_int,
    include_proxy_ext = include_proxy_ext,
    include_proxy_int = include_proxy_int,
    partial_pooling   = partial_pooling
  )
}
