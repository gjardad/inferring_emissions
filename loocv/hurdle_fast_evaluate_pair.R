###############################################################################
# hurdle_fast_evaluate_pair.R
#
# PURPOSE
#   Fast evaluation for a given (extensive, intensive) proxy pair using
#   precomputed LOFOCV predictions:
#     - Inputs:
#         phat_dt(ext): id, year, sector, y_true, phat_raw
#         muhat_dt(int): id, year, muhat_int_raw
#     - Combine:
#         yhat_raw = phat_raw * muhat_int_raw
#     - Calibrate (deployment-style) WITHOUT recomputing fold objects:
#         denom_full(sector,year) = sum_over_all_firms_in_cell(yhat_raw)
#         yhat_cal = E_total * yhat_raw / denom_full
#         fallback equal-split if denom_full == 0
#
#   This is algebraically equivalent to the LOFO "denom_train + yhat_test"
#   computation in hurdle.R, but avoids recomputing cell sums in every fold.
#
# WHERE THIS FILE BELONGS
#   code/inferring_emissions/loocv/hurdle_fast_evaluate_pair.R
#
# INPUTS
#   base         : list
#       Output of prep_hurdle_base_DT() containing SYT and full_cellN.
#   phat_dt      : data.table
#   muhat_dt     : data.table
#   proxy_tag_ext, proxy_tag_int : character
#       Names used for labeling outputs.
#
# OUTPUTS
#   A list with:
#     $predictions : data.table with yhat_raw and yhat_cal
#     $metrics     : standardized metrics table (raw + calibrated)
#     $proxy_ext / $proxy_int : tags for provenance
#
###############################################################################

evaluate_pair_fast <- function(base,
                               phat_dt,
                               muhat_dt,
                               proxy_tag_ext,
                               proxy_tag_int,
                               fp_threshold=0,
                               drop_singleton_cells_in_metrics=TRUE,
                               fallback_equal_split=TRUE) {
  suppressPackageStartupMessages({
    library(data.table)
  })

  if (!exists("calc_metrics")) {
    stop("calc_metrics() not found in scope. Source utils/calc_metrics.R before calling.")
  }

  # helper: robust metric getter (matches hurdle.R style)
  getm <- function(m, candidates, default = NA_real_) {
    for (nm in candidates) {
      if (!is.null(m[[nm]])) return(m[[nm]])
    }
    default
  }

  P <- merge(as.data.table(phat_dt), as.data.table(muhat_dt), by = c("id","year"), all.x = TRUE)
  P[is.na(muhat_int_raw), muhat_int_raw := 0]
  P[, yhat_raw := pmax(phat_raw * muhat_int_raw, 0)]

  # Keys for totals
  P[, sector_key := as.character(sector)]
  P[, year_key   := as.integer(year)]

  # denom_full == sum(yhat_raw) within cell (sector-year)
  cell_sum <- P[, .(denom_full = sum(yhat_raw, na.rm = TRUE)), by = .(sector_key, year_key)]
  setkey(cell_sum, sector_key, year_key)

  # merge totals + cell sizes
  P <- merge(P, cell_sum,       by = c("sector_key","year_key"), all.x = TRUE)
  P <- merge(P, base$SYT,       by = c("sector_key","year_key"), all.x = TRUE)
  P <- merge(P, base$full_cellN,by = c("sector_key","year_key"), all.x = TRUE)

  P[is.na(denom_full), denom_full := 0]
  P[is.na(N_full),     N_full := 1L]

  # Calibration
  P[, yhat_cal := NA_real_]
  P[is.finite(E_total) & E_total == 0, yhat_cal := 0]
  P[is.finite(E_total) & E_total > 0 & denom_full > 0,
    yhat_cal := E_total * (yhat_raw / denom_full)]

  if (isTRUE(fallback_equal_split)) {
    P[is.finite(E_total) & E_total > 0 &
        (is.na(yhat_cal) | !is.finite(yhat_cal)) & N_full > 0,
      yhat_cal := E_total / N_full]
  }

  # Metrics
  eval_dropped_singletons <- isTRUE(drop_singleton_cells_in_metrics)

  Pm_raw <- P
  Pm_cal <- P
  if (eval_dropped_singletons) {
    Pm_raw <- P[N_full > 1]
    Pm_cal <- P[N_full > 1]
  }

  m_raw <- calc_metrics(Pm_raw$y_true, Pm_raw$yhat_raw, fp_threshold = fp_threshold)
  m_cal <- calc_metrics(Pm_cal$y_true, Pm_cal$yhat_cal, fp_threshold = fp_threshold)

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
    predictions = P[],
    metrics     = metrics,
    proxy_ext   = proxy_tag_ext,
    proxy_int   = proxy_tag_int
  )
}
