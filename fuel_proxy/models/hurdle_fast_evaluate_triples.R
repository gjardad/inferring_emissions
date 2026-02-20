###############################################################################
# hurdle_fast_evaluate_triples.R
#
# PURPOSE
#   Evaluate the hurdle model for a specific set of triples:
#     (proxy_tag_ext, threshold_value, proxy_tag_int)
#   using cached step-1 phat and step-2 muhat objects.
#
# INPUTS
#   base      : list from prep_hurdle_base_DT()
#   triples   : data.table with columns proxy_tag_ext, threshold_value, proxy_tag_int
#   phat_map  : env mapping proxy_tag_ext -> cached phat .rds path
#   muhat_map : env mapping proxy_tag_int -> cached muhat .rds path
#
# OUTPUT
#   data.table with metrics per triple and per variant (raw, calibrated).
###############################################################################

evaluate_hurdle_triples <- function(base,
                                    triples,
                                    phat_map,
                                    muhat_map,
                                    drop_singleton_cells_in_metrics = TRUE,
                                    fallback_equal_split = TRUE,
                                    fp_threshold_metrics = 0,
                                    progress_every = 50) {
  suppressPackageStartupMessages({
    library(data.table)
  })

  if (!is.list(base) || is.null(base$DT) || is.null(base$SYT) || is.null(base$full_cellN)) {
    stop("base must be output from prep_hurdle_base_DT() with DT, SYT, full_cellN")
  }

  T <- as.data.table(triples)
  req_cols <- c("proxy_tag_ext", "threshold_value", "proxy_tag_int")
  if (!all(req_cols %in% names(T))) {
    stop("triples must include columns: ", paste(req_cols, collapse = ", "))
  }
  if (nrow(T) == 0) return(data.table())

  DT0 <- data.table::copy(base$DT)
  DT0[, id := id__]
  DT0[, year := year__]

  SYT <- data.table::copy(base$SYT)
  full_cellN <- data.table::copy(base$full_cellN)

  calibrate_cell <- function(dt_in, yhat_col) {
    tmp <- data.table::copy(dt_in)
    tmp[, sector_key := as.character(sector__)]
    tmp[, year_key   := as.integer(year)]

    denom <- tmp[, .(denom_full = sum(get(yhat_col), na.rm = TRUE), n_full = .N),
                 by = .(sector_key, year_key)]
    denom <- merge(denom, SYT, by = c("sector_key", "year_key"), all.x = TRUE)

    tmp <- merge(tmp, denom, by = c("sector_key", "year_key"), all.x = TRUE)

    tmp[is.na(denom_full), denom_full := 0]
    tmp[is.na(n_full), n_full := 1L]

    tmp[, yhat_cal := NA_real_]
    tmp[is.finite(E_total) & E_total == 0, yhat_cal := 0]
    tmp[is.finite(E_total) & E_total > 0 & denom_full > 0,
        yhat_cal := E_total * (get(yhat_col) / denom_full)]

    if (fallback_equal_split) {
      tmp[is.finite(E_total) & E_total > 0 &
            (is.na(yhat_cal) | !is.finite(yhat_cal)) & n_full > 0,
          yhat_cal := E_total / n_full]
    }

    tmp[, .(
      id, year, sector__, y__ = y__, emit__ = emit__,
      N_full = N_full,
      yhat_raw = get(yhat_col),
      yhat_cal = yhat_cal
    )]
  }

  eval_one <- function(proxy_tag_ext, threshold_value, proxy_tag_int) {
    phat_obj <- readRDS(get(proxy_tag_ext, envir = phat_map))
    muhat_obj <- readRDS(get(proxy_tag_int, envir = muhat_map))

    ph <- data.table::as.data.table(phat_obj$phat_dt)[, .(id, year, phat_raw)]
    mu <- data.table::as.data.table(muhat_obj$muhat_dt)[, .(id, year, muhat_int_raw)]

    dt <- merge(
      DT0[, .(id, year, sector__ = sector__, y__ = y__, emit__ = emit__)],
      ph,
      by = c("id", "year"),
      all.x = TRUE
    )
    dt <- merge(dt, mu, by = c("id", "year"), all.x = TRUE)
    dt[is.na(phat_raw), phat_raw := 0]
    dt[is.na(muhat_int_raw), muhat_int_raw := 0]

    dt[, sector_key := as.character(sector__)]
    dt[, year_key   := as.integer(year)]

    dt <- merge(
      dt,
      full_cellN[, .(sector_key, year_key, N_full)],
      by = c("sector_key", "year_key"),
      all.x = TRUE
    )
    dt[is.na(N_full), N_full := 1L]

    dt[, yhat_hard := pmax(as.numeric(phat_raw > threshold_value) * muhat_int_raw, 0)]
    pred_hard <- calibrate_cell(dt, "yhat_hard")

    Pm_raw <- pred_hard
    Pm_cal <- pred_hard
    if (isTRUE(drop_singleton_cells_in_metrics)) {
      Pm_raw <- Pm_raw[N_full > 1]
      Pm_cal <- Pm_cal[N_full > 1]
    }

    m_raw <- calc_metrics(Pm_raw$y__, Pm_raw$yhat_raw, fp_threshold = fp_threshold_metrics)
    m_cal <- calc_metrics(Pm_cal$y__, Pm_cal$yhat_cal, fp_threshold = fp_threshold_metrics)

    data.table::rbindlist(list(
      data.table::data.table(
        variant = "raw",
        threshold_type = "hard",
        threshold_value = as.numeric(threshold_value),
        RMSE = m_raw[["rmse"]],
        nRMSE = m_raw[["nrmse_sd"]],
        spearman = m_raw[["spearman"]],
        MAPD_emitters = m_raw[["mapd_emitters"]],
        FPR_nonemitters = m_raw[["fpr_nonemitters"]],
        TPR_emitters = m_raw[["tpr_emitters"]],
        PPV_precision = m_raw[["ppv_precision"]],
        F1 = m_raw[["f1"]],
        emitter_mass_captured = m_raw[["emitter_mass_captured"]],
        n = m_raw[["n"]],
        TP = m_raw[["TP"]],
        FP = m_raw[["FP"]],
        TN = m_raw[["TN"]],
        FN = m_raw[["FN"]]
      ),
      data.table::data.table(
        variant = "calibrated",
        threshold_type = "hard",
        threshold_value = as.numeric(threshold_value),
        RMSE = m_cal[["rmse"]],
        nRMSE = m_cal[["nrmse_sd"]],
        spearman = m_cal[["spearman"]],
        MAPD_emitters = m_cal[["mapd_emitters"]],
        FPR_nonemitters = m_cal[["fpr_nonemitters"]],
        TPR_emitters = m_cal[["tpr_emitters"]],
        PPV_precision = m_cal[["ppv_precision"]],
        F1 = m_cal[["f1"]],
        emitter_mass_captured = m_cal[["emitter_mass_captured"]],
        n = m_cal[["n"]],
        TP = m_cal[["TP"]],
        FP = m_cal[["FP"]],
        TN = m_cal[["TN"]],
        FN = m_cal[["FN"]]
      )
    ), fill = TRUE)[, `:=`(proxy_tag_ext = proxy_tag_ext, proxy_tag_int = proxy_tag_int)]
  }

  out <- vector("list", nrow(T))
  for (i in seq_len(nrow(T))) {
    if (i %% progress_every == 0) {
      message(sprintf("[hurdle-triples] %d / %d", i, nrow(T)))
    }
    out[[i]] <- eval_one(
      proxy_tag_ext = T$proxy_tag_ext[[i]],
      threshold_value = T$threshold_value[[i]],
      proxy_tag_int = T$proxy_tag_int[[i]]
    )
  }

  data.table::rbindlist(out, fill = TRUE)
}
