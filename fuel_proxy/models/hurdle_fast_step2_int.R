###############################################################################
# hurdle_fast_step2_int.R
#
# PURPOSE
#   Precompute Step 2 (intensive margin) LOFOCV predictions for ONE proxy:
#     - Fit poisson E[y | y>0, X, proxy_int] on TRUE emitters in each LOFO fold
#     - Store out-of-fold predicted means muhat_int_raw for ALL firm-years
#
#   This is designed to be reused across MANY extensive proxies:
#     yhat_raw(ext,int) = phat_raw(ext) * muhat_int_raw(int)
#
# WHERE THIS FILE BELONGS
#   code/inferring_emissions/loocv/hurdle_fast_step2_int.R
#
# INPUTS
#   base           : list
#       Output of prep_hurdle_base_DT().
#   proxy_int_tbl  : data.frame / data.table
#       Proxy table keyed by proxy_keys with value column proxy_var.
#   partial_pooling: logical
#       TRUE => sector random effect s(sector__, bs='re')
#       FALSE => sector fixed effects with unseen-sector fallback to reference
#
# OUTPUTS
#   data.table with columns:
#     id, year, muhat_int_raw
#
###############################################################################

precompute_step2_int <- function(base,
                                 proxy_int_tbl,
                                 proxy_keys=c("vat","year"),
                                 proxy_var="fuel_proxy",
                                 partial_pooling=TRUE,
                                 progress_every=50) {
  suppressPackageStartupMessages({
    library(data.table)
    library(mgcv)
  })

  if (!exists("attach_proxy_cols")) {
    stop("attach_proxy_cols() not found. Source hurdle_fast_prep_base_DT.R first.")
  }

  DT <- copy(base$DT)
  DT <- attach_proxy_cols(DT, proxy_int_tbl,
                          keys=proxy_keys, proxy_var=proxy_var,
                          prefix="int", coalesce_to_zero=TRUE)

  all_years   <- base$all_years
  all_sectors <- base$all_sectors

  sector_term <- if (isTRUE(partial_pooling)) "s(sector__, bs='re')" else "sector__"
  fml_int <- as.formula(paste(
    "y__ ~ x_logrev + I_pos_int__ + w_asinh_int__ + year__ +", sector_term
  ))

  ids <- unique(DT$id__)
  nF  <- length(ids)

  out <- vector("list", nF)
  t0  <- Sys.time()

  for (i in seq_along(ids)) {
    heldout_id <- ids[i]

    if (i %% progress_every == 0) {
      dtm <- difftime(Sys.time(), t0, units = "mins")
      message(sprintf("Step2 int LOFO %d/%d (elapsed %.1f min)", i, nF, as.numeric(dtm)))
    }

    train <- DT[id__ != heldout_id]
    test  <- DT[id__ == heldout_id]

    train_emit <- train[emit__ == 1]

    if (nrow(train_emit) == 0) {
      muhat <- rep(0, nrow(test))
    } else {
      train_emit_df <- as.data.frame(train_emit)
      test_df       <- as.data.frame(test)

      # lock factor levels
      train_emit_df$year__   <- factor(train_emit_df$year__,   levels = all_years)
      test_df$year__         <- factor(test_df$year__,         levels = all_years)
      train_emit_df$sector__ <- factor(train_emit_df$sector__, levels = all_sectors)
      test_df$sector__       <- factor(test_df$sector__,       levels = all_sectors)

      # FE fallback for unseen sectors in test fold (matches hurdle.R)
      if (!isTRUE(partial_pooling)) {
        train_sectors_present <- unique(as.character(train_emit$sector__))
        ref_sector <- train_sectors_present[1]

        train_emit_df$sector__ <- stats::relevel(train_emit_df$sector__, ref = ref_sector)
        test_df$sector__       <- stats::relevel(test_df$sector__,       ref = ref_sector)

        test_sector_chr <- as.character(test_df$sector__)
        unseen_sector   <- !(test_sector_chr %in% train_sectors_present)
        if (any(unseen_sector)) {
          test_sector_chr[unseen_sector] <- ref_sector
          test_df$sector__ <- factor(test_sector_chr, levels = levels(train_emit_df$sector__))
        }
      }

      mod_int <- mgcv::gam(
        formula = fml_int,
        data    = train_emit_df,
        family  = poisson(link = "log"),
        method  = "REML"
      )

      muhat <- pmax(as.numeric(predict(mod_int, newdata = test_df, type = "response")), 0)
    }

    out[[i]] <- data.table(
      id = test$id__,
      year = test$year__,
      muhat_int_raw = muhat
    )
  }

  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}
