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
# hurdle_fast_step1_ext.R
#
# PURPOSE
#   Precompute Step 1 (extensive margin) LOFOCV predictions for ONE proxy:
#     - Fit logit Pr(y>0 | X, proxy_ext) in a leave-one-firm-out loop
#     - Store out-of-fold predicted probabilities phat_raw for ALL firm-years
#
#   This is designed to be reused across MANY intensive proxies:
#     yhat_raw(ext,int) = phat_raw(ext) * muhat_int_raw(int)
#
# WHERE THIS FILE BELONGS
#   code/inferring_emissions/loocv/hurdle_fast_step1_ext.R
#
# INPUTS
#   base           : list
#       Output of prep_hurdle_base_DT().
#   proxy_ext_tbl  : data.frame / data.table
#       Proxy table keyed by proxy_keys with value column proxy_var.
#   partial_pooling: logical
#       TRUE => sector random effect s(sector__, bs='re')
#       FALSE => sector fixed effects with unseen-sector fallback to reference
#
# OUTPUTS
#   data.table with columns:
#     id, year, sector, y_true, phat_raw
#
###############################################################################

precompute_step1_ext <- function(base,
                                 proxy_ext_tbl,
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
  DT <- attach_proxy_cols(DT, proxy_ext_tbl,
                          keys=proxy_keys, proxy_var=proxy_var,
                          prefix="ext", coalesce_to_zero=TRUE)

  all_years   <- base$all_years
  all_sectors <- base$all_sectors

  sector_term <- if (isTRUE(partial_pooling)) "s(sector__, bs='re')" else "sector__"
  fml_ext <- as.formula(paste(
    "emit__ ~ x_logrev + I_pos_ext__ + w_asinh_ext__ + year__ +", sector_term
  ))

  ids <- unique(DT$id__)
  nF  <- length(ids)

  out <- vector("list", nF)
  t0  <- Sys.time()

  for (i in seq_along(ids)) {
    heldout_id <- ids[i]

    if (i %% progress_every == 0) {
      dtm <- difftime(Sys.time(), t0, units = "mins")
      message(sprintf("Step1 ext LOFO %d/%d (elapsed %.1f min)", i, nF, as.numeric(dtm)))
    }

    train <- DT[id__ != heldout_id]
    test  <- DT[id__ == heldout_id]

    train_df <- as.data.frame(train)
    test_df  <- as.data.frame(test)

    # lock factor levels
    train_df$year__   <- factor(train_df$year__,   levels = all_years)
    test_df$year__    <- factor(test_df$year__,    levels = all_years)
    train_df$sector__ <- factor(train_df$sector__, levels = all_sectors)
    test_df$sector__  <- factor(test_df$sector__,  levels = all_sectors)

    # FE fallback for unseen sectors in test fold (matches hurdle.R)
    if (!isTRUE(partial_pooling)) {
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

    mod_ext <- mgcv::gam(
      formula = fml_ext,
      data    = train_df,
      family  = binomial(link = "logit"),
      method  = "REML"
    )

    phat <- pmin(pmax(as.numeric(predict(mod_ext, newdata = test_df, type = "response")), 0), 1)

    out[[i]] <- data.table(
      id     = test$id__,
      year   = test$year__,
      sector = test$sector__,
      y_true = test$y__,
      phat_raw = phat
    )
  }

  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}
