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
# run_one_proxy_pair.R
#
# PURPOSE
#   Run ONE hurdle LOFOCV model for a given (extensive-proxy, intensive-proxy)
#   pair and return the fitted LOFOCV output object.
#
#   The hurdle model is estimated via hurdle_lofo() (03_models/hurdle.R):
#     - Step 1: logit Pr(y>0 | X, proxy_ext)
#     - Step 2: poisson E[y | y>0, X, proxy_int] on emitters
#     - Combine: yhat_raw = phat * muhat
#     - Calibrate: yhat_cal to match sector-year totals (deployment-style)
#
# INPUTS
#   - df_run: firm-year training dataframe
#   - syt_run: sector-year totals table with columns (sector_var, year_var, E_total)
#   - proxy_ext_tbl: data.table with keys (vat, year) and column fuel_proxy (or proxy_var_ext)
#   - proxy_int_tbl: data.table with keys (vat, year) and column fuel_proxy (or proxy_var_int)
#   - Tags/names for proxies and run settings (sample_tag, partial_pooling, etc.)
#
# OUTPUTS
#   - Returns list with:
#       $out         : output from hurdle_lofo()
#       $metrics_tbl : standardized metrics table (build_metrics_table())
#
###############################################################################

run_one_proxy_pair <- function(df_run,
                               syt_run,
                               proxy_ext_tbl,
                               proxy_int_tbl,
                               proxy_name_ext,
                               proxy_name_int,
                               # ---- model settings ----
                               partial_pooling = TRUE,
                               fallback_equal_split = TRUE,
                               drop_singleton_cells_in_metrics = TRUE,
                               fp_threshold = 0,
                               progress_every = 50,
                               # ---- tagging / logging ----
                               sample_tag = "all",
                               step_tag = "12",   # raw = "12" (step1+2), calibrated = still step1+2+3 via variant
                               # ---- variables ----
                               id_var = "vat",
                               year_var = "year",
                               sector_var = "nace2d",
                               y_var = "emissions",
                               revenue_var = "revenue") {
  
  suppressPackageStartupMessages({
    library(data.table)
  })
  
  # ---- coerce + standardize proxy tables ----
  proxy_ext_tbl <- as.data.table(proxy_ext_tbl)
  proxy_int_tbl <- as.data.table(proxy_int_tbl)
  
  # cached proxies often use buyer_id; your df uses vat
  if ("buyer_id" %in% names(proxy_ext_tbl) && !"vat" %in% names(proxy_ext_tbl)) {
    setnames(proxy_ext_tbl, "buyer_id", "vat")
  }
  if ("buyer_id" %in% names(proxy_int_tbl) && !"vat" %in% names(proxy_int_tbl)) {
    setnames(proxy_int_tbl, "buyer_id", "vat")
  }
  
  # basic checks
  if (!all(c("vat", "year") %in% names(proxy_ext_tbl))) stop("proxy_ext_tbl must contain vat, year.")
  if (!all(c("vat", "year") %in% names(proxy_int_tbl))) stop("proxy_int_tbl must contain vat, year.")
  
  # Ensure the proxy value column is named "fuel_proxy" for both (your hurdle.R defaults)
  if (!("fuel_proxy" %in% names(proxy_ext_tbl))) {
    stop("proxy_ext_tbl must contain column 'fuel_proxy' (or rename before calling).")
  }
  if (!("fuel_proxy" %in% names(proxy_int_tbl))) {
    stop("proxy_int_tbl must contain column 'fuel_proxy' (or rename before calling).")
  }
  
  # ---- run hurdle LOFOCV ----
  out <- hurdle_lofo(
    df = df_run,
    sector_year_totals = syt_run,
    id_var = id_var,
    year_var = year_var,
    sector_var = sector_var,
    y_var = y_var,
    revenue_var = revenue_var,
    
    # Step 1 proxy
    proxy_df_ext   = proxy_ext_tbl,
    proxy_keys_ext = c("vat", "year"),
    proxy_var_ext  = "fuel_proxy",
    proxy_tag_ext  = proxy_name_ext,
    
    # Step 2 proxy
    proxy_df_int   = proxy_int_tbl,
    proxy_keys_int = c("vat", "year"),
    proxy_var_int  = "fuel_proxy",
    proxy_tag_int  = proxy_name_int,
    
    coalesce_proxy_to_zero = TRUE,
    
    partial_pooling = partial_pooling,
    
    fallback_equal_split = fallback_equal_split,
    drop_singleton_cells_in_metrics = drop_singleton_cells_in_metrics,
    fp_threshold = fp_threshold,
    progress_every = progress_every
  )
  
  # ---- log-friendly proxy tag ----
  combo_proxy <- paste0("ext=", proxy_name_ext, "|int=", proxy_name_int)
  
  # ---- standardized metrics table ----
  metrics_tbl <- build_metrics_table(
    out             = out,
    model_family    = "hurdle",
    partial_pooling = if (isTRUE(partial_pooling)) "yes" else "no",
    step_tag        = step_tag,
    sample_tag      = sample_tag,
    proxy_tag       = combo_proxy,
    extra_id_cols   = list(
      proxy_ext  = proxy_name_ext,
      proxy_int  = proxy_name_int,
      n_obs_est  = nrow(df_run),
      n_firms_est = data.table::uniqueN(df_run[[id_var]])
    )
  )
  
  list(out = out, metrics_tbl = metrics_tbl)
}
