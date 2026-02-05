# ============================================================================ #
# run_one_proxy.R
# ============================================================================ #
# PURPOSE
#   Run a single proxy specification end-to-end and return a standardized
<<<<<<< HEAD
#   metrics table produced by build_metrics_table().
#
# DESIGN
#   - This function is MODEL-AGNOSTIC.
#   - All model identity and metadata (model_family, step_tag, sample_tag,
#     proxy_tag, partial_pooling, etc.) MUST be added inside build_metrics_table().
#   - This function only:
#       (i)  reads and standardizes the proxy,
#       (ii) calls a user-supplied model_runner(),
#       (iii) returns the metrics table produced by the model.
#
# CONTRACT WITH model_runner()
#   model_runner() MUST:
#     - run the full LOFOCV exercise,
#     - call build_metrics_table(),
#     - return a list containing at least:
#         $metrics : data.frame / data.table
#
# INPUTS
#   proxy_file : character
#       Path to a proxy_*.rds file.
#
#   df_run, syt_run :
#       Training data and auxiliary objects passed through unchanged.
=======
#   metrics table (one row per proxy, unless your model emits multiple rows).
#
# LOGIC (high-level)
#   1) Read a proxy object from an .rds file.
#      - Accepts either a data frame / data.table directly OR a list with
#        elements like $proxy (table) and $name (proxy label).
#   2) Standardize column names (e.g. buyer_id -> vat) when needed.
#   3) Call a user-provided `model_runner()` which does ALL model-specific work:
#        - merges proxy into df (or uses it however it wants)
#        - fits the model / runs LOFOCV
#        - returns a list containing at least: `metrics` (table)
#   4) Add standard tags/metadata fields (model, step, sample, sectorfe, proxy).
#
# INPUTS
#   proxy_file : character
#       Path to a single proxy .rds file.
#
#   df_run : data.frame / data.table
#       Training sample used for estimation / CV. Must include at least id, year,
#       sector, y, revenue (names configurable via *_var arguments).
#
#   syt_run : data.frame / data.table
#       Sector-year totals (or any auxiliary table your model_runner expects).
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
#
#   model_runner : function
#       Signature:
#         function(df_run, syt_run, proxy_tbl, proxy_name, ...) -> list
<<<<<<< HEAD
#
#   ... :
#       Additional arguments forwarded to model_runner().
#
# OUTPUT
#   data.table
#       A standardized metrics table ready for rbind + append_metrics_log().
#
# NOTES
#   - This function intentionally adds *no* metadata columns.
#   - Any error stops the worker and propagates to the parallel driver.
=======
#       Must return a list with at least:
#         $metrics : data.frame/data.table  (required)
#       Optionally:
#         $out, $predictions, $misc, ...
#
#   model_name : character
#       Label stored in the returned metrics table (e.g. "ppml_step2").
#
#   step_tag / sample_tag / partial_pooling : metadata fields added to metrics.
#
#   id_var/year_var/sector_var/y_var/revenue_var : character
#       Column names used in df_run (passed through to model_runner).
#
#   proxy_keys : character vector
#       Join keys for the proxy table (default c("vat","year")).
#
#   proxy_var : character
#       Name of the proxy variable in the proxy table.
#
#   coalesce_proxy_to_zero : logical
#       Passed to model_runner; worker doesn't implement coalesce itself.
#
# OUTPUTS
#   data.table
#       A standardized metrics table (typically 1 row) with added tags:
#         model, step, sample, sectorfe, fuel_proxy (proxy name)
#
# NOTES
#   - This file is intentionally model-agnostic.
#   - Any errors will stop the worker and surface in the parallel driver.
#     If you prefer "fail soft", wrap the model_runner call in tryCatch().
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
# ============================================================================ #

suppressPackageStartupMessages({
  library(data.table)
})

run_one_proxy <- function(
<<<<<<< HEAD
    proxy_file,
    df_run,
    syt_run,
    model_runner,          # function(df_run, syt_run, proxy_tbl, proxy_name, ...)
    id_var = "vat",
    year_var = "year",
    sector_var = "nace2d",
    y_var = "emissions",
    revenue_var = "revenue",
    proxy_keys = c("vat", "year"),
    proxy_var = "fuel_proxy",
    coalesce_proxy_to_zero = TRUE,
    ...
) {
  
  # -----------------------
  # Read proxy file
  # -----------------------
  obj <- readRDS(proxy_file)
  
  proxy_tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name))  obj$name  else
    tools::file_path_sans_ext(basename(proxy_file))
  
  proxy_tbl <- as.data.table(proxy_tbl)
  
  # Common aliasing: cached proxies sometimes use buyer_id instead of vat
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }
  
  # -----------------------
  # Basic validation
  # -----------------------
  needed <- c(proxy_keys, proxy_var)
  missing_cols <- setdiff(needed, names(proxy_tbl))
  if (length(missing_cols) > 0) {
    stop(
      "Proxy file missing required columns {",
      paste(missing_cols, collapse = ", "),
      "} in: ", proxy_file
    )
  }
  
  # -----------------------
  # Run model (model-specific logic)
  # -----------------------
=======
  proxy_file,
  df_run,
  syt_run,
  model_runner,          # function(df_run, syt_run, proxy_tbl, proxy_name, ...)
  model_name,            # e.g. "ppml_step2", "hurdle_step1_step2"
  step_tag,              # e.g. "2" or "1+2"
  sample_tag,            # e.g. "all" or "subsample"
  partial_pooling,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  proxy_keys = c("vat", "year"),
  proxy_var = "fuel_proxy",
  coalesce_proxy_to_zero = TRUE,
  ...
) {

  # ---- Read proxy file ----
  obj <- readRDS(proxy_file)

  proxy_tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name))  obj$name  else
    tools::file_path_sans_ext(basename(proxy_file))

  proxy_tbl <- as.data.table(proxy_tbl)

  # Common aliasing: cached proxies sometimes use buyer_id rather than vat
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }

  # ---- Basic checks ----
  needed <- c(proxy_keys, proxy_var)
  if (!all(needed %in% names(proxy_tbl))) {
    stop(
      "Proxy file missing required columns {",
      paste(setdiff(needed, names(proxy_tbl)), collapse = ", "),
      "} in: ", proxy_file
    )
  }

  # ---- Run model (model-specific) ----
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
  out <- model_runner(
    df_run   = df_run,
    syt_run  = syt_run,
    proxy_tbl  = proxy_tbl,
    proxy_name = proxy_name,
    id_var = id_var,
    year_var = year_var,
    sector_var = sector_var,
    y_var = y_var,
    revenue_var = revenue_var,
    proxy_keys = proxy_keys,
    proxy_var  = proxy_var,
    coalesce_proxy_to_zero = coalesce_proxy_to_zero,
<<<<<<< HEAD
    ...
  )
  
  if (is.null(out$metrics)) {
    stop("model_runner must return a list containing $metrics")
  }
  
  # -----------------------
  # Return standardized metrics table
  # -----------------------
  return(as.data.table(out$metrics))
=======
    partial_pooling = partial_pooling,
    ...
  )

  if (is.null(out$metrics)) stop("model_runner must return a list with $metrics")

  metrics <- as.data.table(out$metrics)

  # ---- Standard tags (consistent across models) ----
  metrics[, `:=`(
    model    = model_name,
    step     = step_tag,
    sample   = sample_tag,
    sectorfe = if (isTRUE(partial_pooling)) "re" else "fe",
    fuel_proxy = proxy_name
  )]

  return(metrics)
>>>>>>> 24e0491104e47a51e8dcad312848d52d91fcfc3f
}
