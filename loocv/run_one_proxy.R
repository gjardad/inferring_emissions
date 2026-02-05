# ============================================================================ #
# run_one_proxy.R
# ============================================================================ #
# PURPOSE
#   Run a single proxy specification end-to-end and return a standardized
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
#
#   model_runner : function
#       Signature:
#         function(df_run, syt_run, proxy_tbl, proxy_name, ...) -> list
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
# ============================================================================ #

suppressPackageStartupMessages({
  library(data.table)
})

run_one_proxy <- function(
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
    ...
  )
  
  if (is.null(out$metrics)) {
    stop("model_runner must return a list containing $metrics")
  }
  
  # -----------------------
  # Return standardized metrics table
  # -----------------------
  return(as.data.table(out$metrics))
}
