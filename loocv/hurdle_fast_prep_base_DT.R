###############################################################################
# hurdle_fast_prep_base_DT.R
#
# PURPOSE
#   Prepare a "base" data bundle used by the fast hurdle evaluation pipeline.
#
#   This script defines:
#     (1) prep_hurdle_base_DT():
#           - Cleans and standardizes the firm-year estimation data
#           - Creates internal aliases used throughout the LOFOCV pipeline
#           - Builds sector-year totals (SYT) keys and full-sample cell counts
#           - Stores fixed factor levels (years, sectors) to avoid new-level issues
#
#     (2) attach_proxy_cols():
#           - Left-joins a proxy table onto the base DT
#           - Creates standardized regressors:
#               I_pos_<prefix>__   = 1{proxy > 0}
#               w_asinh_<prefix>__ = asinh(proxy)
#           - Avoids overwriting existing proxy columns by renaming to
#             <proxy_var>_<prefix> (e.g. fuel_proxy_ext, fuel_proxy_int)
#
# WHERE THIS FILE BELONGS
#   code/inferring_emissions/loocv/hurdle_fast_prep_base_DT.R
#
# INPUTS (prep_hurdle_base_DT)
#   df                 : data.frame / data.table
#       Firm-year estimation sample.
#   sector_year_totals : data.frame / data.table
#       Must contain (sector_var, year_var, E_total).
#
# OUTPUTS
#   A list with:
#     $DT         : cleaned data.table with standardized columns:
#                    id__, year__, sector__, y__, emit__, x_logrev
#     $SYT        : sector-year totals keyed by (sector_key, year_key)
#     $full_cellN : full-sample cell sizes keyed by (sector_key, year_key)
#     $all_years  : vector of all years in DT
#     $all_sectors: vector of all sectors in DT
#
###############################################################################

prep_hurdle_base_DT <- function(df,
                                sector_year_totals,
                                id_var="vat",
                                year_var="year",
                                sector_var="nace2d",
                                y_var="emissions",
                                revenue_var="revenue") {
  suppressPackageStartupMessages({
    library(data.table)
  })

  DT <- as.data.table(df)

  # Basic cleaning consistent with hurdle.R / ppml.R
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

  # Regressor aliases used downstream
  DT[, x_logrev := log(pmax(get(revenue_var), 1e-12))]

  # Canonical internal names
  DT[, id__     := get(id_var)]
  DT[, year__   := get(year_var)]
  DT[, sector__ := get(sector_var)]
  DT[, y__      := as.numeric(get(y_var))]
  DT[, emit__   := as.integer(y__ > 0)]

  # Fixed levels across folds (avoid new-level issues in prediction)
  all_years   <- sort(unique(DT$year__))
  all_sectors <- sort(unique(DT$sector__))

  # Known totals (sector-year)
  SYT <- as.data.table(sector_year_totals)
  if (!all(c(sector_var, year_var, "E_total") %in% names(SYT))) {
    stop("sector_year_totals must contain columns: ",
         sector_var, ", ", year_var, ", E_total")
  }

  SYT <- SYT[, .(
    sector_key = as.character(get(sector_var)),
    year_key   = as.integer(get(year_var)),
    E_total    = as.numeric(E_total)
  )]
  setkey(SYT, sector_key, year_key)

  # Full-sample cell counts once (for singleton filtering + equal-split fallback)
  full_cellN <- DT[, .N, by = .(sector__, year__)]
  setnames(full_cellN,
           old = c("sector__", "year__", "N"),
           new = c("sector_key", "year_key", "N_full"))
  setkey(full_cellN, sector_key, year_key)

  list(
    DT          = DT,
    SYT         = SYT,
    full_cellN  = full_cellN,
    all_years   = all_years,
    all_sectors = all_sectors
  )
}

attach_proxy_cols <- function(DT,
                              proxy_df,
                              keys=c("vat","year"),
                              proxy_var="fuel_proxy",
                              prefix=c("ext","int"),
                              coalesce_to_zero=TRUE) {
  suppressPackageStartupMessages({
    library(data.table)
  })
  prefix <- match.arg(prefix)

  P <- as.data.table(proxy_df)
  if (!all(c(keys, proxy_var) %in% names(P))) {
    stop("proxy_df must contain: ", paste(c(keys, proxy_var), collapse = ", "))
  }

  # Ensure year has the right type if present in keys
  if ("year" %in% keys) P[, year := as.integer(year)]

  # Avoid overwriting an existing column named proxy_var in DT
  pcol <- paste0(proxy_var, "_", prefix)
  if (proxy_var %in% names(P)) setnames(P, proxy_var, pcol)

  setkeyv(DT, keys)
  setkeyv(P,  keys)

  # Left join into DT rows
  DT <- P[DT]

  if (coalesce_to_zero) {
    DT[is.na(get(pcol)), (pcol) := 0]
  }

  DT[, paste0("I_pos_", prefix, "__")   := as.integer(get(pcol) > 0)]
  DT[, paste0("w_asinh_", prefix, "__") := asinh(as.numeric(get(pcol)))]

  DT
}
