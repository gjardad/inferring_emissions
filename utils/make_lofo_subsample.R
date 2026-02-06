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
# make_lofo_subsample.R
#
# PURPOSE
#   Construct a reduced-size subsample of a LOFOCV training dataset for
#   debugging, testing, and rapid iteration.
#
#   The subsample is drawn at the *firm level* (the LOFO unit), not at the
#   observation level. All years for each sampled firm are retained. This
#   preserves the validity of leave-one-firm-out cross-validation logic while
#   substantially reducing runtime.
#
# WHAT THIS FUNCTION DOES
#   Given a firm–year panel used for LOFOCV:
#     1) Randomly selects a fraction of firms.
#     2) Keeps all observations (years) for the selected firms.
#     3) Recomputes sector–year aggregate emissions on the subsample, ensuring
#        internal consistency for calibration steps.
#
#   The function returns both:
#     - the subsampled firm–year dataset, and
#     - the corresponding sector–year totals computed on that subsample.
#
# WHY THIS IS NECESSARY
#   Subsampling observations (rows) directly would:
#     - break the LOFO design,
#     - distort the extensive margin,
#     - and produce misleading calibration behavior.
#
#   Subsampling at the firm level preserves:
#     - the cross-sectional structure within sector–year cells,
#     - the meaning of leave-one-firm-out evaluation,
#     - and comparability with full-sample LOFOCV results.
#
# TYPICAL USE CASES
#   - Smoke-testing long LOFOCV pipelines
#   - Debugging proxy merges or formula changes
#   - Verifying calibration logic on a small sample
#
# ARGUMENTS
#   df         : data.frame or data.table containing the LOFO training sample
#   frac       : fraction of firms to retain (0 < frac <= 1)
#   firm_var   : firm identifier used for LOFO (default: "vat")
#   sector_var : sector identifier (default: "nace2d")
#   year_var   : year variable (default: "year")
#   y_var      : emissions variable (default: "emissions")
#   seed       : optional random seed for reproducibility (default: NULL)
#
# RETURNS
#   A list with two elements:
#     - df_sub              : subsampled firm–year dataset
#     - sector_year_totals  : data.table with columns
#                               (sector_var, year_var, E_total)
#
###############################################################################

make_lofo_subsample <- function(df,
                                frac,
                                firm_var   = "vat",
                                sector_var = "nace2d",
                                year_var   = "year",
                                y_var      = "emissions",
                                seed       = NULL) {
  
  stopifnot(is.numeric(frac), frac > 0, frac <= 1)
  
  if (!is.null(seed)) set.seed(seed)
  
  DT <- data.table::as.data.table(df)
  
  # --- sample firms ---
  all_firms <- unique(DT[[firm_var]])
  n_firms   <- length(all_firms)
  
  n_keep <- ceiling(frac * n_firms)
  keep_firms <- sample(all_firms, size = n_keep, replace = FALSE)
  
  # --- subset data ---
  df_sub <- DT[get(firm_var) %in% keep_firms]
  
  # --- recompute sector-year totals on subsample ---
  sector_year_totals <- df_sub[
    , .(E_total = sum(get(y_var), na.rm = TRUE)),
    by = c(sector_var, year_var)
  ]
  
  # --- attributes for bookkeeping ---
  attr(df_sub, "lofo_frac")      <- frac
  attr(df_sub, "n_firms_full")   <- n_firms
  attr(df_sub, "n_firms_sub")    <- n_keep
  attr(df_sub, "subsample_seed") <- seed
  
  list(
    df_sub             = df_sub,
    sector_year_totals = sector_year_totals
  )
}
