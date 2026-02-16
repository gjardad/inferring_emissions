if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
  OUTPUT_DIR <- "X:/Documents/JARDANG/inferring_emissions_output"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  DATA_DIR <- "C:/Users/jota_/Documents/NBB_data/"
  REPO_DIR <- normalizePath(dirname(sys.frame(sys.nframe())$ofile), winslash = "/")
  OUTPUT_DIR <- "C:/Users/jota_/Documents/inferring_emissions_output"
} else {
  stop("Define directories for this user.")
}


PROC_DATA <- file.path(DATA_DIR, "processed")
RAW_DATA <- file.path(DATA_DIR, "raw")
INT_DATA <- file.path(DATA_DIR, "intermediate")

UTILS_DIR <- file.path(REPO_DIR, "fuel_proxy", "utils")
LOOCV_DIR <- file.path(REPO_DIR, "fuel_proxy", "models")

CACHE_DIR <- file.path(INT_DATA, "cache")
METRICS_PATH_RDS <- file.path(OUTPUT_DIR, "model_performance_metrics.rds")
METRICS_PATH_CSV <- file.path(OUTPUT_DIR, "model_performance_metrics.csv")

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = FALSE)
}
