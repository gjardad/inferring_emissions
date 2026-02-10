###############################################################################
# 01_preprocess/22_build_oil_price_series.R
#
# PURPOSE
#   Create annual Brent oil price series (and currency conversion if included) used for customs implied-price diagnostics / quantity imputation.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/22_build_oil_price_series.R
###############################################################################

#### HEADER -------

## Code creates data set with yearly Brent oil prices

#####################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  DATA_DIR <- "C:/Users/jota_/Documents/NBB_projects/data"
  REPO_DIR <- "C:/Users/jota_/Documents/NBB_projects/inferring_emissions"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

UTILS_DIR <- file.path(REPO_DIR, "utils")
LOOCV_DIR <- file.path(REPO_DIR, "loocv")

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}


# ------------
# Set up -----
# ------------


library(tibble)

# -------------
# Load data ---
# -------------

oil_quarterly <- read_csv(paste0(RAW_DATA, "/crude_oil_bfo_m2_europe_fob_euro_per_barrel_quarterly.csv")) %>% 
  rename(eur_per_bbl = 3)

library(lubridate)
oil_price <- oil_quarterly %>%
  mutate(
    date = as.Date(DATE),
    year = lubridate::year(date)
  ) %>%
  group_by(year) %>%
  summarise(
    n_quarters = sum(!is.na(eur_per_bbl)),
    eur_per_bbl = mean(eur_per_bbl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

# save it
save(oil_price, file = paste0(PROC_DATA,"/oil_price.RData"))
