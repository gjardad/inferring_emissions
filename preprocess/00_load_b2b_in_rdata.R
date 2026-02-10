###############################################################################
# 01_preprocess/00_load_b2b_in_rdata.R
#
# PURPOSE
#   Load/convert the B2B data stored in .RData (or raw form) into a standardized processed object used downstream.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/00_load_b2b_in_rdata.R
###############################################################################

#### HEADER -------

## Save B2B dataset in .RData format

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


## Setup ------


# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ----

library(haven)
df_b2b <- read_dta(paste0(RAW_DATA,"/NBB/B2B_ANO.dta"))

# Save it ----
save(df_b2b, file = paste0(PROC_DATA,"/df_b2b.RData"))
