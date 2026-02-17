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
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))


## Setup ------


# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ----

library(haven)
df_b2b <- read_dta(paste0(RAW_DATA,"/NBB/B2B_ANO.dta"))

# Save it ----
save(df_b2b, file = paste0(PROC_DATA,"/df_b2b.RData"))
