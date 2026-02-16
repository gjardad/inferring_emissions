###############################################################################
# 01_preprocess/01_build_df_trade.R
#
# PURPOSE
#   Upstream trade backbone builder (load/standardize imports & exports microdata into a canonical df_trade object used by customs/fuel preprocessing).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/01_build_df_trade.R
###############################################################################

#### HEADER -------

## Save customs dataset in .RData format

#####################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/"))
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
df_trade <- read_dta(paste0(RAW_DATA,"/NBB/import_export_ANO.dta"))

# Save it ----
save(df_trade, file = paste0(PROC_DATA,"/df_trade.RData"))
