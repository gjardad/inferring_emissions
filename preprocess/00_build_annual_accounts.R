###############################################################################
# 01_preprocess/00_build_annual_accounts.R
#
# PURPOSE
#   Upstream annual-accounts backbone builder (standardize NACE to 5 digits, harmonize identifiers, and save canonical annual-accounts base tables used throughout preprocessing).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/00_build_annual_accounts.R
###############################################################################

#### HEADER -------

## Introduce "0" in front of NACE codes so that all NACE codes contain 5 digits

#####################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "config", "paths.R"))


## Setup ------


# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ----

library(haven)
df_national_accounts <- read_dta(paste0(RAW_DATA,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

# Clean data ----

  # include "0" in front of nace5d
  library(stringr)
  df_national_accounts <- df_national_accounts %>% 
    mutate(nace5d = str_pad(nace5d, width = 5, pad = "0"))
  
# Save it ----
save(df_national_accounts, file = paste0(PROC_DATA,"/df_national_accounts_with_5digits.RData"))  
  
