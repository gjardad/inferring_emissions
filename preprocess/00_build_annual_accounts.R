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

## Setup ------
rm(list = ls())

if(Sys.info()[["user"]] =="JARDANG"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ----

library(haven)
df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

# Clean data ----

  # include "0" in front of nace5d
  library(stringr)
  df_national_accounts <- df_national_accounts %>% 
    mutate(nace5d = str_pad(nace5d, width = 5, pad = "0"))
  
# Save it ----
save(df_national_accounts, file = paste0(proc_data,"/df_national_accounts_with_5digits.RData"))  
  
