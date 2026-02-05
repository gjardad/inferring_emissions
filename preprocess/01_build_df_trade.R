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
df_trade <- read_dta(paste0(raw_data,"/NBB/import_export_ANO.dta"))

# Save it ----
save(df_trade, file = paste0(proc_data,"/df_trade.RData"))
