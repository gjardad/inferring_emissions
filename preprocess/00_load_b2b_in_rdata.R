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
df_b2b <- read_dta(paste0(raw_data,"/NBB/B2B_ANO.dta"))

# Save it ----
save(df_b2b, file = paste0(proc_data,"/df_b2b.RData"))
