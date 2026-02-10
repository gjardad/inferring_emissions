###############################################################################
# 01_preprocess/02_annual_accounts_key_variables.R
#
# PURPOSE
#   Create/clean the annual accounts key variables dataset used throughout the pipeline (VAT-year panel variables, NACE, revenue, costs, etc.).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/02_annual_accounts_key_variables.R
###############################################################################

#### HEADER -------

## Create sample of domestic Belgian firms from annual accounts

# Follows sample selection similar to De Locker et. al (2014) and Dhyne et al (2021)

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



# --------------
## Set up ------
# --------------

rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

library(tidyverse)
library(dplyr)

# ------------
# Load data --
# ------------

load(paste0(proc_data,"/annual_accounts_selected_sample.RData"))

# -----------------------
# Select key variables --
# -----------------------

df_annual_accounts_selected_sample_key_variables <- df_annual_accounts_selected_sample %>% 
  select(vat_ano, year, v_0022_27,turnover_VAT, v_0001023, v_0001003, v_0009800, nace5d) %>% 
  rename(vat = vat_ano, capital = v_0022_27, revenue = turnover_VAT,
         fte = v_0001003, wage_bill = v_0001023, value_added = v_0009800)

# save it
save(df_annual_accounts_selected_sample_key_variables, file = paste0(proc_data,"/annual_accounts_selected_sample_key_variables.RData"))



