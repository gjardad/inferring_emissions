# ============== BEGIN SETTING UP PATHS ============= #
suppressPackageStartupMessages({
  library(data.table)
})

# ========================
# Define data paths ------
# =========================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

# ===========================
# Define paths for code -----
# ===========================

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}

repo_dir <- paste0(getwd(), "/inferring_emissions")
utils_dir <- file.path(repo_dir, "utils")
loocv_dir <- file.path(repo_dir, "loocv")

#================== END SETTING UP PATHS ================ #

###############################################################################
# 01_preprocess/01_annual_accounts_sample_selection.R
#
# PURPOSE
#   Create the annual accounts working sample (domestic Belgian firms) following De Loecker et al. / Dhyne et al.-style restrictions.
#
# INPUTS
#   - data/processed/df_national_accounts_with_5digits.RData
#
# OUTPUTS
#   - data/processed/annual_accounts_selected_sample.RData
#   - data/processed/firms_in_selected_sample.RData
#   - data/processed/annual_accounts_more_selected_sample.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/01_annual_accounts_sample_selection.R
###############################################################################

#### HEADER -------

## Create sample of domestic Belgian firms from annual accounts

# Follows sample selection similar to De Locker et. al (2014) and Dhyne et al (2021)

#####################

## Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tidyverse)
library(dplyr) 

# Import data ----

load(paste0(proc_data,"/df_national_accounts_with_5digits.RData"))

# Clean data ----

  # how many firms per year?
  obs_per_year_national_accounts <- df_national_accounts %>%
    group_by(year) %>%
    summarize(observations = n()) %>%
    ungroup()
  
# Some descriptive stats -----

  # select firms
  # 1. in the private and non-financial sector;
  # 2. with positive labor costs;
  # 3. with at least 1 FTE employee;
  # 4. with positive output;
  # 5. with tangible assets > 100 euros;
  # 6. positive total assets in at least one year

  # create dummy for positive total assets in at least one year
  dummy_total_assets <- df_national_accounts %>%
    group_by(vat_ano) %>%
    summarize(positive_total_assets = as.integer(any(v_0020_58 > 0, na.rm = TRUE))) %>%
    ungroup()

  df_national_accounts <- df_national_accounts %>%
    left_join(dummy_total_assets, by = "vat_ano")

  # create selected sample
  selected_sample <- df_national_accounts %>% 
    filter(!is.na(v_0001023) & v_0001023 > 0) %>%  # positive labor costs
    filter(!is.na(turnover_VAT) & turnover_VAT > 0) # positive output

  # selected sample following criteria in Dhyne et al (2021) and De Loecker et al (2014)
  more_selected_sample <- df_national_accounts %>% 
    filter(!(startsWith(nace5d, "64") | 
               startsWith(nace5d, "65") | 
               startsWith(nace5d, "66") | 
               startsWith(nace5d, "84"))) %>% # excludes financial and public sector
    filter(!is.na(v_0001023) & v_0001023 > 0) %>%  # positive labor costs
    filter(!is.na(v_0001003) & v_0001003 > 1) %>%  # at least one FTE employee
    filter(!is.na(turnover_VAT) & turnover_VAT > 0) %>% # positive output
    filter(!is.na(v_0022_27) & v_0022_27 > 100) %>% # tangible assets > 100
    filter(positive_total_assets == 1) # positive total assets in at least one year
  
# Define set of vat_ano of firms selected into sample --------
  
firms_in_selected_sample <- selected_sample %>% 
    select(vat_ano) %>% 
    distinct()
  
firms_in_more_selected_sample <- more_selected_sample %>% 
  select(vat_ano) %>% 
  distinct()
  
# save it ----
df_annual_accounts_selected_sample <- selected_sample
save(df_annual_accounts_selected_sample, file = paste0(proc_data,"/annual_accounts_selected_sample.RData"))

save(firms_in_selected_sample, file = paste0(proc_data,"/firms_in_selected_sample.RData"))
  
df_annual_accounts_more_selected_sample <- more_selected_sample
save(df_annual_accounts_more_selected_sample, file = paste0(proc_data,"/annual_accounts_more_selected_sample.RData"))  
