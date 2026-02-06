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
# 01_preprocess/04_build_firm_year_domestic_input_costs.R
#
# PURPOSE
#   Build firm-year domestic input costs from annual accounts.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/04_build_firm_year_domestic_input_costs.R
###############################################################################

#### HEADER -------

## This code creates data set with firm-year total domestic input costs

#####################


# Setup ------
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
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------
load(paste0(proc_data, "/b2b_selected_sample.RData"))
df_b2b <- df_b2b_selected_sample

# Clean data ------

firm_year_domestic_input_cost <- df_b2b %>%
  group_by(vat_j_ano, year) %>% # Group by j and year
  summarize(input_cost = sum(corr_sales_ij, na.rm = TRUE)) %>% 
  rename(vat = vat_j_ano)

# Save it -----
save(firm_year_domestic_input_cost, file = paste0(proc_data,"/firm_year_domestic_input_cost.RData"))  

