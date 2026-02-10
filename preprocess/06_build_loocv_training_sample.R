###############################################################################
# 01_preprocess/06_build_loocv_training_sample.R
#
# PURPOSE
#   Construct the canonical LOOCV training sample used by downstream models.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/06_build_loocv_training_sample.R
###############################################################################

#### HEADER -------

# This code builds the LOOCV training data set:

# EUETS firms + NACE2d C19/C24 non-ETS firms

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

library(dplyr)

# ===================
# Load data ---------
# ===================

load(paste0(proc_data, "/firm_year_belgian_euets.RData"))

load(paste0(proc_data, "/annual_accounts_selected_sample_key_variables.RData"))

euets <- firm_year_belgian_euets %>% 
  select(vat, year, emissions) %>% 
  mutate(euets = 1)

loocv_training_sample <- df_annual_accounts_selected_sample_key_variables %>% 
  left_join(euets, by = c("vat", "year")) %>% 
  mutate(nace2d = substr(nace5d, 1, 2),
         euets = coalesce(euets, 0)) %>% 
  filter(euets == 1 | nace2d %in% c("19", "24"),
         year >= 2005) %>% 
  mutate(emissions = if_else(is.na(emissions) & nace2d %in% c("19", "24"), 0, emissions))

# save it
save(loocv_training_sample, file = paste0(proc_data, "/loocv_training_sample.RData"))





