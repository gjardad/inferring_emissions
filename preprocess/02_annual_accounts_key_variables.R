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
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "config", "paths.R"))


# --------------
## Set up ------
# --------------


library(tidyverse)
library(dplyr)

# ------------
# Load data --
# ------------

load(paste0(PROC_DATA,"/annual_accounts_selected_sample.RData"))

# -----------------------
# Select key variables --
# -----------------------

df_annual_accounts_selected_sample_key_variables <- df_annual_accounts_selected_sample %>% 
  select(vat_ano, year, v_0022_27,turnover_VAT, v_0001023, v_0001003, v_0009800, nace5d) %>% 
  rename(vat = vat_ano, capital = v_0022_27, revenue = turnover_VAT,
         fte = v_0001003, wage_bill = v_0001023, value_added = v_0009800)

# save it
save(df_annual_accounts_selected_sample_key_variables, file = paste0(PROC_DATA,"/annual_accounts_selected_sample_key_variables.RData"))


