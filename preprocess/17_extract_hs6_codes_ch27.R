###############################################################################
# 01_preprocess/17_extract_hs6_codes_ch27.R
#
# PURPOSE
#   Extract HS6 codes under Chapter 27 used to define fuel goods in customs data.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/17_extract_hs6_codes_ch27.R
###############################################################################

#### HEADER -------

## Creates data set with HS 6-digit codes and descriptions of all goods in chapter 27

#####################

# ==============
# Set up -------
# ==============

rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

library(readxl)

# ==============
# Import data --
# ==============

hs_codes_for_ch27 <- read_xlsx(
  paste0(raw_data, "/Correspondences_and_dictionaries/HSCodeandDescription.xlsx"),
  sheet = 1
) %>% 
  filter(substr(Code, 1, 2) == "27") %>% 
  select(-c(Classification)) %>% 
  rename(hs_code = 1, description = 2, parent_code = 3, level = 4, is_basic_leve = 5)

# save it
save(hs_codes_for_ch27, file = paste0(proc_data, "/hs_code_for_ch27_goods.RData"))
