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

