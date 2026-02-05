###############################################################################
# 01_preprocess/07_build_b2b_selected_sample.R
#
# PURPOSE
#   Restrict the B2B network to the selected sample (e.g., annual accounts selected firms) and create the firm-to-firm transactions panel used by proxies.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/07_build_b2b_selected_sample.R
###############################################################################

#### HEADER -------

## Code that creates transaction data set (B2B) for random sample of firms

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
library(dplyr)

# Import data ----

load(paste0(proc_data,"/df_b2b.RData"))

load(paste0(proc_data,"/annual_accounts_selected_sample.RData"))

# Create B2B with only transactions between firms in selected sample ----


  # get rid of original sales, only keep corrected number
  df_b2b <- df_b2b %>% 
    select(-sales_ij)

  unique_vat_by_year <- df_annual_accounts_selected_sample %>%
    group_by(year) %>%
    summarise(unique_vat = list(unique(vat_ano))) %>%
    pull(unique_vat)

  i <- 2
  # starts at two because we don't care about first two elements of unique_vat_by_year
  # since df_annual_accounts_selected_sample covers 2000,2001 as well
  
  df_b2b_selected_sample <- c()
  
  for(y in 2002:2022){
    
    i <- i + 1
    
    temp_data <- df_b2b %>%
      filter(year == y) %>% 
      filter(vat_i_ano %in% unique_vat_by_year[[i]],
             vat_j_ano %in% unique_vat_by_year[[i]])
    
    df_b2b_selected_sample <- bind_rows(df_b2b_selected_sample, temp_data)
    
  }
  
  # check if it gives us what we want
  # (already checked and it's all good so commented it out)
  if(FALSE){
  
    # 2002
    unique_vat_b2b_02 <- unique(c(df_b2b_selected_sample[df_b2b_selected_sample$year == 2002, ]$vat_i_ano,
                                  df_b2b_selected_sample[df_b2b_selected_sample$year == 2002, ]$vat_j_ano))
    unique_vat_annual_02 <- unique(df_annual_accounts_selected_sample[df_annual_accounts_selected_sample$year == 2002, ]$vat_ano)
    all_included_02 <- all(unique_vat_b2b_02 %in% unique_vat_annual_02)
  
    # 2012
    unique_vat_b2b_12 <- unique(c(df_b2b_selected_sample[df_b2b_selected_sample$year == 2012, ]$vat_i_ano,
                                  df_b2b_selected_sample[df_b2b_selected_sample$year == 2012, ]$vat_j_ano))
    unique_vat_annual_12 <- unique(df_annual_accounts_selected_sample[df_annual_accounts_selected_sample$year == 2012, ]$vat_ano)
    all_included_12 <- all(unique_vat_b2b_12 %in% unique_vat_annual_12)
    
    # 2022
    unique_vat_b2b_22 <- unique(c(df_b2b_selected_sample[df_b2b_selected_sample$year == 2022, ]$vat_i_ano,
                                  df_b2b_selected_sample[df_b2b_selected_sample$year == 2022, ]$vat_j_ano))
    unique_vat_annual_22 <- unique(df_annual_accounts_selected_sample[df_annual_accounts_selected_sample$year == 2022, ]$vat_ano)
    all_included_22 <- all(unique_vat_b2b_22 %in% unique_vat_annual_22)
  }

# save it ----
save(df_b2b_selected_sample, file = paste0(proc_data,"/b2b_selected_sample.RData"))
