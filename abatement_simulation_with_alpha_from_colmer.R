#### HEADER -------

## Abatement effects from carbon taxation using common alpha,
# calibrated to make sense of evidence in Colmer et al (2025)

# This code simulates emission abatement from carbon taxation following 
# step-by-step in macro lunch of May, 7th

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

# Import data ------

load(paste0(proc_data,"/dlogpz_shocks_that_imply_hicp_energy_increases_in_diego_jmp.RData"))
load(paste0(proc_data, "/firm_year_obs_and_imputed_emissions_using_firm_size.RData"))
load(paste0(proc_data,"/vats_as_ordered_in_io_matrix.RData"))
load(paste0(proc_data, "/agg_emissions_by_nace_year_using_shares_crf.RData"))

alpha <- -0.07526882

# Compute aggregate stationary emissions by year -----

total_emissions <- agg_emissions_by_nace_year_using_shares_crf %>%
  filter(!(str_starts(nace, "[IJKLMNQRS]") & str_length(nace) > 1)) %>% 
  group_by(year) %>% 
  summarise(total_stationary_emissions = sum(agg_emissions, na.rm=T))

# Compute abatement ------

  # choose targeting vector; this needs to be list of VATs of targeted firms
  load(paste0(proc_data,"/vat_ids_and_indices_of_euets_firms.RData"))
  euets_2012 <- euets_vat_ids_list[[8]]$vat
  targeted_vat_ids <- euets_2012
  
  abatement_list <- list()
  i <- 0

  for(y in 2008:2022){
  
  i <- i + 1
  
  # find indices of targeted firms in io_matrix
  firms_ids <- vats_as_ordered_in_io_matrix[[i+3]]
  
  targeted_indices_in_io_matrix <- match(targeted_vat_ids, firms_ids)
  
  # compute emissions
  firm_emissions <- firm_year_balance_sheet_and_emissions_using_firm_size %>% 
    filter(year == y) %>% 
    rename(vat = vat_ano) %>% 
    select(vat, emissions)
  
  targeted_indices_in_firm_year_balance_sheet <- which(firm_emissions$vat %in% targeted_vat_ids)
  
  emissions_unordered <- firm_emissions$emissions
  
  targeted_ordered_emissions <- rep(0, length(firms_ids))
  targeted_ordered_emissions[targeted_indices_in_io_matrix] <- emissions_unordered[targeted_indices_in_firm_year_balance_sheet]
  
  dlogpz <- dlogpz_list[[i+3]]
  
  # total stationary emissions
  Z <- total_emissions %>% 
    filter(year == y) %>% 
    pull(total_stationary_emissions)
  Z <- Z*10^3 # emissions in tons
  
  # compute abatement
  abatement_list[[i+3]] <- 1/Z * dlogpz * alpha/(1-alpha) * sum(targeted_ordered_emissions)
  
}