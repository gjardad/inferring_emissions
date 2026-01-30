###############################################################################
# 01_preprocess/15_build_fuels_used_for_energy_generation.R
#
# PURPOSE
#   Prepare fuel lists / shares used for energy generation (aggregate), used for sectoral fuel-mix logic and validations.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/15_build_fuels_used_for_energy_generation.R
###############################################################################

#### HEADER -------

# Which fuels are burned and emit CO_2 in Belgium?

#####################

# -------------
## Setup ------
# -------------

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
library(tidyr)
library(dplyr)

# --------------------
#  Load data ---------
# --------------------

fuels_for_energy_use <- read_csv(paste0(raw_data, "/Eurostat/fuels_for_energy_use_in_belgium_from_energy_balance.csv"))

df <- fuels_for_energy_use %>% 
  mutate(
    siec_code = str_trim(str_extract(siec, "^[^:]+")),
    siec_name = str_trim(str_extract(siec, "(?<=:).*")),
    use = str_trim(str_extract(nrg_bal, "^[^:]+"))
  ) %>% 
  select(siec_code, siec_name, TIME_PERIOD, OBS_VALUE, use) %>% 
  rename(year = TIME_PERIOD, obs_value = OBS_VALUE) %>% 
  # exclude total, bioenergy, renewables, biofuels, electricity, heat, nuclear, and waste
  filter(!substr(siec_code, 1, 2) %in% c("RA", "R5", "TO", "BI", "W6", "H8", "E7", "FE", "N9")) %>% 
  filter(!siec_code %in% c("C0000X0350-0370", "C0350-0370", "O4000XBIO", "O4100_TOT"))

siec_used_for_energy <- df %>%
  group_by(siec_code, siec_name) %>%
  summarise(any_positive = any(obs_value > 0, na.rm = TRUE), .groups = "drop") %>%
  filter(any_positive) %>%
  distinct(siec_code, siec_name)


# convert to HS codes ----

load(paste0(proc_data,"/hs_to_siec_map.RData"))

hs_used_for_energy <- siec_used_for_energy %>% 
  
