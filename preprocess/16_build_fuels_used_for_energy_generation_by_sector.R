###############################################################################
# 01_preprocess/16_build_fuels_used_for_energy_generation_by_sector.R
#
# PURPOSE
#   Prepare sector-level fuel mix composition used for validations and mapping logic (fuel shares by sector).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/16_build_fuels_used_for_energy_generation_by_sector.R
###############################################################################

#### HEADER -------

# Which sectors use which fuels when conducting emission-relevant activities in Belgium?

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


# -------------
## Setup ------
# -------------


library(tidyverse)
library(tidyr)
library(dplyr)

# -----------------------
# Load and clean data ---
# -----------------------

df <- read_csv(paste0(RAW_DATA, "/Eurostat/fuel_qty_by_year_energy_use_from_energy_balance.csv"))

df_fuel_use_energy_balances <- df %>% 
  mutate(
    siec_code = str_trim(str_extract(siec, "^[^:]+")),
    siec_name = str_trim(str_extract(siec, "(?<=:).*")),
    sector_code = str_trim(str_extract(nrg_bal, "^[^:]+")),
    sector_name =  str_trim(str_extract(nrg_bal, "(?<=:).*"))
  ) %>% 
  select(siec_code, siec_name, TIME_PERIOD, OBS_VALUE, sector_code, sector_name) %>% 
  rename(year = TIME_PERIOD, obs_value = OBS_VALUE) %>% 
  # exclude total, bioenergy, renewables, biofuels, electricity, heat, nuclear, and waste
  filter(!substr(siec_code, 1, 2) %in% c("RA", "R5", "TO", "BI", "W6", "H8", "E7", "FE", "N9")) %>% 
  filter(!siec_code %in% c("C0000X0350-0370", "C0350-0370", "O4000XBIO", "O4100_TOT")) %>% 
  # exclude HH and transportation sector
  filter(
    !str_detect(sector_code, "TRA"),
    !str_detect(sector_code, "HH")
  )

# ----------------------------------------------------
# Generate set of fuels used across all sectors ------
# ----------------------------------------------------

siec_used_for_energy <- df_fuel_use_energy_balances %>%
  group_by(siec_code, siec_name) %>%
  summarise(any_positive = any(obs_value > 0, na.rm = TRUE), .groups = "drop") %>%
  filter(any_positive) %>%
  distinct(siec_code, siec_name)

# ----------------------------------------
# Generate set of fuels used by sector ---
# ----------------------------------------

siec_used_for_energy_by_sector <- df_fuel_use_energy_balances %>%
  group_by(siec_code, siec_name, sector_code, sector_name) %>%
  summarise(any_positive = any(obs_value > 0, na.rm = TRUE), .groups = "drop") %>%
  filter(any_positive) %>% 
  select(-c(any_positive))

# Introduce NACE 2-digit codes
nace_crosswalk <- tribble(
  ~sector_name, ~nace2d,
  
  "Final consumption - industry sector - food, beverages and tobacco - energy use",
  list(c("C10","C11","C12")),
  
  "Final consumption - industry sector - iron and steel - energy use",
  list(c("C24")),
  
  "Final consumption - industry sector - machinery - energy use",
  list(c("C28")),
  
  "Final consumption - industry sector - non-ferrous metals - energy use",
  list(c("C24")),
  
  "Final consumption - industry sector - non-metallic minerals - energy use",
  list(c("C23")),
  
  "Final consumption - industry sector - not elsewhere specified - energy use",
  list(c("C")),
  
  "Final consumption - industry sector - textile and leather - energy use",
  list(c("C13","C14","C15")),
  
  "Final consumption - industry sector - chemical and petrochemical - energy use",
  list(c("C20","C21")),
  
  "Final consumption - industry sector - paper, pulp and printing - energy use",
  list(c("C17","C18")),
  
  "Final consumption - industry sector - construction - energy use",
  list(c("F")),
  
  "Final consumption - industry sector - mining and quarrying - energy use",
  list(c("B05","B06","B07","B08","B09")),
  
  "Final consumption - industry sector - transport equipment - energy use",
  list(c("C29","C30")),
  
  "Final consumption - industry sector - wood and wood products - energy use",
  list(c("C16")),
  
  "Final consumption - other sectors - agriculture and forestry - energy use",
  list(c("A01","A02")),
  
  "Final consumption - other sectors - fishing - energy use",
  list(c("A03")),
  
  "Final consumption - other sectors - commercial and public services - energy use",
  list(c("G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")),
  
  "Final consumption - other sectors - not elsewhere specified - energy use",
  list(c("A","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")),
  
  "Energy sector - coal mines - energy use",
  list(c("B05")),
  
  "Energy sector - petroleum refineries (oil refineries) - energy use",
  list(c("C19")),
  
  "Energy sector - coke ovens - energy use",
  list(c("C19")),
  
  "Energy sector - blast furnaces - energy use",
  list(c("C24")),
  
  "Energy sector - electricity and heat generation - energy use",
  list(c("D35")),
  
  "Energy sector - liquefaction and regasification plants (LNG) - energy use",
  list(c("D35")),
  
  "Transformation input - blast furnaces - energy use",
  list(c("C24")),
  
  "Transformation input - coke ovens - energy use",
  list(c("C19")),
  
  "Transformation input - refineries and petrochemical industry - energy use",
  list(c("C19","C20")),
  
  "Transformation input - energy use",
  list(c("B05","C19","C24","D35")),
  
  "Transformation input - electricity and heat generation - energy use",
  list(c("D35")),
  
  "Transformation input - electricity and heat generation - autoproducer combined heat and power - energy use",
  list(c("D35")),
  
  "Transformation input - electricity and heat generation - autoproducer electricity only - energy use",
  list(c("D35")),
  
  "Transformation input - electricity and heat generation - autoproducer heat only - energy use",
  list(c("D35")),
  
  "Transformation input - electricity and heat generation - main activity producer combined heat and power - energy use",
  list(c("D35")),
  
  "Transformation input - electricity and heat generation - main activity producer electricity only - energy use",
  list(c("D35")),
  
  "Transformation input - electricity and heat generation - main activity producer heat only - energy use",
  list(c("D35"))
)

siec_used_for_energy_by_sector <- siec_used_for_energy_by_sector %>%
  left_join(nace_crosswalk, by = "sector_name") %>%
  mutate(nace2d = map(nace2d, \(x) if (is.null(x)) character(0) else x[[1]])) %>%
  unnest(nace2d)

# save it
save(siec_used_for_energy, file = paste0(PROC_DATA,"/fuels_used_for_energy_across_sectors.RData"))
save(siec_used_for_energy_by_sector, file = paste0(PROC_DATA,"/fuels_used_for_energy_by_sector.RData"))

