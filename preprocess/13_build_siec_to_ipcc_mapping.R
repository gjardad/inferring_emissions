###############################################################################
# 01_preprocess/13_build_siec_to_ipcc_mapping.R
#
# PURPOSE
#   Build SIEC -> IPCC fuel category mapping used to translate Eurostat/energy-balance fuels into IPCC categories.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/13_build_siec_to_ipcc_mapping.R
###############################################################################

#### HEADER -------

## This code creates mapping between SIEC codes and IPCC fuel categories
# (fuel categories consistent with IPCC tables on emission factors)

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

library(tibble)
library(dplyr)
library(stringr)

# ---------------------------
# Baseline SIEC -> IPCC mapping table (combustion-oriented, NCV basis)
# - IPCC fuel names are "practical" labels you can later map to EF tables.
# - mapping_quality: "exact" vs "approx" when SIEC bucket is heterogeneous.
# ---------------------------

library(tibble)
library(dplyr)
library(stringr)

siec_to_ipcc <- tribble(
  ~siec_code, ~siec_name, ~ipcc_fuel, ~mapping_quality, ~notes,
  
  # --- Solid fossil fuels ---
  "C0110", "Anthracite",                 "Anthracite",                "exact",  NA_character_,
  "C0121", "Coking coal",                "Coking Coal",               "exact",  NA_character_,
  "C0129", "Other bituminous coal",      "Other Bituminous Coal",     "exact",  NA_character_,
  "C0210", "Sub-bituminous coal",        "Sub-Bituminous Coal",       "exact",  NA_character_,
  "C0220", "Lignite",                    "Lignite",                   "exact",  NA_character_,
  "C0330", "Brown coal briquettes",      "Brown Coal Briquettes",     "exact",  NA_character_,
  "C0320", "Patent fuel",                "Patent Fuel",               "exact",  NA_character_,
  
  # --- Coke / coal-derived products ---
  "C0311", "Coke oven coke",             "Coke Oven Coke and Lignite Coke", "exact", NA_character_,
  "C0312", "Gas coke",                   "Gas Coke",                  "exact",  NA_character_,
  "C0340", "Coal tar",                   "Coal Tar",                  "exact",  "Can be used as feedstock; keep if you want a combustion mapping too.",
  
  # --- Manufactured / recovered gases ---
  "C0360", "Gas works gas",              "Gas Works Gas",             "exact",  NA_character_,
  "C0350", "Coke oven gas",              "Coke Oven Gas",             "exact",  NA_character_,
  "C0371", "Blast furnace gas",          "Blast Furnace Gas",         "exact",  NA_character_,
  "C0379", "Other recovered gases",      "Oxygen Steel Furnace Gas",  "approx", "SIEC 'other recovered gases' can include BOF/OSF gas and other recovered gases; map to OSF gas as closest IPCC category unless you maintain a separate residual gas bucket.",
  
  # --- Natural gas ---
  "G3000", "Natural gas",                "Natural Gas",               "exact",  NA_character_,
  
  # --- Oil & petroleum products (fossil portion where applicable) ---
  "O4100_TOT", "Crude oil",              "Crude Oil",                 "exact",  NA_character_,
  "O4200", "Natural gas liquids",        "Natural Gas Liquids",       "exact",  NA_character_,
  "O4610", "Refinery gas",               "Refinery Gas",              "exact",  NA_character_,
  "O4620", "Ethane",                     "Ethane",                    "exact",  NA_character_,
  "O4630", "Liquefied petroleum gases",  "Liquefied Petroleum Gases", "exact",  NA_character_,
  "O4640", "Naphtha",                    "Naphtha",                   "exact",  "Often used as feedstock; include/exclude depending on your 'energy use' filter.",
  "O4651", "Aviation gasoline",          "Aviation Gasoline",         "exact",  NA_character_,
  "O4653", "Gasoline-type jet fuel",     "Jet Gasoline",              "exact",  NA_character_,
  "O4661XR5230B", "Kerosene-type jet fuel (excluding biofuel portion)", "Jet Kerosene", "exact", NA_character_,
  "O4669", "Other kerosene",             "Other Kerosene",            "exact",  NA_character_,
  "O4652XR5210B", "Motor gasoline (excluding biofuel portion)", "Motor Gasoline", "exact", NA_character_,
  "O4671XR5220B", "Gas oil and diesel oil (excluding biofuel portion)", "Gas/Diesel Oil", "exact", NA_character_,
  "O4680", "Fuel oil",                   "Residual Fuel Oil",         "exact",  NA_character_,
  "O4691", "White spirit and special boiling point industrial spirits", "White Spirit and SBP", "exact", "Often non-energy use (solvents); keep but flag if restricting to energy use.",
  "O4692", "Lubricants",                 "Lubricants",                "exact",  "Typically non-energy use; combustion is not the main channel.",
  "O4693", "Paraffin waxes",             "Paraffin Waxes",            "exact",  "Typically non-energy use.",
  "O4694", "Petroleum coke",             "Petroleum Coke",            "exact",  NA_character_,
  "O4695", "Bitumen",                    "Bitumen",                   "exact",  "Typically non-energy use (paving).",
  "O4300", "Refinery feedstocks",        "Refinery Feedstocks",       "exact",  "Often non-energy use; include/exclude depending on your energy-use filter.",
  "O4699", "Other oil products n.e.c.",  "Other Petroleum Products",  "exact",  "Residual bucket; if large, consider country-specific split.",
  "O4500", "Other hydrocarbons",         "Other Petroleum Products",  "approx", "Closest IPCC bucket is Other Petroleum Products unless you have a dedicated EF.",
  
  # --- Oil shale / sands ---
  "S2000", "Oil shale and oil sands",    "Oil Shale and Tar Sands",   "exact",  NA_character_,
  
  # --- Peat (note: P1000 excluded as aggregate) ---
  "P1100", "Peat",                       "Peat",                      "exact",  NA_character_,
  "P1200", "Peat products",              "Peat",                      "approx", "IPCC list has 'Peat' but not a separate 'peat products'; treat as peat unless you maintain a separate EF.",
  
  # --- Biomass / biofuels / biogas (kept because you asked to ignore carriers+waste, not bioenergy) ---
  "R5110-5150_W6000RI", "Primary solid biofuels", "Wood / Wood Waste", "approx",
  "This SIEC bucket includes multiple solid biomass types; map to Wood/Wood Waste as default. If you can split (e.g., black liquor, other solid biomass), do so.",
  "R5160", "Charcoal",                   "Charcoal",                 "exact",  NA_character_,
  "R5210B", "Blended biogasoline",       "Biogasoline",              "exact",  "If you later compute fossil-only CO2, you’ll need blend shares.",
  "R5210P", "Pure biogasoline",          "Biogasoline",              "exact",  NA_character_,
  "R5220B", "Blended biodiesels",        "Biodiesels",               "exact",  "If you later compute fossil-only CO2, you’ll need blend shares.",
  "R5220P", "Pure biodiesels",           "Biodiesels",               "exact",  NA_character_,
  "R5230B", "Blended bio jet kerosene",  "Other Liquid Biofuels",    "approx", "IPCC list doesn’t have a dedicated 'bio jet kerosene'; map to Other Liquid Biofuels unless you create a custom subcategory.",
  "R5230P", "Pure bio jet kerosene",     "Other Liquid Biofuels",    "approx", "Same logic as blended bio jet kerosene.",
  "R5290", "Other liquid biofuels",      "Other Liquid Biofuels",    "exact",  NA_character_,
  "R5300", "Biogases",                   "Other Biogas",             "approx", "If you can split landfill/sludge/other biogas, map accordingly to those IPCC categories."
) %>%
  mutate(
    siec_code = str_trim(siec_code),
    ipcc_fuel = str_trim(ipcc_fuel)
  )

# save it
save(siec_to_ipcc, file = paste0(proc_data, "/siec_to_ipcc.RData"))

