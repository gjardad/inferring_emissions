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
# 01_preprocess/14_build_emission_factors_ipcc.R
#
# PURPOSE
#   Prepare IPCC emission factors (e.g., kg CO2 per TJ) used to convert energy quantities into CO2 quantities.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/14_build_emission_factors_ipcc.R
###############################################################################

#### HEADER -------

## This code creates data set with emission factors by fuel
# directly obtained from Table 2.2. in Volume 2, Chapter 2 in the IPCC 2006 guidelines

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

# ==================
# Import data ------
# ==================

library(tibble)

ipcc_ef_table_2_2 <- tibble(
  fuel = c(
    "Crude Oil",
    "Orimulsion",
    "Natural Gas Liquids",
    "Motor Gasoline",
    "Aviation Gasoline",
    "Jet Gasoline",
    "Jet Kerosene",
    "Other Kerosene",
    "Shale Oil",
    "Gas/Diesel Oil",
    "Residual Fuel Oil",
    "Liquefied Petroleum Gases",
    "Ethane",
    "Naphtha",
    "Bitumen",
    "Lubricants",
    "Petroleum Coke",
    "Refinery Feedstocks",
    "Refinery Gas",
    "Paraffin Waxes",
    "White Spirit and SBP",
    "Other Petroleum Products",
    "Anthracite"
  ),
  ef_kgco2_per_tj = c(
    73300,
    77000,
    64200,
    69300,
    70000,
    70000,
    71500,
    71900,
    73300,
    74100,
    77400,
    63100,
    61600,
    73300,
    80700,
    73300,
    97500,
    73300,
    57600,
    73300,
    73300,
    73300,
    98300
  ),
  ef_lower = c(
    71100,
    69300,
    58300,
    67500,
    67500,
    67500,
    69700,
    70800,
    67800,
    72600,
    75500,
    61600,
    56500,
    69300,
    73000,
    71900,
    82900,
    68900,
    48200,
    72200,
    72200,
    72200,
    94600
  ),
  ef_upper = c(
    75500,
    85400,
    70400,
    73000,
    73000,
    73000,
    74400,
    73700,
    79200,
    74800,
    78800,
    65600,
    68600,
    76300,
    89900,
    75200,
    115000,
    76600,
    69000,
    74400,
    74400,
    74400,
    101000
  ),
  source = "IPCC 2006 Table 2.2"
)

library(tibble)
library(dplyr)

# (Optional) if your existing tibble doesn't have this column yet:
ipcc_ef_table_2_2 <- ipcc_ef_table_2_2 %>%
  mutate(ipcc_note = NA_character_)

ipcc_ef_table_2_2_snippet_coal_gas <- tibble(
  fuel = c(
    "Coking Coal",
    "Other Bituminous Coal",
    "Sub-Bituminous Coal",
    "Lignite",
    "Oil Shale and Tar Sands",
    "Brown Coal Briquettes",
    "Patent Fuel",
    "Coke Oven Coke and Lignite Coke",
    "Gas Coke",
    "Coal Tar",
    "Gas Works Gas",
    "Coke Oven Gas",
    "Blast Furnace Gas",
    "Oxygen Steel Furnace Gas",
    "Natural Gas"
  ),
  ef_kgco2_per_tj = c(
    94600, 94600, 96100, 101000, 107000, 97500, 97500,
    107000, 107000, 80700,
    44400, 44400, 260000, 182000,
    56100
  ),
  ef_lower = c(
    87300, 89500, 92800, 90900, 90200, 87300, 87300,
    95700, 95700, 68200,
    37300, 37300, 219000, 145000,
    54300
  ),
  ef_upper = c(
    101000, 99700, 100000, 115000, 125000, 109000, 109000,
    119000, 119000, 95300,
    54100, 54100, 308000, 202000,
    58300
  ),
  ipcc_note = c(
    NA, NA, NA, NA, NA, NA, NA,
    "r", "r", "n",
    "n", "n", "n", "n",
    NA
  ),
  source = "IPCC 2006 Table 2.2"
)

ipcc_ef_table_2_2_snippet_waste_bio <- tibble(
  fuel = c(
    "Municipal Wastes (non-biomass fraction)",
    "Industrial Wastes",
    "Waste Oils",
    "Peat",
    "Wood / Wood Waste",
    "Sulphite lyes (Black Liquor)",
    "Other Primary Solid Biomass",
    "Charcoal",
    "Biogasoline",
    "Biodiesels",
    "Other Liquid Biofuels",
    "Landfill Gas",
    "Sludge Gas",
    "Other Biogas",
    "Municipal Wastes (biomass fraction)"
  ),
  ef_kgco2_per_tj = c(
    91700, 143000, 73300, 106000,
    112000, 95300, 100000, 112000,
    70800, 70800, 79600,
    54600, 54600, 54600,
    100000
  ),
  ef_lower = c(
    73300, 110000, 72200, 100000,
    95000, 80700, 84700, 95000,
    59800, 59800, 67100,
    46200, 46200, 46200,
    84700
  ),
  ef_upper = c(
    121000, 183000, 74400, 108000,
    132000, 110000, 117000, 132000,
    84300, 84300, 95300,
    66000, 66000, 66000,
    117000
  ),
  ipcc_note = c(
    "n","n","n", NA,
    "n","n","n","n",
    "n","n","n",
    "n","n","n",
    "n"
  ),
  source = "IPCC 2006 Table 2.2"
)

# Append to your existing table
emission_factors_from_ipcc <- bind_rows(
  ipcc_ef_table_2_2,
  ipcc_ef_table_2_2_snippet_coal_gas,
  ipcc_ef_table_2_2_snippet_waste_bio
) %>% 
  select(-c(ipcc_note))

# save it
save(emission_factors_from_ipcc, file = paste0(proc_data, "/emission_factors_from_ipcc.RData"))
