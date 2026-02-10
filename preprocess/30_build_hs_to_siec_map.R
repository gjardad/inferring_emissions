###############################################################################
# 01_preprocess/30_build_hs_to_siec_map.R
#
# PURPOSE
#   Build mapping from HS codes to SIEC fuel categories.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/30_build_hs_to_siec_map.R
###############################################################################

#### HEADER -------

## This code creates a crosswalk between HS and SIEC v.1 for fossil fuels

# The crosswalk is taken from Table 3.1 in the UN IRES, 2018

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


# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Create data set --------

library(tibble)
hs_to_siec_map <- tribble(
  ~hs_code,   ~siec_code, ~siec_description,
  
  # -----------------------------
  # 0 — COAL
  # -----------------------------
  "270111", "0110", "Anthracite",
  "270112", "0129", "Other bituminous coal",
  "270119", "0121", "Coking coal",
  "270120", "0320", "Patent fuel",
  
  "270210", "0210", "Sub-bituminous coal",
  "270210", "0220", "Lignite",

  "270220", "0330", "Brown coal briquettes (BKB)",
  
  "2704",    "0311", "Coke oven coke",
  "2704",    "0312", "Gas coke",
  "2704",    "0313", "Coke breeze",
  "2704",    "0314", "Semi-cokes",
  
  "2706",    "0340", "Coal tar",
  
  "2705",    "0350", "Coke oven gas",
  
  "2705",    "0360", "Gas works gas (and other manufactured gases for distribution)",
  "2705",    "0371", "Blast furnace gas",
  "2705",    "0372", "Basic oxygen steel furnace gas",
  "2705",    "0379", "Other recovered gases",
  
  "2707",    "0390", "Other coal products",
  "270810", "0390", "Other coal products",
  "270820", "0390", "Other coal products",
  "271290", "0390", "Other coal products",
  
  # -----------------------------
  # 1 — PEAT AND PEAT PRODUCTS
  # -----------------------------
  "2703",    "1110", "Sod peat",
  "2703",    "1120", "Milled peat",
  
  "2703",    "1210", "Peat briquettes",
  
  "2703",    "1290", "Other peat products",
  
  ## for simplicity, I assume 1290 is only matched with 2703 when comparing
  # customs and eurostat data -------
  "2704",    "1290", "Other peat products",
  "2706",    "1290", "Other peat products",
  "271290", "1290", "Other peat products",
  
  # -----------------------------
  # 2 — OIL SHALE / OIL SANDS
  # -----------------------------
  "271410", "2000", "Oil shale / oil sands",
  
  # -----------------------------
  # 3 — NATURAL GAS
  # -----------------------------
  "271111", "3000", "Natural gas",
  "271121", "3000", "Natural gas",
  
  # -----------------------------
  # 4 — OIL
  # -----------------------------
  "2709",    "4100", "Conventional crude oil",
  
  # 42 — Natural gas liquids (NGL)
  "271114", "4200", "Natural gas liquids (NGL)",
  "271119", "4200", "Natural gas liquids (NGL)",
  "271129", "4200", "Natural gas liquids (NGL)",
  
  # 44 — Additives and oxygenates
  "220720", "4400", "Additives and oxygenates",
  "290511", "4400", "Additives and oxygenates",
  "290919", "4400", "Additives and oxygenates",
  
  # 45 — Other hydrocarbons
  "2709",    "4500", "Other hydrocarbons",
  "280410", "4500", "Other hydrocarbons",
  
  # 46 — Oil products
  
  # 461 — Refinery gas
  "271129", "4610", "Refinery gas",
  
  # 462 — Ethane
  "271119", "4620", "Ethane",
  "271129", "4620", "Ethane",
  
  # 463 — Liquefied petroleum gases (LPG)
  "271112", "4630", "Liquefied petroleum gases (LPG)",
  "271113", "4630", "Liquefied petroleum gases (LPG)",
  
  # ----- 271011 in HS 2002/07 becomes 271012 in HS 2012/17/22
  
  # 464 — Naphtha
  "271011", "4640", "Naphtha",
  "271012", "4640", "Naphtha",
  
  # 465 — Gasolines
  "271011", "4651", "Aviation gasoline",
  "271012", "4651", "Aviation gasoline",
  
  "271011", "4652", "Motor gasoline",
  "271012", "4652", "Motor gasoline",
  
  "271011", "4653", "Gasoline-type jet fuel",
  "271012", "4653", "Gasoline-type jet fuel",
  
  # --------------
  
  # 466 — Kerosenses
  "271019", "4661", "Kerosene-type jet fuel",
  "271019", "4669", "Other kerosene",
  
  # 467 — Gas oil/diesel oil and heavy gas oil
  "271019", "4671", "Gas oil/diesel oil",
  "271019", "4672", "Heavy gas oil",
  
  # 468 — Fuel oil
  "271019", "4680", "Fuel oil",
  
  # 469 — Other oil products
  "271011", "4691", "White spirit and special boiling point industrial spirits",
  "271019", "4692", "Lubricants",
  "271220", "4693", "Paraffin waxes",
  
  "270820", "4694", "Petroleum coke",
  "271311", "4694", "Petroleum coke",
  "271312", "4694", "Petroleum coke",
  
  "271320", "4695", "Bitumen",
  
  "2707",    "4699", "Other oil products n.e.c.",
  "270810", "4699", "Other oil products n.e.c.",
  "271011", "4699", "Other oil products n.e.c.",
  "271019", "4699", "Other oil products n.e.c.",
  "271114", "4699", "Other oil products n.e.c.",
  "271210", "4699", "Other oil products n.e.c.",
  
  # ----- this HS code didn't exist in HS 2007, but exists in the following (12, 17, 22) HS editions
  # ----- I manually mapped 271020 to 4699 by exclusion
  
  "271020", "4699", "Other oil products n.e.c."
)

# Save it ------
save(hs_to_siec_map, file = paste0(PROC_DATA,"/hs_to_siec_map.RData"))
