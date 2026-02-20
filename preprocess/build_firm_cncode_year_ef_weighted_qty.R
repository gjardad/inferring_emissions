###############################################################################
# 01_preprocess/09_build_firm_cncode_year_ef_weighted_qty.R
#
# PURPOSE
#   Convert firm–CNcode–year quantities into energy (TJ, NCV basis) and CO2 (kg) using IPCC NCVs and emission factors; output line-level EF-weighted quantities.
#
# INPUTS
#   - data/processed/ipcc_ncv_year.RData
#   - data/processed/emission_factors_from_ipcc.RData
#   - data/processed/cn8_to_ipcc_fuel_categories.RData
#   - data/processed/hs6_to_ipcc_fuel_categories.RData
#   - data/processed/firm_cncode_year_physical_qty.RData
#
# OUTPUTS
#   - data/processed/firm_cncode_year_ef_weighted_qty.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/09_build_firm_cncode_year_ef_weighted_qty.R
###############################################################################

#### HEADER -------

# Creates firm-product-year EF-weighted quantities for imported fuels

# EF-weighted imported fuel emissions (NCV-consistent)
# Input: fuel_qty (firm-product-year quantities with units: kg/bbl/mmbtu)
# Output: line-level energy (TJ, NCV basis where applicable) and CO2 (kg)

#####################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))


# -------------
## Setup ------
# -------------


library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

# --------------------
# Helper: safely pick a column name that exists
# --------------------
pick_col <- function(df, candidates) {
  col <- candidates[candidates %in% names(df)][1]
  if (is.na(col)) stop("None of these columns exist: ", paste(candidates, collapse = ", "))
  col
}

# --------------------
# Load required data
# --------------------
load(paste0(PROC_DATA, "/ipcc_ncv_year.RData"))                 # expects: ipcc_fuel, year, ncv_tj_per_kg (or NCV in MJ/t etc)
load(paste0(PROC_DATA, "/emission_factors_from_ipcc.RData"))    # expects: fuel, ef (kgCO2/TJ)
load(paste0(PROC_DATA, "/cn8_to_ipcc_fuel_categories.RData"))   # CN8 overrides when HS6 too coarse
load(paste0(PROC_DATA, "/hs6_to_ipcc_fuel_categories.RData"))   # default HS6 mapping
load(paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))

# ------------------------------------------------------------
# 1) Build CN8 -> IPCC mapping (CN8 overrides HS6)
#    Robust to different column names in mapping files
# ------------------------------------------------------------

cn8_code_col <- pick_col(cn8_to_ipcc_fuel_categories, c("cncode","cn8","CN8","cn"))
cn8_fuel_col <- pick_col(cn8_to_ipcc_fuel_categories, c("ipcc_fuel","fuel","ipcc_fuel_category"))

cn8_map <- cn8_to_ipcc_fuel_categories %>%
  transmute(
    cncode = str_pad(str_replace_all(as.character(.data[[cn8_code_col]]), "\\s+", ""), 8, pad = "0"),
    ipcc_fuel_cn8 = as.character(.data[[cn8_fuel_col]])
  ) %>%
  distinct(cncode, ipcc_fuel_cn8)

hs6_code_col <- pick_col(hs6_to_ipcc_fuel_categories, c("hs6","HS6","hs"))
hs6_fuel_col <- pick_col(hs6_to_ipcc_fuel_categories, c("ipcc_fuel","fuel","ipcc_fuel_category"))

hs6_map <- hs6_to_ipcc_fuel_categories %>%
  transmute(
    hs6 = str_pad(str_replace_all(as.character(.data[[hs6_code_col]]), "\\s+", ""), 6, pad = "0"),
    ipcc_fuel_hs6 = as.character(.data[[hs6_fuel_col]])
  ) %>%
  distinct(hs6, ipcc_fuel_hs6)

# Build mapping for the CN codes you actually have in fuel_qty
cn_to_ipcc <- fuel_qty %>% 
  transmute(
    cncode = str_pad(str_replace_all(as.character(cncode), "\\s+", ""), 8, pad = "0"),
    hs6 = substr(cncode, 1, 6)
  ) %>%
  distinct() %>%
  left_join(cn8_map, by = "cncode") %>%
  left_join(hs6_map, by = "hs6") %>%
  mutate(ipcc_fuel = coalesce(ipcc_fuel_cn8, ipcc_fuel_hs6)) %>%
  select(cncode, ipcc_fuel) %>%
  distinct()

# ------------------------------------------------------------
# 2) Harmonize emission factor column names
# ------------------------------------------------------------
ef_col <- pick_col(emission_factors_from_ipcc,
                   c("ef_kgco2_per_tj","ef_kgCO2_per_TJ","ef","value","emission_factor"))

ef_ipcc <- emission_factors_from_ipcc %>%
  transmute(
    ipcc_fuel = as.character(fuel),
    ef_kgco2_per_tj = as.numeric(.data[[ef_col]])
  ) %>%
  distinct(ipcc_fuel, ef_kgco2_per_tj)

# ------------------------------------------------------------
# 3) Convert quantities (kg) to energy in TJ
# ------------------------------------------------------------

ef_weighted_fuel_qty <- fuel_qty %>%
  mutate(
    cncode = str_pad(str_replace_all(as.character(cncode), "\\s+", ""), 8, pad = "0"),
    year   = as.integer(year),
    quantity = as.numeric(quantity)
  ) %>%
  left_join(cn_to_ipcc, by = "cncode") %>%
  left_join(ipcc_ncv_year %>% select(ipcc_fuel, year, ncv_tj_per_kg),
            by = c("ipcc_fuel", "year")) %>%
  left_join(ef_ipcc, by = "ipcc_fuel") %>%
  mutate(
    energy_tj = if_else(
      !is.na(quantity) & !is.na(ncv_tj_per_kg),
      quantity * ncv_tj_per_kg,
      NA_real_
    ),

    co2_kg = if_else(
      !is.na(energy_tj) & !is.na(ef_kgco2_per_tj),
      energy_tj * ef_kgco2_per_tj,
      NA_real_
    )
  )

# ------------------------------------------------------------
# 4) Diagnostics (coverage)
# ------------------------------------------------------------
diag_coverage <- ef_weighted_fuel_qty %>%
  summarise(
    n_rows = n(),
    share_has_ipcc   = mean(!is.na(ipcc_fuel)),
    share_has_ncv    = mean(!is.na(ncv_tj_per_kg)),
    share_has_ef     = mean(!is.na(ef_kgco2_per_tj)),
    share_has_energy = mean(!is.na(energy_tj)),
    share_has_co2    = mean(!is.na(co2_kg))
  )

missing_ncv_fuels <- ef_weighted_fuel_qty %>%
  filter(!is.na(ipcc_fuel) & is.na(ncv_tj_per_kg) & quantity_units %in% c("kg","bbl")) %>%
  distinct(ipcc_fuel) %>%
  arrange(ipcc_fuel)

missing_ef_fuels <- ef_weighted_fuel_qty %>%
  filter(!is.na(ipcc_fuel) & is.na(ef_kgco2_per_tj)) %>%
  distinct(ipcc_fuel) %>%
  arrange(ipcc_fuel)

print(diag_coverage)
print(missing_ncv_fuels)
print(missing_ef_fuels)

# ------------------------------------------------------------
# 6) Save outputs
# ------------------------------------------------------------
save(ef_weighted_fuel_qty,
     file = paste0(PROC_DATA, "/firm_cncode_year_ef_weighted_qty.RData"))

############################################################
# Notes:
# - kg rows use Eurostat NCV (TJ/kg) directly.
# - bbl rows assume a constant kg/bbl conversion (KG_PER_BBL_CRUDE).
# - mmbtu rows are already energy; they convert to TJ without NCV.
############################################################
