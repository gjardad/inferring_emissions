###############################################################################
# 01_preprocess/10_build_ipcc_ncv_year.R
#
# PURPOSE
#   Build IPCC fuel × year net calorific values (NCV) from SIEC-level NCVs and a SIEC→IPCC mapping; add a few custom overrides used by the customs-mapped fuels.
#
# INPUTS
#   - data/processed/ncv_by_fuel_siec_year.RData
#   - data/processed/siec_to_ipcc.RData
#   - data/processed/fuel_imported_by_firm_year.RData
#   - data/processed/cn8_to_ipcc_fuel_categories.RData
#   - data/processed/hs6_to_ipcc_fuel_categories.RData
#
# OUTPUTS
#   - data/processed/ipcc_ncv_year.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/10_build_ipcc_ncv_year.R
###############################################################################

#### HEADER -------

## This code creates NCV by IPCC fuel-year

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


# Setup ------


library(tidyverse)
library(dplyr)

# ===================
# Load data --------
# ===================

load(paste0(PROC_DATA,"/ncv_by_fuel_siec_year.RData"))
load(paste0(PROC_DATA,"/siec_to_ipcc.RData"))

ipcc_ncv_base <- ncv_by_fuel_siec_year %>%
  select(-c(unit, ncv_mj_per_tonne)) %>% 
  left_join(siec_to_ipcc %>% select(siec_code, ipcc_fuel, mapping_quality),
            by = c("siec_fullcode" = "siec_code")) %>%
  filter(!is.na(ipcc_fuel), !is.na(ncv_tj_per_kg)) %>%
  group_by(ipcc_fuel, year) %>%
  summarise(
    n_siec = n_distinct(siec_code),
    ncv_tj_per_kg = mean(ncv_tj_per_kg, na.rm = TRUE),
    ncv_min = min(ncv_tj_per_kg, na.rm = TRUE),
    ncv_max = max(ncv_tj_per_kg, na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# Append missing IPCCs ----
# =========================

# which IPCC fuels show up in my customs data?

load(paste0(PROC_DATA,"/fuel_imported_by_firm_year.RData"))
load(paste0(PROC_DATA,"/cn8_to_ipcc_fuel_categories.RData"))
load(paste0(PROC_DATA,"/hs6_to_ipcc_fuel_categories.RData"))

df <- fuel_imported_by_firm_year %>%
  mutate(
    vat_ano = as.character(vat_ano),
    cncode  = str_replace_all(as.character(cncode), "\\s+", ""),
    year    = as.integer(year),
    imports_value  = as.numeric(imports_value),
    imports_weight = as.numeric(imports_weight),
    imports_value  = ifelse(imports_value  <= 0, NA_real_, imports_value),
    imports_weight = ifelse(imports_weight <= 0, NA_real_, imports_weight)
  ) %>%
  filter(!is.na(cncode), !is.na(year))

unique_cncode <- df %>%
  ungroup() %>% 
  select(cncode) %>%
  distinct()

which_ipcc <- unique_cncode %>% 
  mutate(hs6_code = substr(cncode, 1, 6)) %>% 
  left_join(hs6_to_ipcc_fuel_categories %>% select(hs6, ipcc_fuel, needs_cn8),
            by = c("hs6_code" = "hs6")) %>%
  rename(ipcc_fuel_hs6 = ipcc_fuel) %>% 
  left_join(cn8_to_ipcc_fuel_categories %>% select(cn8, ipcc_fuel),
            by = c("cncode" = "cn8"))

unique_ipcc_fuels <- unique(c(which_ipcc$ipcc_fuel_hs6, which_ipcc$ipcc_fuel))

# among these, only four are not listed in ipcc_ncv_year
# they are: waste oils, jet gasoline, gas Works gas, oil shale and tar sands

# override with NCVs for three of these, and exclude gas works gas

ipcc_ncv_overrides <- tibble::tibble(
  ipcc_fuel = c("Jet Gasoline", "Waste Oils", "Oil Shale and Tar Sands"),
  proxy_ipcc_fuel = c("Aviation Gasoline", "Other Petroleum Products", "Crude Oil")
)

years <- sort(unique(ipcc_ncv_base$year))

ipcc_ncv_overrides_year <- ipcc_ncv_overrides %>%
  tidyr::crossing(year = years)

# base table (as-is)
ipcc_ncv_base <- ipcc_ncv_base %>%
  select(ipcc_fuel, year, ncv_tj_per_kg)

# proxy lookup: proxy_ipcc_fuel × year -> ncv
proxy_lookup <- ipcc_ncv_base %>%
  select(proxy_ipcc_fuel = ipcc_fuel, year, ncv_proxy = ncv_tj_per_kg)

# build overrides with actual ncv from proxy, year by year
overrides_filled <- ipcc_ncv_overrides_year %>%
  left_join(proxy_lookup, by = c("proxy_ipcc_fuel", "year")) %>%
  transmute(ipcc_fuel, year, ncv_tj_per_kg = ncv_proxy)

# combine: base first, then overrides only where missing
ipcc_ncv_full <- ipcc_ncv_base %>%
  full_join(overrides_filled, by = c("ipcc_fuel", "year"), suffix = c("_base", "_ovr")) %>%
  mutate(ncv_tj_per_kg = coalesce(ncv_tj_per_kg_base, ncv_tj_per_kg_ovr)) %>%
  select(ipcc_fuel, year, ncv_tj_per_kg)

ipcc_ncv_year <- ipcc_ncv_full

# save it
save(ipcc_ncv_year, file = paste0(PROC_DATA,"/ipcc_ncv_year.RData"))
