###############################################################################
# 01_preprocess/08_build_firm_cncode_year_physical_qty.R
#
# PURPOSE
#   Construct firm–CNcode–year physical quantities for imported fuels (hierarchy: weight if reliable; internal unit value deflation; external price deflation).
#
# INPUTS
#   - data/processed/fuel_imported_by_firm_year.RData
#   - data/processed/coal_price.RData
#   - data/processed/gas_price.RData
#   - data/processed/oil_price.RData
#
# OUTPUTS
#   - data/processed/firm_cncode_year_physical_qty.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/08_build_firm_cncode_year_physical_qty.R
###############################################################################

#### HEADER -------

# Creates firm-product-year physical quantities for fossil fuels imported into Belgium

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

# --------------------
#  Load data ---------
# --------------------

load(paste0(PROC_DATA,"/fuel_imported_by_firm_year.RData"))

# ---------------------------------------------
# CN-year physical qty using customs data -------

  # we have data on value and wegiht for each good
  # weight is in kg. if reliable, that's the physical qty
  # but not always realiable.

  # compute within good-year distribution of implied prices (value/weight)
  # across firms. if p90/p10 < 30, then weight data is reliable.

  # if p90/p10 < 30 and the implied price of a given firm-good-year obs is not an outlier,
  # then use the obs weight as the physical qty

  # if p90/p10 < 30 but the implied price of a given firm-good-year obs is an outlier,
  # deflate value using the median price of the good-year price distribution

  # if p90/p10 > 30, then weight data is not reliable and we deflate the value
  # using external time series on prices
# ---------------------------------------------

# clean it
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

# Calculate firm-level unit values (implied prices)
df_price_firm <- df %>%
  filter(!is.na(imports_value), !is.na(imports_weight)) %>%
  mutate(
    price_eur_per_kg = imports_value / imports_weight,
    log_price = log(price_eur_per_kg)
  ) %>%
  filter(is.finite(log_price))

# Calculate 90, 10 percentiles of cncode-year distribution of prices
diag_cn_year <- df_price_firm %>%
  group_by(cncode, year) %>%
  summarise(
    n_firms = n_distinct(vat_ano),
    p10 = quantile(price_eur_per_kg, 0.10, na.rm = TRUE),
    p90 = quantile(price_eur_per_kg, 0.90, na.rm = TRUE),
    median_price = median(price_eur_per_kg, na.rm = TRUE),
    max_price = max(price_eur_per_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ratio_p90_p10 = p90 / p10,
    ratio_max_med = max_price / median_price,
    quality_flag = case_when(
      n_firms < 3 ~ "too_few_obs",
      ratio_p90_p10 <= 10  ~ "very_good",
      ratio_p90_p10 <= 30  ~ "ok",
      ratio_p90_p10 <= 100 ~ "noisy",
      TRUE ~ "bad"
    )
  )

# Identify firm-level outliers within cncode-year
df_price_firm_flagged <- df_price_firm %>%
  group_by(cncode, year) %>%
  mutate(
    med = median(log_price, na.rm = TRUE),
    mad_ = mad(log_price, constant = 1, na.rm = TRUE),
    z_mad = ifelse(mad_ > 0, (log_price - med) / mad_, NA_real_),
    outlier = !is.na(z_mad) & abs(z_mad) > 4
  ) %>%
  ungroup() %>%
  select(vat_ano, cncode, year, z_mad, outlier)

# Flag observations
usable <- c("very_good", "ok")

cn_year_price_final <- df_price_firm %>%
  left_join(diag_cn_year %>% select(cncode, year, quality_flag),
            by = c("cncode", "year")) %>%
  filter(quality_flag %in% usable) %>%
  group_by(cncode, year) %>%
  summarise(
    price_eur_per_kg = median(price_eur_per_kg, na.rm = TRUE),
    n_firms = n_distinct(vat_ano),
    quality_flag = first(quality_flag),
    .groups = "drop"
  )

# Build final quantity variable with two-layer rule
fuel_qty <- df %>%
  left_join(diag_cn_year %>% select(cncode, year, quality_flag),
            by = c("cncode", "year")) %>%
  left_join(df_price_firm_flagged,
            by = c("vat_ano", "cncode", "year")) %>%
  left_join(cn_year_price_final %>% select(cncode, year, price_eur_per_kg),
            by = c("cncode", "year")) %>%
  mutate(
    quality_flag = ifelse(is.na(quality_flag), "no_unit_value", quality_flag),
    outlier = ifelse(is.na(outlier), FALSE, outlier),
    
    use_weight = quality_flag %in% c("very_good", "ok") &
      !outlier &
      !is.na(imports_weight),
    
    fuel_qty_kg = case_when(
      use_weight ~ imports_weight,
      !is.na(price_eur_per_kg) & !is.na(imports_value) ~ imports_value / price_eur_per_kg,
      TRUE ~ NA_real_
    ),
    
    qty_source = case_when(
      use_weight ~ "weight_direct",
      !is.na(price_eur_per_kg) ~ "value_deflated",
      TRUE ~ "missing"
    )
  )

# ======================================
# Merge with external price data -------
# ======================================

fuel_qty <- fuel_qty %>%
  mutate(
    cncode = str_replace_all(as.character(cncode), "\\s+", ""),
    cn4 = substr(cncode, 1, 4),
    year = as.integer(year)
  )

# Load price data
load(paste0(PROC_DATA,"/coal_price.RData"))  # has: year, eur_per_ton
load(paste0(PROC_DATA,"/gas_price.RData"))   # has: year, eur_per_mmbtu
load(paste0(PROC_DATA,"/oil_price.RData"))   # has: year, eur_per_bbl

# --- Standardize price series names/units (using known column names) ---
coal_price_std <- coal_price %>%
  transmute(
    year = as.integer(year),
    price_eur_per_kg = as.numeric(eur_per_ton) / 1000
  )

gas_price_std <- gas_price %>%
  transmute(
    year = as.integer(year),
    price_eur_per_mmbtu = as.numeric(eur_per_mmbtu)
  )

oil_price_std <- oil_price %>%
  transmute(
    year = as.integer(year),
    price_eur_per_bbl = as.numeric(eur_per_bbl)
  )

# Map cn4 to benchmark fuel (your existing mapping)
cn4_price_map <- tibble::tibble(
  cn4 = c("2711","2710","2713","2709","2701","2702","2703","2704"),
  fuel_benchmark = c("gas","oil","oil","oil","coal","coal","coal","coal")
)

# Create cn4-year table with the appropriate price unit for each benchmark
years <- sort(unique(fuel_qty$year))

cn4_year_price_units <- cn4_price_map %>%
  tidyr::crossing(year = years) %>%
  left_join(gas_price_std %>% mutate(fuel_benchmark = "gas"),
            by = c("fuel_benchmark", "year")) %>%
  left_join(oil_price_std %>% mutate(fuel_benchmark = "oil"),
            by = c("fuel_benchmark", "year")) %>%
  left_join(coal_price_std %>% mutate(fuel_benchmark = "coal"),
            by = c("fuel_benchmark", "year")) %>%
  mutate(
    price_for_deflation = case_when(
      fuel_benchmark == "gas"  ~ price_eur_per_mmbtu,
      fuel_benchmark == "oil"  ~ price_eur_per_bbl,
      fuel_benchmark == "coal" ~ price_eur_per_kg,
      TRUE ~ NA_real_
    ),
    deflation_units = case_when(
      fuel_benchmark == "gas"  ~ "mmbtu",
      fuel_benchmark == "oil"  ~ "bbl",
      fuel_benchmark == "coal" ~ "kg",
      TRUE ~ NA_character_
    )
  ) %>%
  select(cn4, year, fuel_benchmark, price_for_deflation, deflation_units)

# Apply fallback deflation ONLY when fuel_qty_kg is missing
fuel_qty <- fuel_qty %>%
  left_join(cn4_year_price_units, by = c("cn4", "year")) %>%
  mutate(
    quantity = case_when(
      !is.na(fuel_qty_kg) ~ fuel_qty_kg,
      is.na(fuel_qty_kg) & !is.na(imports_value) & !is.na(price_for_deflation) ~ imports_value / price_for_deflation,
      TRUE ~ NA_real_
    ),
    quantity_units = case_when(
      !is.na(fuel_qty_kg) ~ "kg",
      is.na(fuel_qty_kg) & !is.na(imports_value) & !is.na(price_for_deflation) ~ deflation_units,
      TRUE ~ NA_character_
    ),
    qty_source = case_when(
      qty_source %in% c("weight_direct", "value_deflated") ~ qty_source,
      is.na(fuel_qty_kg) & !is.na(imports_value) & !is.na(price_for_deflation) ~ paste0("value_deflated_external_", fuel_benchmark),
      TRUE ~ "missing"
    )
  ) %>%
  filter(year >= 2005) %>% 
  ungroup()

# save it
save(fuel_qty, file = paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))

# ==================================================================
# Diagnosing missing quantities ------------------------------------

# The construction of physical quantities follows a strict
# hierarchy:
#
# (1) Use reported customs weight (kg) when reliable
# (2) Otherwise, deflate values using internal CN–year median
#     unit values (€/kg) when those distributions are well-behaved
# (3) Otherwise, deflate values using external benchmark prices
#     (coal in kg, oil in barrels, gas in MMBtu)
#
# As a result, missing quantities arise for two structural reasons:
#
# - "no_value_and_no_kg":
#     Observations with neither reported weight nor reported value.
#     These quantities are unrecoverable from customs data and are
#     excluded from downstream energy and emissions calculations.
#
# - "no_external_price_match":
#     Observations with reported values but belonging to CN4 product
#     groups for which no external benchmark price is assigned
#     (e.g. non-energy petroleum products, coal-processing by-products,
#     or process gases such as CN4 = 2712, 2707, 2706, 2705, 2708, 2714).
#     For these products, imputing physical quantities would require
#     additional assumptions that are not justified, so quantities
#     are left missing by construction.
#
# These missing quantities are therefore intentional and reflect
# conservative identification choices rather than data errors.
# ==================================================================

fuel_qty %>%
  ungroup() %>%
  mutate(
    missing_qty = is.na(quantity),
    reason = case_when(
      !missing_qty ~ "has_quantity",
      is.na(fuel_qty_kg) & is.na(imports_value) ~ "no_value_and_no_kg",
      is.na(fuel_qty_kg) & !is.na(imports_value) & is.na(price_for_deflation) ~ "no_external_price_match",
      is.na(fuel_qty_kg) & !is.na(imports_value) & !is.na(price_for_deflation) ~ "should_have_external_qty_check_logic",
      TRUE ~ "other"
    )
  ) %>%
  count(reason, sort = TRUE)

fuel_qty %>%
  ungroup() %>% 
  filter(is.na(quantity), !is.na(imports_value)) %>%
  count(substr(cncode, 1, 4), sort = TRUE)


