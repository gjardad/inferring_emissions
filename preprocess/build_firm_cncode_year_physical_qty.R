###############################################################################
# 01_preprocess/08_build_firm_cncode_year_physical_qty.R
#
# PURPOSE
#   Construct firm–CNcode–year physical quantities for imported fuels.
#   Hierarchy: (1) customs weight if reliable, (2) CN8-year internal median
#   deflation, (3a-b) progressively coarser dispersion-gated deflation.
#
# INPUTS
#   - data/processed/fuel_imported_by_firm_year.RData
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

# --------------------
#  Load data ---------
# --------------------

load(paste0(PROC_DATA,"/fuel_imported_by_firm_year.RData"))

# ---------------------------------------------
# CN-year physical qty using customs data -------
#
# We have data on value and weight for each good.
# Weight is in kg. If reliable, that's the physical qty,
# but not always reliable.
#
# Tier hierarchy:
#   1. Use reported weight if CN8-year price distribution is well-behaved
#      (p90/p10 <= 30) and firm is not an outlier
#   2. Deflate value using CN8-year internal median price (p90/p10 <= 30)
#   3a. Deflate value using CN6-year median price (p90/p10 < 50, >= 5 firms)
#   3b. Deflate value using CN4-year median price (p90/p10 < 100, >= 10 firms)
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
# Tier 3: Dispersion-gated deflation --
# ======================================

# Tier 3a: CN6-year median price (p90/p10 < 50, >= 5 firms)
cn6_year_price <- df_price_firm %>%
  mutate(cn6 = substr(cncode, 1, 6)) %>%
  group_by(cn6, year) %>%
  summarise(
    n_firms   = n_distinct(vat_ano),
    p10       = quantile(price_eur_per_kg, 0.10, na.rm = TRUE),
    p90       = quantile(price_eur_per_kg, 0.90, na.rm = TRUE),
    price_cn6 = median(price_eur_per_kg, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(n_firms >= 5, p90 / p10 < 50) %>%
  select(cn6, year, price_cn6)

# Tier 3b: CN4-year median price (p90/p10 < 100, >= 10 firms)
cn4_year_price <- df_price_firm %>%
  mutate(cn4 = substr(cncode, 1, 4)) %>%
  group_by(cn4, year) %>%
  summarise(
    n_firms   = n_distinct(vat_ano),
    p10       = quantile(price_eur_per_kg, 0.10, na.rm = TRUE),
    p90       = quantile(price_eur_per_kg, 0.90, na.rm = TRUE),
    price_cn4 = median(price_eur_per_kg, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  filter(n_firms >= 10, p90 / p10 < 100) %>%
  select(cn4, year, price_cn4)

# Merge fallback prices and apply cascade
fuel_qty <- fuel_qty %>%
  mutate(
    cn6 = substr(cncode, 1, 6),
    cn4 = substr(cncode, 1, 4)
  ) %>%
  left_join(cn6_year_price, by = c("cn6", "year")) %>%
  left_join(cn4_year_price, by = c("cn4", "year")) %>%
  mutate(
    quantity = case_when(
      !is.na(fuel_qty_kg) ~ fuel_qty_kg,
      !is.na(imports_value) & !is.na(price_cn6) ~ imports_value / price_cn6,
      !is.na(imports_value) & !is.na(price_cn4) ~ imports_value / price_cn4,
      TRUE ~ NA_real_
    ),
    quantity_units = "kg",
    qty_source = case_when(
      qty_source %in% c("weight_direct", "value_deflated") ~ qty_source,
      !is.na(imports_value) & !is.na(price_cn6) ~ "deflated_cn6",
      !is.na(imports_value) & !is.na(price_cn4) ~ "deflated_cn4",
      TRUE ~ "missing"
    )
  ) %>%
  filter(year >= 2005) %>%
  ungroup()

# save it
save(fuel_qty, file = paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))

# ==================================================================
# Diagnostics: quantity source breakdown
# ==================================================================

cat("\n--- Quantity source breakdown ---\n")
fuel_qty %>%
  count(qty_source, sort = TRUE) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\n--- Missing quantities by CN4 ---\n")
fuel_qty %>%
  filter(is.na(quantity), !is.na(imports_value)) %>%
  count(cn4 = substr(cncode, 1, 4), sort = TRUE) %>%
  print()

