###############################################################################
# 01_preprocess/12_build_ncv_by_fuel_year.R
#
# PURPOSE
#   Build net calorific values (NCV) by fuel and year (SIEC level or similar), used to convert physical quantities into energy units.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/12_build_ncv_by_fuel_year.R
###############################################################################

#### HEADER -------

## This code creates data sets with Belgium-specific NCV by fuel and year

# It creates two data sets:
# 1. NCVs by fuel by SIEC code
# 2. NCVs by fuel by HS code (# Obs: I do NOT trust hs_to_siec_map)

# It contains information directly obtained from Table 4.1 in the UN IRES, 2018

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

# Libraries ----

library(tidyverse)
library(dplyr)

# Import and clean data --------

ncv_by_fuel_siec_year <- read_csv(paste0(raw_data, "/Eurostat/ncv_by_fuel_year.csv")) %>%
  select(calval, unit, siec, TIME_PERIOD, OBS_VALUE) %>% 
  rename(ncv_type = calval, year = TIME_PERIOD, value = OBS_VALUE) %>%
  tidyr::separate(
    siec,
    into = c("siec_fullcode", "siec_name"),
    sep = ":", 
    remove = TRUE
  ) %>% 
  mutate(
    siec_code = sub("^[A-Za-z]", "", siec_fullcode),
    siec_code = gsub("\\D+$", "", siec_code),
    value = value %>%
      gsub("\\.", "", .) %>%     # remove all dots
      as.numeric() / 1000        # correct decimal placement
  ) %>%
  select(ncv_type, unit, siec_fullcode, siec_name, siec_code, year, value)
  
  load(paste0(proc_data, "/hs_to_siec_map.RData"))

# Impute data from IRES -----------

  # for fuels for which NCV in Belgium are missing in Eurostat, impute them using default
  # values from IRES 2018, Table 4.1

  years <- 1990:2023

  # IRES default NCVs in GJ/tonne (only where IRES actually gives a default)
  # 0313, 4500, 4672 -> NA for now

  ncv_ires <- tibble::tibble(
    siec_code     = c("0312", "0313", "0314",
                      "1110", "1120", "1210", "1290",
                      "3000", "4500", "4620", "4672"),
    siec_fullcode = c("C0312", NA,      "C0314",
                      "P1100", "P1100", "P1200", "P1200",
                      "G3000", "O4500", "O4620", NA),
    siec_name     = c("Gas coke",
                      "Coke breeze",
                      "Semi cokes",
                      "Sod peat",
                      "Milled peat",
                      "Peat briquettes",
                      "Other peat products",
                      "Natural gas",
                      "Other hydrocarbons",
                      "Ethane",
                      "Heavy gas oil"),
    # IRES default values (GJ/tonne)
    ncv_gj_per_t  = c(
      28.2,   # 0312 Gas coke
      NA,     # 0313 Coke breeze (no IRES default)
      28.2,   # 0314 Semi cokes
      9.76,   # 1110 Sod peat
      9.76,   # 1120 Milled peat
      9.76,   # 1210 Peat briquettes
      9.76,   # 1290 Other peat products
      48.0,   # 3000 Natural gas
      NA,     # 4500 Other hydrocarbons (no IRES default)
      46.4,   # 4620 Ethane
      NA      # 4672 Heavy gas oil (no IRES default)
    )
  )
  
  # Convert GJ/tonne -> MJ/tonne (multiply by 1000)
  ncv_ires <- ncv_ires %>%
    mutate(
      value = ncv_gj_per_t * 1000,  # MJ/tonne
      unit = "MJ_T_NCV:Megajoule per tonne (net calorific value - NCV)",
      ncv_type = "IRES default value"
    ) %>%
    select(ncv_type, unit, siec_fullcode, siec_name, siec_code, value)
  
  # Expand over years 1990–2023, keeping value constant across years
  ncv_ires <- tidyr::expand_grid(
    year = years,
    ncv_ires
  ) %>%
    relocate(ncv_type, unit, siec_fullcode, siec_name, year, value, siec_code)
  
  # bind ncv_ires with original data set
  ncv_by_fuel_siec_year <- bind_rows(ncv_by_fuel_siec_year, ncv_ires)
  
  ncv_by_fuel_siec_year <- ncv_by_fuel_siec_year %>% 
    mutate(
      ncv_type = case_when(
        ncv_type == "NCV_AVG:Net calorific value - average" ~ "avg",
        ncv_type == "NCV_IMP:Net calorific value - imports" ~ "I",
        ncv_type == "NCV_EXP:Net calorific value - exports" ~ "X",
        ncv_type == "IRES default value"                    ~ "ires",
        TRUE ~ NA_character_
      )
    )
  
# Report values in TJ per kg
  
  ncv_by_fuel_siec_year <- ncv_by_fuel_siec_year %>% 
    mutate(ncv_tj_per_kg = value * 1e-9) %>% 
    rename(ncv_mj_per_tonne = value)

# ====================================================================  
# Create version of data set by fuel following HS categories --------
  
  # Obs: I do NOT trust hs_to_siec_map
# ====================================================================
  
  hs_siec_ncv <- hs_to_siec_map %>%
    inner_join(
      ncv_by_fuel_siec_year,
      by = "siec_code"
    )
  
  # If HS code is associated with two or more SIEC codes,
  # and one of them has NCV imputed from IRES, drop that one
  hs_siec_ncv <- hs_siec_ncv %>%
    group_by(hs_code, year) %>%
    mutate(
      has_eurostat = any(ncv_type != "ires" & !is.na(value))
    ) %>%
    ungroup() %>%
    # If there is a Eurostat NCV for this HS/year, drop the IRES ones
    filter(!(ncv_type == "ires" & has_eurostat))
  
  # Aggregate to HS-level NCVs
  #    - For HS codes with 1 SIEC -> mean(value) = that SIEC's NCV
  #    - For HS codes with multiple SIEC -> mean(value) = average NCV
  ncv_by_fuel_hs_year <- hs_siec_ncv %>%
    group_by(hs_code, year, ncv_type) %>%
    summarise(
      # average NCV across matching SIEC codes
      value      = mean(value, na.rm = TRUE),
      unit       = dplyr::first(unit),
      siec_codes = paste(sort(unique(siec_code)), collapse = "; "),
      n_siec     = dplyr::n_distinct(siec_code),
      .groups    = "drop"
    )
  
  # Add rows such that all HS codes have NCV between 2000 and 2022
  # Impute values when it's NA
  
  years_target <- 2000:2022
  
  ncv_by_fuel_hs_year <- ncv_by_fuel_hs_year %>%
    # Tag original observations BEFORE expansion
    mutate(original = TRUE) %>%
    
    # (Optional) restrict to target years if you want
    filter(year >= min(years_target), year <= max(years_target)) %>%
    
    # 1) Expand to full panel: every hs_code × ncv_type × year(2000–2022)
    group_by(hs_code, ncv_type) %>%
    complete(year = years_target) %>%
    arrange(hs_code, ncv_type, year) %>%
    
    # After complete(): original == NA → this row was added
    mutate(imputed_row = if_else(is.na(original), TRUE, FALSE)) %>%
    select(-original) %>%
    
    # 2) Fill metadata that should be constant within hs_code × ncv_type
    mutate(
      unit = {
        u <- unit
        u_ref <- u[!is.na(u)][1]
        if (!is.na(u_ref)) coalesce(u, u_ref) else u
      },
      siec_codes = {
        s <- siec_codes
        s_ref <- s[!is.na(s)][1]
        if (!is.na(s_ref)) coalesce(s, s_ref) else s
      },
      n_siec = {
        n <- n_siec
        n_ref <- n[!is.na(n)][1]
        if (!is.na(n_ref)) coalesce(n, n_ref) else n
      }
    ) %>%
    
    # 3) Nearest-year imputation of NCV value
    group_modify(~ {
      df  <- .x
      yrs <- df$year
      vals <- df$value
      non_na <- which(!is.na(vals))
      
      if (length(non_na) == 0) {
        # No real NCV data for this hs_code × ncv_type → leave as NA
        return(df)
      }
      
      # for each NA, find nearest year with valid NCV
      for (i in seq_along(vals)) {
        if (is.na(vals[i])) {
          j <- non_na[which.min(abs(yrs[non_na] - yrs[i]))]
          vals[i] <- vals[j]
        }
      }
      
      df$value <- vals
      df
    }) %>%
    ungroup() %>% 
    rename(ncv_value = value, ncv_unit = unit)

# Save it ------
save(ncv_by_fuel_siec_year, file = paste0(proc_data,"/ncv_by_fuel_siec_year.RData"))
save(ncv_by_fuel_hs_year, file = paste0(proc_data,"/ncv_by_fuel_hs_year.RData"))
  
