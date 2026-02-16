###############################################################################
# 01_preprocess/07_fuel_imported_by_firm_year.R
#
# PURPOSE
#   Identify firm-year fuel importers from customs (CN27 excl. 2716) and merge ETS + NACE + sample flags.
#
# INPUTS
#   - data/processed/df_trade.RData
#   - data/processed/firm_year_belgian_euets.RData
#   - data/processed/annual_accounts_selected_sample.RData
#   - data/processed/firms_in_selected_sample.RData
#
# OUTPUTS
#   - data/processed/fuel_imported_by_firm_year.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/07_fuel_imported_by_firm_year.R
###############################################################################

#### HEADER -------

## This code identifies fuel importers in Belgium

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
source(file.path(REPO_DIR, "paths.R"))


# Setup ------


# Libraries ----

library(tidyverse)
library(dplyr) 

# Import and data --------

load(paste0(PROC_DATA,"/df_trade.RData"))
#load(paste0(PROC_DATA,"/cn8digit_codes_for_fossil_fuels.RData"))

# Calculate for each firm amount of fuel imported and share of imports that are fuels -----

  df_total_imports <- df_trade %>%
    filter(flow == "I") %>%        # imports only
    group_by(vat_ano, year) %>%
    summarise(
      total_imports_value_all = sum(cn_value, na.rm = TRUE),
      .groups = "drop"
    )

  df_fuel_trade <- df_trade %>%
    filter(substr(cncode, 1, 2) == "27" & substr(cncode, 1, 4) != "2716")

  df_fuel_stats <- df_fuel_trade %>%
    group_by(vat_ano, year, cncode) %>%
    summarise(
      # ---- Weight ----
      imports_weight = sum(cn_weight[flow == "I"], na.rm = TRUE),
      exports_weight = sum(cn_weight[flow == "X"], na.rm = TRUE),
      
      # ---- Value ----
      imports_value  = sum(cn_value[flow == "I"], na.rm = TRUE),
      exports_value  = sum(cn_value[flow == "X"], na.rm = TRUE),
      
      # ---- Units ----
      imports_units  = sum(cn_units[flow == "I"], na.rm = TRUE),
      exports_units  = sum(cn_units[flow == "X"], na.rm = TRUE),
      
      .groups = "drop"
    )
  
  eps <- 1e-12
  
  df_firm_year_fuel <- df_fuel_stats %>%
    left_join(df_total_imports, by = c("vat_ano", "year")) %>%
    mutate(
      fuel_share_of_imports = if_else(
        total_imports_value_all > 0,
        100 * imports_value / total_imports_value_all,
        NA_real_
      )
    ) %>% 
    group_by(vat_ano, year) %>% 
    mutate(
      fuel_importer = if_else(
        any(imports_weight > eps |
            imports_value  > eps |
            imports_units  > eps),
        1L, 0L
      )
    ) %>% 
    select(-c(exports_weight, exports_value, exports_units, total_imports_value_all, fuel_share_of_imports)) %>% 
    filter(fuel_importer == 1)
  
# Include variable that identifies EUETS firms -------
  
  load(paste0(PROC_DATA, "/firm_year_belgian_euets.RData"))
  firm_year_belgian_euets$is_euets <- 1
  
  fuel_imported_by_firm_year <- df_firm_year_fuel %>% 
    left_join(firm_year_belgian_euets %>% select(vat, year, is_euets),
              by = c("year", "vat_ano" = "vat")) %>% 
    mutate(is_euets = if_else(is.na(is_euets), 0, is_euets))
  
# Include NACE 5-digit code -------

load(paste0(PROC_DATA,"/annual_accounts_selected_sample.RData"))

fuel_imported_by_firm_year <- fuel_imported_by_firm_year %>% 
  left_join(df_annual_accounts_selected_sample %>% select(vat_ano, year, nace5d),
            by = c("year", "vat_ano"))

# Include variable that identifies if firm is in annual accounts working sample --------

load(paste0(PROC_DATA,"/firms_in_selected_sample.RData"))

firms_in_selected_sample$is_in_sample <- 1

fuel_imported_by_firm_year <- fuel_imported_by_firm_year %>% 
  left_join(firms_in_selected_sample, by = "vat_ano")
  
# Save it ------
save(fuel_imported_by_firm_year, file = paste0(PROC_DATA,"/fuel_imported_by_firm_year.RData"))
  
  
