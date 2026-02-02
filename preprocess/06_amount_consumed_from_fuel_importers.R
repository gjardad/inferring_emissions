###############################################################################
# 01_preprocess/06_amount_consumed_from_fuel_importers.R
#
# PURPOSE
#   Compute firm-year monetary amounts purchased from identified fuel importers.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/06_amount_consumed_from_fuel_importers.R
###############################################################################

#### HEADER -------

## This code creates data set at the firm-year level with the following variables:

  # 1. amount bought from fuel importers
  # 2. amount of fuel imported
  # 3. EUETS dummy
  # 4. emissions from EUETS
  # 5. NACE5d code from annual accounts

#####################

## Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Import data --------

  #load(paste0(proc_data, "/b2b_selected_random_sample.RData"))
  #df_b2b <- df_b2b_selected_random_sample

  load(paste0(proc_data, "/b2b_selected_sample.RData"))
  df_b2b <- df_b2b_selected_sample
  # j is buyer, i is supplier
  
  load(paste0(proc_data, "/fuel_imported_by_firm_year.RData"))
  
  load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
  
### Identify fuel importers and their EU ETS status -------
  
  # EUETS list
  euets_firms <- firm_year_belgian_euets %>%
    select(vat) %>%
    distinct()
  
  # Fuel importers
  fuel_importers <- fuel_imported_by_firm_year %>%
    mutate(vat = vat_ano) %>%
    group_by(vat, year) %>%   # in case multiple cn codes exist
    summarise(fuel_importer = max(fuel_importer),
              is_euets = max(is_euets),
              imports_value = sum(imports_value, na.rm = TRUE),
              .groups = "drop")

### Tag buyers (vat_j_ano) with importer_in_euets -------
  
  library(tidyr)

  buyers_status <- fuel_importers %>%
    mutate(
      is_in_euets_list = vat %in% euets_firms$vat,
      is_importer_in_euets =
        as.integer(fuel_importer == 1 & is_in_euets_list)
    ) %>%
    mutate(
      is_importer_in_euets = tidyr::replace_na(is_importer_in_euets, 0)
    ) %>%
    select(vat_j_ano = vat, year, is_importer_in_euets, imports_value)
  
### Identify which fuel suppliers are EU ETS -------
  
  fuel_suppliers_non_euets <- fuel_importers %>%
    filter(fuel_importer == 1, is_euets == 0) %>%
    select(vat_i_ano = vat) %>%
    distinct()
  
  fuel_suppliers_euets <- fuel_importers %>%
    filter(fuel_importer == 1, is_euets == 1) %>%
    select(vat_i_ano = vat) %>%
    distinct()

####  Compute sales from non-EUETS and EUETS fuel suppliers to each buyer ----
  
  purchases_from_non_euets <- df_b2b %>%
    inner_join(fuel_suppliers_non_euets, by = "vat_i_ano") %>%
    group_by(vat_j_ano, year) %>%
    summarise(purchases_from_non_euets = sum(corr_sales_ij, na.rm = TRUE),
              .groups = "drop")
  
  purchases_from_euets <- df_b2b %>%
    inner_join(fuel_suppliers_euets, by = "vat_i_ano") %>%
    group_by(vat_j_ano, year) %>%
    summarise(purchases_from_euets_importers = sum(corr_sales_ij, na.rm = TRUE),
              .groups = "drop")
  
##### Generate data set with amount spent in fuel ------
  
  df_with_amount_spent_on_fuel <- df_b2b %>%
    select(vat_j_ano, year) %>%
    distinct() %>%
    left_join(buyers_status, by = c("vat_j_ano", "year")) %>%
    left_join(purchases_from_non_euets, by = c("vat_j_ano", "year")) %>%
    left_join(purchases_from_euets,     by = c("vat_j_ano", "year")) %>%
    left_join(firm_year_belgian_euets %>% select(vat, year, emissions, nace5d),
              by = c("vat_j_ano" = "vat", "year")) %>%
    mutate(
      purchases_from_non_euets       = replace_na(purchases_from_non_euets, 0),
      purchases_from_euets_importers = replace_na(purchases_from_euets_importers, 0),
      imports_value                  = replace_na(imports_value, 0),
      is_importer_in_euets           = replace_na(is_importer_in_euets, 0),
      emissions = replace_na(emissions, 0),
      
      amount_spent_on_fuel =
        if_else(is_importer_in_euets == 1,
                purchases_from_non_euets + purchases_from_euets_importers + imports_value,
                purchases_from_non_euets + purchases_from_euets_importers),
      
      amount_spent_on_fuel_excl_euets_importers = amount_spent_on_fuel - purchases_from_euets_importers
    )
  
  amount_spent_on_fuel_by_firm_year <- df_with_amount_spent_on_fuel
  
# Save it -----
save(amount_spent_on_fuel_by_firm_year, file = paste0(proc_data, "/amount_spent_on_fuel_by_firm_year.RData"))
  
  
  
