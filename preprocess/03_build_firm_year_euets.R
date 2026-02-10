###############################################################################
# 01_preprocess/03_build_firm_year_euets.R
#
# PURPOSE
#   Construct firm-year EU ETS indicator and emissions by aggregating installation-level EU ETS data.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/03_build_firm_year_euets.R
###############################################################################

#### HEADER -------

## This code creates data set at the firm-year level with info on
# 1. emissions
# 2. BvD id
# 3. NACE code
# 4. Belgian VAT
# 5. labor input
# 6. capital input
# 7. revenue
# 8. emissions productivity measure
# 9. labor productivity measure
# 10. capital productivity measure
# 11. total input cost
# 12. allowance shortage
# 13. whether firm belongs to sample from annual accounts
# 14. value added

# for all firms in Belgium treated by the EUETS

# Obs:

# 1. firm_id and bvd_id are not 1:1 matching. There are more distinct firm_ids than
# there are bvd_ids. This is because there are some firm_ids which contain "0" in front and some that
# don't, even in cases when the underlying identifier is the same
# (e.g. firm_ids 0419052173 and 419052173 are matched with the same bvd_id)
# also, there are multiple firm_ids for which bvd_id is missing (and therefore belongs to the category
# of firms for which firm_ids are different but bvd_id is the same, in particular bvd_id == "")

# 2. firm_ids from firm_year_emissions for which country_id is Belgian is a smaller number than the
# firm_ids from df_belgium_euets. the former is a subset of the latter. 

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
rm(list = ls())

if(Sys.info()[["user"]] =="JARDANG"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data -------

library(haven)
df_belgium_euets <- read_dta(paste0(raw_data,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

load(paste0(proc_data, "/firm_year_emissions.RData"))

df_national_accounts <- read_dta(paste0(raw_data,"/NBB/Annual_Accounts_MASTER_ANO.dta"))

load(paste0(proc_data,"/annual_accounts_selected_sample.RData"))

# Create df ------

firm_year_belgian_euets <- firm_year_emissions %>% 
  left_join(df_belgium_euets %>% select(bvd_id, vat_ano), by = "bvd_id") %>% 
  filter(!is.na(vat_ano)) %>% 
  distinct() %>% 
  left_join(df_national_accounts %>%  select(vat_ano, year, 
                                             v_0022_27,turnover_VAT, v_0001023,
                                             v_0001003, v_0009800, nace5d),
            by = c("vat_ano", "year")) %>% 
  rename(vat = vat_ano, capital = v_0022_27, revenue = turnover_VAT,
         fte = v_0001003, wage_bill = v_0001023, value_added = v_0009800) %>% 
  mutate(emissions_prod = ifelse(revenue == 0 | emissions == 0, 0, log(revenue/emissions)),
         fte_prod = ifelse(revenue == 0 | fte == 0 , 0, log(revenue/fte)),
         labor_prod = ifelse(revenue == 0 | wage_bill == 0 , 0, log(revenue/wage_bill)),
         capital_prod = ifelse(revenue == 0 | capital == 0, 0, log(revenue/capital)),
         nace5d = as.character(nace5d)) %>%
  mutate(allowance_shortage = emissions - allocated_free,
         shortage_prod = ifelse(revenue == 0 | allowance_shortage == 0, 0, log(revenue/allowance_shortage)))

  # add dummy for whether it is part of sample from annual accounts
  sample_dummy <- df_annual_accounts_selected_sample %>% 
    select(vat_ano, year) %>% 
    mutate(in_sample = 1)
  
  firm_year_belgian_euets <- firm_year_belgian_euets %>% 
    left_join(sample_dummy,
              by = c("vat" = "vat_ano", "year"))

  # clean duplicates (different firmid but are actually the same obs)
  firm_year_belgian_euets <- firm_year_belgian_euets %>% 
    distinct(vat, year, bvd_id, .keep_all = TRUE)

# Save it -------
save(firm_year_belgian_euets, file = paste0(proc_data,"/firm_year_belgian_euets.RData"))  
