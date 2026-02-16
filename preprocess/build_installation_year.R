###############################################################################
# 01_preprocess/04_build_installation_year.R
#
# PURPOSE
#   Prepare installation-year panel used to aggregate EU ETS installations to firms.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/04_build_installation_year.R
###############################################################################

#### HEADER -------

## This code creates data set at the installation-year level with info on
# 1. emissions
# 2. activity ids
# 3. NACE codes
# 4. country codes
# 5. number of allowances allocated for free
# 6. number of total allowances allocated

#####################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))


## Setup ------


# Libraries ----

library(tidyverse)
library(dplyr)

## Import data ------

df_installation <- read.csv(paste0(RAW_DATA,"/EUTL/Oct_2024_version/installation.csv"))

df_compliance <- read.csv(paste0(RAW_DATA,"/EUTL/Oct_2024_version/compliance.csv"))

library(haven)
df_belgium_euets <- read_dta(paste0(RAW_DATA,"/NBB/EUTL_Belgium.dta")) %>% 
  rename(bvd_id = bvdid, firm_id = companyregistrationnumber)

df_account <- read.csv(paste0(RAW_DATA,"/EUTL/Oct_2024_version/account.csv"))

## Clean data -------

df_installation <- df_installation %>% 
  rename(installation_id = id) %>% 
  select(c(installation_id, country_id, activity_id, nace_id)) %>% 
  filter(nace_id != 51.00) # exclude aviation

installation_year_emissions <- df_installation %>% 
  left_join(df_compliance, by = "installation_id") %>% 
  select(c(installation_id, year, country_id, activity_id, nace_id,
           allocatedFree, allocatedTotal, verified)) %>% 
  filter(year <= 2023) %>% 
  mutate(allocatedFree = ifelse(is.na(allocatedFree),0, allocatedFree),
         allocatedTotal = ifelse(is.na(allocatedTotal),0, allocatedTotal),
         verified = ifelse(is.na(verified),0, verified)) %>% 
  # make activity_ids consistent
    # in the first two phases of EUETS installations were classified into 1-10 activity ids
    # in phase 3 onwards, installations were classified into 20-99
    # crosswalk between the two is in Table C1 in Abrell's documentation of EUETS.INFO
  mutate(activity_id = case_when(
    activity_id >= 1 & activity_id <= 5 ~ activity_id + 19,
    activity_id == 6 ~ 29, # ambiguous: activity 6 could become 29 or 30
    activity_id == 7 ~ 31,
    activity_id == 8 ~ 32,
    activity_id == 9 ~ 35, # ambiguous: activity 9 could become 35 or 36
    TRUE ~ activity_id  # Leave unchanged if it doesn't match any condition
  )) %>% 
  mutate(nace_id = ifelse(str_detect(nace_id, "^[0-9]\\."),
                          paste0("0", nace_id),
                          nace_id))

# ----------------------------------
# Add info on Belgian firms' VAT ---
# ----------------------------------

df_account <- df_account %>% 
  rename(account_id = id, account_type = accountType_id, bvd_id = bvdId,
         firm_id = companyRegistrationNumber) %>% 
  select(account_id, account_type, bvd_id, installation_id, firm_id) %>% 
  filter(account_type %in% c("100-7","120-0"))

# unique bvd_id, vat in Belgium
unique_id <- df_belgium_euets %>% 
  select(bvd_id, vat_ano) %>% 
  distinct()

installation_year_in_belgium <- installation_year_emissions %>% 
  inner_join(df_account %>% select(bvd_id, installation_id),
            by = "installation_id") %>% 
  distinct() %>%
  left_join(unique_id, by = "bvd_id") %>%
  filter(substr(installation_id, 1, 2) == "BE")

# clean duplicates and keep the ones for which we have BvD id
installation_year_in_belgium <- installation_year_in_belgium %>%
  mutate(
    # standardize to character + trim
    bvd_id  = str_trim(as.character(bvd_id)),
    vat_ano = str_trim(as.character(vat_ano)),
    
    # treat empty / whitespace / "NA" as missing
    bvd_id  = na_if(bvd_id, ""),
    vat_ano = na_if(vat_ano, ""),
    bvd_id  = na_if(bvd_id, "NA"),
    vat_ano = na_if(vat_ano, "NA"),
    
    has_bvd = !is.na(bvd_id),
    has_vat = !is.na(vat_ano)
  ) %>%
  arrange(installation_id, year, desc(has_bvd), desc(has_vat)) %>%
  distinct(installation_id, year, .keep_all = TRUE) %>%
  select(-has_bvd, -has_vat)

## Save it ------
save(installation_year_emissions, file = paste0(PROC_DATA,"/installation_year_emissions.RData"))
save(installation_year_in_belgium, file = paste0(PROC_DATA,"/installation_year_in_belgium.RData"))

  
