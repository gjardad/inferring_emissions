###############################################################################
# 05_analysis/05_nace_sectors_totally_covered_by_euets.R
#
# PURPOSE
#   Identify NACE sectors whose emissions are entirely covered by the EU ETS.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/05_analysis/05_nace_sectors_totally_covered_by_euets.R
###############################################################################

#### HEADER -------

# Identify NACE codes for which 100% of emissions are covered by the EUETS

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


# -------------
# Set up -----
# -------------


library(tidyverse)
library(tidyr)
library(dplyr)

# -------------
# Load data ---
# -------------

# Annexx XII 2024 and 2025
library(readxl)
annexx_xii_2024 <- read_excel(paste0(RAW_DATA, "/NIR/BE_2024_Art14_AnnexXII_Consistency_with_ETS_280224.xlsx")) %>% 
  select(1,4) %>% 
  rename(crf = 1, share = 2) %>% 
  filter(!is.na(crf) & !is.na(share)) %>% 
  filter(share != "NO") %>% 
  separate(crf, into = c("crf_code", "crf_text"), sep = " ",
           extra = "merge", remove = TRUE) %>% 
  filter(!crf_code %in% c("Greenhouse", "CO2", "Iron")) %>% 
  mutate(share_2024 = as.numeric(share)*100) %>% 
  select(crf_code, crf_text, share_2024)

annexx_xii_2025 <- read_excel(paste0(RAW_DATA, "/NIR/BE_2025_Art14_Annex_XII-Consistency_with_ETS_2025_1281_Final_130325.xlsx")) %>% 
  select(1,4) %>% 
  rename(crf = 1, share = 2) %>% 
  filter(!is.na(crf) & !is.na(share)) %>% 
  separate(crf, into = c("crf_code", "crf_text"), sep = " ",
           extra = "merge", remove = TRUE) %>% 
  filter(!crf_code %in% c("Greenhouse", "CO2", "Iron")) %>%
  mutate(share = if_else(share == "NA", "", share)) %>% 
  mutate(share_2025 = as.numeric(share)) %>% 
  select(-c(share))

# CRF to NACE map
load(paste0(PROC_DATA, "crf_to_nace_map.RData"))

# --------------------
# Create data set ----
# --------------------

shares <- annexx_xii_2025 %>% 
  left_join(annexx_xii_2024, by = c("crf_code", "crf_text"))

shares_clean <- shares %>%
  mutate(
    crf_code_clean = str_remove(str_trim(crf_code), "\\.+$")  # drop trailing dots
  )

map_clean <- crf_to_nace_map %>%
  mutate(
    crf_clean = str_remove(str_trim(crf), "\\.+$")
  )

crf_nace <- map_clean %>%
  left_join(
    shares_clean %>% select(crf_code_clean, share_2025, share_2024),
    by = c("crf_clean" = "crf_code_clean")
  )

group_crf <- crf_nace %>%
  distinct(nace_group, crf, share_2025)

group_coverage <- group_crf %>%
  group_by(nace_group) %>%
  summarise(
    min_share = min(share_2025, na.rm = TRUE),
    fully_covered = (min_share == 100)
  )

nace_coverage <- crf_nace %>%
  distinct(nace_code, nace_group) %>%
  left_join(group_coverage, by = "nace_group")
