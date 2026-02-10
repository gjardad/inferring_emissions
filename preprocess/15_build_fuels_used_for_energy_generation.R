###############################################################################
# 01_preprocess/15_build_fuels_used_for_energy_generation.R
#
# PURPOSE
#   Prepare fuel lists / shares used for energy generation (aggregate), used for sectoral fuel-mix logic and validations.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/15_build_fuels_used_for_energy_generation.R
###############################################################################

#### HEADER -------

# Which fuels are burned and emit CO_2 in Belgium?

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


# -------------
## Setup ------
# -------------


library(tidyverse)
library(tidyr)
library(dplyr)

# --------------------
#  Load data ---------
# --------------------

fuels_for_energy_use <- read_csv(paste0(RAW_DATA, "/Eurostat/fuels_for_energy_use_in_belgium_from_energy_balance.csv"))

df <- fuels_for_energy_use %>% 
  mutate(
    siec_code = str_trim(str_extract(siec, "^[^:]+")),
    siec_name = str_trim(str_extract(siec, "(?<=:).*")),
    use = str_trim(str_extract(nrg_bal, "^[^:]+"))
  ) %>% 
  select(siec_code, siec_name, TIME_PERIOD, OBS_VALUE, use) %>% 
  rename(year = TIME_PERIOD, obs_value = OBS_VALUE) %>% 
  # exclude total, bioenergy, renewables, biofuels, electricity, heat, nuclear, and waste
  filter(!substr(siec_code, 1, 2) %in% c("RA", "R5", "TO", "BI", "W6", "H8", "E7", "FE", "N9")) %>% 
  filter(!siec_code %in% c("C0000X0350-0370", "C0350-0370", "O4000XBIO", "O4100_TOT"))

siec_used_for_energy <- df %>%
  group_by(siec_code, siec_name) %>%
  summarise(any_positive = any(obs_value > 0, na.rm = TRUE), .groups = "drop") %>%
  filter(any_positive) %>%
  distinct(siec_code, siec_name)


# convert to HS codes ----

load(paste0(PROC_DATA,"/hs_to_siec_map.RData"))

hs_used_for_energy <- siec_used_for_energy %>% 
  
