# ============== BEGIN SETTING UP PATHS ============= #
suppressPackageStartupMessages({
  library(data.table)
})

# ========================
# Define data paths ------
# =========================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

# ===========================
# Define paths for code -----
# ===========================

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}

repo_dir <- paste0(getwd(), "/inferring_emissions")
utils_dir <- file.path(repo_dir, "utils")
loocv_dir <- file.path(repo_dir, "loocv")

#================== END SETTING UP PATHS ================ #

###############################################################################
# 01_preprocess/21_build_coal_price_series.R
#
# PURPOSE
#   Create annual coal price series (EUR/ton) from Energy Institute Statistical Review data + ECB USD/EUR exchange rates.
#
# INPUTS
#   - data/raw/ecb_usd_eur.xml
#
# OUTPUTS
#   - data/processed/coal_price.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/21_build_coal_price_series.R
###############################################################################

#### HEADER -------

## Code creates data set with yearly coal prices

# Data directly retrieved from the Statistical Review of World Energy 2025,
# by the Energy Institute. I collect the column referring to the Northwest Europe
# price of coal, which correspond to IHS Northwest Europe average weekly prices
# for 2000-2018, and for 2019 onwards the monthly average prices of
# S&P Global Commodity Insights S&P Global Inc Thermal Coal CIF ARA 6,000 kcal/kg NAR

#####################

# ------------
# Set up -----
# ------------

rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

library(tibble)

# -------------
# Load data ---
# -------------

library(xml2)
library(dplyr)
library(lubridate)
library(readr)

## Exchange rate -----

library(xml2)
xml <- read_xml(paste0(raw_data, "/ecb_usd_eur.xml"))

obs_nodes <- xml_find_all(xml, ".//*[local-name()='Obs']")

length(obs_nodes)  # should be > 0

fx_daily <- tibble(
  date       = ymd(xml_attr(obs_nodes, "TIME_PERIOD")),
  usd_per_eur = as.numeric(xml_attr(obs_nodes, "OBS_VALUE")),
  obs_status = xml_attr(obs_nodes, "OBS_STATUS"),
  obs_conf   = xml_attr(obs_nodes, "OBS_CONF")
) %>%
  filter(!is.na(date), !is.na(usd_per_eur))

fx_annual <- fx_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    usd_per_eur = mean(usd_per_eur, na.rm = TRUE),
    eur_per_usd = 1 / usd_per_eur,
    .groups = "drop"
  )

## Coal data ----------

coal_price <- tibble(
  year = c(
    1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995,
    1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
    2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
    2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022,
    2023, 2024
  ),
  price_usd_per_ton = c(
    31.30, 39.94, 42.08, 43.48, 42.80, 38.53, 33.68, 37.18, 44.50,
    41.25, 38.92, 32.00, 28.79, 35.99, 39.03, 31.65, 43.60, 72.13,
    60.54, 64.11, 88.79, 147.67, 70.39, 92.50, 121.48, 92.50, 81.69,
    75.38, 56.79, 60.09, 84.51, 91.83, 60.17, 50.13, 122.60, 291.28,
    129.54, 112.00
  )
)

coal_price <- coal_price %>% 
  left_join(fx_annual %>% select(year, eur_per_usd), by = "year") %>% 
  mutate(eur_per_ton = price_usd_per_ton*eur_per_usd)

# save it
save(coal_price, file = paste0(proc_data,"/coal_price.RData"))
