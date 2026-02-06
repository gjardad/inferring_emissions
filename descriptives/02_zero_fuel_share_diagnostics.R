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
# 05_analysis/02_zero_fuel_share_diagnostics.R
#
# PURPOSE
#   Analysis-only: diagnostics on share of firms with zero fuel consumption by ETS status.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/05_analysis/02_zero_fuel_share_diagnostics.R
###############################################################################

#### HEADER -------

## This code computes share of firm-year obs with fuel cost share = 0

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

library(dplyr)
library(ggplot2)
library(scales)

# =========================
# Load data ---------------
# =========================

load(paste0(proc_data, "/fuel_input_cost_share.RData"))

# =========================================================
# Share with zero emissions by sector x EUETS status-------
# =========================================================

zero_share <- fuel_input_cost_share %>%
  filter(year >= 2005) %>% 
  mutate(nace2d = substr(nace5d, 1, 2),
    is_zero = !is.na(fuel_share_costs) & fuel_share_costs == 0
  ) %>%
  group_by(nace2d, euets) %>%
  summarise(
    n = sum(!is.na(fuel_share_costs)),
    share_zero = mean(is_zero[!is.na(fuel_share_costs)]),
    .groups = "drop"
  )

# ========================================================
# Diagnose EUETS firms with zero fuel cost share ---------
# ========================================================

euets_zero_fuel <- fuel_input_cost_share %>%
  filter(year >= 2005,
    euets == 1,
    fuel_share_costs == 0
  )

# How many such obs?
nrow(euets_zero_fuel)
# should be 17

# Check emissions
euets_zero_fuel %>%
  summarise(
    n_obs = n(),
    n_emit_positive = sum(emissions > 0, na.rm = TRUE),
    share_emit_positive = mean(emissions > 0, na.rm = TRUE),
    min_emissions = min(emissions, na.rm = TRUE),
    max_emissions = max(emissions, na.rm = TRUE)
  )

# =================================
# Compute moving average ----------
# =================================

# 2-year MA
df_ma <- fuel_input_cost_share %>%
  arrange(vat, year) %>%
  group_by(vat) %>%
  mutate(
    fuel_share_costs_lag1 = lag(fuel_share_costs),
    fuel_share_costs_biavg = rowMeans(
      cbind(fuel_share_costs, fuel_share_costs_lag1),
      na.rm = FALSE   # IMPORTANT: require both t and t-1
    )
  ) %>%
  ungroup()

df_ma %>%
  filter(
    year >= 2005,
    euets == 1,
    !is.na(fuel_share_costs_biavg)
  ) %>%
  summarise(
    n_obs = n(),
    n_zero_biavg = sum(fuel_share_costs_biavg == 0),
    share_zero_biavg = mean(fuel_share_costs_biavg == 0)
  )

df_ma %>%
  filter(
    year >= 2005,
    euets == 1,
    fuel_share_costs_biavg == 0
  ) %>%
  summarise(
    n_obs = n(),
    n_emit_positive = sum(emissions > 0, na.rm = TRUE),
    share_emit_positive = mean(emissions > 0, na.rm = TRUE),
    min_emissions = min(emissions, na.rm = TRUE),
    max_emissions = max(emissions, na.rm = TRUE)
  )

df_ma %>%
  filter(
    year >= 2005,
    euets == 0,
    !is.na(fuel_share_costs_biavg)
  ) %>%
  summarise(
    n_obs = n(),
    n_zero_biavg = sum(fuel_share_costs_biavg == 0),
    share_zero_biavg = mean(fuel_share_costs_biavg == 0)
  )

# 3-year MA
df_ma3 <- fuel_input_cost_share %>%
  arrange(vat, year) %>%
  group_by(vat) %>%
  mutate(
    fuel_share_lag1 = lag(fuel_share_costs, 1),
    fuel_share_lag2 = lag(fuel_share_costs, 2),
    fuel_share_ma3 = rowMeans(
      cbind(fuel_share_costs, fuel_share_lag1, fuel_share_lag2),
      na.rm = FALSE
    )
  ) %>%
  ungroup()

df_ma3 %>%
  filter(
    year >= 2005,
    euets == 1,
    !is.na(fuel_share_ma3)
  ) %>%
  summarise(
    n_obs = n(),
    n_zero_ma3 = sum(fuel_share_ma3 == 0),
    share_zero_ma3 = mean(fuel_share_ma3 == 0)
  )

df_ma3 %>%
  filter(
    year >= 2005,
    euets == 1,
    fuel_share_ma3 == 0
  ) %>%
  summarise(
    n_obs = n(),
    n_emit_positive = sum(emissions > 0, na.rm = TRUE),
    share_emit_positive = mean(emissions > 0, na.rm = TRUE),
    min_emissions = min(emissions, na.rm = TRUE),
    max_emissions = max(emissions, na.rm = TRUE)
  )

df_ma3 %>%
  filter(
    year >= 2005,
    euets == 0,
    !is.na(fuel_share_ma3)
  ) %>%
  summarise(
    n_obs = n(),
    n_zero_ma3 = sum(fuel_share_ma3 == 0),
    share_zero_ma3 = mean(fuel_share_ma3 == 0)
  )


