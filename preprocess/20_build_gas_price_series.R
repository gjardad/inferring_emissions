###############################################################################
# 01_preprocess/20_build_gas_price_series.R
#
# PURPOSE
#   Create annual gas price series (EUR/MMBtu) from Energy Institute Statistical Review data + ECB USD/EUR exchange rates.
#
# INPUTS
#   - data/raw/ecb_usd_eur.xml
#
# OUTPUTS
#   - data/processed/gas_price.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/20_build_gas_price_series.R
###############################################################################

#### HEADER -------

## Code creates data set with yearly natural gas prices

# Data directly retrieved from the Statistical Review of World Energy 2025,
# by the Energy Institute. I collect the column referring to the Netherlands TTF
# price of natural gas. The original source is
# the S&P Global Commodity Insights, ©2025 by S&P Global Inc

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


# ------------
# Set up -----
# ------------


library(tibble)

# -------------
# Load data ---
# -------------

## Exchange rate -----

library(xml2)
xml <- read_xml(paste0(RAW_DATA, "/ecb_usd_eur.xml"))

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

## Natural gas data ------

gas_price <- tibble(
  year = 2005:2024,
  price_usd_per_mmbtu = c(
    5.81,  # 2005
    7.26,  # 2006
    5.90,  # 2007
    10.67, # 2008
    4.91,  # 2009
    6.52,  # 2010
    9.00,  # 2011
    9.42,  # 2012
    10.60, # 2013
    8.22,  # 2014
    6.52,  # 2015
    4.65,  # 2016
    5.78,  # 2017
    7.97,  # 2018
    4.46,  # 2019
    3.17,  # 2020
    15.45, # 2021
    24.55, # 2022
    12.30, # 2023
    10.69  # 2024
  )
)

gas_price <- gas_price %>% 
  left_join(fx_annual %>% select(year, eur_per_usd), by = "year") %>% 
  mutate(eur_per_mmbtu = price_usd_per_mmbtu*eur_per_usd)

# save it
save(gas_price, file = paste0(PROC_DATA,"/gas_price.RData"))
