###############################################################################
# 01_preprocess/22_build_oil_price_series.R
#
# PURPOSE
#   Create annual Brent oil price series (and currency conversion if included) used for customs implied-price diagnostics / quantity imputation.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/22_build_oil_price_series.R
###############################################################################

#### HEADER -------

## Code creates data set with yearly Brent oil prices

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

oil_quarterly <- read_csv(paste0(raw_data, "/crude_oil_bfo_m2_europe_fob_euro_per_barrel_quarterly.csv")) %>% 
  rename(eur_per_bbl = 3)

library(lubridate)
oil_price <- oil_quarterly %>%
  mutate(
    date = as.Date(DATE),
    year = lubridate::year(date)
  ) %>%
  group_by(year) %>%
  summarise(
    n_quarters = sum(!is.na(eur_per_bbl)),
    eur_per_bbl = mean(eur_per_bbl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

# save it
save(oil_price, file = paste0(proc_data,"/oil_price.RData"))
