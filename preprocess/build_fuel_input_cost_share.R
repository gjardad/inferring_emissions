###############################################################################
# 01_preprocess/05_build_fuel_input_cost_share.R
#
# PURPOSE
#   Build firm-year fuel spending and fuel cost share dataset used by proxies.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/05_build_fuel_input_cost_share.R
###############################################################################

###############################################################################
# 01_preprocess/05_build_fuel_input_cost_share.R
#
# PURPOSE
#   Build firm-year fuel input cost shares used in proxy diagnostics and
#   extensive-margin analysis.
#
# LOGIC
#   - Merge fuel spending, domestic input costs, imports, and EU ETS status
#   - Define total costs = input_cost + imports
#   - Compute fuel_share_costs = fuel_spend / total_costs
#
# INPUTS (from data/processed)
#   - amount_spent_on_fuel_by_firm_year.RData
#   - firm_year_belgian_euets.RData
#   - firm_year_domestic_input_cost.RData
#   - firm_year_total_imports.RData
#   - annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS (to data/processed)
#   - fuel_input_cost_share.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/05_build_fuel_input_cost_share.R
###############################################################################

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



suppressPackageStartupMessages({
  library(dplyr)
})

# ---- Load inputs ----
load(file.path(DATA_PROC, "amount_spent_on_fuel_by_firm_year.RData"))
load(file.path(DATA_PROC, "firm_year_belgian_euets.RData"))
load(file.path(DATA_PROC, "firm_year_domestic_input_cost.RData"))
load(file.path(DATA_PROC, "firm_year_total_imports.RData"))
load(file.path(DATA_PROC, "annual_accounts_selected_sample_key_variables.RData"))

fuel <- amount_spent_on_fuel_by_firm_year %>%
  rename(
    vat = vat_j_ano,
    fuel_spend = amount_spent_on_fuel_excl_euets_importers
  )

euets_flag <- firm_year_belgian_euets %>%
  mutate(euets = 1L) %>%
  select(vat, year, euets, emissions)

domestic_costs <- firm_year_domestic_input_cost %>%
  select(vat, year, input_cost)

imports <- firm_year_total_imports %>%
  rename(vat = vat_ano) %>%
  select(vat, year, total_imports)

fuel_input_cost_share <- fuel %>%
  left_join(domestic_costs, by = c("vat", "year")) %>%
  left_join(imports,       by = c("vat", "year")) %>%
  left_join(euets_flag,    by = c("vat", "year")) %>%
  mutate(
    euets = coalesce(euets, 0L),
    total_costs = input_cost + coalesce(total_imports, 0),
    fuel_share_costs = if_else(
      !is.na(total_costs) & total_costs > 0,
      fuel_spend / total_costs,
      NA_real_
    ),
    fuel_positive = coalesce(fuel_spend, 0) > 0
  ) %>%
  left_join(
    df_annual_accounts_selected_sample_key_variables %>%
      select(vat, year, nace5d),
    by = c("vat", "year")
  )

# ---- Save ----
save(fuel_input_cost_share,
     file = file.path(DATA_PROC, "fuel_input_cost_share.RData"))

message("Saved fuel_input_cost_share with ",
        nrow(fuel_input_cost_share), " rows.")
