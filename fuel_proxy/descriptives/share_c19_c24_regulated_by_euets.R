###############################################################################
# 05_analysis/06_share_c19_c24_regulated_by_euets.R
#
# PURPOSE
#   Compute share of activity/emissions in C19 and C24 sectors covered by the EU ETS.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/05_analysis/06_share_c19_c24_regulated_by_euets.R
###############################################################################

#### HEADER -------

# Check share of value added in sectors C19, C24 which are not regulated by EUETS

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
## Setup ------
# -------------


library(tidyverse)
library(tidyr)
library(dplyr)

# ------------
# Load data --
# ------------

load(paste0(PROC_DATA,"/annual_accounts_selected_sample.RData"))

load(paste0(PROC_DATA,"/firm_year_belgian_euets.RData"))

load(paste0(PROC_DATA,"/installation_year_in_belgium.RData"))

# ----------------------------
# Clean and generate data ----
# ----------------------------

firm_year_belgian_euets$is_euets <- 1

df <- df_annual_accounts_selected_sample %>% 
  left_join(
    firm_year_belgian_euets %>% 
      select(vat, year, is_euets),
    by = c("vat_ano" = "vat", "year")
  ) %>% 
  select(
    vat_ano, year,
    turnover_VAT, v_0001023, v_0009800,
    nace5d, is_euets
  ) %>% 
  rename(
    revenue     = turnover_VAT,
    wage_bill   = v_0001023,
    value_added = v_0009800
  ) %>% 
  mutate(
    nace2d = substr(nace5d, 1, 2),
    is_euets = if_else(is.na(is_euets), 0L, is_euets)
  )

# Share of output regulated by EUETS, by sector-year
euets_shares_nace2d <- df %>%
  group_by(nace2d, year) %>%
  summarise(
    # totals
    revenue_total     = sum(revenue, na.rm = TRUE),
    value_added_total = sum(value_added, na.rm = TRUE),
    wage_bill_total   = sum(wage_bill, na.rm = TRUE),
    
    # ETS-covered totals
    revenue_euets     = sum(revenue[is_euets == 1], na.rm = TRUE),
    value_added_euets = sum(value_added[is_euets == 1], na.rm = TRUE),
    wage_bill_euets   = sum(wage_bill[is_euets == 1], na.rm = TRUE),
    
    # shares
    share_revenue_euets =
      revenue_euets / revenue_total,
    
    share_value_added_euets =
      value_added_euets / value_added_total,
    
    share_wage_bill_euets =
      wage_bill_euets / wage_bill_total,
    
    .groups = "drop"
  )

# Number of firms regulated by EUETS, by sector-year
firms_by_sector_year <- df %>%
  group_by(nace2d, year) %>%
  summarise(
    n_firms_total = n_distinct(vat_ano),
    n_firms_euets = n_distinct(vat_ano[is_euets == 1]),
    n_firms_non_euets = n_distinct(vat_ano[is_euets == 0]),
    share_firms_euets = n_firms_euets / n_firms_total,
    .groups = "drop"
  ) %>%
  arrange(nace2d, year)

# Number of installations regulated by EUETS, by sector-year
# 1) Build firm-year NACE5d evidence from annual accounts (unique vat_ano-year)
aa_nace <- df_annual_accounts_selected_sample %>%
  transmute(
    vat_ano = str_trim(as.character(vat_ano)),
    year,
    nace5d = str_trim(as.character(nace5d))
  ) %>%
  mutate(
    nace5d = na_if(nace5d, ""),
    vat_ano = na_if(vat_ano, "")
  ) %>%
  distinct(vat_ano, year, nace5d) %>%
  group_by(vat_ano, year) %>%
  summarise(nace5d = first(na.omit(nace5d)), .groups = "drop")

# 2) Join installation-year info to firm-year nace5d
inst_with_nace <- installation_year_in_belgium %>%
  mutate(
    vat_ano = str_trim(as.character(vat_ano)),
    nace_id = str_trim(as.character(nace_id)),
    installation_id = str_trim(as.character(installation_id))
  ) %>%
  left_join(aa_nace, by = c("vat_ano", "year")) %>%
  mutate(
    # NACE2d from installation-level nace_id
    nace2d_from_install = if_else(!is.na(nace_id) & nchar(nace_id) >= 3,
                                  substr(nace_id, 1, 2),
                                  NA_character_),
    
    # NACE2d from firm accounts nace5d (same logic)
    nace2d_from_accounts = if_else(!is.na(nace5d),
                                   substr(nace5d, 1, 2),
                                   NA_character_)
  )

install_counts_by_sector_year <- inst_with_nace %>%
  select(installation_id, year, nace2d_from_install, nace2d_from_accounts) %>%
  pivot_longer(
    cols = c(nace2d_from_install, nace2d_from_accounts),
    names_to = "classification",
    values_to = "nace2d"
  ) %>%
  filter(!is.na(nace2d)) %>%
  group_by(classification, nace2d, year) %>%
  summarise(
    n_installations = n_distinct(installation_id),
    .groups = "drop"
  ) %>%
  arrange(classification, nace2d, year)


