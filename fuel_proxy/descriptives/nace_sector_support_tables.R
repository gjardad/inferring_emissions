###############################################################################
# 05_analysis/01_nace_sector_support_tables.R
#
# PURPOSE
#   Analysis-only: sector support tables within ETS and deployability to non-ETS.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/05_analysis/01_nace_sector_support_tables.R
###############################################################################

#### HEADER -------

## This code computes sector support table among EUETS firms,
# for NACE5d and NACE2d sector levels

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


# Setup ------


library(dplyr)
library(ggplot2)

# ==================
# Load data --------
# ==================

load(paste0(PROC_DATA, "/firm_year_belgian_euets.RData"))

load(paste0(PROC_DATA, "/annual_accounts_selected_sample_key_variables.RData"))

euets <- firm_year_belgian_euets %>% 
  select(vat, year, emissions) %>% 
  mutate(euets = 1)

df <- df_annual_accounts_selected_sample_key_variables %>% 
  left_join(euets, by = c("vat", "year")) %>% 
  mutate(nace2d = substr(nace5d, 1, 2))

# ==========================
# EUETS sector support ----
# ==========================

df_ets <- df %>% 
  filter(euets == 1, !is.na(emissions))

# NACE 5d

support_5d <- df_ets %>%
  group_by(nace5d) %>%
  summarise(
    n_firms = n_distinct(vat),
    n_firm_years = n(),
    .groups = "drop"
  )

support_table_5d <- support_5d %>%
  mutate(
    support_bin = cut(
      n_firm_years,
      breaks = c(0, 1, 5, 10, 25, 50, 100, Inf),
      labels = c("1", "2–5", "6–10", "11–25", "26–50", "51–100", ">100"),
      right = TRUE
    )
  ) %>%
  group_by(support_bin) %>%
  summarise(
    n_sectors = n(),
    pct_sectors = n() / n_distinct(support_5d$nace5d),
    pct_obs = sum(n_firm_years) / sum(support_5d$n_firm_years),
    .groups = "drop"
  )

# NACE 2d

support_2d <- df_ets %>%
  group_by(nace2d) %>%
  summarise(
    n_firms = n_distinct(vat),
    n_firm_years = n(),
    .groups = "drop"
  )

support_table_2d <- support_2d %>%
  mutate(
    support_bin = cut(
      n_firm_years,
      breaks = c(0, 1, 5, 10, 25, 50, 100, Inf),
      labels = c("1", "2–5", "6–10", "11–25", "26–50", "51–100", ">100"),
      right = TRUE
    )
  ) %>%
  group_by(support_bin) %>%
  summarise(
    n_sectors = n(),
    pct_sectors = n() / n_distinct(support_2d$nace2d),
    pct_obs = sum(n_firm_years) / sum(support_2d$n_firm_years),
    .groups = "drop"
  )

# ==============================================================
# Deployability of EUETS sectors into non-EUETS firms ---
# ==============================================================

# NACE 5d
df_with_support_5d <- df %>%
  left_join(
    support_5d %>% select(nace5d, n_firm_years),
    by = "nace5d"
  ) %>%
  mutate(
    support_bin = case_when(
      is.na(n_firm_years)        ~ "Unseen sector",
      n_firm_years <= 5          ~ "1–5",
      n_firm_years <= 10         ~ "6–10",
      n_firm_years <= 25         ~ "11–25",
      n_firm_years <= 50         ~ "26–50",
      n_firm_years <= 100        ~ "51–100",
      TRUE                       ~ ">100"
    )
  )

df_with_support_5d %>%
  count(support_bin) %>%
  mutate(pct_firms = n / sum(n))

rev_table_5d <- df_with_support_5d %>%
  filter(!is.na(revenue)) %>%
  group_by(support_bin) %>%
  summarise(
    total_revenue = sum(revenue),
    .groups = "drop"
  )

rev_table_5d <- rev_table_5d %>%
  mutate(pct_revenue = total_revenue / sum(total_revenue))

# NACE 2d
df_with_support_2d <- df %>%
  left_join(
    support_2d %>% select(nace2d, n_firm_years),
    by = "nace2d"
  ) %>%
  mutate(
    support_bin = case_when(
      is.na(n_firm_years)        ~ "Unseen sector",
      n_firm_years <= 5          ~ "1–5",
      n_firm_years <= 10         ~ "6–10",
      n_firm_years <= 25         ~ "11–25",
      n_firm_years <= 50         ~ "26–50",
      n_firm_years <= 100        ~ "51–100",
      TRUE                       ~ ">100"
    )
  )

df_with_support_2d %>%
  count(support_bin) %>%
  mutate(pct_firms = n / sum(n))

rev_table_2d <- df_with_support_2d %>%
  filter(!is.na(revenue)) %>%
  group_by(support_bin) %>%
  summarise(
    total_revenue = sum(revenue),
    .groups = "drop"
  )

rev_table_2d <- rev_table_2d %>%
  mutate(pct_revenue = total_revenue / sum(total_revenue))

