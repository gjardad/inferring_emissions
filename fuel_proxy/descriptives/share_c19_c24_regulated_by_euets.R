###############################################################################
# fuel_proxy/descriptives/share_c19_c24_regulated_by_euets.R
#
# PURPOSE
#   Show that non-ETS firms in sectors C19 and C24 represent a meaningful
#   share of economic activity, motivating the use of non-regulated firms
#   for out-of-sample prediction.
#
#   Table contents (for C19 and C24):
#     1) Number of distinct firms: ETS vs non-ETS
#     2) Revenue share: ETS vs non-ETS
#     3) Value-added share: ETS vs non-ETS
#     4) Median firm revenue: ETS vs non-ETS (shows non-ETS are not tiny)
#
# INPUTS
#   - PROC_DATA/annual_accounts_selected_sample_key_variables.RData
#   - PROC_DATA/firm_year_belgian_euets.RData
#
# OUTPUTS (to OUTPUT_DIR)
#   - ets_share_c19_c24.tex
###############################################################################

# ====================
# Define paths -------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# ==================
# Load data --------
# ==================

load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))
load(file.path(PROC_DATA, "firm_year_belgian_euets.RData"))

euets_flag <- firm_year_belgian_euets %>%
  mutate(euets = 1L) %>%
  select(vat, year, euets)

df <- df_annual_accounts_selected_sample_key_variables %>%
  left_join(euets_flag, by = c("vat", "year")) %>%
  mutate(
    euets  = coalesce(euets, 0L),
    nace2d = substr(nace5d, 1, 2),
    group  = if_else(euets == 1, "ETS", "non-ETS")
  ) %>%
  filter(nace2d %in% c("19", "24"))


# =====================================================================
# 1) FIRM COUNTS
# =====================================================================

firm_counts <- df %>%
  group_by(nace2d, group) %>%
  summarise(n_firms = n_distinct(vat), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = n_firms, values_fill = 0) %>%
  mutate(total = ETS + `non-ETS`,
         pct_non_ets = round(`non-ETS` / total * 100, 1)) %>%
  rename(sector = nace2d)


# =====================================================================
# 2) REVENUE AND VALUE-ADDED SHARES (pooled across years)
# =====================================================================

shares <- df %>%
  group_by(nace2d, group) %>%
  summarise(
    total_revenue     = sum(revenue, na.rm = TRUE),
    total_value_added = sum(value_added, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(nace2d) %>%
  mutate(
    revenue_share = round(total_revenue / sum(total_revenue) * 100, 1),
    va_share      = round(total_value_added / sum(total_value_added) * 100, 1)
  ) %>%
  ungroup()


# =====================================================================
# 3) MEDIAN FIRM REVENUE (across firm-years)
# =====================================================================

median_size <- df %>%
  filter(!is.na(revenue), revenue > 0) %>%
  group_by(nace2d, group) %>%
  summarise(
    median_revenue = median(revenue, na.rm = TRUE),
    .groups = "drop"
  )


# =====================================================================
# Combine into one table
# =====================================================================

summary_table <- shares %>%
  left_join(median_size, by = c("nace2d", "group")) %>%
  select(sector = nace2d, group,
         n_firm_years = total_revenue,  # placeholder, replace below
         revenue_share, va_share, median_revenue)

# Actually rebuild cleanly
summary_table <- df %>%
  group_by(nace2d, group) %>%
  summarise(
    n_firms            = n_distinct(vat),
    n_firm_years       = n(),
    total_revenue      = sum(revenue, na.rm = TRUE),
    total_value_added  = sum(value_added, na.rm = TRUE),
    median_revenue     = median(revenue[revenue > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(nace2d) %>%
  mutate(
    revenue_share_pct = round(total_revenue / sum(total_revenue) * 100, 1),
    va_share_pct      = round(total_value_added / sum(total_value_added) * 100, 1)
  ) %>%
  ungroup() %>%
  select(sector = nace2d, group, n_firms, n_firm_years,
         revenue_share_pct, va_share_pct, median_revenue)

cat("\n=== ETS vs NON-ETS IN SECTORS C19 AND C24 ===\n")
print(as.data.frame(summary_table), row.names = FALSE)


# =====================================================================
# Save
# =====================================================================

writeLines(
  summary_table %>%
    mutate(median_revenue = round(median_revenue)) %>%
    kable(format = "latex",
          col.names = c("Sector", "Group", "Firms", "Firm-years",
                        "Revenue share (\\%)", "VA share (\\%)",
                        "Median revenue"),
          booktabs = TRUE, escape = FALSE,
          align = c("l", "l", rep("r", 5))) %>%
    kable_styling(latex_options = "hold_position"),
  file.path(OUTPUT_DIR, "ets_share_c19_c24.tex")
)

cat("\nSaved to", file.path(OUTPUT_DIR, "ets_share_c19_c24.tex"), "\n")
