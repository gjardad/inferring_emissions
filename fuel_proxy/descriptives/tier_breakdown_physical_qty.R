###############################################################################
# fuel_proxy/descriptives/08_tier_breakdown_physical_qty.R
#
# PURPOSE
#   Tabulate the number and share of observations falling into each tier
#   of the physical quantity deflation procedure (script 08).
#
# INPUTS
#   - data/processed/firm_cncode_year_physical_qty.RData
#
# OUTPUTS
#   - Console tables
#   - data/processed/tier_breakdown_physical_qty.csv
###############################################################################

# ====================
# Define paths -------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

# ====================
# Setup --------------
# ====================

library(dplyr)
library(readr)

# ====================
# Load data ----------
# ====================

load(paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))

# ====================
# Tier breakdown -----
# ====================

tier_labels <- c(
  "weight_direct"       = "1: Customs weight (reliable)",
  "value_deflated"      = "2: CN8-year internal median (p90/p10 <= 30)",
  "deflated_cn8_relaxed"= "3a: CN8-year winsorized median (relaxed)",
  "deflated_cn6"        = "3b: CN6-year median from winsorized pool",
  "deflated_cn4"        = "3c: CN4-year median from winsorized pool",
  "missing"             = "Missing"
)

tier_breakdown <- fuel_qty %>%
  count(qty_source, sort = TRUE) %>%
  mutate(
    pct = round(n / sum(n) * 100, 2),
    tier = tier_labels[qty_source]
  ) %>%
  select(tier, qty_source, n, pct)

cat("\n=== Tier breakdown: observation counts ===\n\n")
print(tier_breakdown, n = Inf)
cat("\nTotal observations:", sum(tier_breakdown$n), "\n")

# ====================
# Breakdown by CN4 ---
# ====================

tier_by_cn4 <- fuel_qty %>%
  mutate(cn4 = substr(cncode, 1, 4)) %>%
  count(cn4, qty_source) %>%
  tidyr::pivot_wider(names_from = qty_source, values_from = n, values_fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(total))

cat("\n=== Tier breakdown by CN4 product group ===\n\n")
print(tier_by_cn4, n = 20)

# ====================
# Breakdown by year --
# ====================

tier_by_year <- fuel_qty %>%
  count(year, qty_source) %>%
  tidyr::pivot_wider(names_from = qty_source, values_from = n, values_fill = 0) %>%
  arrange(year)

cat("\n=== Tier breakdown by year ===\n\n")
print(tier_by_year, n = Inf)

# ====================
# Save ---------------
# ====================

write_csv(tier_breakdown, file.path(PROC_DATA, "tier_breakdown_physical_qty.csv"))
cat("\nSaved:", file.path(PROC_DATA, "tier_breakdown_physical_qty.csv"), "\n")
