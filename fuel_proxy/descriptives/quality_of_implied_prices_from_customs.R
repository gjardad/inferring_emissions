###############################################################################
# fuel_proxy/descriptives/quality_of_implied_prices_from_customs.R
#
# PURPOSE
#   Diagnose the quality of physical-quantity imputation from customs data.
#   Three diagnostics:
#     1) Tier breakdown: observation counts/shares by imputation tier
#     2) Pseudo ground-truth test: for tier-1 obs (trusted customs weight),
#        simulate what lower-tier deflation methods would have produced
#        and report error distributions
#     3) Within-CN8-year price dispersion: how noisy are the firm-level
#        unit values that feed each tier's median deflator?
#
# INPUTS
#   - PROC_DATA/firm_cncode_year_physical_qty.RData
#
# OUTPUTS (to OUTPUT_DIR)
#   - tier_breakdown.csv
#   - pseudo_ground_truth_test.csv
#   - price_dispersion_by_cn4.csv
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

# -------------
## Setup ------
# -------------

library(dplyr)
library(tidyr)

load(paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))


# =====================================================================
# 1) TIER BREAKDOWN
# =====================================================================

tier_order <- c("weight_direct", "value_deflated",
                "deflated_cn8_relaxed", "deflated_cn6", "deflated_cn4",
                "missing")

tier_breakdown <- fuel_qty %>%
  count(qty_source) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    qty_source = factor(qty_source, levels = tier_order)
  ) %>%
  arrange(qty_source)

cat("\n=== TIER BREAKDOWN ===\n")
print(tier_breakdown, n = 20)

write.csv(tier_breakdown,
          file.path(OUTPUT_DIR, "tier_breakdown.csv"),
          row.names = FALSE)

# By CN4
tier_by_cn4 <- fuel_qty %>%
  mutate(cn4 = substr(cncode, 1, 4)) %>%
  count(cn4, qty_source) %>%
  mutate(qty_source = factor(qty_source, levels = tier_order)) %>%
  tidyr::pivot_wider(names_from = qty_source, values_from = n, values_fill = 0) %>%
  arrange(cn4)

cat("\n=== TIER BREAKDOWN BY CN4 ===\n")
print(tier_by_cn4, n = 40)


# =====================================================================
# 2) PSEUDO GROUND-TRUTH TEST
#
#    For tier-1 observations (weight_direct), we trust imports_weight.
#    We simulate what each lower tier's deflation would have produced:
#      deflated_qty = imports_value / tier_median_price
#    and compare to the trusted weight via ratio = deflated / true.
#    A perfect deflator gives ratio = 1.
# =====================================================================

tier1 <- fuel_qty %>%
  filter(
    qty_source == "weight_direct",
    !is.na(imports_value), imports_value > 0,
    !is.na(imports_weight), imports_weight > 0
  ) %>%
  mutate(
    true_qty = imports_weight,
    defl_tier2  = if_else(!is.na(price_eur_per_kg)  & price_eur_per_kg  > 0,
                          imports_value / price_eur_per_kg,  NA_real_),
    defl_tier3a = if_else(!is.na(price_cn8_relaxed)  & price_cn8_relaxed  > 0,
                          imports_value / price_cn8_relaxed,  NA_real_),
    defl_tier3b = if_else(!is.na(price_cn6) & price_cn6 > 0,
                          imports_value / price_cn6, NA_real_),
    defl_tier3c = if_else(!is.na(price_cn4) & price_cn4 > 0,
                          imports_value / price_cn4, NA_real_)
  )

# Helper: summarise error for one deflation column
summarise_defl_error <- function(true_qty, defl_qty, tier_label) {
  ok <- !is.na(defl_qty) & is.finite(defl_qty)
  if (sum(ok) == 0) {
    return(tibble(tier = tier_label, n_obs = 0L,
                  median_ratio = NA_real_, p25_ratio = NA_real_,
                  p75_ratio = NA_real_, mape = NA_real_,
                  median_ape = NA_real_, within_10pct = NA_real_,
                  within_25pct = NA_real_, within_50pct = NA_real_))
  }
  ratio <- defl_qty[ok] / true_qty[ok]
  ape   <- abs(ratio - 1) * 100

  tibble(
    tier         = tier_label,
    n_obs        = sum(ok),
    median_ratio = median(ratio),
    p25_ratio    = quantile(ratio, 0.25),
    p75_ratio    = quantile(ratio, 0.75),
    mape         = mean(ape),
    median_ape   = median(ape),
    within_10pct = mean(ape <= 10) * 100,
    within_25pct = mean(ape <= 25) * 100,
    within_50pct = mean(ape <= 50) * 100
  )
}

error_summary <- bind_rows(
  summarise_defl_error(tier1$true_qty, tier1$defl_tier2,  "Tier 2  (CN8 quality-gated)"),
  summarise_defl_error(tier1$true_qty, tier1$defl_tier3a, "Tier 3a (CN8 relaxed)"),
  summarise_defl_error(tier1$true_qty, tier1$defl_tier3b, "Tier 3b (CN6)"),
  summarise_defl_error(tier1$true_qty, tier1$defl_tier3c, "Tier 3c (CN4)")
)

cat("\n=== PSEUDO GROUND-TRUTH TEST (on tier-1 obs) ===\n")
cat("ratio = deflated_qty / true_weight  (ideal = 1)\n\n")
print(as.data.frame(error_summary), row.names = FALSE)

write.csv(error_summary,
          file.path(OUTPUT_DIR, "pseudo_ground_truth_test.csv"),
          row.names = FALSE)

# Breakdown by CN4 for tier 2 vs tier 3c (best vs coarsest deflator)
error_by_cn4 <- function(true_qty, defl_qty, cncode, tier_label) {
  df <- tibble(true_qty = true_qty, defl_qty = defl_qty,
               cn4 = substr(cncode, 1, 4)) %>%
    filter(!is.na(defl_qty), is.finite(defl_qty)) %>%
    mutate(ratio = defl_qty / true_qty,
           ape   = abs(ratio - 1) * 100)

  df %>%
    group_by(cn4) %>%
    summarise(
      tier         = tier_label,
      n            = n(),
      median_ratio = median(ratio),
      mape         = round(mean(ape), 1),
      within_25pct = round(mean(ape <= 25) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(cn4)
}

cat("\n--- By CN4: Tier 2 (CN8 quality-gated median) ---\n")
print(error_by_cn4(tier1$true_qty, tier1$defl_tier2, tier1$cncode, "Tier 2"), n = 30)

cat("\n--- By CN4: Tier 3c (CN4 median) ---\n")
print(error_by_cn4(tier1$true_qty, tier1$defl_tier3c, tier1$cncode, "Tier 3c"), n = 30)


# =====================================================================
# 3) WITHIN-CN8-YEAR PRICE DISPERSION
#
#    Recompute firm-level unit values (imports_value / imports_weight)
#    and measure how dispersed they are within each CN8-year cell.
#    High dispersion means the median deflator is a noisier stand-in.
# =====================================================================

price_dispersion <- fuel_qty %>%
  filter(!is.na(imports_value),  imports_value  > 0,
         !is.na(imports_weight), imports_weight > 0) %>%
  mutate(
    firm_price = imports_value / imports_weight,
    log_price  = log(firm_price)
  ) %>%
  filter(is.finite(log_price)) %>%
  group_by(cncode, year) %>%
  summarise(
    n_firms       = n(),
    sd_log_price  = sd(log_price, na.rm = TRUE),
    iqr_log_price = IQR(log_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_firms >= 3)

cat("\n=== WITHIN-CN8-YEAR PRICE DISPERSION ===\n")
cat("(SD and IQR of log(price), across CN8-year cells with >= 3 firms)\n\n")

dispersion_overall <- price_dispersion %>%
  summarise(
    n_cells        = n(),
    median_n_firms = median(n_firms),
    median_sd_log  = round(median(sd_log_price, na.rm = TRUE), 3),
    p75_sd_log     = round(quantile(sd_log_price, 0.75, na.rm = TRUE), 3),
    p90_sd_log     = round(quantile(sd_log_price, 0.90, na.rm = TRUE), 3),
    median_iqr_log = round(median(iqr_log_price, na.rm = TRUE), 3),
    p75_iqr_log    = round(quantile(iqr_log_price, 0.75, na.rm = TRUE), 3)
  )

print(as.data.frame(dispersion_overall), row.names = FALSE)

# By CN4
cat("\n--- Price dispersion by CN4 ---\n")
dispersion_by_cn4 <- price_dispersion %>%
  mutate(cn4 = substr(cncode, 1, 4)) %>%
  group_by(cn4) %>%
  summarise(
    n_cells        = n(),
    median_n_firms = median(n_firms),
    median_sd_log  = round(median(sd_log_price, na.rm = TRUE), 3),
    median_iqr_log = round(median(iqr_log_price, na.rm = TRUE), 3),
    .groups = "drop"
  )
print(dispersion_by_cn4, n = 30)

write.csv(dispersion_by_cn4,
          file.path(OUTPUT_DIR, "price_dispersion_by_cn4.csv"),
          row.names = FALSE)
