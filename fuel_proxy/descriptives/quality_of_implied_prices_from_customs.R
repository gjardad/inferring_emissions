###############################################################################
# fuel_proxy/descriptives/quality_of_implied_prices_from_customs.R
#
# PURPOSE
#   Diagnose the quality of physical-quantity imputation from customs data.
#   Two outputs:
#     1) Combined table: tier breakdown (observation counts/shares) plus
#        pseudo ground-truth accuracy (for tier-1 obs, simulate lower-tier
#        deflation and report error relative to trusted weight)
#     2) Within-CN8-year price dispersion by CN4
#
# INPUTS
#   - PROC_DATA/firm_cncode_year_physical_qty.RData
#
# OUTPUTS (to OUTPUT_DIR)
#   - tier_breakdown_and_accuracy.tex
#   - price_dispersion_by_cn4.tex
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
library(knitr)
library(kableExtra)

load(paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))


# =====================================================================
# 1) TIER BREAKDOWN + PSEUDO GROUND-TRUTH (combined table)
# =====================================================================

# Display labels
tier_labels <- c(
  weight_direct  = "Observed weight",
  value_deflated = "CN8 deflated",
  deflated_cn6   = "CN6 deflated",
  deflated_cn4   = "CN4 deflated",
  missing        = "Missing"
)
tier_order <- names(tier_labels)

# --- Tier breakdown ---
tier_breakdown <- fuel_qty %>%
  count(qty_source) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    qty_source = factor(qty_source, levels = tier_order)
  ) %>%
  arrange(qty_source)

cat("\n=== TIER BREAKDOWN ===\n")
print(tier_breakdown, n = 20)

# By CN4 (console only)
tier_by_cn4 <- fuel_qty %>%
  mutate(cn4 = substr(cncode, 1, 4)) %>%
  count(cn4, qty_source) %>%
  mutate(qty_source = factor(qty_source, levels = tier_order)) %>%
  tidyr::pivot_wider(names_from = qty_source, values_from = n, values_fill = 0) %>%
  arrange(cn4)

cat("\n=== TIER BREAKDOWN BY CN4 ===\n")
print(tier_by_cn4, n = 40)

# --- Pseudo ground-truth test ---
#     For tier-1 obs (weight_direct), simulate what lower-tier deflation
#     would have produced and compare to the trusted weight.
tier1 <- fuel_qty %>%
  filter(
    qty_source == "weight_direct",
    !is.na(imports_value), imports_value > 0,
    !is.na(imports_weight), imports_weight > 0
  ) %>%
  mutate(
    true_qty    = imports_weight,
    defl_tier2  = if_else(!is.na(price_eur_per_kg) & price_eur_per_kg > 0,
                          imports_value / price_eur_per_kg, NA_real_),
    defl_tier3a = if_else(!is.na(price_cn6) & price_cn6 > 0,
                          imports_value / price_cn6, NA_real_),
    defl_tier3b = if_else(!is.na(price_cn4) & price_cn4 > 0,
                          imports_value / price_cn4, NA_real_)
  )

# Helper: summarise error for one deflation column
summarise_defl_error <- function(true_qty, defl_qty) {
  ok <- !is.na(defl_qty) & is.finite(defl_qty)
  if (sum(ok) == 0) {
    return(tibble(median_ratio = NA_real_, mape = NA_real_,
                  within_25pct = NA_real_, within_50pct = NA_real_))
  }
  ratio <- defl_qty[ok] / true_qty[ok]
  ape   <- abs(ratio - 1) * 100

  tibble(
    median_ratio = median(ratio),
    mape         = mean(ape),
    within_25pct = mean(ape <= 25) * 100,
    within_50pct = mean(ape <= 50) * 100
  )
}

error_summary <- bind_rows(
  summarise_defl_error(tier1$true_qty, tier1$defl_tier2)  %>% mutate(qty_source = "value_deflated"),
  summarise_defl_error(tier1$true_qty, tier1$defl_tier3a) %>% mutate(qty_source = "deflated_cn6"),
  summarise_defl_error(tier1$true_qty, tier1$defl_tier3b) %>% mutate(qty_source = "deflated_cn4")
)

cat("\n=== PSEUDO GROUND-TRUTH TEST (on tier-1 obs) ===\n")
cat("ratio = deflated_qty / true_weight  (ideal = 1)\n\n")
print(as.data.frame(error_summary), row.names = FALSE)

# --- Build combined table ---
fmt_acc <- function(x, digits = 1) {
  if_else(is.na(x), "$-$", formatC(x, format = "f", digits = digits))
}

combined <- tier_breakdown %>%
  mutate(tier = tier_labels[as.character(qty_source)]) %>%
  left_join(error_summary, by = "qty_source") %>%
  mutate(
    median_ratio = fmt_acc(median_ratio, 2),
    mape         = fmt_acc(mape, 1),
    within_25pct = fmt_acc(within_25pct, 1),
    within_50pct = fmt_acc(within_50pct, 1)
  ) %>%
  select(tier, n, pct, median_ratio, mape, within_25pct, within_50pct)

writeLines(
  combined %>%
    kable(format = "latex",
          col.names = c("Tier", "N", "\\%",
                        "Med.\\ ratio", "MAPE",
                        "$\\leq$ 25\\%", "$\\leq$ 50\\%"),
          booktabs = TRUE, escape = FALSE,
          align = c("l", rep("c", 6))) %>%
    kable_styling(latex_options = "hold_position") %>%
    add_header_above(c(" " = 3, "Accuracy of lower tiers" = 4)),
  file.path(OUTPUT_DIR, "tier_breakdown_and_accuracy.tex")
)

cat("\nSaved combined table to", file.path(OUTPUT_DIR, "tier_breakdown_and_accuracy.tex"), "\n")

# By-CN4 error diagnostics (console only)
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

cat("\n--- By CN4: Tier 2 (CN8 deflated) ---\n")
print(error_by_cn4(tier1$true_qty, tier1$defl_tier2, tier1$cncode, "Tier 2"), n = 30)

cat("\n--- By CN4: Tier 3b (CN4 deflated) ---\n")
print(error_by_cn4(tier1$true_qty, tier1$defl_tier3b, tier1$cncode, "Tier 3b"), n = 30)


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

writeLines(
  dispersion_by_cn4 %>%
    kable(format = "latex",
          col.names = c("CN4", "N cells", "Median firms",
                        "Median SD(log p)", "Median IQR(log p)"),
          booktabs = TRUE,
          align = c("l", rep("c", 4))) %>%
    kable_styling(latex_options = "hold_position"),
  file.path(OUTPUT_DIR, "price_dispersion_by_cn4.tex")
)
