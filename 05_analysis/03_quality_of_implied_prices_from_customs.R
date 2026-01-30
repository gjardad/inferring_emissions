###############################################################################
# 05_analysis/03_quality_of_implied_prices_from_customs.R
#
# PURPOSE
#   Diagnostic: compare implied CN-code unit values from customs to external benchmark price series (coal/oil/gas) and compute comovement/fit statistics.
#
# INPUTS
#   - data/processed/coal_price.RData
#   - data/processed/gas_price.RData
#   - data/processed/oil_price.RData
#   - data/processed/firm_cncode_year_physical_qty.RData
#
# OUTPUTS
#   - Console tables and optional plots (no default saved outputs).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/05_analysis/03_quality_of_implied_prices_from_customs.R
###############################################################################

#### HEADER -------

# Compare implied CN-code unit values (imports_value/imports_weight)
# with external benchmark price time series (coal/oil/gas).
#
# REQUIREMENTS:
#   fuel_qty: cncode, year, imports_value, imports_weight, quality_flag,
#             and a mapping variable fuel_benchmark in {"coal","oil","gas"}.
#   coal_price, oil_price, gas_price: yearly time series.
#
# OUTPUTS:
#   1) cn_year_implied: median implied price by cncode-year (usable quality only)
#   2) compare_cn_year: cncode-year panel with matched benchmark price (converted)
#   3) cn_metrics: per-cncode diagnostics:
#        - correlation (levels + logs)
#        - scale factor that best matches levels
#        - RMSE / MAPE in levels (after scaling benchmark)
#        - log-RMSE + mean log error (bias)
#   4) optional plots (ggplot)

#####################

# -------------
## Setup ------
# -------------

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
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)

# -----------------------------
# 0) Parameters / assumptions / load data
# -----------------------------

usable_flags <- c("very_good", "ok")

# Unit conversions for external prices:
# - gas: EUR/MMBtu -> EUR/GJ via 1 MMBtu = 1.055056 GJ
# - oil: EUR/bbl -> EUR/GJ via 1 bbl ~ 5.8 MMBtu (rule-of-thumb), then to GJ
#   => 1 bbl ~ 5.8 * 1.055056 = 6.1193248 GJ
MMBTU_TO_GJ <- 1.055056
BBL_TO_MMBTU <- 5.8
BBL_TO_GJ <- BBL_TO_MMBTU * MMBTU_TO_GJ
COAL_GJ_PER_TON <- 24.0           # common ballpark for steam coal (~24 GJ/ton)

# Load price data
load(paste0(proc_data,"/coal_price.RData"))  # has: year, eur_per_ton
load(paste0(proc_data,"/gas_price.RData"))   # has: year, eur_per_mmbtu
load(paste0(proc_data,"/oil_price.RData"))   # has: year, eur_per_bbl

# Load data on fuel implied prices
load(paste0(proc_data,"/firm_cncode_year_physical_qty.RData"))

# -----------------------------
# 1) Implied price per CN-year
# -----------------------------

cn_year_implied <- fuel_qty %>%
  ungroup() %>%
  filter(
    quality_flag %in% usable_flags,
    !is.na(imports_value), !is.na(imports_weight),
    imports_weight > 0
  ) %>%
  group_by(cncode, year, fuel_benchmark) %>%
  summarise(
    n_obs = n(),
    median_eur_per_kg = median(price_eur_per_kg, na.rm = TRUE),
    p25_eur_per_kg    = quantile(price_eur_per_kg, 0.25, na.rm = TRUE),
    p75_eur_per_kg    = quantile(price_eur_per_kg, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------
# 2) Build a unified benchmark series (EUR per GJ)
# -------------------------------------------------------

bench_price_gj <- bind_rows(
  coal_price %>%
    transmute(
      fuel_benchmark = "coal",
      year = year,
      bench_price_eur_per_gj = eur_per_ton / COAL_GJ_PER_TON
    ),
  oil_price %>%
    transmute(
      fuel_benchmark = "oil",
      year = year,
      bench_price_eur_per_gj = eur_per_bbl / BBL_TO_GJ
    ),
  gas_price %>%
    transmute(
      fuel_benchmark = "gas",
      year = year,
      bench_price_eur_per_gj = eur_per_mmbtu / MMBTU_TO_GJ
    )
) %>%
  filter(!is.na(year), !is.na(bench_price_eur_per_gj), bench_price_eur_per_gj > 0)


# -------------------------------------------------------------------
# 3) Join implied CN prices with benchmark series
# -------------------------------------------------------------------
# Note: implied is EUR/kg, benchmark is EUR/GJ.
# Levels are NOT directly comparable without calorific value.
# We therefore:
#   (i) compare time-series comovement via correlation (logs & levels)
#   (ii) compare levels AFTER estimating a cn-specific scaling factor:
#       implied ≈ alpha * bench (alpha chosen by least squares through origin)
# This alpha absorbs BOTH unit differences and any constant wedge.

compare_cn_year <- cn_year_implied %>%
  inner_join(bench_price_gj, by = c("fuel_benchmark", "year")) %>%
  mutate(
    # Useful for log-comovement diagnostics (guard against nonpositive values)
    log_implied = if_else(median_eur_per_kg > 0,
                          log(median_eur_per_kg), NA_real_),
    log_bench   = if_else(bench_price_eur_per_gj > 0,
                          log(bench_price_eur_per_gj), NA_real_)
  )

# -------------------------------------------------------------------
# 4) Per-CN diagnostics: correlation + level-fit statistics
# -------------------------------------------------------------------

fit_alpha_through_origin <- function(y, x) {
  # Minimizes sum (y - alpha*x)^2 with intercept fixed at 0:
  # alpha = sum(x*y)/sum(x^2)
  denom <- sum(x^2, na.rm = TRUE)
  if (is.na(denom) || denom == 0) return(NA_real_)
  sum(x * y, na.rm = TRUE) / denom
}

mape <- function(y, yhat) {
  # Mean absolute percentage error (in %), ignoring zeros / NAs in y
  ok <- is.finite(y) & is.finite(yhat) & (y != 0)
  if (!any(ok)) return(NA_real_)
  mean(abs((y[ok] - yhat[ok]) / y[ok])) * 100
}

rmse <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((y[ok] - yhat[ok])^2))
}

cn_metrics <- compare_cn_year %>%
  group_by(cncode, fuel_benchmark) %>%
  summarise(
    n_years = n(),
    
    # Comovement (unit-free-ish)
    corr_levels = if (n() >= 3)
      cor(median_eur_per_kg, bench_price_eur_per_gj, use = "complete.obs")
    else NA_real_,
    
    corr_logs = if (sum(is.finite(log_implied) & is.finite(log_bench)) >= 3)
      cor(log_implied, log_bench, use = "complete.obs")
    else NA_real_,
    
    # Level comparison via best-fit scaling factor alpha:
    alpha_level = fit_alpha_through_origin(
      y = median_eur_per_kg,
      x = bench_price_eur_per_gj
    ),
    
    # Errors in levels after scaling benchmark:
    rmse_level = {
      a <- fit_alpha_through_origin(median_eur_per_kg, bench_price_eur_per_gj)
      rmse(median_eur_per_kg, a * bench_price_eur_per_gj)
    },
    mape_level = {
      a <- fit_alpha_through_origin(median_eur_per_kg, bench_price_eur_per_gj)
      mape(median_eur_per_kg, a * bench_price_eur_per_gj)
    },
    
    # Log-space errors (scale-free): implied vs (scaled) benchmark
    # Choose beta = median(log_implied - log_bench) so scaled bench matches median level in logs.
    log_shift = {
      d <- log_implied - log_bench
      if (all(!is.finite(d))) NA_real_ else median(d, na.rm = TRUE)
    },
    rmse_log = {
      d <- (log_implied - log_bench)
      b <- if (all(!is.finite(d))) NA_real_ else median(d, na.rm = TRUE)
      ok <- is.finite(d)
      if (!any(ok) || !is.finite(b)) NA_real_ else sqrt(mean((d[ok] - b)^2))
    },
    mean_log_error = {
      d <- (log_implied - log_bench)
      if (all(!is.finite(d))) NA_real_ else mean(d, na.rm = TRUE)
    },
    
    .groups = "drop"
  ) %>%
  arrange(desc(corr_logs), mape_level)

# Inspect the “best looking” CN codes first
cn_metrics %>% print(n = 50)

# -------------------------------------------------------------------
# 5) (Optional) Plot implied vs scaled benchmark for selected CN codes
# -------------------------------------------------------------------

plot_cn_comparison <- function(df, cn) {
  df_cn <- df %>% filter(cncode == cn)
  if (nrow(df_cn) == 0) return(NULL)
  
  a <- fit_alpha_through_origin(
    y = df_cn$median_eur_per_kg,
    x = df_cn$bench_price_eur_per_gj
  )
  
  df_cn <- df_cn %>%
    mutate(bench_scaled_to_implied = a * bench_price_eur_per_gj)
  
  ggplot(df_cn, aes(x = year)) +
    geom_line(aes(y = median_eur_per_kg, linetype = "median_eur_per_kg")) +
    geom_line(aes(y = bench_scaled_to_implied, linetype = "benchmark_scaled")) +
    labs(
      title = paste0("CN ", cn, " — implied unit values vs benchmark (scaled)"),
      subtitle = paste0("fuel_benchmark = ", unique(df_cn$fuel_benchmark),
                        " | alpha = ", signif(a, 4)),
      y = "Price (implied EUR/kg; benchmark scaled to implied units)",
      linetype = NULL
    ) +
    theme_minimal()
}

# Example: plot top 6 CN codes by corr_logs (with at least 5 years)
top_cn <- cn_metrics %>%
  filter(n_years >= 5, is.finite(corr_logs)) %>%
  slice_max(corr_logs, n = 6) %>%
  pull(cncode)

plots <- map(top_cn, ~plot_cn_comparison(compare_cn_year, .x))
# Print plots (in RStudio this will show them sequentially)
walk(plots, print)

# -------------------------------------------------------------------
# 6) (Optional) A single “overall” summary by benchmark type
# -------------------------------------------------------------------
benchmark_summary <- cn_metrics %>%
  group_by(fuel_benchmark) %>%
  summarise(
    n_cn = n(),
    median_corr_logs  = median(corr_logs, na.rm = TRUE),
    median_mape_level = median(mape_level, na.rm = TRUE),
    .groups = "drop"
  )

benchmark_summary
