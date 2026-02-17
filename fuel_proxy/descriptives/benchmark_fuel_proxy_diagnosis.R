###############################################################################
# fuel_proxy/descriptives/benchmark_fuel_proxy_diagnosis.R
#
# PURPOSE
#   Diagnostics for the benchmark fuel proxy (monetary amount purchased from
#   fuel importers). Three analyses:
#     1) Regression: log(emissions) ~ log(revenue) + asinh(fuel_proxy)
#        + year FE + sector FE, on ETS emitters (emissions > 0).
#     2) Summary stats by ETS status: share with proxy > 0, mean/median
#        fuel proxy input cost share.
#     3) Kernel density of fuel proxy input cost share for sectors C19
#        and C24, split by ETS / non-ETS.
#
# INPUTS
#   - PROC_DATA/fuel_input_cost_share.RData
#   - PROC_DATA/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS (to OUTPUT_DIR)
#   - benchmark_proxy_regression.txt
#   - benchmark_proxy_summary_stats.tex
#   - benchmark_proxy_density_C19.pdf
#   - benchmark_proxy_density_C24.pdf
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
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)

# ==================
# Load data --------
# ==================

load(file.path(PROC_DATA, "fuel_input_cost_share.RData"))
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))

df <- fuel_input_cost_share %>%
  left_join(
    df_annual_accounts_selected_sample_key_variables %>% select(vat, year, revenue),
    by = c("vat", "year")
  ) %>%
  mutate(nace2d = substr(nace5d, 1, 2))


# =====================================================================
# 1) REGRESSION: benchmark proxy predicts emissions after controls
# =====================================================================

reg_data <- df %>%
  filter(euets == 1, emissions > 0,
         !is.na(revenue), revenue > 0,
         !is.na(fuel_spend))

model <- lm(log(emissions) ~ log(revenue) + asinh(fuel_spend)
             + factor(year) + factor(nace2d),
             data = reg_data)

reg_summary <- summary(model)

cat("\n=== REGRESSION: log(emissions) ~ log(revenue) + asinh(fuel_spend) + year FE + sector FE ===\n")
cat("N =", nobs(model), "\n")
print(reg_summary)

# Save regression output
reg_file <- file.path(OUTPUT_DIR, "benchmark_proxy_regression.txt")
sink(reg_file)
cat("Benchmark fuel proxy regression\n")
cat("Sample: ETS firms with emissions > 0\n")
cat("N =", nobs(model), "\n\n")
print(reg_summary)
sink()


# =====================================================================
# 2) SUMMARY STATS BY ETS STATUS
# =====================================================================

summary_stats <- df %>%
  group_by(euets) %>%
  summarise(
    n_obs              = n(),
    share_proxy_positive = round(mean(fuel_positive, na.rm = TRUE) * 100, 1),
    mean_cost_share    = round(mean(fuel_share_costs, na.rm = TRUE), 4),
    median_cost_share  = round(median(fuel_share_costs, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(euets == 1, "ETS", "non-ETS")) %>%
  select(group, everything(), -euets)

cat("\n=== SUMMARY STATS: BENCHMARK FUEL PROXY BY ETS STATUS ===\n")
print(as.data.frame(summary_stats), row.names = FALSE)

writeLines(
  summary_stats %>%
    kable(format = "latex",
          col.names = c("Group", "N", "Share proxy $> 0$ (\\%)",
                        "Mean cost share", "Median cost share"),
          booktabs = TRUE, escape = FALSE,
          align = c("l", rep("r", 4))) %>%
    kable_styling(latex_options = "hold_position"),
  file.path(OUTPUT_DIR, "benchmark_proxy_summary_stats.tex")
)


# =====================================================================
# 3) KERNEL DENSITY PLOTS: fuel cost share for C19 and C24
# =====================================================================

plot_density_by_sector <- function(df, sector_code, adjust = 1.1) {
  df_sector <- df %>%
    filter(nace2d == sector_code,
           !is.na(fuel_share_costs)) %>%
    mutate(
      group = if_else(euets == 1, "EUETS", "Non-EUETS"),
      group = factor(group, levels = c("EUETS", "Non-EUETS"))
    )

  ggplot(df_sector, aes(x = fuel_share_costs, color = group, linetype = group)) +
    geom_density(linewidth = 1.05, adjust = adjust, key_glyph = "path") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, NA)) +
    scale_color_manual(values = c("EUETS" = "black", "Non-EUETS" = "grey45")) +
    scale_linetype_manual(values = c("EUETS" = "solid", "Non-EUETS" = "dashed")) +
    labs(x = "Fuel proxy input cost share", y = "Density",
         color = NULL, linetype = NULL) +
    theme_classic(base_size = 13) +
    theme(
      panel.grid       = element_blank(),
      axis.title.x     = element_text(size = 15, margin = margin(t = 12)),
      axis.title.y     = element_text(size = 15, margin = margin(r = 12)),
      axis.text        = element_text(size = 12),
      legend.position  = "bottom",
      legend.text      = element_text(size = 12),
      legend.key       = element_blank(),
      legend.key.width = unit(3.5, "lines")
    )
}

p_c19 <- plot_density_by_sector(df, "19")
p_c24 <- plot_density_by_sector(df, "24")

ggsave(file.path(OUTPUT_DIR, "benchmark_proxy_density_C19.pdf"),
       p_c19, width = 7.5, height = 4.8, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "benchmark_proxy_density_C24.pdf"),
       p_c24, width = 7.5, height = 4.8, dpi = 300)

cat("\nSaved outputs to", OUTPUT_DIR, "\n")
