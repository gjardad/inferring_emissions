###############################################################################
# fuel_proxy/descriptives/selected_proxy_pair_diagnosis.R
#
# PURPOSE
#   Same diagnostics as benchmark_fuel_proxy_diagnosis.R but for the proxy
#   pair selected by the hurdle CV procedure. Runs the three analyses for
#   each proxy in the pair (extensive-margin and intensive-margin).
#
#   1) Regression: log(emissions) ~ log(revenue) + asinh(proxy)
#      + year FE + sector FE, on ETS emitters (emissions > 0).
#   2) Summary stats by ETS status: share with proxy > 0, mean/median
#      proxy value.
#   3) Kernel density of asinh(proxy) for sectors C19 and C24, split by
#      ETS / non-ETS.
#
# INPUTS
#   - Proxy .rds files from CACHE_DIR (names set in PROXY NAMES below)
#   - PROC_DATA/loocv_training_sample.RData
#   - PROC_DATA/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS (to OUTPUT_DIR, one set per proxy)
#   - selected_proxy_{step}_regression.txt
#   - selected_proxy_{step}_summary_stats.tex
#   - selected_proxy_{step}_density_C19.pdf
#   - selected_proxy_{step}_density_C24.pdf
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
library(knitr)
library(kableExtra)

# =====================================================================
# PROXY NAMES — edit these to match the selected hurdle pair
# =====================================================================

proxy_config <- list(
  extensive = list(
    file = "proxy_PLACEHOLDER_EXTENSIVE.rds",
    label = "Extensive margin proxy"
  ),
  intensive = list(
    file = "proxy_PLACEHOLDER_INTENSIVE.rds",
    label = "Intensive margin proxy"
  )
)


# ==================
# Load data --------
# ==================

load(file.path(PROC_DATA, "loocv_training_sample.RData"))
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))

# Build deployment-wide frame with revenue + sector + ETS status
deploy <- df_annual_accounts_selected_sample_key_variables %>%
  mutate(nace2d = substr(nace5d, 1, 2)) %>%
  left_join(
    loocv_training_sample %>% select(vat, year, euets, emissions),
    by = c("vat", "year")
  ) %>%
  mutate(euets = coalesce(euets, 0L))


# =====================================================================
# Run diagnostics for each proxy in the pair
# =====================================================================

run_proxy_diagnostics <- function(proxy_file, step_tag, step_label) {

  proxy_path <- file.path(CACHE_DIR, proxy_file)
  if (!file.exists(proxy_path)) {
    warning("Proxy file not found: ", proxy_path, " — skipping ", step_tag)
    return(invisible(NULL))
  }

  px <- readRDS(proxy_path)
  proxy_dt <- px$proxy %>%
    mutate(buyer_id = as.character(buyer_id),
           year = as.integer(year))

  # Merge proxy into deployment frame
  df <- deploy %>%
    mutate(buyer_id = as.character(vat)) %>%
    left_join(proxy_dt %>% select(buyer_id, year, fuel_proxy),
              by = c("buyer_id", "year")) %>%
    mutate(fuel_proxy = coalesce(fuel_proxy, 0))

  # ------------------------------------------------------------------
  # 1) Regression (ETS emitters only)
  # ------------------------------------------------------------------
  reg_data <- df %>%
    filter(euets == 1, emissions > 0,
           !is.na(revenue), revenue > 0)

  model <- lm(log(emissions) ~ log(revenue) + asinh(fuel_proxy)
               + factor(year) + factor(nace2d),
               data = reg_data)

  reg_summary <- summary(model)

  cat("\n=== REGRESSION:", step_label, "===\n")
  cat("N =", nobs(model), "\n")
  print(reg_summary)

  reg_file <- file.path(OUTPUT_DIR,
                         paste0("selected_proxy_", step_tag, "_regression.txt"))
  sink(reg_file)
  cat(step_label, "\n")
  cat("Proxy file:", proxy_file, "\n")
  cat("Sample: ETS firms with emissions > 0\n")
  cat("N =", nobs(model), "\n\n")
  print(reg_summary)
  sink()

  # ------------------------------------------------------------------
  # 2) Summary stats by ETS status
  # ------------------------------------------------------------------
  summary_stats <- df %>%
    group_by(euets) %>%
    summarise(
      n_obs              = n(),
      share_proxy_positive = round(mean(fuel_proxy > 0, na.rm = TRUE) * 100, 1),
      mean_proxy         = mean(fuel_proxy, na.rm = TRUE),
      median_proxy       = median(fuel_proxy, na.rm = TRUE),
      mean_asinh_proxy   = mean(asinh(fuel_proxy), na.rm = TRUE),
      median_asinh_proxy = median(asinh(fuel_proxy), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(group = if_else(euets == 1, "ETS", "non-ETS")) %>%
    select(group, everything(), -euets)

  cat("\n=== SUMMARY STATS:", step_label, "===\n")
  print(as.data.frame(summary_stats), row.names = FALSE)

  writeLines(
    summary_stats %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      kable(format = "latex",
            col.names = c("Group", "N", "Share proxy $> 0$ (\\%)",
                          "Mean proxy", "Median proxy",
                          "Mean asinh", "Median asinh"),
            booktabs = TRUE, escape = FALSE,
            align = c("l", rep("r", 6))) %>%
      kable_styling(latex_options = "hold_position"),
    file.path(OUTPUT_DIR,
              paste0("selected_proxy_", step_tag, "_summary_stats.tex"))
  )

  # ------------------------------------------------------------------
  # 3) Kernel density of asinh(proxy) for C19 and C24
  # ------------------------------------------------------------------
  plot_density <- function(df, sector_code, adjust = 1.1) {
    df_sector <- df %>%
      filter(nace2d == sector_code) %>%
      mutate(
        group = if_else(euets == 1, "EUETS", "Non-EUETS"),
        group = factor(group, levels = c("EUETS", "Non-EUETS"))
      )

    ggplot(df_sector, aes(x = asinh(fuel_proxy),
                          color = group, linetype = group)) +
      geom_density(linewidth = 1.05, adjust = adjust, key_glyph = "path") +
      scale_color_manual(values = c("EUETS" = "black",
                                     "Non-EUETS" = "grey45")) +
      scale_linetype_manual(values = c("EUETS" = "solid",
                                        "Non-EUETS" = "dashed")) +
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

  p_c19 <- plot_density(df, "19")
  p_c24 <- plot_density(df, "24")

  ggsave(file.path(OUTPUT_DIR,
                    paste0("selected_proxy_", step_tag, "_density_C19.pdf")),
         p_c19, width = 7.5, height = 4.8, dpi = 300)
  ggsave(file.path(OUTPUT_DIR,
                    paste0("selected_proxy_", step_tag, "_density_C24.pdf")),
         p_c24, width = 7.5, height = 4.8, dpi = 300)

  cat("Saved outputs for", step_tag, "to", OUTPUT_DIR, "\n")
}


# =====================================================================
# Execute
# =====================================================================

for (step in names(proxy_config)) {
  cfg <- proxy_config[[step]]
  run_proxy_diagnostics(cfg$file, step, cfg$label)
}
