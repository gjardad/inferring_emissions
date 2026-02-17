###############################################################################
# fuel_proxy/descriptives/selected_proxy_pair_diagnosis.R
#
# PURPOSE
#   Same diagnostics as benchmark_fuel_proxy_diagnosis.R but for the proxy
#   pair selected by the hurdle CV procedure. Runs the three analyses for
#   each proxy in the pair (extensive-margin and intensive-margin).
#
#   1) Regressions (log-log, ETS emitters with emissions > 0 and proxy > 0):
#      (1) log(emissions) ~ log(fuel_proxy) + year FE + sector FE
#      (2) log(emissions) ~ log(fuel_proxy) + log(revenue) + year FE + sector FE
#   2) Summary stats by ETS status: share with proxy > 0, mean/median
#      proxy input cost share.
#   3) Kernel density of proxy input cost share for sectors C19 and C24,
#      split by ETS / non-ETS.
#
# INPUTS
#   - Proxy .rds files from CACHE_DIR (names set in PROXY NAMES below)
#   - PROC_DATA/loocv_training_sample.RData
#   - PROC_DATA/annual_accounts_selected_sample_key_variables.RData
#   - PROC_DATA/fuel_input_cost_share.RData (for total_costs denominator)
#
# OUTPUTS (to OUTPUT_DIR, one set per proxy)
#   - selected_proxy_{step}_regression.tex
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
library(scales)
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
load(file.path(PROC_DATA, "fuel_input_cost_share.RData"))

# Build deployment-wide frame with revenue + sector + ETS status
deploy <- df_annual_accounts_selected_sample_key_variables %>%
  mutate(nace2d = substr(nace5d, 1, 2)) %>%
  left_join(
    loocv_training_sample %>% select(vat, year, euets, emissions),
    by = c("vat", "year")
  ) %>%
  mutate(euets = coalesce(euets, 0L)) %>%
  left_join(
    fuel_input_cost_share %>% select(vat, year, total_costs),
    by = c("vat", "year")
  )


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
    mutate(
      fuel_proxy = coalesce(fuel_proxy, 0),
      proxy_cost_share = case_when(
        fuel_proxy == 0 ~ 0,
        !is.na(total_costs) & total_costs > 0 ~ fuel_proxy / total_costs,
        TRUE ~ NA_real_
      )
    )

  # ------------------------------------------------------------------
  # 1) Regressions (log-log, ETS emitters only)
  # ------------------------------------------------------------------
  reg_data <- df %>%
    filter(euets == 1, emissions > 0,
           !is.na(revenue), revenue > 0,
           fuel_proxy > 0)

  # (1) log(emissions) ~ log(fuel_proxy) + year FE + sector FE
  model1 <- lm(log(emissions) ~ log(fuel_proxy)
                + factor(year) + factor(nace2d),
                data = reg_data)

  # (2) log(emissions) ~ log(fuel_proxy) + log(revenue) + year FE + sector FE
  model2 <- lm(log(emissions) ~ log(fuel_proxy) + log(revenue)
                + factor(year) + factor(nace2d),
                data = reg_data)

  s1 <- summary(model1)
  s2 <- summary(model2)

  cat("\n=== REGRESSION (1):", step_label, "===\n")
  cat("N =", nobs(model1), "\n")
  print(s1)

  cat("\n=== REGRESSION (2):", step_label, "===\n")
  cat("N =", nobs(model2), "\n")
  print(s2)

  # Helper: extract coefficient + std error with significance stars
  fmt_coef <- function(model_summary, var_name) {
    ct <- coef(model_summary)
    if (!var_name %in% rownames(ct)) return(c("", ""))
    est   <- ct[var_name, "Estimate"]
    se    <- ct[var_name, "Std. Error"]
    pv    <- ct[var_name, "Pr(>|t|)"]
    stars <- if (pv < 0.01) "***" else if (pv < 0.05) "**" else if (pv < 0.1) "*" else ""
    c(sprintf("%.4f%s", est, stars),
      sprintf("(%.4f)", se))
  }

  fuel1 <- fmt_coef(s1, "log(fuel_proxy)")
  fuel2 <- fmt_coef(s2, "log(fuel_proxy)")
  rev1  <- c("", "")
  rev2  <- fmt_coef(s2, "log(revenue)")

  tex_lines <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "& (1) & (2) \\\\",
    "\\hline",
    sprintf("Revenue & %s & %s \\\\", rev1[1], rev2[1]),
    sprintf("& %s & %s \\\\", rev1[2], rev2[2]),
    sprintf("Fuel consumption & %s & %s \\\\", fuel1[1], fuel2[1]),
    sprintf("& %s & %s \\\\", fuel1[2], fuel2[2]),
    "\\hline \\hline",
    "Sector FE & Y & Y \\\\",
    "Year FE & Y & Y \\\\",
    sprintf("$R^2$ & %.4f & %.4f \\\\", s1$r.squared, s2$r.squared),
    sprintf("Adj.\\ $R^2$ & %.4f & %.4f \\\\", s1$adj.r.squared, s2$adj.r.squared),
    sprintf("$N$ & %d & %d \\\\", nobs(model1), nobs(model2)),
    "\\bottomrule",
    "\\end{tabular}"
  )

  writeLines(tex_lines, file.path(OUTPUT_DIR,
             paste0("selected_proxy_", step_tag, "_regression.tex")))
  cat("\nSaved regression table for", step_tag, "\n")

  # ------------------------------------------------------------------
  # 2) Summary stats by ETS status
  # ------------------------------------------------------------------
  summary_stats <- df %>%
    group_by(euets) %>%
    summarise(
      n_obs              = n(),
      share_proxy_positive = round(mean(fuel_proxy > 0, na.rm = TRUE) * 100, 1),
      mean_cost_share    = round(mean(proxy_cost_share, na.rm = TRUE), 4),
      median_cost_share  = round(median(proxy_cost_share, na.rm = TRUE), 4),
      .groups = "drop"
    ) %>%
    mutate(group = if_else(euets == 1, "ETS", "non-ETS")) %>%
    select(group, everything(), -euets)

  cat("\n=== SUMMARY STATS:", step_label, "===\n")
  print(as.data.frame(summary_stats), row.names = FALSE)

  writeLines(
    summary_stats %>%
      kable(format = "latex",
            col.names = c("ETS status", "N", "Share proxy $> 0$ (\\%)",
                          "Mean cost share", "Median cost share"),
            booktabs = TRUE, escape = FALSE,
            align = c("l", rep("c", 4))) %>%
      kable_styling(latex_options = "hold_position"),
    file.path(OUTPUT_DIR,
              paste0("selected_proxy_", step_tag, "_summary_stats.tex"))
  )

  # ------------------------------------------------------------------
  # 3) Kernel density of proxy cost share for C19 and C24
  # ------------------------------------------------------------------
  plot_density <- function(df, sector_code, adjust = 1.1) {
    df_sector <- df %>%
      filter(nace2d == sector_code,
             !is.na(proxy_cost_share)) %>%
      mutate(
        group = if_else(euets == 1, "EUETS", "Non-EUETS"),
        group = factor(group, levels = c("EUETS", "Non-EUETS"))
      )

    ggplot(df_sector, aes(x = proxy_cost_share,
                          color = group, linetype = group)) +
      geom_density(linewidth = 1.05, adjust = adjust, key_glyph = "path") +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0, NA)) +
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
