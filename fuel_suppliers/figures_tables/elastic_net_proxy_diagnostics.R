###############################################################################
# fuel_suppliers/descriptives/elastic_net_proxy_diagnostics.R
#
# PURPOSE
#   Diagnostics for the fuel-consumption proxies built from elastic-net-
#   identified suppliers (pooled and within-buyer). Same three analyses as
#   fuel_proxy/descriptives/selected_proxy_pair_diagnostics.R:
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
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/b2b_selected_sample.RData
#   - {PROC_DATA}/loocv_training_sample.RData
#   - {PROC_DATA}/annual_accounts_selected_sample_key_variables.RData
#   - {PROC_DATA}/firm_year_domestic_input_cost.RData
#   - {PROC_DATA}/firm_year_total_imports.RData
#
# OUTPUTS (to OUTPUT_DIR, one set per proxy variant)
#   - enet_proxy_{variant}_regression.tex
#   - enet_proxy_{variant}_summary_stats.tex
#   - enet_proxy_{variant}_density_C19.pdf
#   - enet_proxy_{variant}_density_C24.pdf
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
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


# ── Load data ────────────────────────────────────────────────────────────────

cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading LOOCV training sample...\n")
load(file.path(PROC_DATA, "loocv_training_sample.RData"))

cat("Loading annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))

cat("Loading domestic input costs...\n")
load(file.path(PROC_DATA, "firm_year_domestic_input_cost.RData"))

cat("Loading total imports...\n")
load(file.path(PROC_DATA, "firm_year_total_imports.RData"))


# ── Extract identified supplier sets ─────────────────────────────────────────
suppliers_pooled <- supplier_summary_pooled %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

suppliers_fe <- supplier_summary_fe %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

cat("Identified suppliers (pooled):       ", length(suppliers_pooled), "\n")
cat("Identified suppliers (within-buyer): ", length(suppliers_fe), "\n")


# ── Build proxies from B2B ───────────────────────────────────────────────────
build_proxy <- function(b2b_df, supplier_set) {
  b2b_df %>%
    filter(vat_i_ano %in% supplier_set) %>%
    group_by(vat_j_ano, year) %>%
    summarise(fuel_proxy = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
    rename(vat = vat_j_ano)
}

proxy_pooled <- build_proxy(b2b, suppliers_pooled)
proxy_fe     <- build_proxy(b2b, suppliers_fe)
rm(b2b)

proxy_config <- list(
  pooled = list(
    proxy = proxy_pooled,
    label = "Elastic net proxy (pooled)"
  ),
  within_buyer = list(
    proxy = proxy_fe,
    label = "Elastic net proxy (within-buyer)"
  )
)


# ── Build deployment-wide frame ──────────────────────────────────────────────

# Build total_costs from domestic input costs + imports
domestic_costs <- firm_year_domestic_input_cost %>%
  select(vat, year, input_cost)

imports <- firm_year_total_imports %>%
  rename(vat = vat_ano) %>%
  select(vat, year, total_imports)

total_costs_df <- domestic_costs %>%
  left_join(imports, by = c("vat", "year")) %>%
  mutate(total_costs = input_cost + coalesce(total_imports, 0)) %>%
  select(vat, year, total_costs)

deploy <- df_annual_accounts_selected_sample_key_variables %>%
  mutate(nace2d = substr(nace5d, 1, 2)) %>%
  left_join(
    loocv_training_sample %>% select(vat, year, euets, emissions),
    by = c("vat", "year")
  ) %>%
  mutate(euets = coalesce(euets, 0L)) %>%
  left_join(total_costs_df, by = c("vat", "year"))


# ── Diagnostics function ────────────────────────────────────────────────────

run_proxy_diagnostics <- function(proxy_dt, variant_tag, variant_label) {

  cat(sprintf("\n%s\n  %s\n%s\n",
              strrep("=", 60), variant_label, strrep("=", 60)))

  # Merge proxy into deployment frame
  df <- deploy %>%
    left_join(proxy_dt %>% select(vat, year, fuel_proxy),
              by = c("vat", "year")) %>%
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

  if (nrow(reg_data) < 10) {
    cat("  Too few observations for regressions (", nrow(reg_data), "). Skipping.\n")
    return(invisible(NULL))
  }

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

  cat("\n=== REGRESSION (1):", variant_label, "===\n")
  cat("N =", nobs(model1), "\n")
  print(s1)

  cat("\n=== REGRESSION (2):", variant_label, "===\n")
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
             paste0("enet_proxy_", variant_tag, "_regression.tex")))
  cat("\nSaved regression table for", variant_tag, "\n")

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

  cat("\n=== SUMMARY STATS:", variant_label, "===\n")
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
              paste0("enet_proxy_", variant_tag, "_summary_stats.tex"))
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

    if (nrow(df_sector) < 5) {
      cat("  Too few obs for density plot in sector", sector_code, "\n")
      return(NULL)
    }

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

  for (sec in c("19", "24")) {
    p <- plot_density(df, sec)
    if (!is.null(p)) {
      ggsave(file.path(OUTPUT_DIR,
                        paste0("enet_proxy_", variant_tag, "_density_C", sec, ".pdf")),
             p, width = 7.5, height = 4.8, dpi = 300)
    }
  }

  cat("Saved outputs for", variant_tag, "to", OUTPUT_DIR, "\n")
}


# ── Execute ──────────────────────────────────────────────────────────────────

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

for (variant in names(proxy_config)) {
  cfg <- proxy_config[[variant]]
  run_proxy_diagnostics(cfg$proxy, variant, cfg$label)
}
