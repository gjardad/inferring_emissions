###############################################################################
# fuel_suppliers/descriptives/within_firm_signal_test.R
#
# PURPOSE
#   Test whether within-firm variation in purchases from elastic-net-identified
#   suppliers predicts within-firm variation in emissions. This addresses the
#   concern that the elastic net simply captures suppliers who sell to
#   high-emitting firms (cross-sectional sorting), rather than fuel suppliers
#   whose sales causally relate to emissions.
#
#   Two proxies are tested side by side:
#     (A) Pooled proxy: from suppliers identified by the pooled elastic net
#     (B) Within-buyer proxy: from suppliers identified by the within-buyer
#         (buyer FE) elastic net
#
#   For each proxy, three regressions (LHS = asinh(emissions)):
#     1) Pooled OLS with sector + year FE (between + within variation)
#     2) Firm FE OLS (within-firm variation only)
#     3) First differences (alternative within-firm specification)
#
#   If the proxy coefficient survives firm FE, the relationship cannot be
#   driven by time-invariant firm characteristics (sector, baseline size,
#   inherent emission intensity, etc.).
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/b2b_selected_sample.RData
#
# OUTPUTS (to console + OUTPUT_DIR)
#   - enet_within_firm_test.csv
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


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net inputs...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData"))

cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)


# ── Build fuel proxies from both sets of identified suppliers ────────────────

# Helper: build proxy from a set of supplier VATs
build_proxy <- function(supplier_vats, b2b_data, buyer_vats) {
  b2b_data %>%
    filter(vat_i_ano %in% supplier_vats, vat_j_ano %in% buyer_vats, year >= 2005) %>%
    group_by(vat_j_ano, year) %>%
    summarise(fuel_proxy = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
    rename(vat = vat_j_ano)
}

# (A) Pooled proxy
suppliers_pooled <- supplier_summary_pooled %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

proxy_pooled_df <- build_proxy(suppliers_pooled, b2b, lhs$vat)
cat("\nPooled suppliers selected:", length(suppliers_pooled), "\n")

# (B) Within-buyer proxy
suppliers_fe <- supplier_summary_fe %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

proxy_fe_df <- build_proxy(suppliers_fe, b2b, lhs$vat)
cat("Within-buyer suppliers selected:", length(suppliers_fe), "\n")

# Overlap
n_both <- length(intersect(suppliers_pooled, suppliers_fe))
cat("Overlap (in both):", n_both, "\n")

rm(b2b)


# ── Helper: run the full within-firm test for one proxy ──────────────────────
run_within_firm_test <- function(lhs_panel, proxy_df, proxy_label) {

  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat(sprintf("# WITHIN-FIRM SIGNAL TEST: %s\n", toupper(proxy_label)))
  cat(strrep("#", 70), "\n")

  # Build panel
  panel <- lhs_panel %>%
    left_join(proxy_df, by = c("vat", "year")) %>%
    mutate(
      fuel_proxy   = coalesce(fuel_proxy, 0),
      asinh_y      = asinh(y),
      asinh_proxy  = asinh(fuel_proxy),
      has_proxy    = as.integer(fuel_proxy > 0),
      year_f       = factor(year),
      nace2d_f     = factor(nace2d)
    )

  n_firms <- n_distinct(panel$vat)
  n_obs   <- nrow(panel)
  cat("\nPanel:", n_obs, "obs,", n_firms, "firms,", n_distinct(panel$year), "years\n")
  cat("Emitters:", sum(panel$y > 0), sprintf("(%.1f%%)\n", 100 * mean(panel$y > 0)))
  cat("Proxy > 0:", sum(panel$has_proxy), sprintf("(%.1f%%)\n", 100 * mean(panel$has_proxy)))

  # ── Variance decomposition ──
  cat("\n", strrep("=", 60), "\n")
  cat("  Proxy variance decomposition\n")
  cat(strrep("=", 60), "\n\n")

  firm_means <- panel %>%
    group_by(vat) %>%
    summarise(fm = mean(asinh_proxy), .groups = "drop")

  panel <- panel %>%
    left_join(firm_means, by = "vat") %>%
    mutate(asinh_proxy_within = asinh_proxy - fm)

  total_var   <- var(panel$asinh_proxy)
  between_var <- var(panel$fm)
  within_var  <- var(panel$asinh_proxy_within)

  cat(sprintf("  Total variance of asinh(proxy):   %.4f\n", total_var))
  cat(sprintf("  Between-firm variance:            %.4f (%.1f%%)\n",
              between_var, 100 * between_var / total_var))
  cat(sprintf("  Within-firm variance:             %.4f (%.1f%%)\n",
              within_var, 100 * within_var / total_var))

  firms_with_variation <- panel %>%
    group_by(vat) %>%
    summarise(proxy_sd = sd(fuel_proxy), .groups = "drop")

  n_vary <- sum(firms_with_variation$proxy_sd > 0, na.rm = TRUE)
  cat(sprintf("\n  Firms with time-varying proxy: %d / %d (%.1f%%)\n",
              n_vary, n_firms, 100 * n_vary / n_firms))

  # ── Regression 1: Pooled OLS ──
  cat("\n", strrep("=", 60), "\n")
  cat("  Regression 1: Pooled OLS (sector + year FE)\n")
  cat(strrep("=", 60), "\n\n")

  fit_pooled <- lm(asinh_y ~ asinh_proxy + has_proxy + log_revenue + year_f + nace2d_f,
                   data = panel)

  s_pooled <- summary(fit_pooled)
  coef_pooled <- s_pooled$coefficients
  key_vars <- c("asinh_proxy", "has_proxy", "log_revenue")
  key_vars_present <- intersect(key_vars, rownames(coef_pooled))

  cat("  LHS: asinh(emissions)\n")
  cat("  RHS: asinh(proxy) + I(proxy > 0) + log(revenue) + year FE + NACE 2d FE\n")
  cat(sprintf("  N = %d, R² = %.4f, Adj R² = %.4f\n\n",
              n_obs, s_pooled$r.squared, s_pooled$adj.r.squared))

  cat(sprintf("  %-20s  %10s  %10s  %8s  %10s\n",
              "Variable", "Coef", "Std.Err", "t-stat", "p-value"))
  cat("  ", strrep("-", 65), "\n")
  for (v in key_vars_present) {
    cat(sprintf("  %-20s  %10.4f  %10.4f  %8.2f  %10.4f\n",
                v, coef_pooled[v, 1], coef_pooled[v, 2],
                coef_pooled[v, 3], coef_pooled[v, 4]))
  }

  # ── Regression 2: Firm FE OLS ──
  cat("\n", strrep("=", 60), "\n")
  cat("  Regression 2: Firm FE OLS (within-firm variation)\n")
  cat(strrep("=", 60), "\n\n")

  fit_fe <- lm(asinh_y ~ asinh_proxy + has_proxy + log_revenue + year_f + factor(vat),
               data = panel)

  s_fe <- summary(fit_fe)
  coef_fe <- s_fe$coefficients

  cat("  LHS: asinh(emissions)\n")
  cat("  RHS: asinh(proxy) + I(proxy > 0) + log(revenue) + year FE + firm FE\n")
  cat(sprintf("  N = %d, R² = %.4f, Adj R² = %.4f\n\n",
              n_obs, s_fe$r.squared, s_fe$adj.r.squared))

  cat(sprintf("  %-20s  %10s  %10s  %8s  %10s\n",
              "Variable", "Coef", "Std.Err", "t-stat", "p-value"))
  cat("  ", strrep("-", 65), "\n")
  for (v in key_vars_present) {
    if (v %in% rownames(coef_fe)) {
      cat(sprintf("  %-20s  %10.4f  %10.4f  %8.2f  %10.4f\n",
                  v, coef_fe[v, 1], coef_fe[v, 2],
                  coef_fe[v, 3], coef_fe[v, 4]))
    }
  }

  # ── Regression 3: First differences ──
  cat("\n", strrep("=", 60), "\n")
  cat("  Regression 3: First differences\n")
  cat(strrep("=", 60), "\n\n")

  panel_fd <- panel %>%
    arrange(vat, year) %>%
    group_by(vat) %>%
    mutate(
      d_asinh_y     = asinh_y     - lag(asinh_y),
      d_asinh_proxy = asinh_proxy - lag(asinh_proxy),
      d_has_proxy   = has_proxy   - lag(has_proxy),
      d_log_revenue = log_revenue - lag(log_revenue)
    ) %>%
    ungroup() %>%
    filter(!is.na(d_asinh_y))

  fit_fd <- lm(d_asinh_y ~ d_asinh_proxy + d_has_proxy + d_log_revenue + year_f,
               data = panel_fd)

  s_fd <- summary(fit_fd)
  coef_fd <- s_fd$coefficients
  key_vars_fd <- c("d_asinh_proxy", "d_has_proxy", "d_log_revenue")
  key_vars_fd_present <- intersect(key_vars_fd, rownames(coef_fd))

  cat("  LHS: Δ asinh(emissions)\n")
  cat("  RHS: Δ asinh(proxy) + Δ I(proxy > 0) + Δ log(revenue) + year FE\n")
  cat(sprintf("  N = %d, R² = %.4f, Adj R² = %.4f\n\n",
              nrow(panel_fd), s_fd$r.squared, s_fd$adj.r.squared))

  cat(sprintf("  %-20s  %10s  %10s  %8s  %10s\n",
              "Variable", "Coef", "Std.Err", "t-stat", "p-value"))
  cat("  ", strrep("-", 65), "\n")
  for (v in key_vars_fd_present) {
    cat(sprintf("  %-20s  %10.4f  %10.4f  %8.2f  %10.4f\n",
                v, coef_fd[v, 1], coef_fd[v, 2],
                coef_fd[v, 3], coef_fd[v, 4]))
  }

  # ── Build summary rows ──
  extract_coef <- function(fit_summary, var_name, spec_label) {
    cc <- fit_summary$coefficients
    if (!(var_name %in% rownames(cc))) return(NULL)
    data.frame(
      proxy   = proxy_label,
      spec    = spec_label,
      coef    = cc[var_name, 1],
      se      = cc[var_name, 2],
      t_stat  = cc[var_name, 3],
      p_value = cc[var_name, 4],
      r2      = fit_summary$r.squared,
      adj_r2  = fit_summary$adj.r.squared,
      n       = length(fit_summary$residuals),
      stringsAsFactors = FALSE
    )
  }

  summary_rows <- bind_rows(
    extract_coef(s_pooled, "asinh_proxy", "Pooled (sector + year FE)"),
    extract_coef(s_fe,     "asinh_proxy", "Firm FE (+ year FE)"),
    extract_coef(s_fd,     "d_asinh_proxy", "First differences (+ year FE)")
  )

  # Print per-proxy summary
  cat("\n\n", strrep("=", 60), "\n")
  cat(sprintf("  SUMMARY [%s]: asinh(proxy) coefficient across specs\n", proxy_label))
  cat(strrep("=", 60), "\n\n")

  cat(sprintf("  %-30s  %8s  %8s  %8s  %10s  %6s\n",
              "Specification", "Coef", "SE", "t-stat", "p-value", "R²"))
  cat("  ", strrep("-", 80), "\n")
  for (r in seq_len(nrow(summary_rows))) {
    cat(sprintf("  %-30s  %8.4f  %8.4f  %8.2f  %10.4f  %6.4f\n",
                summary_rows$spec[r],
                summary_rows$coef[r], summary_rows$se[r],
                summary_rows$t_stat[r], summary_rows$p_value[r],
                summary_rows$r2[r]))
  }

  if (nrow(summary_rows) >= 2) {
    pooled_coef <- summary_rows$coef[1]
    fe_coef     <- summary_rows$coef[2]
    attenuation <- 1 - fe_coef / pooled_coef

    cat(sprintf("\n  Attenuation (pooled → firm FE): %.1f%%\n", 100 * attenuation))
    cat(sprintf("  The firm FE coefficient is %.1f%% of the pooled coefficient.\n",
                100 * fe_coef / pooled_coef))
  }

  summary_rows
}


# ══════════════════════════════════════════════════════════════════════════════
#   RUN FOR BOTH PROXIES
# ══════════════════════════════════════════════════════════════════════════════
summary_pooled_proxy <- run_within_firm_test(lhs, proxy_pooled_df, "pooled")
summary_fe_proxy     <- run_within_firm_test(lhs, proxy_fe_df, "within-buyer")


# ══════════════════════════════════════════════════════════════════════════════
#   COMBINED COMPARISON
# ══════════════════════════════════════════════════════════════════════════════
cat("\n\n")
cat(strrep("#", 70), "\n")
cat("# COMBINED COMPARISON: POOLED vs WITHIN-BUYER PROXY\n")
cat(strrep("#", 70), "\n\n")

summary_table <- bind_rows(summary_pooled_proxy, summary_fe_proxy)

cat(sprintf("  %-15s  %-30s  %8s  %8s  %8s  %10s\n",
            "Proxy", "Specification", "Coef", "SE", "t-stat", "p-value"))
cat("  ", strrep("-", 90), "\n")
for (r in seq_len(nrow(summary_table))) {
  cat(sprintf("  %-15s  %-30s  %8.4f  %8.4f  %8.2f  %10.4f\n",
              summary_table$proxy[r],
              summary_table$spec[r],
              summary_table$coef[r], summary_table$se[r],
              summary_table$t_stat[r], summary_table$p_value[r]))
}

# Side-by-side attenuation comparison
cat("\n  Attenuation comparison:\n")
for (px in c("pooled", "within-buyer")) {
  rows <- summary_table[summary_table$proxy == px, ]
  if (nrow(rows) >= 2) {
    att <- 1 - rows$coef[2] / rows$coef[1]
    cat(sprintf("    %-15s  pooled coef: %7.4f → firm FE coef: %7.4f  (attenuation: %.1f%%)\n",
                px, rows$coef[1], rows$coef[2], 100 * att))
  }
}

cat("\nInterpretation:\n")
cat("  If the proxy coefficient remains significant with firm FE, the relationship\n")
cat("  between supplier purchases and emissions operates within firms over time,\n")
cat("  ruling out time-invariant confounds (sector, baseline size, inherent\n")
cat("  emission intensity). A large attenuation (>50%) suggests the pooled signal\n")
cat("  is partly driven by cross-sectional sorting, but a surviving significant\n")
cat("  coefficient still confirms a within-firm channel.\n")
cat("  Comparing the two proxies shows whether the within-buyer elastic net\n")
cat("  (which controls for buyer characteristics in the selection stage) yields\n")
cat("  a proxy with different within-firm signal properties.\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(summary_table,
          file.path(OUTPUT_DIR, "enet_within_firm_test.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "enet_within_firm_test.csv"), "\n")
