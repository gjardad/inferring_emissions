###############################################################################
# figures_tables/table_proxy_ols.R
#
# PURPOSE
#   OLS regressions comparing the EN-selected fuel-supply proxy and the
#   Tabachova (fuel-related NACE) proxy in explaining firm-level emissions.
#
#   Specification: asinh(y) ~ proxy (+ controls).
#
#   Six columns:
#     EN-selected Suppliers:
#       (1) asinh(y) ~ log(revenue) + EN proxy + year FE
#       (2) asinh(y) ~ log(revenue) + EN proxy + year FE + sector FE
#       (3) asinh(y) ~ log(revenue) + EN proxy + year FE + firm FE
#     Fuel-related Suppliers:
#       (4) asinh(y) ~ log(revenue) + Tabachova proxy + year FE
#       (5) asinh(y) ~ log(revenue) + Tabachova proxy + year FE + sector FE
#       (6) asinh(y) ~ log(revenue) + Tabachova proxy + year FE + firm FE
#
#   Sample: all training-sample firms (no restriction on y > 0 or proxy > 0).
#   Standard errors clustered at the firm level.
#
# INPUTS
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/fold_specific_proxy_asinh.RData
#
# OUTPUTS
#   {OUTPUT_DIR}/proxy_ols_regression.tex
#
# RUNS ON: local 1
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
library(sandwich)
library(lmtest)

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

cat("Loading fold-specific proxy (asinh LHS)...\n")
load(file.path(PROC_DATA, "fold_specific_proxy_asinh.RData"))

# Merge
panel <- training_sample %>%
  left_join(fs_proxy_panel_asinh %>% select(vat, year, fold_specific_proxy_asinh),
            by = c("vat", "year")) %>%
  mutate(fold_specific_proxy_asinh = coalesce(fold_specific_proxy_asinh, 0),
         proxy_tabachova_asinh     = coalesce(proxy_tabachova_asinh, 0))
rm(training_sample, fs_proxy_panel_asinh)

# Log revenue (matches the EN specification)
if (!"log_revenue" %in% names(panel)) {
  if ("turnover_VAT" %in% names(panel)) {
    panel$log_revenue <- log(coalesce(panel$turnover_VAT, 1))
  } else {
    stop("log_revenue not found in training_sample")
  }
}

# ── Regression sample ────────────────────────────────────────────────────────
reg <- panel %>%
  filter(!is.na(y), !is.na(log_revenue)) %>%
  mutate(
    asinh_y    = asinh(y),
    year_f     = factor(year),
    sector_f   = factor(nace2d),
    firm_f     = factor(vat)
  )

cat("Regression sample:", nrow(reg), "obs,", n_distinct(reg$vat), "firms\n")

# ── Estimate models ──────────────────────────────────────────────────────────
# EN-selected proxy: columns (1)-(3)
m1 <- lm(asinh_y ~ log_revenue + fold_specific_proxy_asinh + year_f, data = reg)
m2 <- lm(asinh_y ~ log_revenue + fold_specific_proxy_asinh + year_f + sector_f, data = reg)
m3 <- lm(asinh_y ~ log_revenue + fold_specific_proxy_asinh + year_f + firm_f, data = reg)

# Tabachova proxy: columns (4)-(6)
m4 <- lm(asinh_y ~ log_revenue + proxy_tabachova_asinh + year_f, data = reg)
m5 <- lm(asinh_y ~ log_revenue + proxy_tabachova_asinh + year_f + sector_f, data = reg)
m6 <- lm(asinh_y ~ log_revenue + proxy_tabachova_asinh + year_f + firm_f, data = reg)

# ── Firm-clustered standard errors ───────────────────────────────────────────
cluster_vcov <- function(model, cluster_var) {
  vcovCL(model, cluster = cluster_var, type = "HC1")
}

ct1 <- coeftest(m1, vcov. = cluster_vcov(m1, reg$vat))
ct2 <- coeftest(m2, vcov. = cluster_vcov(m2, reg$vat))
ct3 <- coeftest(m3, vcov. = cluster_vcov(m3, reg$vat))
ct4 <- coeftest(m4, vcov. = cluster_vcov(m4, reg$vat))
ct5 <- coeftest(m5, vcov. = cluster_vcov(m5, reg$vat))
ct6 <- coeftest(m6, vcov. = cluster_vcov(m6, reg$vat))

# ── Formatting helpers ───────────────────────────────────────────────────────
stars <- function(p) {
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  return("")
}

extract_coef <- function(ct, varname) {
  idx <- which(rownames(ct) == varname)
  if (length(idx) == 0) return(list(est = NA, se = NA, p = NA))
  list(est = ct[idx, 1], se = ct[idx, 2], p = ct[idx, 4])
}

fmt_coef_row <- function(ct_list, varname) {
  vals <- lapply(ct_list, extract_coef, varname = varname)
  est_line <- paste(
    sapply(vals, function(v) {
      if (is.na(v$est)) return("")
      sprintf("%.3f%s", v$est, stars(v$p))
    }),
    collapse = " & "
  )
  se_line <- paste(
    sapply(vals, function(v) {
      if (is.na(v$se)) return("")
      sprintf("(%.3f)", v$se)
    }),
    collapse = " & "
  )
  list(est = est_line, se = se_line)
}

# ── Build LaTeX table ────────────────────────────────────────────────────────
# EN columns (1)-(3): extract log_revenue and EN proxy
cts_en <- list(ct1, ct2, ct3)
rev_en     <- fmt_coef_row(cts_en, "log_revenue")
proxy_en   <- fmt_coef_row(cts_en, "fold_specific_proxy_asinh")

# Tabachova columns (4)-(6): extract log_revenue and Tabachova proxy
cts_tab <- list(ct4, ct5, ct6)
rev_tab    <- fmt_coef_row(cts_tab, "log_revenue")
proxy_tab  <- fmt_coef_row(cts_tab, "proxy_tabachova_asinh")

models  <- list(m1, m2, m3, m4, m5, m6)
n_obs   <- sapply(models, nobs)
r2_vals <- sapply(models, function(m) summary(m)$r.squared)

n_firms_str <- format(n_distinct(reg$vat), big.mark = ",")

tex <- c(
  "\\begin{tabular}{l cccccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{EN-selected Suppliers} & \\multicolumn{3}{c}{Fuel-related Suppliers} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  sprintf("log(revenue) & %s & %s \\\\", rev_en$est, rev_tab$est),
  sprintf(" & %s & %s \\\\", rev_en$se, rev_tab$se),
  "\\addlinespace",
  sprintf("Proxy & %s & %s \\\\", proxy_en$est, proxy_tab$est),
  sprintf(" & %s & %s \\\\", proxy_en$se, proxy_tab$se),
  "\\midrule",
  "Year FE & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Sector FE & No & Yes & No & No & Yes & No \\\\",
  "Firm FE & No & No & Yes & No & No & Yes \\\\",
  "\\midrule",
  sprintf("$R^2$ & %.3f & %.3f & %.3f & %.3f & %.3f & %.3f \\\\",
          r2_vals[1], r2_vals[2], r2_vals[3], r2_vals[4], r2_vals[5], r2_vals[6]),
  sprintf("Firms & %s & %s & %s & %s & %s & %s \\\\",
          n_firms_str, n_firms_str, n_firms_str,
          n_firms_str, n_firms_str, n_firms_str),
  sprintf("$N$ & %s & %s & %s & %s & %s & %s \\\\",
          format(n_obs[1], big.mark = ","), format(n_obs[2], big.mark = ","),
          format(n_obs[3], big.mark = ","), format(n_obs[4], big.mark = ","),
          format(n_obs[5], big.mark = ","), format(n_obs[6], big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{7}{p{\\linewidth}}{\\footnotesize \\textit{Notes:} Dependent variable: $\\operatorname{asinh}(y)$, where $y$ is verified emissions (tCO\\textsubscript{2}). ``EN-selected Suppliers'' uses the coefficient-weighted fuel-supply proxy from the asinh-LHS elastic net. ``Fuel-related Suppliers'' uses the Tabachova proxy (sum of asinh purchases from suppliers in fuel-related NACE codes). All training-sample firms. Standard errors clustered at the firm level in parentheses. $^{***}$\\,$p<0.01$, $^{**}$\\,$p<0.05$, $^{*}$\\,$p<0.10$.}",
  "\\end{tabular}"
)

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "proxy_ols_regression.tex")
writeLines(tex, out_path)
cat("Saved proxy OLS table to:", out_path, "\n")

# ── Print summary to console ─────────────────────────────────────────────────
cat("\n=== EN-selected Suppliers ===\n")
cat("\n--- Column (1): Year FE ---\n")
print(ct1[c("log_revenue", "fold_specific_proxy_asinh"), ])
cat("\n--- Column (2): Year + Sector FE ---\n")
print(ct2[c("log_revenue", "fold_specific_proxy_asinh"), ])
cat("\n--- Column (3): Year + Firm FE ---\n")
print(ct3[c("log_revenue", "fold_specific_proxy_asinh"), ])

cat("\n=== Fuel-related Suppliers (Tabachova) ===\n")
cat("\n--- Column (4): Year FE ---\n")
print(ct4[c("log_revenue", "proxy_tabachova_asinh"), ])
cat("\n--- Column (5): Year + Sector FE ---\n")
print(ct5[c("log_revenue", "proxy_tabachova_asinh"), ])
cat("\n--- Column (6): Year + Firm FE ---\n")
print(ct6[c("log_revenue", "proxy_tabachova_asinh"), ])
