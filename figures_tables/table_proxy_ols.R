###############################################################################
# figures_tables/table_proxy_ols.R
#
# PURPOSE
#   OLS regressions showing the fuel-supply proxy has bite in explaining
#   firm-level emissions, conditional on revenue, year, sector, and firm FE.
#
#   Four columns:
#     (1) log(y) ~ log(revenue) + log(proxy) + year FE
#     (2) log(y) ~ log(revenue) + log(proxy) + year FE + sector FE
#     (3) log(y) ~ log(revenue) + log(proxy) + year FE + firm FE
#     (4) log(y) ~ log(proxy)  [no FE, no controls]
#
#   Sample: EU ETS emitters with y > 0 and fold_specific_proxy > 0.
#   Standard errors clustered at the firm level.
#
# INPUTS
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/fold_specific_proxy.RData
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

cat("Loading fold-specific proxy...\n")
load(file.path(PROC_DATA, "fold_specific_proxy.RData"))

# Merge
panel <- training_sample %>%
  left_join(fs_proxy_panel %>% select(vat, year, fold_k, fold_specific_proxy,
                                       primary_nace2d),
            by = c("vat", "year")) %>%
  mutate(fold_specific_proxy = coalesce(fold_specific_proxy, 0))
rm(training_sample, fs_proxy_panel)

# Revenue
if ("turnover_VAT" %in% names(panel)) {
  panel$revenue <- coalesce(panel$turnover_VAT, exp(panel$log_revenue))
} else {
  panel$revenue <- exp(panel$log_revenue)
}

# ── Regression sample ────────────────────────────────────────────────────────
reg <- panel %>%
  filter(y > 0, fold_specific_proxy > 0, revenue > 0) %>%
  mutate(
    log_y      = log(y),
    log_rev    = log(revenue),
    log_proxy  = log(fold_specific_proxy),
    year_f     = factor(year),
    sector_f   = factor(nace2d),
    firm_f     = factor(vat)
  )

cat("Regression sample:", nrow(reg), "obs,", n_distinct(reg$vat), "firms\n")

# ── Estimate models ──────────────────────────────────────────────────────────
m1 <- lm(log_y ~ log_rev + log_proxy + year_f, data = reg)
m2 <- lm(log_y ~ log_rev + log_proxy + year_f + sector_f, data = reg)
m3 <- lm(log_y ~ log_rev + log_proxy + year_f + firm_f, data = reg)
m4 <- lm(log_y ~ log_proxy, data = reg)

# ── Firm-clustered standard errors ───────────────────────────────────────────
cluster_vcov <- function(model, cluster_var) {
  vcovCL(model, cluster = cluster_var, type = "HC1")
}

ct1 <- coeftest(m1, vcov. = cluster_vcov(m1, reg$vat))
ct2 <- coeftest(m2, vcov. = cluster_vcov(m2, reg$vat))
ct3 <- coeftest(m3, vcov. = cluster_vcov(m3, reg$vat))
ct4 <- coeftest(m4, vcov. = cluster_vcov(m4, reg$vat))

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
cts <- list(ct1, ct2, ct3, ct4)

rev_rows   <- fmt_coef_row(cts, "log_rev")
proxy_rows <- fmt_coef_row(cts, "log_proxy")

models  <- list(m1, m2, m3, m4)
n_obs   <- sapply(models, nobs)
r2_vals <- sapply(models, function(m) summary(m)$r.squared)

n_firms_str <- format(n_distinct(reg$vat), big.mark = ",")

tex <- c(
  "\\begin{tabular}{l *{4}{>{\\centering\\arraybackslash}p{1.5cm}}}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  "\\midrule",
  sprintf("log(revenue) & %s \\\\", rev_rows$est),
  sprintf(" & %s \\\\", rev_rows$se),
  "\\addlinespace",
  sprintf("log(proxy) & %s \\\\", proxy_rows$est),
  sprintf(" & %s \\\\", proxy_rows$se),
  "\\midrule",
  sprintf("Year FE & Yes & Yes & Yes & No \\\\"),
  sprintf("Sector FE & No & Yes & No & No \\\\"),
  sprintf("Firm FE & No & No & Yes & No \\\\"),
  "\\midrule",
  sprintf("$R^2$ & %.3f & %.3f & %.3f & %.3f \\\\", r2_vals[1], r2_vals[2], r2_vals[3], r2_vals[4]),
  sprintf("Firms & %s & %s & %s & %s \\\\",
          n_firms_str, n_firms_str, n_firms_str, n_firms_str),
  sprintf("$N$ & %s & %s & %s & %s \\\\",
          format(n_obs[1], big.mark = ","),
          format(n_obs[2], big.mark = ","),
          format(n_obs[3], big.mark = ","),
          format(n_obs[4], big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.75\\textwidth}}{\\footnotesize \\textit{Notes:} Sample restricted to EU ETS firms with positive emissions and positive proxy. Standard errors clustered at the firm level in parentheses. $^{***}$\\,$p<0.01$, $^{**}$\\,$p<0.05$, $^{*}$\\,$p<0.10$.}",
  "\\end{tabular}"
)

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "proxy_ols_regression.tex")
writeLines(tex, out_path)
cat("Saved proxy OLS table to:", out_path, "\n")

# ── Print summary to console ─────────────────────────────────────────────────
cat("\n--- Column (1): Year FE ---\n")
print(ct1[c("log_rev", "log_proxy"), ])
cat("\n--- Column (2): Year + Sector FE ---\n")
print(ct2[c("log_rev", "log_proxy"), ])
cat("\n--- Column (3): Year + Firm FE ---\n")
print(ct3[c("log_rev", "log_proxy"), ])
cat("\n--- Column (4): Proxy only ---\n")
print(ct4["log_proxy", ])
cat("R2:", summary(m4)$r.squared, "\n")
