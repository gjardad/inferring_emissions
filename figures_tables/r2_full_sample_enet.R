###############################################################################
# figures_tables/r2_full_sample_enet.R
#
# PURPOSE
#   Compare the R² of the full-sample elastic net proxy (proxy_weighted) with
#   the fold-specific (CV) proxy (fold_specific_proxy). The full-sample proxy
#   is built from an elastic net estimated on ALL training firms; the CV proxy
#   is built from an elastic net that never saw the held-out firms' emissions.
#
#   If R²(full-sample) >> R²(CV), the gap is due to limited supplier overlap
#   across sectors (the EN selects different suppliers for different folds).
#   If R²(full-sample) ≈ R²(CV), suppliers generalize well across sectors.
#
#   Regressions:
#     (1) log(y) ~ log(proxy_weighted)              [full-sample proxy]
#     (2) log(y) ~ log(fold_specific_proxy)          [CV proxy]
#     (3) log(y) ~ log(proxy_weighted) + year FE + sector FE
#     (4) log(y) ~ log(fold_specific_proxy) + year FE + sector FE
#
#   Sample: EU ETS emitters with y > 0 and proxy > 0 (both proxies positive).
#
# INPUTS
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/fold_specific_proxy.RData
#
# OUTPUTS
#   Console output (diagnostic, not a paper table)
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

cat("Loading fold-specific proxy...\n")
load(file.path(PROC_DATA, "fold_specific_proxy.RData"))

# Merge
panel <- training_sample %>%
  left_join(fs_proxy_panel %>% select(vat, year, fold_specific_proxy),
            by = c("vat", "year")) %>%
  mutate(fold_specific_proxy = coalesce(fold_specific_proxy, 0))
rm(training_sample, fs_proxy_panel)

# ── Regression sample ────────────────────────────────────────────────────────
# Common sample: both proxies positive, y > 0
reg <- panel %>%
  filter(y > 0, proxy_weighted > 0, fold_specific_proxy > 0) %>%
  mutate(
    log_y       = log(y),
    log_pw      = log(proxy_weighted),
    log_fsp     = log(fold_specific_proxy),
    year_f      = factor(year),
    sector_f    = factor(nace2d)
  )

cat("Common sample (both proxies > 0, y > 0):", nrow(reg), "obs,",
    n_distinct(reg$vat), "firms\n\n")

# Also report sample sizes when using each proxy alone
reg_pw <- panel %>% filter(y > 0, proxy_weighted > 0)
reg_fsp <- panel %>% filter(y > 0, fold_specific_proxy > 0)
cat("Full-sample proxy only (y > 0, proxy_weighted > 0):", nrow(reg_pw), "obs\n")
cat("CV proxy only (y > 0, fold_specific_proxy > 0):    ", nrow(reg_fsp), "obs\n\n")

# ── Regressions ──────────────────────────────────────────────────────────────
m1 <- lm(log_y ~ log_pw, data = reg)
m2 <- lm(log_y ~ log_fsp, data = reg)
m3 <- lm(log_y ~ log_pw + year_f + sector_f, data = reg)
m4 <- lm(log_y ~ log_fsp + year_f + sector_f, data = reg)

cat("=== R² COMPARISON: Full-Sample vs CV Proxy ===\n\n")
cat(sprintf("%-45s  R² = %.4f\n", "(1) log(y) ~ log(proxy_weighted)",
            summary(m1)$r.squared))
cat(sprintf("%-45s  R² = %.4f\n", "(2) log(y) ~ log(fold_specific_proxy)",
            summary(m2)$r.squared))
cat(sprintf("%-45s  R² = %.4f\n", "(3) log(y) ~ log(proxy_weighted) + FE",
            summary(m3)$r.squared))
cat(sprintf("%-45s  R² = %.4f\n", "(4) log(y) ~ log(fold_specific_proxy) + FE",
            summary(m4)$r.squared))

cat(sprintf("\nR² gap (no FE):  %.4f  (full-sample is %.1f%% higher)\n",
            summary(m1)$r.squared - summary(m2)$r.squared,
            100 * (summary(m1)$r.squared / summary(m2)$r.squared - 1)))
cat(sprintf("R² gap (with FE): %.4f  (full-sample is %.1f%% higher)\n",
            summary(m3)$r.squared - summary(m4)$r.squared,
            100 * (summary(m3)$r.squared / summary(m4)$r.squared - 1)))

# ── Correlations between the two proxies ─────────────────────────────────────
cat("\n=== PROXY CORRELATIONS (common sample) ===\n")
cat(sprintf("Pearson (levels):  %.3f\n",
            cor(reg$proxy_weighted, reg$fold_specific_proxy)))
cat(sprintf("Pearson (logs):    %.3f\n",
            cor(reg$log_pw, reg$log_fsp)))
cat(sprintf("Spearman (ranks):  %.3f\n",
            cor(reg$proxy_weighted, reg$fold_specific_proxy, method = "spearman")))

# ── Coefficient comparison ───────────────────────────────────────────────────
cat("\n=== COEFFICIENTS ===\n")
cat(sprintf("(1) beta(log proxy_weighted):        %.3f  (SE %.3f)\n",
            coef(m1)["log_pw"], summary(m1)$coefficients["log_pw", "Std. Error"]))
cat(sprintf("(2) beta(log fold_specific_proxy):   %.3f  (SE %.3f)\n",
            coef(m2)["log_fsp"], summary(m2)$coefficients["log_fsp", "Std. Error"]))
cat(sprintf("(3) beta(log proxy_weighted) + FE:   %.3f  (SE %.3f)\n",
            coef(m3)["log_pw"], summary(m3)$coefficients["log_pw", "Std. Error"]))
cat(sprintf("(4) beta(log fold_specific_proxy)+FE:%.3f  (SE %.3f)\n",
            coef(m4)["log_fsp"], summary(m4)$coefficients["log_fsp", "Std. Error"]))

cat("\nDone.\n")
