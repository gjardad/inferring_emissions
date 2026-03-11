###############################################################################
# figures_tables/table_proxy_comparison.R
#
# PURPOSE
#   Compare R² and Spearman ρ across proxy variants:
#     EN proxy (levels-LHS vs asinh-LHS) × Tabachova proxy (levels vs asinh)
#   Univariate regressions: y ~ proxy (levels) and asinh(y) ~ proxy.
#   Reports R², Spearman ρ, and coverage for each proxy variant.
#
# INPUTS
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUTS
#   Console output (results to be formatted for paper Section 3)
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

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading firm-year panel with proxies...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

panel <- training_sample
rm(training_sample)

reg <- panel %>% filter(!is.na(y))

# ── Regressions ──────────────────────────────────────────────────────────────
cat("\n", strrep("=", 70), "\n")
cat("  PROXY COMPARISON: R² from univariate regressions\n")
cat(strrep("=", 70), "\n\n")

# All y ~ proxy (levels)
m1 <- lm(y ~ fold_specific_proxy, data = reg)
m2 <- lm(y ~ fold_specific_proxy_asinh, data = reg)
m3 <- lm(y ~ proxy_tabachova, data = reg)
m4 <- lm(y ~ proxy_tabachova_asinh, data = reg)

# Natural spec for asinh EN: asinh(y) ~ proxy
m5 <- lm(asinh(y) ~ fold_specific_proxy_asinh, data = reg)
# Also: asinh(y) ~ levels EN proxy for comparison
m6 <- lm(asinh(y) ~ fold_specific_proxy, data = reg)
# And: asinh(y) ~ tabachova asinh
m7 <- lm(asinh(y) ~ proxy_tabachova_asinh, data = reg)

cat(sprintf("  %-45s  R² = %.4f\n", "y ~ EN proxy (levels LHS)", summary(m1)$r.squared))
cat(sprintf("  %-45s  R² = %.4f\n", "y ~ EN proxy (asinh LHS)", summary(m2)$r.squared))
cat(sprintf("  %-45s  R² = %.4f\n", "y ~ Tabachova (levels)", summary(m3)$r.squared))
cat(sprintf("  %-45s  R² = %.4f\n", "y ~ Tabachova (asinh)", summary(m4)$r.squared))

cat("\n")
cat(sprintf("  %-45s  R² = %.4f\n", "asinh(y) ~ EN proxy (asinh LHS)", summary(m5)$r.squared))
cat(sprintf("  %-45s  R² = %.4f\n", "asinh(y) ~ EN proxy (levels LHS)", summary(m6)$r.squared))
cat(sprintf("  %-45s  R² = %.4f\n", "asinh(y) ~ Tabachova (asinh)", summary(m7)$r.squared))

# ── Coverage ─────────────────────────────────────────────────────────────────
cat("\n\n  Coverage:\n")
cat(sprintf("    EN levels proxy > 0:    %d (%.1f%%)\n",
    sum(reg$fold_specific_proxy > 0), 100 * mean(reg$fold_specific_proxy > 0)))
cat(sprintf("    EN asinh proxy > 0:     %d (%.1f%%)\n",
    sum(reg$fold_specific_proxy_asinh > 0), 100 * mean(reg$fold_specific_proxy_asinh > 0)))
cat(sprintf("    Tabachova levels > 0:   %d (%.1f%%)\n",
    sum(reg$proxy_tabachova > 0), 100 * mean(reg$proxy_tabachova > 0)))
cat(sprintf("    Tabachova asinh > 0:    %d (%.1f%%)\n",
    sum(reg$proxy_tabachova_asinh > 0), 100 * mean(reg$proxy_tabachova_asinh > 0)))

# ── Spearman correlations ────────────────────────────────────────────────────
cat("\n\n  Spearman ρ (proxy vs y, full sample):\n")
cat(sprintf("    EN levels proxy:   %.4f\n", cor(reg$y, reg$fold_specific_proxy, method = "spearman")))
cat(sprintf("    EN asinh proxy:    %.4f\n", cor(reg$y, reg$fold_specific_proxy_asinh, method = "spearman")))
cat(sprintf("    Tabachova levels:  %.4f\n", cor(reg$y, reg$proxy_tabachova, method = "spearman")))
cat(sprintf("    Tabachova asinh:   %.4f\n", cor(reg$y, reg$proxy_tabachova_asinh, method = "spearman")))

# ── Supplier overlap: EN levels vs EN asinh ──────────────────────────────────
cat("\n\n  EN proxy correlation (levels vs asinh specification):\n")
both_pos <- reg$fold_specific_proxy > 0 & reg$fold_specific_proxy_asinh > 0
cat(sprintf("    Pearson (both > 0):  %.4f\n", cor(reg$fold_specific_proxy[both_pos], reg$fold_specific_proxy_asinh[both_pos])))
cat(sprintf("    Spearman (both > 0): %.4f\n", cor(reg$fold_specific_proxy[both_pos], reg$fold_specific_proxy_asinh[both_pos], method = "spearman")))
cat(sprintf("    N both > 0: %d\n", sum(both_pos)))

cat("\n  N =", nrow(reg), "\n")
