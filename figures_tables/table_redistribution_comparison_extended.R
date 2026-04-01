###############################################################################
# figures_tables/table_redistribution_comparison_extended.R
#
# PURPOSE
#   Generate LaTeX table comparing 5 redistribution methods:
#   Sinh-calibrated, Quantile mapping, GPA, GEV, GLO.
#
# INPUT
#   {OUTPUT_DIR}/redistribution_comparison.RData
#     (produced by analysis/active/diagnostic_redistribution_comparison.R)
#
# OUTPUT
#   {OUTPUT_DIR}/table_redistribution_comparison.tex
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
REPO_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
  error = function(e) normalizePath(getwd(), winslash = "/")
)
while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
source(file.path(REPO_DIR, "paths.R"))

# ── Load results ─────────────────────────────────────────────────────────────
load(file.path(OUTPUT_DIR, "redistribution_comparison.RData"))

# ── Select methods (drop Log-normal) ─────────────────────────────────────────
METHODS <- c("Sinh-calibrated", "Quantile mapping", "GPA", "GEV", "GLO")
N_METHODS <- length(METHODS)
COL_HEADERS <- c("Sinh-calibrated", "Quantile mapping", "GPA", "GEV", "GLO")

fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt0 <- function(x)             formatC(x, format = "f", digits = 0)

# ── Build LaTeX ──────────────────────────────────────────────────────────────
col_spec <- paste(rep("c", N_METHODS), collapse = "")
header_cols <- paste(COL_HEADERS, collapse = " & ")

tex <- c(
  sprintf("\\begin{tabular}{l %s}", col_spec),
  "\\toprule",
  sprintf(" & %s \\\\", header_cols),
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Firm-level metrics (emitters only)}} \\\\"
)

# Firm-level metrics
for (metric in c("Pearson", "Spearman", "RMSE")) {
  mat <- switch(metric,
    Pearson  = repeat_pearson,
    Spearman = repeat_spearman,
    RMSE     = repeat_rmse_lev)
  f <- if (metric == "RMSE") fmt0 else fmt
  vals <- paste(sapply(METHODS, function(m) {
    mn <- mean(mat[, m], na.rm = TRUE)
    s  <- sd(mat[, m], na.rm = TRUE) / sqrt(M)
    sprintf("%s {\\scriptsize(%s)}", f(mn), f(s))
  }), collapse = " & ")
  tex <- c(tex, sprintf("%s & %s \\\\", metric, vals))
}

# Distributional bias
tex <- c(tex,
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Distributional bias (imputed $-$ actual, mean across cells)}} \\\\"
)
stat_labels <- c("$p_{90}/p_{10}$", "$\\log p_{90} - \\log p_{10}$", "Gini",
                 "$\\tau_3$ (L-skew)", "$\\tau_4$ (L-kurt)")
for (si in seq_along(STAT_NAMES)) {
  vals <- paste(sapply(METHODS, function(m) {
    mn <- mean(repeat_bias[, STAT_NAMES[si], m], na.rm = TRUE)
    s  <- sd(repeat_bias[, STAT_NAMES[si], m], na.rm = TRUE) / sqrt(M)
    sprintf("%s {\\scriptsize(%s)}", fmt(mn), fmt(s))
  }), collapse = " & ")
  tex <- c(tex, sprintf("%s & %s \\\\", stat_labels[si], vals))
}

# Distributional RMSE
tex <- c(tex,
  "\\midrule",
  "\\multicolumn{6}{l}{\\textit{Distributional RMSE (across cells)}} \\\\"
)
for (si in seq_along(STAT_NAMES)) {
  vals <- paste(sapply(METHODS, function(m) {
    mn <- mean(repeat_rmse[, STAT_NAMES[si], m], na.rm = TRUE)
    s  <- sd(repeat_rmse[, STAT_NAMES[si], m], na.rm = TRUE) / sqrt(M)
    sprintf("%s {\\scriptsize(%s)}", fmt(mn), fmt(s))
  }), collapse = " & ")
  tex <- c(tex, sprintf("%s & %s \\\\", stat_labels[si], vals))
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")

# ── Write ────────────────────────────────────────────────────────────────────
tex_path <- file.path(OUTPUT_DIR, "table_redistribution_comparison.tex")
writeLines(tex, tex_path)
cat("LaTeX table written to:", tex_path, "\n")
