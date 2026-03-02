###############################################################################
# figures_tables/table_rho_star.R
#
# PURPOSE
#   Generate LaTeX table for rho* test results (weighted proxy, n_firms only).
#   Shows the largest rho* consistent with all large sectors rejecting
#   H0: rho_s <= rho* and small sectors falling within a 2 SD band.
#
# INPUT
#   {OUTPUT_DIR}/rho_star_test_results.csv
#
# OUTPUT
#   {OUTPUT_DIR}/rho_star_test_table.tex
###############################################################################

# ---- Paths -----------------------------------------------------------------
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

# ---- Load results ----------------------------------------------------------
res <- read.csv(file.path(OUTPUT_DIR, "rho_star_test_results.csv"),
                stringsAsFactors = FALSE)

# Filter: n_firms assumption, weighted proxy, strict_2sd test
df <- res[res$n_assumption == "n_firms" &
          res$proxy == "weighted" &
          res$test == "strict_2sd", ]

df <- df[order(df$n_min), ]

# ---- Build LaTeX -----------------------------------------------------------
tex <- character()
tex <- c(tex, "\\begin{tabular}{c c c c c}")
tex <- c(tex, "\\toprule")
tex <- c(tex, "Threshold on \\# firms & Large sectors & Small sectors & $\\rho^*$ & \\% within band \\\\")
tex <- c(tex, "\\midrule")

for (i in seq_len(nrow(df))) {
  r <- df[i, ]
  rho_str <- if (is.na(r$rho_star)) "---" else sprintf("%.3f", r$rho_star)
  pct_str <- if (is.na(r$frac_ok)) "---" else sprintf("%.0f", r$frac_ok * 100)
  tex <- c(tex, sprintf("%d & %d & %d & %s & %s \\\\",
                        r$n_min, r$n_large, r$n_small, rho_str, pct_str))
}

tex <- c(tex, "\\bottomrule")
tex <- c(tex, "\\end{tabular}")
tex <- c(tex, "")
tex <- c(tex, "\\medskip")
tex <- c(tex, "{\\footnotesize")

note <- paste0(
  "\\textit{Notes:} $\\rho^*$ is the largest value such that ",
  "$H_0\\colon \\rho_s \\leq \\rho^*$ is rejected at 5\\% (one-sided) ",
  "for all large sectors, using the coefficient-weighted fuel-supply proxy. ",
  "``\\% within band'' is the share of small sectors whose observed ",
  "$\\hat{\\rho}_s$ falls within a 2\\,SD band of $\\rho^*$. ",
  "Variance is $\\text{Var}(\\hat{\\rho}_s) = 1/(n_s - 1)$, where $n_s$ is ",
  "the number of firms in sector~$s$. ",
  "Using the number of firms as the effective sample size is justified by the ",
  "within-firm, across-year Spearman rank autocorrelation of 0.95, which implies ",
  "that additional years of data contribute negligible independent ranking ",
  "information per firm.}"
)
tex <- c(tex, note)

# ---- Save ------------------------------------------------------------------
out_fn <- file.path(OUTPUT_DIR, "rho_star_test_table.tex")
writeLines(tex, out_fn)
cat("Saved:", out_fn, "\n")
