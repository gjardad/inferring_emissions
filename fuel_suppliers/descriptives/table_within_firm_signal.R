###############################################################################
# fuel_suppliers/descriptives/table_within_firm_signal.R
#
# PURPOSE
#   Generate a paper-ready LaTeX table from the within-firm signal test results.
#
#   T3 — Within-firm signal (enet_within_firm_table.tex):
#     Tests whether within-firm variation in purchases from elastic-net-
#     identified suppliers predicts within-firm variation in emissions.
#     Reports the coefficient on asinh(proxy) under three specifications
#     (pooled OLS, firm FE, first differences), separately for the pooled
#     and within-buyer proxies.
#
#     Structure: two panels (pooled proxy, within-buyer proxy), each with
#     three rows (specifications) and columns for coefficient, SE, N, R².
#
# INPUTS
#   {OUTPUT_DIR}/enet_within_firm_test.csv
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_within_firm_table.tex
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


# ── Load results ─────────────────────────────────────────────────────────
csv_path <- file.path(OUTPUT_DIR, "enet_within_firm_test.csv")
if (!file.exists(csv_path)) stop("Within-firm test results not found: ", csv_path)

df <- read.csv(csv_path, stringsAsFactors = FALSE)


# ── Specification labels ─────────────────────────────────────────────────
spec_labels <- c(
  "Pooled (sector + year FE)"      = "Sector + year FE",
  "Firm FE (+ year FE)"            = "Firm + year FE",
  "First differences (+ year FE)"  = "First differences"
)


# ── Formatting helpers ────────────────────────────────────────────────────
fmt4 <- function(x) sprintf("%.4f", as.numeric(x))
fmt3 <- function(x) sprintf("%.3f", as.numeric(x))
fmt_n <- function(x) format(as.integer(x), big.mark = ",")

# Significance stars
stars <- function(p) {
  p <- as.numeric(p)
  if (p < 0.001) return("$^{***}$")
  if (p < 0.01)  return("$^{**}$")
  if (p < 0.05)  return("$^{*}$")
  return("")
}


# ── Build LaTeX ──────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{l cc r r}",
  "\\toprule",
  "Specification & $\\hat{\\beta}$ & (SE) & $N$ & $R^2$ \\\\",
  "\\midrule"
)

proxy_labels <- c("pooled" = "Panel A: Pooled proxy",
                   "within-buyer" = "Panel B: Within-buyer proxy")

proxies <- unique(df$proxy)

for (px_idx in seq_along(proxies)) {
  px <- proxies[px_idx]
  px_rows <- df[df$proxy == px, ]

  # Panel header
  label <- if (px %in% names(proxy_labels)) proxy_labels[px] else px
  tex <- c(tex, sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\[2pt]", label))

  for (i in seq_len(nrow(px_rows))) {
    r <- px_rows[i, ]
    spec_lab <- if (r$spec %in% names(spec_labels)) spec_labels[r$spec] else r$spec

    line <- sprintf(
      "\\quad %s & %s%s & (%s) & %s & %s \\\\",
      spec_lab,
      fmt4(r$coef), stars(r$p_value),
      fmt4(r$se),
      fmt_n(r$n),
      fmt3(r$r2)
    )
    tex <- c(tex, line)
  }

  # Visual break between panels
  if (px_idx < length(proxies)) tex <- c(tex, "\\addlinespace[6pt]")
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")


# ── Save ──────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "enet_within_firm_table.tex")
writeLines(tex, out_path)
cat("Saved T3 (within-firm signal) to:", out_path, "\n")
