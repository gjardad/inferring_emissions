###############################################################################
# fuel_suppliers/descriptives/table_placebo_test.R
#
# PURPOSE
#   Generate a paper-ready LaTeX table from the placebo test results.
#
#   T2 — Placebo tests (enet_placebo_test_table.tex):
#     Tests whether the elastic net signal is driven by emissions rather than
#     spurious correlation. Each row replaces the LHS with a placebo variable,
#     reruns elastic net, and reports how many suppliers are selected and
#     their overlap with the real selection.
#
#     Rows:
#       Real (emissions)
#       Fully shuffled emissions (mean over 50 permutations)
#       Employment as LHS
#       Value added as LHS
#       Tangible assets as LHS
#       Shuffled within sector-year (mean over 50 permutations)
#
#     Columns:
#       Suppliers selected (n_positive)
#       Overlap with real selection
#       Jaccard similarity with real selection
#
# INPUTS
#   {OUTPUT_DIR}/enet_placebo_test.csv
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_placebo_test_table.tex
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
csv_path <- file.path(OUTPUT_DIR, "enet_placebo_test.csv")
if (!file.exists(csv_path)) stop("Placebo results not found: ", csv_path)

df <- read.csv(csv_path, stringsAsFactors = FALSE)


# ── Display labels ────────────────────────────────────────────────────────
# Map from CSV test names to LaTeX labels
label_map <- c(
  "Real (emissions)"                   = "Real emissions (baseline)",
  "Placebo: shuffled"                  = "Fully shuffled emissions",
  "Placebo: employment"                = "Employment",
  "Placebo: value_added"               = "Value added",
  "Placebo: tangible_assets"           = "Tangible assets",
  "Placebo: shuffled within sector-year" = "Shuffled within sector $\\times$ year"
)


# ── Formatting helpers ────────────────────────────────────────────────────
fmt_n <- function(x) {
  # Integer or 1-decimal for averages
  x_num <- as.numeric(x)
  ifelse(x_num == round(x_num),
         format(as.integer(x_num), big.mark = ","),
         sprintf("%.1f", x_num))
}

fmt3 <- function(x) sprintf("%.3f", as.numeric(x))


# ── Build LaTeX ──────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{l rrr}",
  "\\toprule",
  "LHS variable & Suppliers & Overlap & Jaccard \\\\",
  " & selected & with real & similarity \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(df))) {
  test_name <- df$test[i]
  label <- if (test_name %in% names(label_map)) label_map[test_name] else test_name

  line <- sprintf(
    "%s & %s & %s & %s \\\\",
    label,
    fmt_n(df$n_positive[i]),
    fmt_n(df$overlap_with_real[i]),
    fmt3(df$jaccard_with_real[i])
  )
  tex <- c(tex, line)

  # Visual break after baseline row
  if (i == 1) tex <- c(tex, "\\midrule")
  # Break between permutation and size placebos
  if (i == 2) tex <- c(tex, "\\addlinespace")
  # Break before within-sector-year
  if (i == 5) tex <- c(tex, "\\addlinespace")
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")


# ── Save ──────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "enet_placebo_test_table.tex")
writeLines(tex, out_path)
cat("Saved T2 (placebo tests) to:", out_path, "\n")
