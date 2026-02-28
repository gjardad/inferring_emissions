###############################################################################
# fuel_suppliers/descriptives/table_cn8_validation.R
#
# PURPOSE
#   Generate a paper-ready LaTeX table from the CN8 validation and random
#   baseline results (appendix table).
#
#   TA1 — CN8 validation (enet_cn8_validation_table.tex):
#     For each CN8 tier (broad, strict, core), compares the share of elastic-
#     net-selected suppliers that are fuel-linked against a random baseline
#     of the same size drawn from eligible sellers.
#
#     Columns: Tier | Metric | Elastic net (%) | Random mean (%) |
#              Random [5%, 95%] | p-value
#
# INPUTS
#   {OUTPUT_DIR}/enet_cn8_random_baseline.csv
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_cn8_validation_table.tex
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
csv_path <- file.path(OUTPUT_DIR, "enet_cn8_random_baseline.csv")
if (!file.exists(csv_path)) stop("CN8 validation results not found: ", csv_path)

df <- read.csv(csv_path, stringsAsFactors = FALSE)


# ── Tier labels ──────────────────────────────────────────────────────────
tier_labels <- c(
  "broad"  = "Broad (Ch.~27 excl.~2716)",
  "strict" = "Strict (LLM-curated)",
  "core"   = "Core (unambiguous fuels)"
)

# Metric labels
metric_labels <- c(
  "CN8 importers (%)"       = "Direct importers",
  "1-degree downstream (%)" = "1-degree downstream",
  "Fuel-linked (%)"         = "Fuel-linked (union)"
)


# ── Formatting helpers ────────────────────────────────────────────────────
fmt1 <- function(x) sprintf("%.1f", as.numeric(x))
fmt3 <- function(x) sprintf("%.3f", as.numeric(x))


# ── Build LaTeX ──────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{ll rr c r}",
  "\\toprule",
  "CN8 tier & Link type & Elastic net & Random & Random & $p$-value \\\\",
  " & & (\\%) & mean (\\%) & {[}5\\%, 95\\%{]} & \\\\",
  "\\midrule"
)

tiers <- unique(df$tier)

for (t_idx in seq_along(tiers)) {
  tier <- tiers[t_idx]
  tier_rows <- df[df$tier == tier, ]
  tier_lab <- if (tier %in% names(tier_labels)) tier_labels[tier] else tier

  for (i in seq_len(nrow(tier_rows))) {
    r <- tier_rows[i, ]
    met_lab <- if (r$metric %in% names(metric_labels)) metric_labels[r$metric] else r$metric

    # Show tier label only on first row of each tier block
    tier_cell <- if (i == 1) tier_lab else ""

    ci <- sprintf("[%s, %s]", fmt1(r$random_p5), fmt1(r$random_p95))

    line <- sprintf(
      "%s & %s & %s & %s & %s & %s \\\\",
      tier_cell, met_lab, fmt1(r$elastic_net), fmt1(r$random_mean),
      ci, fmt3(r$p_value)
    )
    tex <- c(tex, line)
  }

  # Visual break between tiers
  if (t_idx < length(tiers)) tex <- c(tex, "\\addlinespace")
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")


# ── Save ──────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "enet_cn8_validation_table.tex")
writeLines(tex, out_path)
cat("Saved TA1 (CN8 validation) to:", out_path, "\n")
