###############################################################################
# fuel_suppliers/descriptives/table_cv_performance.R
#
# PURPOSE
#   Generate paper-ready LaTeX tables from the group k-fold CV results.
#
#   Table 1 — Model selection (cv_performance.tex):
#     Rows: 6 models (3 PPML + 3 hurdle), calibrated variant.
#     Columns: N | Extensive margin (FPR, TPR, Mass capt.) |
#              Intensive margin (nRMSE, MAPD, Spearman rho)
#     PPML models show "---" for extensive margin (no explicit classification).
#
#   Table 2 — False-positive severity (fp_severity.tex):
#     For each calibrated model, how large are imputed emissions for confirmed
#     non-emitters (y=0) relative to the sector's emitter distribution?
#     Separate panels for NACE 19 (coke/petroleum) and NACE 24 (metals).
#     Entries: percentile rank within the emitter ecdf of the median/p90/p99
#     of predicted emissions among true non-emitters in that sector.
#
# INPUTS
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance.csv
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_cv_performance.tex
#   {OUTPUT_DIR}/enet_fp_severity.tex
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


# ── Load CV results ──────────────────────────────────────────────────────────
cv_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance.csv")
if (!file.exists(cv_path)) stop("CV results not found: ", cv_path)

cv <- read.csv(cv_path, stringsAsFactors = FALSE)


# ===========================================================================
# Table 1: Model selection (calibrated variant only)
# ===========================================================================

# Model display order and labels
model_order <- c(
  "benchmark",            "proxy_pooled",            "proxy_within_buyer",
  "hurdle_benchmark",     "hurdle_proxy_pooled",     "hurdle_proxy_within_buyer"
)
model_labels <- c(
  "PPML benchmark",       "PPML + pooled proxy",     "PPML + within-buyer proxy",
  "Hurdle benchmark",     "Hurdle + pooled proxy",   "Hurdle + within-buyer proxy"
)
is_hurdle <- grepl("^hurdle", model_order)

# Filter to calibrated variant
cal <- cv[cv$variant == "calibrated", ]
cal$model <- factor(cal$model, levels = model_order)
cal <- cal[order(cal$model), ]

if (nrow(cal) == 0) stop("No calibrated rows found in CV results.")

# Formatting helpers
fmt3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))
fmt_n <- function(x) ifelse(is.na(x), "---", format(as.integer(x), big.mark = ","))

# Build LaTeX
tex <- c(
  "\\begin{tabular}{l r ccc ccc}",
  "\\toprule",
  " & & \\multicolumn{3}{c}{Extensive margin} & \\multicolumn{3}{c}{Intensive margin} \\\\",
  "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8}",
  "Model & $N$ & FPR & TPR & Mass capt. & nRMSE & MAPD & $\\rho$ \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(cal))) {
  r <- cal[i, ]
  lab <- model_labels[match(as.character(r$model), model_order)]
  hurdle <- is_hurdle[match(as.character(r$model), model_order)]

  # PPML has no meaningful extensive margin (everything predicted positive)
  if (hurdle) {
    fpr <- fmt3(r$fpr_nonemitters)
    tpr <- fmt3(r$tpr_emitters)
    mc  <- fmt3(r$emitter_mass_captured)
  } else {
    fpr <- "---"
    tpr <- "---"
    mc  <- "---"
  }

  line <- sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
    lab,
    fmt_n(r$n),
    fpr, tpr, mc,
    fmt3(r$nRMSE), fmt3(r$mapd_emitters), fmt3(r$spearman)
  )
  tex <- c(tex, line)

  # Visual break between PPML and hurdle blocks
  if (i == 3) tex <- c(tex, "\\addlinespace")
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")

out1 <- file.path(OUTPUT_DIR, "enet_cv_performance.tex")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
writeLines(tex, out1)
cat("Saved Table 1 (model selection) to:", out1, "\n")


# ===========================================================================
# Table 2: False-positive severity (NACE 19 and 24)
# ===========================================================================

# Check if any sector-specific columns exist
has_fp <- any(!is.na(cal$nonemit_p50_rank_19)) || any(!is.na(cal$nonemit_p50_rank_24))

if (has_fp) {

  fmt_pct <- function(x) ifelse(is.na(x), "---", sprintf("%.0f", x * 100))

  tex2 <- c(
    "\\begin{tabular}{l ccc ccc}",
    "\\toprule",
    " & \\multicolumn{3}{c}{NACE 19 (Coke \\& petroleum)} & \\multicolumn{3}{c}{NACE 24 (Basic metals)} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "Model & $p_{50}$ & $p_{90}$ & $p_{99}$ & $p_{50}$ & $p_{90}$ & $p_{99}$ \\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(cal))) {
    r <- cal[i, ]
    lab <- model_labels[match(as.character(r$model), model_order)]

    line <- sprintf(
      "%s & %s & %s & %s & %s & %s & %s \\\\",
      lab,
      fmt_pct(r$nonemit_p50_rank_19),
      fmt_pct(r$nonemit_p90_rank_19),
      fmt_pct(r$nonemit_p99_rank_19),
      fmt_pct(r$nonemit_p50_rank_24),
      fmt_pct(r$nonemit_p90_rank_24),
      fmt_pct(r$nonemit_p99_rank_24)
    )
    tex2 <- c(tex2, line)
    if (i == 3) tex2 <- c(tex2, "\\addlinespace")
  }

  tex2 <- c(tex2, "\\bottomrule", "\\end{tabular}")

  out2 <- file.path(OUTPUT_DIR, "enet_fp_severity.tex")
  writeLines(tex2, out2)
  cat("Saved Table 2 (false-positive severity) to:", out2, "\n")

} else {
  cat("Skipping Table 2: no sector-specific false-positive severity data available.\n")
}
