###############################################################################
# fuel_suppliers/descriptives/table_cv_performance.R
#
# PURPOSE
#   Generate the main prediction performance table (T1) from group k-fold CV
#   and LOSOCV results.
#
#   T1 — Prediction performance (enet_cv_performance.tex):
#     Five rows showing progressive model improvements:
#       1. PPML benchmark (sector RE, raw)
#       2. PPML + proxy (sector RE, raw)
#       3. Hurdle + proxy (raw — no calibration or clipping)
#       4. Hurdle + proxy (calibrated + clipped)
#       5. LOSOCV Hurdle + proxy (calibrated + clipped)
#
#     Columns split into two groups:
#       "Prediction accuracy": nRMSE, MAPD, Spearman rho
#       "Extensive margin":    FPR, TPR, p50, p99
#         where p50 and p99 are the within-sector-year averaged percentile
#         ranks of non-emitter predictions in the emitter ecdf (NACE 19 & 24).
#
#     PPML rows show "---" for extensive margin (no explicit classification).
#     Hurdle raw row shows median [min, max] for p50 and p99 across cells.
#
# INPUTS
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance.csv
#   {OUTPUT_DIR}/cell_fp_severity_hurdle_proxy_pooled_raw.csv  (optional)
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_cv_performance.tex
#
# NOTE
#   The generated LaTeX requires \usepackage{booktabs} and \usepackage{multirow}.
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

# Load per-cell FP severity CSVs (raw, pre-calibration, pre-cap)
load_cell_fp <- function(fname) {
  p <- file.path(OUTPUT_DIR, fname)
  if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL
}
cell_fp_benchmark_raw   <- load_cell_fp("cell_fp_severity_benchmark_raw.csv")
cell_fp_proxy_pooled_raw <- load_cell_fp("cell_fp_severity_proxy_pooled_raw.csv")
cell_fp_hurdle_raw      <- load_cell_fp("cell_fp_severity_hurdle_proxy_pooled_raw.csv")


# ===========================================================================
# T1: Prediction performance — 5 progressive rows
# ===========================================================================

# ── Define the 5 rows ─────────────────────────────────────────────────────
# Each row = (model key in CSV, variant, display label, show extensive margin?)
row_specs <- list(
  list(model = "benchmark",                 variant = "raw",                label = "PPML benchmark",                show_ext = TRUE),
  list(model = "proxy_pooled",              variant = "raw",                label = "\\quad + fuel-supply proxy",     show_ext = TRUE),
  list(model = "hurdle_proxy_pooled",       variant = "raw",                label = "Hurdle + proxy",                show_ext = TRUE),
  list(model = "hurdle_proxy_pooled",       variant = "calibrated_clipped", label = "\\quad + calibration \\& clip",  show_ext = TRUE),
  list(model = "losocv_hurdle_proxy_pooled", variant = "calibrated_clipped", label = "Leave-one-sector-out",        show_ext = TRUE)
)

# ── Extract rows from CSV ─────────────────────────────────────────────────
rows <- lapply(row_specs, function(spec) {
  r <- cv[cv$model == spec$model & cv$variant == spec$variant, ]
  if (nrow(r) == 0) {
    warning(sprintf("Row not found: model=%s, variant=%s", spec$model, spec$variant))
    return(NULL)
  }
  r[1, ]
})

# ── Formatting helpers ────────────────────────────────────────────────────
fmt3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))

# FP severity as integer percentile (0-100)
fmt_pct <- function(x) ifelse(is.na(x), "---", sprintf("%.0f", x * 100))

# FP severity: median and [min, max] from per-cell data (as integer percentiles)
fmt_pct_med <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return("---")
  sprintf("%.0f", round(median(vals) * 100))
}
fmt_pct_minmax <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return("")
  sprintf("{\\scriptsize [%d, %d]}", round(min(vals) * 100), round(max(vals) * 100))
}


# ── Build LaTeX ───────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{l ccc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction accuracy} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-8}",
  "Model & nRMSE & MAPD & $\\rho$ & FPR & TPR & $p_{50}$ & $p_{99}$ \\\\",
  "\\midrule"
)

# Map model names to per-cell FP severity data
cell_fp_map <- list(
  benchmark            = cell_fp_benchmark_raw,
  proxy_pooled         = cell_fp_proxy_pooled_raw,
  hurdle_proxy_pooled  = cell_fp_hurdle_raw
)

for (i in seq_along(row_specs)) {
  spec <- row_specs[[i]]
  r <- rows[[i]]

  # Look up per-cell data for raw rows
  cell_fp <- NULL
  if (spec$variant == "raw" && spec$model %in% names(cell_fp_map)) {
    cf <- cell_fp_map[[spec$model]]
    if (!is.null(cf) && nrow(cf) > 0) cell_fp <- cf
  }

  # Prediction accuracy columns (always shown)
  if (!is.null(r)) {
    nrmse <- fmt3(r$nRMSE)
    mapd  <- fmt3(r$mapd_emitters)
    rho   <- fmt3(r$spearman)
  } else {
    nrmse <- "---"; mapd <- "---"; rho <- "---"
  }

  # Extensive margin columns
  if (spec$show_ext && !is.null(r)) {
    fpr <- fmt3(r$fpr_nonemitters)
    tpr <- fmt3(r$tpr_emitters)

    if (!is.null(cell_fp)) {
      p50 <- fmt_pct_med(cell_fp$p50_rank)
      p99 <- fmt_pct_med(cell_fp$p99_rank)
    } else {
      p50 <- fmt_pct(r$avg_nonemit_p50_rank)
      p99 <- fmt_pct(r$avg_nonemit_p99_rank)
    }
  } else {
    fpr <- "---"; tpr <- "---"; p50 <- "---"; p99 <- "---"
  }

  if (!is.null(cell_fp)) {
    # Two-row block: first row with \multirow for all columns except p50/p99
    line1 <- sprintf(
      "\\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & %s & %s \\\\",
      spec$label, nrmse, mapd, rho, fpr, tpr, p50, p99
    )
    # Second row: only [min, max] in p50 and p99 columns
    p50_range <- fmt_pct_minmax(cell_fp$p50_rank)
    p99_range <- fmt_pct_minmax(cell_fp$p99_rank)
    line2 <- sprintf(
      " & & & & & & %s & %s \\\\",
      p50_range, p99_range
    )
    tex <- c(tex, line1, line2)
  } else {
    line <- sprintf(
      "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
      spec$label, nrmse, mapd, rho, fpr, tpr, p50, p99
    )
    tex <- c(tex, line)
  }

  # Visual break: after PPML block (row 2) and after k-fold hurdle block (row 4)
  if (i == 2 || i == 4) tex <- c(tex, "\\addlinespace")
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")


# ── Save ──────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "enet_cv_performance.tex")
writeLines(tex, out_path)
cat("Saved T1 (prediction performance) to:", out_path, "\n")
