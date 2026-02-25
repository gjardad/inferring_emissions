###############################################################################
# fuel_suppliers/descriptives/table_cv_performance.R
#
# PURPOSE
#   Generate the main prediction performance table (T1) from group k-fold CV
#   and LOSOCV results.
#
#   T1 — Prediction performance (enet_cv_performance.tex):
#     Two panels, seven rows:
#
#     Panel A: k-fold cross-validation (leave-firms-out)
#       1. PPML                        (benchmark, raw)
#       2.   + fuel-supply proxy       (proxy_pooled, raw — no indicator)
#       3. Hurdle + proxy              (hurdle_proxy_pooled, raw — no indicator)
#       4.   + fuel-purchase indicator  (hurdle_proxy_pooled_ind, raw)
#       5.     + calibrated            (hurdle_proxy_pooled_ind, calibrated_clipped)
#
#     Panel B: Leave-one-sector-out cross-validation
#       6. Hurdle + proxy, calibrated  (losocv_hurdle_proxy_pooled, calibrated_clipped)
#       7.   + fuel-purchase indicator  (losocv_hurdle_proxy_pooled_ind, calibrated_clipped)
#
#     Columns split into two groups:
#       "Prediction accuracy": nRMSE, MAPD, Spearman rho
#       "Extensive margin":    FPR, TPR, p50, p99
#
#     Raw rows with per-cell FP severity data show median // [min, max].
#     Calibrated rows show single values (p50, p99 = 0 by construction).
#
# INPUTS
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance.csv
#   {OUTPUT_DIR}/cell_fp_severity_*.csv  (per-cell FP severity, optional)
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
cell_fp_benchmark_raw       <- load_cell_fp("cell_fp_severity_benchmark_raw.csv")
cell_fp_proxy_pooled_raw    <- load_cell_fp("cell_fp_severity_proxy_pooled_raw.csv")
cell_fp_hurdle_raw          <- load_cell_fp("cell_fp_severity_hurdle_proxy_pooled_raw.csv")
cell_fp_hurdle_ind_raw      <- load_cell_fp("cell_fp_severity_hurdle_proxy_pooled_ind_raw.csv")


# ===========================================================================
# T1: Prediction performance — 7 rows, 2 panels
# ===========================================================================

# ── Define the 7 rows ─────────────────────────────────────────────────────
# Each row = (model key in CSV, variant, display label)
# Panel breaks inserted via panel_after indices below.
row_specs <- list(
  # Panel A: k-fold CV
  list(model = "benchmark",                      variant = "raw",                label = "PPML"),
  list(model = "proxy_pooled",                   variant = "raw",                label = "\\quad + fuel-supply proxy"),
  list(model = "hurdle_proxy_pooled",            variant = "raw",                label = "Hurdle + proxy"),
  list(model = "hurdle_proxy_pooled_ind",        variant = "raw",                label = "\\quad + fuel-purchase indicator"),
  list(model = "hurdle_proxy_pooled_ind",        variant = "calibrated_clipped", label = "\\quad\\quad + calibration"),
  # Panel B: LOSO CV
  list(model = "losocv_hurdle_proxy_pooled",     variant = "calibrated_clipped", label = "Hurdle + proxy, calibration"),
  list(model = "losocv_hurdle_proxy_pooled_ind", variant = "calibrated_clipped", label = "\\quad + fuel-purchase indicator")
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


# ── Map model names to per-cell FP severity data ─────────────────────────
cell_fp_map <- list(
  benchmark                = cell_fp_benchmark_raw,
  proxy_pooled             = cell_fp_proxy_pooled_raw,
  hurdle_proxy_pooled      = cell_fp_hurdle_raw,
  hurdle_proxy_pooled_ind  = cell_fp_hurdle_ind_raw
)

# ── Build LaTeX ───────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{l ccc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction accuracy} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-8}",
  "Model & nRMSE & MAPD & $\\rho$ & FPR & TPR & $p_{50}$ & $p_{99}$ \\\\",
  "\\midrule",
  "\\multicolumn{8}{l}{\\textit{Panel A: k-fold cross-validation}} \\\\",
  "\\addlinespace"
)

for (i in seq_along(row_specs)) {
  spec <- row_specs[[i]]
  r <- rows[[i]]

  # Panel B header before row 6
  if (i == 6) {
    tex <- c(tex,
      "\\addlinespace",
      "\\midrule",
      "\\multicolumn{8}{l}{\\textit{Panel B: Leave-one-sector-out cross-validation}} \\\\",
      "\\addlinespace"
    )
  }

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
  if (!is.null(r)) {
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

  # Visual breaks within panels
  if (i == 2) tex <- c(tex, "\\addlinespace")  # after PPML block
  if (i == 5) tex <- c(tex, "")                 # end of Panel A (midrule added before row 6)
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")


# ── Save ──────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "enet_cv_performance.tex")
writeLines(tex, out_path)
cat("Saved T1 (prediction performance) to:", out_path, "\n")
