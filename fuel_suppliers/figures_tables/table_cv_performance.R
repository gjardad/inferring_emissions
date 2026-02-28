###############################################################################
# fuel_suppliers/descriptives/table_cv_performance.R
#
# PURPOSE
#   Generate prediction performance tables from group k-fold CV and LOSOCV
#   results. Two versions of the table are produced — one for each proxy:
#
#   enet_cv_performance_pooled.tex   (unweighted fuel-supply proxy)
#   enet_cv_performance_weighted.tex (coefficient-weighted fuel-supply proxy)
#
#   Each table has the same structure:
#
#     Panel A: Leave-firms-out cross-validation
#       1. PPML                        (benchmark, raw)
#       2.   + fuel-supply proxy       (proxy_X, raw)
#       3. Hurdle + proxy              (hurdle_proxy_X, raw)
#       4.   + fuel-purchase indicator  (hurdle_proxy_X_ind, raw)
#       5.     + calibration           (hurdle_proxy_X_ind, calibrated_clipped)
#
#     Panel B: Leave-one-sector-out cross-validation
#       6. Sector RE                   (losocv_hurdle_proxy_X_ind_base, cal_clip)
#       7. Year FE only                (losocv_hurdle_proxy_X_ind_year, cal_clip)
#
#     Columns split into two groups:
#       "Prediction accuracy": nRMSE, MAPD, Spearman rho, within-sector-year rho
#       "Extensive margin":    FPR, TPR, p50, p99
#
# INPUTS
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance_lofocv.csv
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance_losocv.csv
#   {OUTPUT_DIR}/cell_fp_severity_*.csv  (per-cell FP severity, optional)
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_cv_performance_pooled.tex
#   {OUTPUT_DIR}/enet_cv_performance_weighted.tex
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
lofocv_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance_lofocv.csv")
losocv_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance_losocv.csv")
if (!file.exists(lofocv_path)) stop("LOFOCV results not found: ", lofocv_path)
if (!file.exists(losocv_path)) stop("LOSOCV results not found: ", losocv_path)

cv <- rbind(
  read.csv(lofocv_path, stringsAsFactors = FALSE),
  read.csv(losocv_path, stringsAsFactors = FALSE)
)

# Load per-cell FP severity CSVs (raw, pre-calibration, pre-cap)
load_cell_fp <- function(fname) {
  p <- file.path(OUTPUT_DIR, fname)
  if (file.exists(p)) read.csv(p, stringsAsFactors = FALSE) else NULL
}
cell_fp_benchmark_raw    <- load_cell_fp("cell_fp_severity_benchmark_raw.csv")
cell_fp_proxy_pooled_raw <- load_cell_fp("cell_fp_severity_proxy_pooled_raw.csv")
cell_fp_hurdle_raw       <- load_cell_fp("cell_fp_severity_hurdle_proxy_pooled_raw.csv")
cell_fp_hurdle_ind_raw   <- load_cell_fp("cell_fp_severity_hurdle_proxy_pooled_ind_raw.csv")


# ── Formatting helpers ────────────────────────────────────────────────────────
fmt3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))

fmt_pct <- function(x) ifelse(is.na(x), "---", sprintf("%.0f", x * 100))

fmt_pct_med <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return("---")
  sprintf("%.0f", round(median(vals) * 100))
}
fmt_pct_minmax <- function(vals) {
  if (is.null(vals) || length(vals) == 0) return("")
  sprintf("{\\scriptsize [%d, %d]}", round(min(vals) * 100), round(max(vals) * 100))
}

fmt_rho_minmax <- function(rmin, rmax) {
  if (is.na(rmin) || is.na(rmax)) return("")
  sprintf("{\\scriptsize [%.2f, %.2f]}", rmin, rmax)
}


# ===========================================================================
# Table generation function
# ===========================================================================
generate_table <- function(row_specs, cell_fp_map, panel_b_start = 6) {
  # Extract rows from CSV
  rows <- lapply(row_specs, function(spec) {
    r <- cv[cv$model == spec$model & cv$variant == spec$variant, ]
    if (nrow(r) == 0) {
      warning(sprintf("Row not found: model=%s, variant=%s", spec$model, spec$variant))
      return(NULL)
    }
    r[1, ]
  })

  # Build LaTeX
  tex <- c(
    "\\begin{tabular}{l cccc cccc}",
    "\\toprule",
    " & \\multicolumn{4}{c}{Prediction accuracy} & \\multicolumn{4}{c}{Extensive margin} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    "Model & nRMSE & MAPD & $\\rho$ & $\\rho_{s,t}$ & FPR & TPR & $p_{50}$ & $p_{99}$ \\\\",
    "\\midrule",
    "\\multicolumn{9}{l}{\\textit{Panel A: Leave-firms-out cross-validation}} \\\\",
    "\\addlinespace"
  )

  for (i in seq_along(row_specs)) {
    spec <- row_specs[[i]]
    r <- rows[[i]]

    # Panel B header
    if (i == panel_b_start) {
      tex <- c(tex,
        "\\addlinespace",
        "\\midrule",
        "\\multicolumn{9}{l}{\\textit{Panel B: Leave-one-sector-out cross-validation}} \\\\",
        "\\addlinespace"
      )
    }

    # Look up per-cell FP severity data for raw rows
    cell_fp <- NULL
    if (spec$variant == "raw" && spec$model %in% names(cell_fp_map)) {
      cf <- cell_fp_map[[spec$model]]
      if (!is.null(cf) && nrow(cf) > 0) cell_fp <- cf
    }

    # Prediction accuracy columns
    if (!is.null(r)) {
      nrmse  <- fmt3(r$nRMSE)
      mapd   <- fmt3(r$mapd_emitters)
      rho    <- fmt3(r$spearman)
      rho_w  <- fmt3(r$within_sy_rho_med)
    } else {
      nrmse <- "---"; mapd <- "---"; rho <- "---"; rho_w <- "---"
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

    # Two-row block: median + [min, max] for rho_w and p50/p99
    has_rho_range <- !is.null(r) && !is.na(r$within_sy_rho_min) && !is.na(r$within_sy_rho_max)

    if (has_rho_range) {
      rho_w_range <- fmt_rho_minmax(r$within_sy_rho_min, r$within_sy_rho_max)

      if (!is.null(cell_fp)) {
        p50_range <- fmt_pct_minmax(cell_fp$p50_rank)
        p99_range <- fmt_pct_minmax(cell_fp$p99_rank)
      } else {
        p50_range <- ""
        p99_range <- ""
      }

      line1 <- sprintf(
        "\\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & %s & \\multirow{2}{*}{%s} & \\multirow{2}{*}{%s} & %s & %s \\\\",
        spec$label, nrmse, mapd, rho, rho_w, fpr, tpr, p50, p99
      )
      line2 <- sprintf(
        " & & & & %s & & & %s & %s \\\\",
        rho_w_range, p50_range, p99_range
      )
      tex <- c(tex, line1, line2)
    } else {
      line <- sprintf(
        "%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
        spec$label, nrmse, mapd, rho, rho_w, fpr, tpr, p50, p99
      )
      tex <- c(tex, line)
    }

    # Visual breaks within panels
    if (i == 2) tex <- c(tex, "\\addlinespace")  # after PPML block
    if (i == (panel_b_start - 1)) tex <- c(tex, "")  # end of Panel A
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  tex
}


# ===========================================================================
# Table 1: Unweighted proxy (proxy_pooled)
# ===========================================================================
row_specs_pooled <- list(
  # Panel A: k-fold CV (all _base variants)
  list(model = "benchmark_base",                         variant = "raw",                label = "PPML"),
  list(model = "proxy_pooled_base",                      variant = "raw",                label = "\\quad + fuel-supply proxy"),
  list(model = "hurdle_proxy_pooled_base",               variant = "raw",                label = "Hurdle + proxy"),
  list(model = "hurdle_proxy_pooled_ind_base",           variant = "raw",                label = "\\quad + fuel-purchase indicator"),
  list(model = "hurdle_proxy_pooled_ind_base",           variant = "calibrated_clipped", label = "\\quad\\quad + calibration"),
  # Panel B: LOSO CV
  list(model = "losocv_hurdle_proxy_pooled_ind_base",    variant = "calibrated_clipped", label = "Sector RE"),
  list(model = "losocv_hurdle_proxy_pooled_ind_year",    variant = "calibrated_clipped", label = "Year FE only")
)

cell_fp_map_pooled <- list(
  benchmark_base            = cell_fp_benchmark_raw,
  proxy_pooled_base         = cell_fp_proxy_pooled_raw,
  hurdle_proxy_pooled_base  = cell_fp_hurdle_raw,
  hurdle_proxy_pooled_ind_base = cell_fp_hurdle_ind_raw
)

tex_pooled <- generate_table(row_specs_pooled, cell_fp_map_pooled)


# ===========================================================================
# Table 2: Coefficient-weighted proxy (proxy_weighted)
# ===========================================================================
row_specs_weighted <- list(
  # Panel A: k-fold CV (all _base variants)
  list(model = "benchmark_base",                          variant = "raw",                label = "PPML"),
  list(model = "proxy_weighted_base",                     variant = "raw",                label = "\\quad + fuel-supply proxy"),
  list(model = "hurdle_proxy_weighted_base",              variant = "raw",                label = "Hurdle + proxy"),
  list(model = "hurdle_proxy_weighted_ind_base",          variant = "raw",                label = "\\quad + fuel-purchase indicator"),
  list(model = "hurdle_proxy_weighted_ind_base",          variant = "calibrated_clipped", label = "\\quad\\quad + calibration"),
  # Panel B: LOSO CV
  list(model = "losocv_hurdle_proxy_weighted_ind_base",   variant = "calibrated_clipped", label = "Sector RE"),
  list(model = "losocv_hurdle_proxy_weighted_ind_year",   variant = "calibrated_clipped", label = "Year FE only")
)

# No per-cell FP severity data for weighted proxy (uses CSV fallback)
cell_fp_map_weighted <- list(
  benchmark_base = cell_fp_benchmark_raw
)

tex_weighted <- generate_table(row_specs_weighted, cell_fp_map_weighted)


# ── Save ──────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_pooled <- file.path(OUTPUT_DIR, "enet_cv_performance_pooled.tex")
writeLines(tex_pooled, out_pooled)
cat("Saved pooled proxy table to:", out_pooled, "\n")

out_weighted <- file.path(OUTPUT_DIR, "enet_cv_performance_weighted.tex")
writeLines(tex_weighted, out_weighted)
cat("Saved weighted proxy table to:", out_weighted, "\n")
