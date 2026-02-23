###############################################################################
# fuel_suppliers/descriptives/table_cv_performance.R
#
# PURPOSE
#   Generate the main prediction performance table (T1) from group k-fold CV
#   and LOSOCV results.
#
#   T1 — Prediction performance (enet_cv_performance.tex):
#     Five rows showing progressive model improvements:
#       1. PPML benchmark (sector RE, calibrated)
#       2. PPML + proxy (sector RE, calibrated)
#       3. Hurdle + proxy (calibrated)
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
#
# INPUTS
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance.csv
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_cv_performance.tex
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
# T1: Prediction performance — 5 progressive rows
# ===========================================================================

# ── Define the 5 rows ─────────────────────────────────────────────────────
# Each row = (model key in CSV, variant, display label, show extensive margin?)
row_specs <- list(
  list(model = "benchmark",                 variant = "calibrated",         label = "PPML benchmark",                show_ext = FALSE),
  list(model = "proxy_pooled",              variant = "calibrated",         label = "\\quad + fuel-supply proxy",     show_ext = FALSE),
  list(model = "hurdle_proxy_pooled",       variant = "calibrated",         label = "Hurdle + proxy",                show_ext = TRUE),
  list(model = "hurdle_proxy_pooled",       variant = "calibrated_clipped", label = "\\quad + calibration \\& clip",  show_ext = TRUE),
  list(model = "losocv_hurdle_proxy_pooled", variant = "calibrated_clipped", label = "LOSOCV (out-of-sector)",       show_ext = TRUE)
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


# ── Build LaTeX ───────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{l ccc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction accuracy} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-8}",
  "Model & nRMSE & MAPD & $\\rho$ & FPR & TPR & $p_{50}$ & $p_{99}$ \\\\",
  "\\midrule"
)

for (i in seq_along(row_specs)) {
  spec <- row_specs[[i]]
  r <- rows[[i]]

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
    fpr  <- fmt3(r$fpr_nonemitters)
    tpr  <- fmt3(r$tpr_emitters)
    p50  <- fmt_pct(r$avg_nonemit_p50_rank)
    p99  <- fmt_pct(r$avg_nonemit_p99_rank)
  } else {
    fpr <- "---"; tpr <- "---"; p50 <- "---"; p99 <- "---"
  }

  line <- sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
    spec$label, nrmse, mapd, rho, fpr, tpr, p50, p99
  )
  tex <- c(tex, line)

  # Visual break: after PPML block (row 2) and after k-fold hurdle block (row 4)
  if (i == 2 || i == 4) tex <- c(tex, "\\addlinespace")
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")


# ── Save ──────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "enet_cv_performance.tex")
writeLines(tex, out_path)
cat("Saved T1 (prediction performance) to:", out_path, "\n")
