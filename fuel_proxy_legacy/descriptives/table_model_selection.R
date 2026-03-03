###############################################################################
# descriptives/table_model_selection.R
#
# PURPOSE
#   Generate a LaTeX table summarising model selection results across six
#   progressively richer specifications:
#     1. PPML, fixed effects       (benchmark, no proxy)
#     2. PPML, random effects      (partial pooling, no proxy)
#     3. PPML + fuel proxy         (best proxy by step-2 RMSE)
#     4. Hurdle                    (best triple, raw)
#     5. Hurdle, calibrated        (same triple, calibrated to sector-year totals)
#     6. Hurdle, leave-sector-out  (LOSOCV, calibrated)
#
#   Columns:
#     N | Extensive margin (FPR, TPR, Mass captured) |
#         Intensive margin (nRMSE, MAPD, Spearman rho)
#
# OUTPUT
#   model_selection.tex  ->  OUTPUT_DIR
###############################################################################

# ---- paths ----
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(data.table)

# ===========================================================================
# Load result files
# ===========================================================================

main_log       <- as.data.table(readRDS(METRICS_PATH_RDS))
hurdle_triples <- as.data.table(readRDS(file.path(OUTPUT_DIR, "hurdle_metrics_topk_triples.rds")))
step2_ranking  <- as.data.table(readRDS(file.path(OUTPUT_DIR, "step2_metrics_all_from_precompute.rds")))

losocv_path <- file.path(OUTPUT_DIR, "losocv_aggregate_metrics.rds")
has_losocv  <- file.exists(losocv_path)
if (has_losocv) {
  losocv_agg <- as.data.table(readRDS(losocv_path))
} else {
  cat("NOTE: losocv_aggregate_metrics.rds not found; row 6 will show NA.\n")
}

# ===========================================================================
# Extract one row per model specification
# ===========================================================================

# Safe column accessor (returns NA_real_ if column is missing)
safe_col <- function(dt, col) {
  if (col %in% names(dt)) dt[[col]] else NA_real_
}

# Helper: pull the columns we need into a uniform one-row data.table
extract_row <- function(dt, label, extensive = TRUE) {
  N   <- safe_col(dt, "TP") + safe_col(dt, "FP") +
         safe_col(dt, "TN") + safe_col(dt, "FN")
  fpr <- if (extensive) safe_col(dt, "FPR_nonemitters")      else NA_real_
  tpr <- if (extensive) safe_col(dt, "TPR_emitters")         else NA_real_
  mc  <- if (extensive) safe_col(dt, "emitter_mass_captured") else NA_real_
  data.table(
    label          = label,
    N              = as.integer(N),
    FPR            = fpr,
    TPR            = tpr,
    mass_captured  = mc,
    nRMSE          = safe_col(dt, "nRMSE"),
    MAPD           = safe_col(dt, "MAPD_emitters"),
    spearman       = safe_col(dt, "spearman")
  )
}

# Placeholder row for missing data
placeholder_row <- function(label) {
  data.table(
    label = label, N = NA_integer_,
    FPR = NA_real_, TPR = NA_real_, mass_captured = NA_real_,
    nRMSE = NA_real_, MAPD = NA_real_, spearman = NA_real_
  )
}

# --- Row 1: PPML, fixed effects (raw, no proxy) ---
r1 <- main_log[model_family == "ppml" & partial_pooling == "no" &
               proxy_tag == "none" & variant == "raw"]
stopifnot(nrow(r1) == 1)
row1 <- extract_row(r1, "PPML, fixed effects", extensive = FALSE)

# --- Row 2: PPML, random effects (raw, no proxy) ---
r2 <- main_log[model_family == "ppml" & partial_pooling == "yes" &
               proxy_tag == "none" & variant == "raw"]
stopifnot(nrow(r2) == 1)
row2 <- extract_row(r2, "PPML, random effects", extensive = FALSE)

# --- Row 3: PPML + fuel proxy (best proxy by step-2 RMSE, raw) ---
best_proxy <- step2_ranking[order(RMSE)][1, proxy_tag_int]
r3 <- main_log[model_family == "ppml" & partial_pooling == "yes" &
               proxy_tag == best_proxy & variant == "raw"]
stopifnot(nrow(r3) == 1)
row3 <- extract_row(r3, "PPML + fuel proxy", extensive = FALSE)

# --- Row 4: Hurdle, best triple (raw) ---
r4 <- hurdle_triples[variant == "raw" & is.finite(RMSE)][order(RMSE)][1]
row4 <- extract_row(r4, "Hurdle", extensive = TRUE)

# --- Row 5: Hurdle, calibrated (same triple) ---
r5 <- hurdle_triples[variant == "calibrated" &
                     proxy_tag_ext     == r4$proxy_tag_ext &
                     proxy_tag_int     == r4$proxy_tag_int &
                     threshold_value   == r4$threshold_value]
stopifnot(nrow(r5) == 1)
row5 <- extract_row(r5, "Hurdle, calibrated", extensive = TRUE)

# --- Row 6: Hurdle, leave-sector-out (calibrated) ---
if (has_losocv) {
  r6 <- losocv_agg[variant == "calibrated"]
  stopifnot(nrow(r6) == 1)
  row6 <- extract_row(r6, "Hurdle, leave-sector-out", extensive = TRUE)
} else {
  row6 <- placeholder_row("Hurdle, leave-sector-out")
}

# ===========================================================================
# Assemble and format
# ===========================================================================

tbl <- rbindlist(list(row1, row2, row3, row4, row5, row6))

fmt3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))
fmt_n <- function(x) ifelse(is.na(x), "---", format(as.integer(x), big.mark = ","))

# ===========================================================================
# Build LaTeX
# ===========================================================================

tex <- c(
  "\\begin{tabular}{l r ccc ccc}",
  "\\toprule",
  " & & \\multicolumn{3}{c}{Extensive margin} & \\multicolumn{3}{c}{Intensive margin} \\\\",
  "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8}",
  "Model & $N$ & FPR & TPR & Mass capt. & nRMSE & MAPD & $\\rho$ \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(tbl))) {
  r <- tbl[i]
  line <- sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
    r$label,
    fmt_n(r$N),
    fmt3(r$FPR), fmt3(r$TPR), fmt3(r$mass_captured),
    fmt3(r$nRMSE), fmt3(r$MAPD), fmt3(r$spearman)
  )
  tex <- c(tex, line)
  # visual break between single-equation and hurdle models
  if (i == 3) tex <- c(tex, "\\addlinespace")
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")

# ===========================================================================
# Write
# ===========================================================================

out_path <- file.path(OUTPUT_DIR, "model_selection.tex")
writeLines(tex, out_path)
cat("Saved to", out_path, "\n")
