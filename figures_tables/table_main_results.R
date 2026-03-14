###############################################################################
# figures_tables/table_main_results.R
#
# PURPOSE
#   Generate the main results table (Table X) for Section 4.
#   Three rows, all using rank-and-calibrate with national-aggregate target:
#     1. EN, sector CV    — EN proxy built with sector-level K=5 CV (M=200 repeats)
#     2. EN, firm CV      — EN proxy built with firm-level K=10 CV (M=200 repeats)
#     3. NACE-based       — Tabachova (deterministic, NACE-based) proxy
#
#   Columns: nRMSE | Median APD | Rank corr. | FPR | TPR | FP sev. p50 | FP sev. p99
#   Column groups: "Prediction accuracy" (first 3) | "Extensive margin" (last 4)
#
#   For EN rows: calibration and metrics are computed within each repeat using
#   the repeat's own fold assignment, then averaged across M repeats.
#   Fold assignments are reconstructed deterministically from the same seeds
#   used on RMD (seed = BASE_SEED + r).
#
#   For NACE-based: the proxy is deterministic so we use a single calibration
#   with sector-fold assignment from repeat 1 (arbitrary, since the proxy
#   itself doesn't depend on fold assignment).
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#     Contains: proxy_matrix (N x M), repeated_cv_proxy_panel, firmyear_index
#   {PROC_DATA}/repeated_cv_proxy_firm_asinh.RData
#     Contains: proxy_matrix (N x M), repeated_cv_proxy_panel, firmyear_index
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#     Contains: training_sample (with proxy_tabachova_asinh)
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_results.tex
#   Printed to console
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
REPO_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
  error = function(e) normalizePath(getwd(), winslash = "/")
)
while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
source(file.path(UTILS_DIR, "calc_metrics.R"))

# ── Parameters (must match build_repeated_cv_proxy_asinh.R) ────────────────
BASE_SEED <- 2026L

# ── Load data ──────────────────────────────────────────────────────────────

# Repeated CV outputs
cat("Loading repeated CV proxy (sector)...\n")
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel_sec        <- e_sec$repeated_cv_proxy_panel
M_sec <- ncol(proxy_matrix_sec)
rm(e_sec)

cat("Loading repeated CV proxy (firm)...\n")
e_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_firm_asinh.RData"), envir = e_firm)
proxy_matrix_firm <- e_firm$proxy_matrix
panel_firm        <- e_firm$repeated_cv_proxy_panel
M_firm <- ncol(proxy_matrix_firm)
rm(e_firm)

cat(sprintf("  Sector CV: %d obs x %d repeats\n", nrow(proxy_matrix_sec), M_sec))
cat(sprintf("  Firm CV:   %d obs x %d repeats\n", nrow(proxy_matrix_firm), M_firm))

# Training sample (for Tabachova proxy)
cat("Loading training sample (for NACE-based proxy)...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

# Merge Tabachova proxy into sector panel (they may have different row orders)
tabachova_df <- training_sample %>%
  select(vat, year, proxy_tabachova_asinh)
rm(training_sample, syt)

panel_sec <- panel_sec %>%
  left_join(tabachova_df, by = c("vat", "year"))

# ── Helper: back-transform asinh proxy to levels ─────────────────────────
proxy_to_levels <- function(proxy) pmax(sinh(proxy), 0)

# ── Helper: fold-aware national-aggregate calibration ────────────────────
# For each fold k:
#   E_target_k = E_nat - E_train_k  (what's left to distribute to held-out)
#   Distribute proportionally to proxy among held-out firms, per year.
calibrate_national <- function(df, proxy_raw, fold_k) {
  E_nat_by_year <- tapply(df$y, df$year, sum, na.rm = TRUE)
  years <- names(E_nat_by_year)
  folds <- sort(unique(fold_k))
  result <- rep(NA_real_, nrow(df))

  for (k in folds) {
    held_out <- which(fold_k == k)
    train_k  <- which(fold_k != k)

    E_train_by_year <- tapply(df$y[train_k], df$year[train_k], sum, na.rm = TRUE)

    for (yr in years) {
      idx <- held_out[df$year[held_out] == as.integer(yr)]
      if (length(idx) == 0) next

      E_target <- E_nat_by_year[yr] - ifelse(is.na(E_train_by_year[yr]), 0, E_train_by_year[yr])

      if (E_target <= 0) {
        result[idx] <- 0
        next
      }

      raw <- proxy_raw[idx]
      denom <- sum(raw, na.rm = TRUE)
      if (denom > 0) {
        result[idx] <- E_target * (raw / denom)
      } else {
        result[idx] <- E_target / length(idx)
      }
    }
  }
  result
}

# ── Reconstruct fold assignments (same logic as build_repeated_cv_proxy_asinh.R)
assign_folds <- function(panel, cv_type, K, seed) {
  set.seed(seed)
  if (cv_type == "sector") {
    sectors <- sort(unique(panel$primary_nace2d))
    sector_folds <- sample(rep(1:K, length.out = length(sectors)))
    sfm <- data.frame(primary_nace2d = sectors, fold_k = sector_folds,
                       stringsAsFactors = FALSE)
    fold_k <- sfm$fold_k[match(panel$primary_nace2d, sfm$primary_nace2d)]
  } else {
    firms <- unique(panel[, c("vat", "primary_nace2d")])
    firms <- firms[order(firms$vat), ]
    firm_folds <- integer(nrow(firms))
    for (sec in unique(firms$primary_nace2d)) {
      idx <- which(firms$primary_nace2d == sec)
      firm_folds[idx] <- sample(rep(1:K, length.out = length(idx)))
    }
    firms$fold_k <- firm_folds
    fold_k <- firms$fold_k[match(panel$vat, firms$vat)]
  }
  fold_k
}

# ── Metric names to extract ─────────────────────────────────────────────────
metric_names <- c("nrmse_sd", "median_apd", "rho_pooled_global",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")

extract_metrics <- function(m) {
  sapply(metric_names, function(nm) m[[nm]])
}

# =============================================================================
# Row 1: EN, sector CV — average metrics across M repeats
# =============================================================================
cat("\nComputing Row 1: EN, sector CV (", M_sec, "repeats)...\n")

K_sec <- 5L
metrics_sec <- matrix(NA_real_, nrow = M_sec, ncol = length(metric_names))
colnames(metrics_sec) <- metric_names

for (r in seq_len(M_sec)) {
  fold_k_r <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  proxy_raw_r <- proxy_to_levels(proxy_matrix_sec[, r])
  yhat_r <- calibrate_national(panel_sec, proxy_raw_r, fold_k_r)
  m_r <- calc_metrics(panel_sec$y, yhat_r, fp_threshold = 0,
                      nace2d = panel_sec$nace2d, year = panel_sec$year)
  metrics_sec[r, ] <- extract_metrics(m_r)

  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M_sec))
}

m1_mean <- colMeans(metrics_sec, na.rm = TRUE)
m1_sd   <- apply(metrics_sec, 2, sd, na.rm = TRUE)
cat("  Done.\n")

# =============================================================================
# Row 2: EN, firm CV — average metrics across M repeats
# =============================================================================
cat("Computing Row 2: EN, firm CV (", M_firm, "repeats)...\n")

K_firm <- 10L
metrics_firm <- matrix(NA_real_, nrow = M_firm, ncol = length(metric_names))
colnames(metrics_firm) <- metric_names

for (r in seq_len(M_firm)) {
  fold_k_r <- assign_folds(panel_firm, "firm", K_firm, BASE_SEED + r)
  proxy_raw_r <- proxy_to_levels(proxy_matrix_firm[, r])
  yhat_r <- calibrate_national(panel_firm, proxy_raw_r, fold_k_r)
  m_r <- calc_metrics(panel_firm$y, yhat_r, fp_threshold = 0,
                      nace2d = panel_firm$nace2d, year = panel_firm$year)
  metrics_firm[r, ] <- extract_metrics(m_r)

  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M_firm))
}

m2_mean <- colMeans(metrics_firm, na.rm = TRUE)
m2_sd   <- apply(metrics_firm, 2, sd, na.rm = TRUE)
cat("  Done.\n")

# =============================================================================
# Row 3: NACE-based (deterministic — single evaluation)
# =============================================================================
cat("Computing Row 3: NACE-based...\n")

# Use sector-fold assignment from repeat 1 (arbitrary; proxy doesn't depend on folds)
fold_k_tab <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + 1L)
tabachova_raw <- proxy_to_levels(panel_sec$proxy_tabachova_asinh)
yhat_tab <- calibrate_national(panel_sec, tabachova_raw, fold_k_tab)
m3 <- calc_metrics(panel_sec$y, yhat_tab, fp_threshold = 0,
                   nace2d = panel_sec$nace2d, year = panel_sec$year)
m3_vals <- extract_metrics(m3)

cat("  Done.\n")

# =============================================================================
# Assemble and print results
# =============================================================================
col_labels <- c("nRMSE", "Med.APD", "Rho", "FPR", "TPR", "FPsev50", "FPsev99")

cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("MAIN RESULTS TABLE (national-aggregate calibration)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s\n",
            "", col_labels[1], col_labels[2], col_labels[3],
            col_labels[4], col_labels[5], col_labels[6], col_labels[7]))
cat(paste(rep("-", 90), collapse = ""), "\n")

# EN rows: mean (sd)
for (i in 1:2) {
  lbl <- c("EN, sector CV", "EN, firm CV")[i]
  mn <- if (i == 1) m1_mean else m2_mean
  sd <- if (i == 1) m1_sd else m2_sd
  cat(sprintf("%-20s %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f\n",
              lbl, mn[1], mn[2], mn[3], mn[4], mn[5], mn[6], mn[7]))
  cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s\n",
              "",
              sprintf("(%.3f)", sd[1]), sprintf("(%.3f)", sd[2]),
              sprintf("(%.3f)", sd[3]), sprintf("(%.3f)", sd[4]),
              sprintf("(%.3f)", sd[5]), sprintf("(%.3f)", sd[6]),
              sprintf("(%.3f)", sd[7])))
}

# NACE-based: no sd
cat(sprintf("%-20s %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f\n",
            "NACE-based",
            m3_vals[1], m3_vals[2], m3_vals[3],
            m3_vals[4], m3_vals[5], m3_vals[6], m3_vals[7]))
cat("\n")

# ── Generate LaTeX table ─────────────────────────────────────────────────────
fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

# Format mean with sd in parentheses below
fmt_with_sd <- function(mn, sd, digits = 3) {
  paste0(fmt(mn, digits), "\n", "\\textrm{\\scriptsize(", fmt(sd, digits), ")}")
}

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Main prediction results}",
  "\\label{tab:main_results}",
  "\\begin{tabular}{l ccc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction accuracy} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-8}",
  " & nRMSE & Med.\\ APD & Rank corr. & FPR & TPR & \\multicolumn{2}{c}{FP severity} \\\\",
  "\\cmidrule(lr){7-8}",
  " & & & & & & p50 & p99 \\\\",
  "\\midrule"
)

# EN rows with sd
for (i in 1:2) {
  lbl <- c("EN, sector CV", "EN, firm CV")[i]
  mn <- if (i == 1) m1_mean else m2_mean
  sd <- if (i == 1) m1_sd else m2_sd

  # Main row
  tex_lines <- c(tex_lines, sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
    lbl, fmt(mn[1]), fmt(mn[2]), fmt(mn[3]),
    fmt(mn[4]), fmt(mn[5]), fmt(mn[6]), fmt(mn[7])
  ))

  # SD row (smaller font, in parentheses)
  tex_lines <- c(tex_lines, sprintf(
    " & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} \\\\",
    fmt(sd[1]), fmt(sd[2]), fmt(sd[3]),
    fmt(sd[4]), fmt(sd[5]), fmt(sd[6]), fmt(sd[7])
  ))
}

# NACE-based (no sd)
tex_lines <- c(tex_lines, sprintf(
  "NACE-based & %s & %s & %s & %s & %s & %s & %s \\\\",
  fmt(m3_vals[1]), fmt(m3_vals[2]), fmt(m3_vals[3]),
  fmt(m3_vals[4]), fmt(m3_vals[5]), fmt(m3_vals[6]), fmt(m3_vals[7])
))

tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

# Write LaTeX
tex_path <- file.path(OUTPUT_DIR, "table_main_results.tex")
writeLines(tex_lines, tex_path)
cat("LaTeX table written to:", tex_path, "\n")

# ── Save full results for later use ──────────────────────────────────────────
full_results <- list(
  metric_names = metric_names,
  col_labels   = col_labels,
  # EN sector CV
  m1_mean = m1_mean, m1_sd = m1_sd, metrics_sec = metrics_sec,
  # EN firm CV
  m2_mean = m2_mean, m2_sd = m2_sd, metrics_firm = metrics_firm,
  # NACE-based
  m3_vals = m3_vals,
  # Metadata
  M_sec = M_sec, M_firm = M_firm,
  K_sec = K_sec, K_firm = K_firm,
  BASE_SEED = BASE_SEED
)
rds_path <- file.path(OUTPUT_DIR, "table_main_results.rds")
saveRDS(full_results, rds_path)
cat("Full results saved to:", rds_path, "\n")
