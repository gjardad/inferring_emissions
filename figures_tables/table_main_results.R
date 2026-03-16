###############################################################################
# figures_tables/table_main_results.R
#
# PURPOSE
#   Generate the main results table (Table X) for Section 4.
#   Two panels, three rows each, all using sector-level calibration:
#
#     Panel A: Sector-level CV design
#       Revenue     — deterministic (computed once, sd = 0)
#       EN, sector CV — M repeats (proxy varies per repeat)
#       NACE-based  — deterministic (computed once, sd = 0)
#
#     Panel B: Firm-level CV design
#       Revenue     — M repeats (fold assignment varies → E_target varies)
#       EN, firm CV — M repeats (fold + proxy vary)
#       NACE-based  — M repeats (fold assignment varies → E_target varies)
#       Restricted to sectors with >= MIN_FIRMS_FIRM_CV unique firms.
#
#   Calibration is fold-aware sector-level:
#     For each fold k and each (sector, year) cell, the emission total to
#     distribute is E_sy - E_train_sy_k, allocated proportionally to
#     the proxy among held-out firms in that cell.
#
#   Panel A detail: With sector-level folds, all firms in a primary_nace2d
#   are held out together, so E_target = E_total_sy (training has no firms
#   from that sector). The deterministic proxies (revenue, Tabachova) yield
#   identical calibrated predictions regardless of fold assignment → no
#   variation across repeats, sd = 0.
#
#   Panel B detail: With firm-level folds, only a fraction of each sector
#   is held out per fold, so E_target varies across repeats even for
#   deterministic proxies. Restricted to sectors with >= 15 firms to
#   ensure >= 3 held-out firms per fold (K=5), making within-sector
#   allocation meaningful.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#     Contains: proxy_matrix (N x M), repeated_cv_proxy_panel
#   {PROC_DATA}/repeated_cv_proxy_firm_asinh.RData
#     Contains: proxy_matrix (N x M), repeated_cv_proxy_panel
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#     Contains: training_sample (with revenue, proxy_tabachova)
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_results.tex
#   {OUTPUT_DIR}/table_main_results.rds
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
source(file.path(UTILS_DIR, "calibration_helpers.R"))

# ── Parameters (must match build_repeated_cv_proxy_asinh.R) ────────────────
BASE_SEED          <- 2026L
K_sec              <- 5L
K_firm             <- 5L
MIN_FIRMS_FIRM_CV  <- 15L   # Panel B: exclude sectors with fewer firms

# ── Load data ──────────────────────────────────────────────────────────────

# EN proxy matrices (the only inputs that require build scripts)
cat("Loading EN proxy (sector CV)...\n")
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel_sec        <- e_sec$repeated_cv_proxy_panel
M_sec <- ncol(proxy_matrix_sec)
rm(e_sec)

cat("Loading EN proxy (firm CV)...\n")
e_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_firm_asinh.RData"), envir = e_firm)
proxy_matrix_firm <- e_firm$proxy_matrix
panel_firm        <- e_firm$repeated_cv_proxy_panel
M_firm <- ncol(proxy_matrix_firm)
rm(e_firm)

# Deterministic proxies: revenue and Tabachova
cat("Loading training sample (for revenue and NACE-based proxies)...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

determ_df <- training_sample %>%
  select(vat, year, revenue, proxy_tabachova)
rm(training_sample, syt)

# Merge deterministic proxies into EN panels
panel_sec <- panel_sec %>%
  left_join(determ_df, by = c("vat", "year"))

panel_firm <- panel_firm %>%
  left_join(determ_df, by = c("vat", "year"))
rm(determ_df)

# ── Verify panel identity ────────────────────────────────────────────────────
# panel_sec and panel_firm are built from the same training_sample with the
# same filtering/sorting, so they should have identical rows.
stopifnot(
  nrow(panel_sec) == nrow(panel_firm),
  all(panel_sec$vat == panel_firm$vat),
  all(panel_sec$year == panel_firm$year)
)
cat("  Panel identity verified: panel_sec and panel_firm have identical rows.\n")

cat(sprintf("  EN sector CV: %d obs x %d repeats\n", nrow(proxy_matrix_sec), M_sec))
cat(sprintf("  EN firm CV:   %d obs x %d repeats\n", nrow(proxy_matrix_firm), M_firm))

# Use common M across both EN models
M <- min(M_sec, M_firm)
cat(sprintf("  Using M = %d repeats\n", M))

# ── Metric names to extract ─────────────────────────────────────────────────
metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")

extract_metrics <- function(m) {
  sapply(metric_names, function(nm) m[[nm]])
}

# =============================================================================
# Panel A: Sector-level CV design
# =============================================================================
# Revenue and NACE-based: deterministic (computed once, sd = 0).
#   All firms in a primary_nace2d are held out together → E_target = E_total_sy.
#   Calibrated prediction = E_total_sy * proxy_i / sum(proxy), invariant to
#   fold assignment. Use any fold assignment (r = 1).
#
# EN: proxy varies per repeat → loop over M repeats.
# =============================================================================
cat("\n── Panel A: Sector-level CV design ──\n")

# Revenue and NACE-based: compute once
cat("  Computing Revenue and NACE-based (deterministic)...\n")
fold_k_A <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + 1L)

yhat_rev_A <- calibrate_sector(panel_sec, panel_sec$revenue, fold_k_A)
mA_rev <- calc_metrics(panel_sec$y, yhat_rev_A, fp_threshold = 0,
                       nace2d = panel_sec$nace2d, year = panel_sec$year)
mA_rev_mean <- extract_metrics(mA_rev)
mA_rev_sd   <- setNames(rep(0, length(metric_names)), metric_names)

yhat_nace_A <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_A)
mA_nace <- calc_metrics(panel_sec$y, yhat_nace_A, fp_threshold = 0,
                        nace2d = panel_sec$nace2d, year = panel_sec$year)
mA_nace_mean <- extract_metrics(mA_nace)
mA_nace_sd   <- setNames(rep(0, length(metric_names)), metric_names)

cat("  Done.\n")

# EN sector CV: loop over M repeats
cat(sprintf("  Computing EN sector CV (%d repeats)...\n", M))
metrics_en_sec <- matrix(NA_real_, nrow = M, ncol = length(metric_names),
                         dimnames = list(NULL, metric_names))

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  yhat_r <- calibrate_sector(panel_sec, proxy_to_levels(proxy_matrix_sec[, r]),
                             fold_k_r)
  m_r <- calc_metrics(panel_sec$y, yhat_r, fp_threshold = 0,
                      nace2d = panel_sec$nace2d, year = panel_sec$year)
  metrics_en_sec[r, ] <- extract_metrics(m_r)

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}

mA_en_mean <- colMeans(metrics_en_sec, na.rm = TRUE)
mA_en_sd   <- apply(metrics_en_sec, 2, sd, na.rm = TRUE)
cat("  Done.\n")


# =============================================================================
# Panel B: Firm-level CV design
# =============================================================================
# All three models loop over M repeats (fold assignment varies → E_target
# varies even for deterministic proxies).
#
# Restricted to sectors with >= MIN_FIRMS_FIRM_CV unique firms.
# Calibration uses the FULL panel (so E_target is correct), then metrics
# are computed on the subset.
# =============================================================================
cat("\n── Panel B: Firm-level CV design ──\n")

# Identify sectors with enough firms for meaningful firm-level evaluation
firms_per_sector <- panel_sec %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")

eligible_sectors <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_FIRM_CV) %>%
  pull(primary_nace2d)

cat(sprintf("  Sector filter: >= %d firms → %d of %d sectors retained\n",
            MIN_FIRMS_FIRM_CV, length(eligible_sectors), nrow(firms_per_sector)))
cat(sprintf("  Eligible sectors: %s\n", paste(eligible_sectors, collapse = ", ")))

# Indices of firm-years in eligible sectors (for metric computation)
idx_eligible <- which(panel_sec$primary_nace2d %in% eligible_sectors)
cat(sprintf("  Firm-years in eligible sectors: %d of %d (%.1f%%)\n",
            length(idx_eligible), nrow(panel_sec),
            100 * length(idx_eligible) / nrow(panel_sec)))

metrics_rev_firm  <- matrix(NA_real_, nrow = M, ncol = length(metric_names),
                            dimnames = list(NULL, metric_names))
metrics_en_firm   <- matrix(NA_real_, nrow = M, ncol = length(metric_names),
                            dimnames = list(NULL, metric_names))
metrics_nace_firm <- matrix(NA_real_, nrow = M, ncol = length(metric_names),
                            dimnames = list(NULL, metric_names))

cat(sprintf("  Computing all models (%d repeats)...\n", M))

for (r in seq_len(M)) {
  # Fold assignment on the full panel (same seed convention as build script)
  fold_k_r <- assign_folds(panel_sec, "firm", K_firm, BASE_SEED + r)

  # Revenue: calibrate on full panel, metrics on eligible subset
  yhat_r <- calibrate_sector(panel_sec, panel_sec$revenue, fold_k_r)
  m_r <- calc_metrics(panel_sec$y[idx_eligible], yhat_r[idx_eligible],
                      fp_threshold = 0,
                      nace2d = panel_sec$nace2d[idx_eligible],
                      year = panel_sec$year[idx_eligible])
  metrics_rev_firm[r, ] <- extract_metrics(m_r)

  # EN firm CV: calibrate on full panel, metrics on eligible subset
  yhat_r <- calibrate_sector(panel_sec,
                             proxy_to_levels(proxy_matrix_firm[, r]),
                             fold_k_r)
  m_r <- calc_metrics(panel_sec$y[idx_eligible], yhat_r[idx_eligible],
                      fp_threshold = 0,
                      nace2d = panel_sec$nace2d[idx_eligible],
                      year = panel_sec$year[idx_eligible])
  metrics_en_firm[r, ] <- extract_metrics(m_r)

  # NACE-based: calibrate on full panel, metrics on eligible subset
  yhat_r <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_r)
  m_r <- calc_metrics(panel_sec$y[idx_eligible], yhat_r[idx_eligible],
                      fp_threshold = 0,
                      nace2d = panel_sec$nace2d[idx_eligible],
                      year = panel_sec$year[idx_eligible])
  metrics_nace_firm[r, ] <- extract_metrics(m_r)

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

mB_rev_mean  <- colMeans(metrics_rev_firm,  na.rm = TRUE)
mB_rev_sd    <- apply(metrics_rev_firm,  2, sd, na.rm = TRUE)
mB_en_mean   <- colMeans(metrics_en_firm,   na.rm = TRUE)
mB_en_sd     <- apply(metrics_en_firm,   2, sd, na.rm = TRUE)
mB_nace_mean <- colMeans(metrics_nace_firm, na.rm = TRUE)
mB_nace_sd   <- apply(metrics_nace_firm, 2, sd, na.rm = TRUE)


# =============================================================================
# Assemble and print results
# =============================================================================
# nRMSE normalized separately within each panel by that panel's Revenue RMSE.
rmse_baseline_A <- mA_rev_mean["rmse"]
rmse_baseline_B <- mB_rev_mean["rmse"]

# Helper: compute display values given a panel-specific RMSE baseline.
# deterministic = TRUE for Panel A revenue/NACE (no sd row).
display_vals <- function(mn, sd_vec, rmse_baseline, deterministic = FALSE) {
  list(
    rmse    = c(mn["rmse"]    / 1e3, sd_vec["rmse"]    / 1e3),
    nrmse   = c(mn["rmse"] / rmse_baseline,
                sd_vec["rmse"] / rmse_baseline),
    mapd    = c(mn["median_apd"],           sd_vec["median_apd"]),
    pearson = c(mn["pearson"],              sd_vec["pearson"]),
    spear   = c(mn["spearman"],             sd_vec["spearman"]),
    fpr     = c(mn["fpr_nonemitters"],      sd_vec["fpr_nonemitters"]),
    tpr     = c(mn["tpr_emitters"],         sd_vec["tpr_emitters"]),
    p50     = c(mn["avg_nonemit_p50_rank"], sd_vec["avg_nonemit_p50_rank"]),
    p99     = c(mn["avg_nonemit_p99_rank"], sd_vec["avg_nonemit_p99_rank"]),
    deterministic = deterministic
  )
}

print_row <- function(lbl, dv) {
  cat(sprintf("%-20s %10.1f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f\n",
              lbl,
              dv$rmse[1], dv$nrmse[1], dv$mapd[1],
              dv$pearson[1], dv$spear[1],
              dv$fpr[1], dv$tpr[1], dv$p50[1], dv$p99[1]))
  if (!dv$deterministic) {
    cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
                "",
                sprintf("(%.1f)",  dv$rmse[2]),
                sprintf("(%.3f)", dv$nrmse[2]),
                sprintf("(%.3f)", dv$mapd[2]),
                sprintf("(%.3f)", dv$pearson[2]),
                sprintf("(%.3f)", dv$spear[2]),
                sprintf("(%.3f)", dv$fpr[2]),
                sprintf("(%.3f)", dv$tpr[2]),
                sprintf("(%.3f)", dv$p50[2]),
                sprintf("(%.3f)", dv$p99[2])))
  }
}

col_labels <- c("RMSE (kt)", "nRMSE", "Med.APD", "rho", "rho_S",
                "FPR", "TPR", "FPsev50", "FPsev99")

hdr <- sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
               "", col_labels[1], col_labels[2], col_labels[3], col_labels[4],
               col_labels[5], col_labels[6], col_labels[7], col_labels[8],
               col_labels[9])
sep <- paste(rep("-", 110), collapse = "")

cat("\n======================================================================\n")
cat("MAIN RESULTS TABLE\n")
cat("  Panel A: all sectors | Panel B: sectors with >=",
    MIN_FIRMS_FIRM_CV, "firms\n")
cat("  nRMSE normalized within each panel by that panel's Revenue RMSE\n")
cat("======================================================================\n\n")

cat("Panel A: Sector-level CV design\n")
cat(hdr); cat(sep, "\n")
print_row("Revenue",       display_vals(mA_rev_mean,  mA_rev_sd,  rmse_baseline_A, deterministic = TRUE))
print_row("EN, sector CV", display_vals(mA_en_mean,   mA_en_sd,   rmse_baseline_A))
print_row("NACE-based",    display_vals(mA_nace_mean, mA_nace_sd, rmse_baseline_A, deterministic = TRUE))
cat(sprintf("  Revenue RMSE baseline: %.1f kt\n\n", rmse_baseline_A / 1e3))

cat("Panel B: Firm-level CV design\n")
cat(hdr); cat(sep, "\n")
print_row("Revenue",       display_vals(mB_rev_mean,  mB_rev_sd,  rmse_baseline_B))
print_row("EN, firm CV",   display_vals(mB_en_mean,   mB_en_sd,   rmse_baseline_B))
print_row("NACE-based",    display_vals(mB_nace_mean, mB_nace_sd, rmse_baseline_B))
cat(sprintf("  Revenue RMSE baseline: %.1f kt\n\n", rmse_baseline_B / 1e3))

# ── Generate LaTeX table ─────────────────────────────────────────────────────
fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt1 <- function(x)             formatC(x, format = "f", digits = 1)

tex_row <- function(lbl, dv) {
  mean_line <- sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                       lbl,
                       fmt1(dv$rmse[1]), fmt(dv$nrmse[1]), fmt(dv$mapd[1]),
                       fmt(dv$pearson[1]), fmt(dv$spear[1]),
                       fmt(dv$fpr[1]), fmt(dv$tpr[1]), fmt(dv$p50[1]), fmt(dv$p99[1]))
  if (dv$deterministic) return(mean_line)
  sd_line <- sprintf(" & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} \\\\",
                     fmt1(dv$rmse[2]),
                     fmt(dv$nrmse[2]), fmt(dv$mapd[2]),
                     fmt(dv$pearson[2]), fmt(dv$spear[2]),
                     fmt(dv$fpr[2]), fmt(dv$tpr[2]), fmt(dv$p50[2]), fmt(dv$p99[2]))
  c(mean_line, sd_line)
}

tex_lines <- c(
  "\\begin{tabular}{l ccc cc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction error} & \\multicolumn{2}{c}{Correlation} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-10}",
  " & RMSE (kt) & nRMSE & Med.\\ APD & Levels & Rank & FPR & TPR & p50 & p99 \\\\",
  "\\midrule",
  "\\multicolumn{10}{l}{\\textit{Panel A: Sector-level CV design}} \\\\",
  "\\addlinespace[2pt]",
  tex_row("Revenue",       display_vals(mA_rev_mean,  mA_rev_sd,  rmse_baseline_A, deterministic = TRUE)),
  tex_row("EN, sector CV", display_vals(mA_en_mean,   mA_en_sd,   rmse_baseline_A)),
  tex_row("NACE-based",    display_vals(mA_nace_mean, mA_nace_sd, rmse_baseline_A, deterministic = TRUE)),
  "\\addlinespace[4pt]",
  "\\multicolumn{10}{l}{\\textit{Panel B: Firm-level CV design}} \\\\",
  "\\addlinespace[2pt]",
  tex_row("Revenue",       display_vals(mB_rev_mean,  mB_rev_sd,  rmse_baseline_B)),
  tex_row("EN, firm CV",   display_vals(mB_en_mean,   mB_en_sd,   rmse_baseline_B)),
  tex_row("NACE-based",    display_vals(mB_nace_mean, mB_nace_sd, rmse_baseline_B)),
  "\\bottomrule",
  "\\end{tabular}"
)

# Write LaTeX
tex_path <- file.path(OUTPUT_DIR, "table_main_results.tex")
writeLines(tex_lines, tex_path)
cat("LaTeX table written to:", tex_path, "\n")

# ── Save full results for later use ──────────────────────────────────────────
full_results <- list(
  metric_names = metric_names,
  col_labels   = col_labels,
  # Panel A: sector folds (revenue/NACE deterministic, EN averaged)
  mA_rev_mean  = mA_rev_mean,  mA_rev_sd  = mA_rev_sd,
  mA_en_mean   = mA_en_mean,   mA_en_sd   = mA_en_sd,   metrics_en_sec   = metrics_en_sec,
  mA_nace_mean = mA_nace_mean, mA_nace_sd = mA_nace_sd,
  # Panel B: firm folds (all averaged across repeats)
  mB_rev_mean  = mB_rev_mean,  mB_rev_sd  = mB_rev_sd,  metrics_rev_firm  = metrics_rev_firm,
  mB_en_mean   = mB_en_mean,   mB_en_sd   = mB_en_sd,   metrics_en_firm   = metrics_en_firm,
  mB_nace_mean = mB_nace_mean, mB_nace_sd = mB_nace_sd, metrics_nace_firm = metrics_nace_firm,
  # Baselines
  rmse_baseline_A = rmse_baseline_A,
  rmse_baseline_B = rmse_baseline_B,
  # Metadata
  M = M, K_sec = K_sec, K_firm = K_firm, BASE_SEED = BASE_SEED,
  MIN_FIRMS_FIRM_CV = MIN_FIRMS_FIRM_CV,
  eligible_sectors = eligible_sectors,
  firms_per_sector = firms_per_sector
)
rds_path <- file.path(OUTPUT_DIR, "table_main_results.rds")
saveRDS(full_results, rds_path)
cat("Full results saved to:", rds_path, "\n")
