###############################################################################
# figures_tables/sector_heterogeneity.R
#
# PURPOSE
#   Produce two outputs for the sector heterogeneity subsection:
#
#   A. Cross-sector distribution of within-sector metrics (Purpose 1)
#      - For each sector, compute year-demeaned Spearman rho and Median APD
#        (averaged across M repeats). Report the distribution across ~29 sectors.
#      - Output: dot/strip figure + summary table.
#
#   B. Sector-specific table for 17/18, 19, 24 (Purpose 2)
#      - Same structure as main results table but restricted to each sector.
#      - Shows that revenue benchmark cannot distinguish emitters from
#        non-emitters within these sectors.
#      - Output: LaTeX table with 3 panels.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/repeated_cv_proxy_firm_asinh.RData
#   {PROC_DATA}/repeated_cv_revenue_benchmark_sector.RData
#   {PROC_DATA}/repeated_cv_revenue_benchmark_firm.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/figure_sector_heterogeneity.pdf
#   {OUTPUT_DIR}/table_sector_heterogeneity_summary.tex
#   {OUTPUT_DIR}/table_sector_specific.tex
#   {OUTPUT_DIR}/sector_heterogeneity_results.rds
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

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED <- 2026L
K_sec     <- 5L
K_firm    <- 10L

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading data...\n")

# Revenue benchmark (sector folds)
e_rev <- new.env()
load(file.path(PROC_DATA, "repeated_cv_revenue_benchmark_sector.RData"), envir = e_rev)
proxy_matrix_rev <- e_rev$proxy_matrix
panel_rev        <- e_rev$repeated_cv_proxy_panel
M_rev <- ncol(proxy_matrix_rev)
rm(e_rev)

# Revenue benchmark (firm folds)
e_rev_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_revenue_benchmark_firm.RData"), envir = e_rev_firm)
proxy_matrix_rev_firm <- e_rev_firm$proxy_matrix
panel_rev_firm        <- e_rev_firm$repeated_cv_proxy_panel
M_rev_firm <- ncol(proxy_matrix_rev_firm)
rm(e_rev_firm)

# EN sector CV
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel_sec        <- e_sec$repeated_cv_proxy_panel
M_sec <- ncol(proxy_matrix_sec)
rm(e_sec)

# EN firm CV
e_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_firm_asinh.RData"), envir = e_firm)
proxy_matrix_firm <- e_firm$proxy_matrix
panel_firm        <- e_firm$repeated_cv_proxy_panel
M_firm <- ncol(proxy_matrix_firm)
rm(e_firm)

# Training sample (for Tabachova proxy)
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
tabachova_df <- training_sample %>%
  select(vat, year, proxy_tabachova, proxy_tabachova_asinh)
rm(training_sample, syt)

panel_sec <- panel_sec %>%
  left_join(tabachova_df, by = c("vat", "year"))

cat(sprintf("  Revenue (sector): %d obs x %d repeats\n", nrow(proxy_matrix_rev), M_rev))
cat(sprintf("  Revenue (firm):   %d obs x %d repeats\n", nrow(proxy_matrix_rev_firm), M_rev_firm))
cat(sprintf("  EN sector CV:     %d obs x %d repeats\n", nrow(proxy_matrix_sec), M_sec))
cat(sprintf("  EN firm CV:       %d obs x %d repeats\n", nrow(proxy_matrix_firm), M_firm))

# Use common M across all models
M <- min(M_rev, M_rev_firm, M_sec, M_firm)
cat(sprintf("  Using M = %d repeats (minimum across models)\n", M))

# ── Sector list ──────────────────────────────────────────────────────────────
all_sectors <- sort(unique(panel_sec$nace2d))
n_sectors   <- length(all_sectors)
cat(sprintf("  %d unique NACE 2-digit sectors\n", n_sectors))

# =============================================================================
# PURPOSE 1: Within-sector metrics across all sectors
# =============================================================================
cat("\n=== PURPOSE 1: Within-sector heterogeneity ===\n")

# Storage: [repeat, sector, model]
model_names <- c("Revenue", "EN, sector CV", "EN, firm CV", "NACE-based")
n_models    <- length(model_names)

rho_array <- array(NA_real_, dim = c(M, n_sectors, n_models),
                   dimnames = list(NULL, all_sectors, model_names))
apd_array <- array(NA_real_, dim = c(M, n_sectors, n_models),
                   dimnames = list(NULL, all_sectors, model_names))

# Helper: compute year-demeaned within-sector Spearman rho and Median APD
# for all sectors at once from calibrated predictions.
compute_sector_metrics <- function(y, yhat, nace2d, year) {
  n <- length(y)

  # Year-demean y and yhat
  all_yrs <- sort(unique(year))
  y_dm    <- numeric(n)
  yhat_dm <- numeric(n)
  for (yr in all_yrs) {
    idx <- which(year == yr)
    y_dm[idx]    <- y[idx] - mean(y[idx])
    yhat_dm[idx] <- yhat[idx] - mean(yhat[idx])
  }

  # Per-sector: demeaned Spearman rho + Median APD among emitters
  sectors <- sort(unique(nace2d))
  rho_vec <- setNames(rep(NA_real_, length(sectors)), sectors)
  apd_vec <- setNames(rep(NA_real_, length(sectors)), sectors)

  for (sec in sectors) {
    idx <- which(nace2d == sec)

    # Demeaned Spearman rho (pooled across years)
    if (length(idx) >= 3 && sd(y_dm[idx]) > 0 && sd(yhat_dm[idx]) > 0) {
      rho_vec[sec] <- suppressWarnings(
        stats::cor(y_dm[idx], yhat_dm[idx], method = "spearman", use = "complete.obs")
      )
    }

    # Median APD among emitters (non-demeaned)
    emit_idx <- idx[y[idx] > 0]
    if (length(emit_idx) >= 1) {
      apd <- abs(y[emit_idx] - yhat[emit_idx]) / y[emit_idx]
      apd_vec[sec] <- median(apd, na.rm = TRUE)
    }
  }

  list(rho = rho_vec, apd = apd_vec)
}

# Main loop across repeats
cat("Computing within-sector metrics across repeats...\n")

for (r in seq_len(M)) {
  # --- Model 0: Revenue (sector CV folds) ---
  fold_k_r    <- assign_folds(panel_rev, "sector", K_sec, BASE_SEED + r)
  proxy_raw_r <- proxy_to_levels(proxy_matrix_rev[, r])
  yhat_r      <- calibrate_sector(panel_rev, proxy_raw_r, fold_k_r)
  sm <- compute_sector_metrics(panel_rev$y, yhat_r, panel_rev$nace2d, panel_rev$year)
  rho_array[r, , 1] <- sm$rho[all_sectors]
  apd_array[r, , 1] <- sm$apd[all_sectors]

  # --- Model 1: EN, sector CV ---
  fold_k_r    <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  proxy_raw_r <- proxy_to_levels(proxy_matrix_sec[, r])
  yhat_r      <- calibrate_sector(panel_sec, proxy_raw_r, fold_k_r)
  sm <- compute_sector_metrics(panel_sec$y, yhat_r, panel_sec$nace2d, panel_sec$year)
  rho_array[r, , 2] <- sm$rho[all_sectors]
  apd_array[r, , 2] <- sm$apd[all_sectors]

  # --- Model 2: EN, firm CV ---
  fold_k_r    <- assign_folds(panel_firm, "firm", K_firm, BASE_SEED + r)
  proxy_raw_r <- proxy_to_levels(proxy_matrix_firm[, r])
  yhat_r      <- calibrate_sector(panel_firm, proxy_raw_r, fold_k_r)
  sm <- compute_sector_metrics(panel_firm$y, yhat_r, panel_firm$nace2d, panel_firm$year)
  rho_array[r, , 3] <- sm$rho[all_sectors]
  apd_array[r, , 3] <- sm$apd[all_sectors]

  # --- Model 3: NACE-based (sector CV folds) ---
  fold_k_r    <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  yhat_r      <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_r)
  sm <- compute_sector_metrics(panel_sec$y, yhat_r, panel_sec$nace2d, panel_sec$year)
  rho_array[r, , 4] <- sm$rho[all_sectors]
  apd_array[r, , 4] <- sm$apd[all_sectors]

  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("  Done.\n")

# Average across repeats
rho_sector_avg <- apply(rho_array, c(2, 3), mean, na.rm = TRUE)  # [sector, model]
apd_sector_avg <- apply(apd_array, c(2, 3), mean, na.rm = TRUE)

# Sector sizes (number of firm-years)
sector_sizes <- table(panel_sec$nace2d)[all_sectors]

# ── Purpose 1: Summary table ────────────────────────────────────────────────
cat("\n── Within-sector Spearman rho: distribution across sectors ──\n")
cat(sprintf("%-20s %8s %8s %8s %8s %8s\n",
            "", "Median", "p25", "p75", "Min", "Max"))
cat(paste(rep("-", 72), collapse = ""), "\n")

for (m in seq_len(n_models)) {
  vals <- rho_sector_avg[, m]
  vals <- vals[is.finite(vals)]
  cat(sprintf("%-20s %8.3f %8.3f %8.3f %8.3f %8.3f\n",
              model_names[m],
              median(vals), quantile(vals, 0.25), quantile(vals, 0.75),
              min(vals), max(vals)))
}

cat("\n── Within-sector Median APD: distribution across sectors ──\n")
cat(sprintf("%-20s %8s %8s %8s %8s %8s\n",
            "", "Median", "p25", "p75", "Min", "Max"))
cat(paste(rep("-", 72), collapse = ""), "\n")

for (m in seq_len(n_models)) {
  vals <- apd_sector_avg[, m]
  vals <- vals[is.finite(vals)]
  cat(sprintf("%-20s %8.3f %8.3f %8.3f %8.3f %8.3f\n",
              model_names[m],
              median(vals), quantile(vals, 0.25), quantile(vals, 0.75),
              min(vals), max(vals)))
}

# ── Purpose 1: Figure ────────────────────────────────────────────────────────
fig_path <- file.path(OUTPUT_DIR, "figure_sector_heterogeneity.pdf")
cat("\nGenerating figure:", fig_path, "\n")

pdf(fig_path, width = 10, height = 5)
par(mfrow = c(1, 2), mar = c(4, 5, 3, 1))

# Panel A: Spearman rho
plot(NULL, xlim = c(0.5, n_models + 0.5), ylim = range(rho_sector_avg, na.rm = TRUE),
     xlab = "", ylab = "Year-demeaned Spearman rho",
     xaxt = "n", main = "A. Within-sector rank correlation")
axis(1, at = 1:n_models, labels = model_names, las = 2, cex.axis = 0.7)
abline(h = 0, lty = 2, col = "grey60")

for (m in seq_len(n_models)) {
  vals <- rho_sector_avg[, m]
  valid <- is.finite(vals)
  jitter_x <- m + runif(sum(valid), -0.15, 0.15)
  # Scale dot size by log sector size
  sizes <- log(as.numeric(sector_sizes[valid])) / max(log(as.numeric(sector_sizes)))
  points(jitter_x, vals[valid], pch = 19, col = adjustcolor("steelblue", 0.6),
         cex = 0.5 + 1.5 * sizes)
  # Median line
  segments(m - 0.25, median(vals, na.rm = TRUE),
           m + 0.25, median(vals, na.rm = TRUE),
           col = "red", lwd = 2)
}

# Panel B: Median APD
plot(NULL, xlim = c(0.5, n_models + 0.5), ylim = range(apd_sector_avg, na.rm = TRUE),
     xlab = "", ylab = "Median APD (among emitters)",
     xaxt = "n", main = "B. Within-sector prediction error")
axis(1, at = 1:n_models, labels = model_names, las = 2, cex.axis = 0.7)

for (m in seq_len(n_models)) {
  vals <- apd_sector_avg[, m]
  valid <- is.finite(vals)
  jitter_x <- m + runif(sum(valid), -0.15, 0.15)
  sizes <- log(as.numeric(sector_sizes[valid])) / max(log(as.numeric(sector_sizes)))
  points(jitter_x, vals[valid], pch = 19, col = adjustcolor("steelblue", 0.6),
         cex = 0.5 + 1.5 * sizes)
  segments(m - 0.25, median(vals, na.rm = TRUE),
           m + 0.25, median(vals, na.rm = TRUE),
           col = "red", lwd = 2)
}

dev.off()
cat("  Figure saved.\n")

# ── Purpose 1: LaTeX summary table ──────────────────────────────────────────
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

tex_summary <- c(
  "\\begin{tabular}{l ccccc ccccc}",
  "\\toprule",
  " & \\multicolumn{5}{c}{Spearman $\\rho$ (year-demeaned)} & \\multicolumn{5}{c}{Median APD (emitters)} \\\\",
  "\\cmidrule(lr){2-6} \\cmidrule(lr){7-11}",
  " & Median & p25 & p75 & Min & Max & Median & p25 & p75 & Min & Max \\\\",
  "\\midrule"
)

for (m in seq_len(n_models)) {
  rho_vals <- rho_sector_avg[, m]; rho_vals <- rho_vals[is.finite(rho_vals)]
  apd_vals <- apd_sector_avg[, m]; apd_vals <- apd_vals[is.finite(apd_vals)]
  tex_summary <- c(tex_summary, sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
    model_names[m],
    fmt(median(rho_vals)), fmt(quantile(rho_vals, 0.25)),
    fmt(quantile(rho_vals, 0.75)), fmt(min(rho_vals)), fmt(max(rho_vals)),
    fmt(median(apd_vals)), fmt(quantile(apd_vals, 0.25)),
    fmt(quantile(apd_vals, 0.75)), fmt(min(apd_vals)), fmt(max(apd_vals))
  ))
}

tex_summary <- c(tex_summary, "\\bottomrule", "\\end{tabular}")
tex_summary_path <- file.path(OUTPUT_DIR, "table_sector_heterogeneity_summary.tex")
writeLines(tex_summary, tex_summary_path)
cat("Summary table written to:", tex_summary_path, "\n")


# =============================================================================
# PURPOSE 2: Sector-specific table for 17/18, 19, 24
#   Two-panel structure matching table_main_results.R:
#     Panel A: Sector-level CV design (Revenue, EN sector CV, NACE-based)
#     Panel B: Firm-level CV design   (Revenue, EN firm CV, NACE-based)
#   Column groups: NACE 17/18 | NACE 19 | NACE 24
#   Metrics per group: nRMSE, rho_S, FPR, p99
# =============================================================================
cat("\n=== PURPOSE 2: Sector-specific metrics for 17/18, 19, 24 ===\n")

target_sectors <- list(
  "17/18" = c("17", "18"),
  "19"    = c("19"),
  "24"    = c("24")
)

metric_names_p2 <- c("rmse", "median_apd", "pearson", "spearman",
                     "fpr_nonemitters", "tpr_emitters",
                     "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")

extract_metrics <- function(m) {
  sapply(metric_names_p2, function(nm) m[[nm]])
}

# 6 models: Panel A (3) + Panel B (3)
p2_model_names <- c(
  # Panel A: Sector-level CV
  "A: Revenue", "A: EN, sector CV", "A: NACE-based",
  # Panel B: Firm-level CV
  "B: Revenue", "B: EN, firm CV", "B: NACE-based"
)
n_p2_models <- length(p2_model_names)

# Storage: [repeat, metric, model, target_sector]
n_targets    <- length(target_sectors)
target_names <- names(target_sectors)
sector_metrics <- array(NA_real_,
  dim = c(M, length(metric_names_p2), n_p2_models, n_targets),
  dimnames = list(NULL, metric_names_p2, p2_model_names, target_names)
)

# Helper: recode nace2d so 17+18 are treated as one group
recode_nace <- function(nace2d, sec_codes) {
  ifelse(nace2d %in% sec_codes, sec_codes[1], nace2d)
}

cat("Computing sector-specific metrics across repeats...\n")

tabachova_raw <- panel_sec$proxy_tabachova

for (r in seq_len(M)) {

  # --- Panel A: Sector-level CV design ---

  # Revenue (sector folds)
  fold_k_rev_r <- assign_folds(panel_rev, "sector", K_sec, BASE_SEED + r)
  yhat_rev_r   <- calibrate_sector(panel_rev,
                    proxy_to_levels(proxy_matrix_rev[, r]), fold_k_rev_r)

  # EN, sector CV
  fold_k_sec_r <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  yhat_sec_r   <- calibrate_sector(panel_sec,
                    proxy_to_levels(proxy_matrix_sec[, r]), fold_k_sec_r)

  # NACE-based (sector folds)
  yhat_tab_sec_r <- calibrate_sector(panel_sec, tabachova_raw, fold_k_sec_r)

  # --- Panel B: Firm-level CV design ---

  # Revenue (firm folds)
  fold_k_rev_firm_r <- assign_folds(panel_rev_firm, "firm", K_firm, BASE_SEED + r)
  yhat_rev_firm_r   <- calibrate_sector(panel_rev_firm,
                         proxy_to_levels(proxy_matrix_rev_firm[, r]),
                         fold_k_rev_firm_r)

  # EN, firm CV
  fold_k_firm_r <- assign_folds(panel_firm, "firm", K_firm, BASE_SEED + r)
  yhat_firm_r   <- calibrate_sector(panel_firm,
                     proxy_to_levels(proxy_matrix_firm[, r]), fold_k_firm_r)

  # NACE-based (firm folds)
  fold_k_tab_firm_r <- assign_folds(panel_sec, "firm", K_firm, BASE_SEED + r)
  yhat_tab_firm_r   <- calibrate_sector(panel_sec, tabachova_raw,
                         fold_k_tab_firm_r)

  # --- For each target sector, compute metrics on the subset ---
  for (t in seq_along(target_sectors)) {
    sec_codes <- target_sectors[[t]]

    # Subset indices for each panel
    idx_rev      <- which(panel_rev$nace2d %in% sec_codes)
    idx_sec      <- which(panel_sec$nace2d %in% sec_codes)
    idx_rev_firm <- which(panel_rev_firm$nace2d %in% sec_codes)
    idx_firm     <- which(panel_firm$nace2d %in% sec_codes)

    # Panel A, Model 1: Revenue (sector folds)
    if (length(idx_rev) > 0) {
      m_r <- calc_metrics(panel_rev$y[idx_rev], yhat_rev_r[idx_rev],
                fp_threshold = 0,
                nace2d = recode_nace(panel_rev$nace2d[idx_rev], sec_codes),
                year = panel_rev$year[idx_rev])
      sector_metrics[r, , 1, t] <- extract_metrics(m_r)
    }

    # Panel A, Model 2: EN, sector CV
    if (length(idx_sec) > 0) {
      m_r <- calc_metrics(panel_sec$y[idx_sec], yhat_sec_r[idx_sec],
                fp_threshold = 0,
                nace2d = recode_nace(panel_sec$nace2d[idx_sec], sec_codes),
                year = panel_sec$year[idx_sec])
      sector_metrics[r, , 2, t] <- extract_metrics(m_r)
    }

    # Panel A, Model 3: NACE-based (sector folds)
    if (length(idx_sec) > 0) {
      m_r <- calc_metrics(panel_sec$y[idx_sec], yhat_tab_sec_r[idx_sec],
                fp_threshold = 0,
                nace2d = recode_nace(panel_sec$nace2d[idx_sec], sec_codes),
                year = panel_sec$year[idx_sec])
      sector_metrics[r, , 3, t] <- extract_metrics(m_r)
    }

    # Panel B, Model 4: Revenue (firm folds)
    if (length(idx_rev_firm) > 0) {
      m_r <- calc_metrics(panel_rev_firm$y[idx_rev_firm],
                yhat_rev_firm_r[idx_rev_firm],
                fp_threshold = 0,
                nace2d = recode_nace(panel_rev_firm$nace2d[idx_rev_firm], sec_codes),
                year = panel_rev_firm$year[idx_rev_firm])
      sector_metrics[r, , 4, t] <- extract_metrics(m_r)
    }

    # Panel B, Model 5: EN, firm CV
    if (length(idx_firm) > 0) {
      m_r <- calc_metrics(panel_firm$y[idx_firm], yhat_firm_r[idx_firm],
                fp_threshold = 0,
                nace2d = recode_nace(panel_firm$nace2d[idx_firm], sec_codes),
                year = panel_firm$year[idx_firm])
      sector_metrics[r, , 5, t] <- extract_metrics(m_r)
    }

    # Panel B, Model 6: NACE-based (firm folds)
    if (length(idx_sec) > 0) {
      m_r <- calc_metrics(panel_sec$y[idx_sec], yhat_tab_firm_r[idx_sec],
                fp_threshold = 0,
                nace2d = recode_nace(panel_sec$nace2d[idx_sec], sec_codes),
                year = panel_sec$year[idx_sec])
      sector_metrics[r, , 6, t] <- extract_metrics(m_r)
    }
  }

  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("  Done.\n")

# Average across repeats
sector_metrics_mean <- apply(sector_metrics, c(2, 3, 4), mean, na.rm = TRUE)
sector_metrics_sd   <- apply(sector_metrics, c(2, 3, 4), sd,   na.rm = TRUE)

# ── Purpose 2: Console output ───────────────────────────────────────────────
# Print full metrics by sector (for inspection), then the compact 4-metric table
p2_display_labels <- c("Revenue", "EN", "NACE-based")

for (panel_label in c("A", "B")) {
  panel_idx <- if (panel_label == "A") 1:3 else 4:6
  cv_type   <- if (panel_label == "A") "Sector-level" else "Firm-level"

  cat(sprintf("\n── Panel %s: %s CV design ──\n", panel_label, cv_type))

  for (t in seq_along(target_sectors)) {
    sec_label <- target_names[t]
    # nRMSE baseline: revenue RMSE within this sector for this panel
    rev_rmse <- sector_metrics_mean["rmse", p2_model_names[panel_idx[1]], t]

    cat(sprintf("\n  NACE %s:\n", sec_label))
    cat(sprintf("  %-15s %8s %8s %8s %8s\n", "", "nRMSE", "rho_S", "FPR", "p99"))
    cat(sprintf("  %s\n", paste(rep("-", 55), collapse = "")))

    for (i in seq_along(panel_idx)) {
      mn   <- sector_metrics_mean[, p2_model_names[panel_idx[i]], t]
      sd_v <- sector_metrics_sd[, p2_model_names[panel_idx[i]], t]
      nrmse    <- mn["rmse"] / rev_rmse
      nrmse_sd <- sd_v["rmse"] / rev_rmse

      cat(sprintf("  %-15s %8.3f %8.3f %8.3f %8.3f\n",
                  p2_display_labels[i],
                  nrmse, mn["spearman"],
                  mn["fpr_nonemitters"], mn["avg_nonemit_p99_rank"]))
      cat(sprintf("  %-15s %8s %8s %8s %8s\n",
                  "",
                  sprintf("(%.3f)", nrmse_sd),
                  sprintf("(%.3f)", sd_v["spearman"]),
                  sprintf("(%.3f)", sd_v["fpr_nonemitters"]),
                  sprintf("(%.3f)", sd_v["avg_nonemit_p99_rank"])))
    }
  }
}

# ── Purpose 2: LaTeX table ──────────────────────────────────────────────────
# Layout: rows = 2 panels × 3 models, columns = 3 sector groups × 4 metrics
#   l | cccc | cccc | cccc
fmt3 <- function(x) formatC(x, format = "f", digits = 3)

tex_sector <- c(
  "\\begin{tabular}{l cccc cccc cccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{4}{c}{Paper, pulp \\& printing} & \\multicolumn{4}{c}{Petroleum refining} & \\multicolumn{4}{c}{Iron \\& steel} \\\\"),
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9} \\cmidrule(lr){10-13}",
  " & nRMSE & $\\rho_S$ & FPR & p99 & nRMSE & $\\rho_S$ & FPR & p99 & nRMSE & $\\rho_S$ & FPR & p99 \\\\"
)

for (panel_label in c("A", "B")) {
  panel_idx <- if (panel_label == "A") 1:3 else 4:6
  cv_type   <- if (panel_label == "A") "Sector-level CV design" else "Firm-level CV design"

  tex_sector <- c(tex_sector,
    "\\midrule",
    sprintf("\\multicolumn{13}{l}{\\textit{Panel %s: %s}} \\\\", panel_label, cv_type),
    "\\addlinespace[2pt]"
  )

  for (i in seq_along(panel_idx)) {
    model_key <- p2_model_names[panel_idx[i]]
    row_label <- p2_display_labels[i]

    # Build the mean row and sd row across all 3 target sectors
    mean_cells <- character(0)
    sd_cells   <- character(0)

    for (t in seq_along(target_sectors)) {
      # nRMSE baseline: revenue RMSE for this panel in this sector
      rev_model <- p2_model_names[panel_idx[1]]
      rev_rmse  <- sector_metrics_mean["rmse", rev_model, t]

      mn   <- sector_metrics_mean[, model_key, t]
      sd_v <- sector_metrics_sd[, model_key, t]
      nrmse    <- mn["rmse"] / rev_rmse
      nrmse_sd <- sd_v["rmse"] / rev_rmse

      mean_cells <- c(mean_cells,
        fmt3(nrmse), fmt3(mn["spearman"]),
        fmt3(mn["fpr_nonemitters"]), fmt3(mn["avg_nonemit_p99_rank"]))
      sd_cells <- c(sd_cells,
        sprintf("{\\scriptsize(%s)}", fmt3(nrmse_sd)),
        sprintf("{\\scriptsize(%s)}", fmt3(sd_v["spearman"])),
        sprintf("{\\scriptsize(%s)}", fmt3(sd_v["fpr_nonemitters"])),
        sprintf("{\\scriptsize(%s)}", fmt3(sd_v["avg_nonemit_p99_rank"])))
    }

    tex_sector <- c(tex_sector,
      sprintf("%s & %s \\\\", row_label, paste(mean_cells, collapse = " & ")),
      sprintf(" & %s \\\\", paste(sd_cells, collapse = " & "))
    )
  }
}

tex_sector <- c(tex_sector, "\\bottomrule", "\\end{tabular}")
tex_sector_path <- file.path(OUTPUT_DIR, "table_sector_specific.tex")
writeLines(tex_sector, tex_sector_path)
cat("\nSector-specific table written to:", tex_sector_path, "\n")

# ── Save all results ─────────────────────────────────────────────────────────
results <- list(
  # Purpose 1
  rho_array       = rho_array,
  apd_array       = apd_array,
  rho_sector_avg  = rho_sector_avg,
  apd_sector_avg  = apd_sector_avg,
  sector_sizes    = sector_sizes,
  all_sectors     = all_sectors,
  # Purpose 2
  sector_metrics      = sector_metrics,
  sector_metrics_mean = sector_metrics_mean,
  sector_metrics_sd   = sector_metrics_sd,
  target_sectors      = target_sectors,
  p2_model_names      = p2_model_names,
  # Metadata
  model_names  = model_names,
  metric_names_p2 = metric_names_p2,
  M = M, K_sec = K_sec, K_firm = K_firm, BASE_SEED = BASE_SEED
)
rds_path <- file.path(OUTPUT_DIR, "sector_heterogeneity_results.rds")
saveRDS(results, rds_path)
cat("Full results saved to:", rds_path, "\n")
