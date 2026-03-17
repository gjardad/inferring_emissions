###############################################################################
# figures_tables/table_main_results_with_combined_models.R
#
# PURPOSE
#   Same as table_main_results.R but with two additional rows per panel:
#     - Gated Revenue: 1(EN > 0) * revenue (parameter-free)
#     - Combined: EN^alpha * revenue^(1-alpha) with cross-validated alpha
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_results_with_combined_models.tex
#   {OUTPUT_DIR}/table_main_results_with_combined_models.rds
#   Console output
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
BASE_SEED           <- 2026L
K_sec               <- 5L
K_firm              <- 5L
K_INNER             <- 5L
MIN_FIRMS_SECTOR_CV <- 3L
MIN_FIRMS_FIRM_CV   <- 15L
ALPHA_GRID          <- seq(0, 1, by = 0.1)

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading EN proxy (sector CV)...\n")
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel            <- e_sec$repeated_cv_proxy_panel
M_sec <- ncol(proxy_matrix_sec)
rm(e_sec)

cat("Loading EN proxy (firm CV)...\n")
e_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_firm_asinh.RData"), envir = e_firm)
proxy_matrix_firm <- e_firm$proxy_matrix
M_firm <- ncol(proxy_matrix_firm)
rm(e_firm)

cat("Loading training sample (for revenue and NACE-based proxies)...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
determ_df <- training_sample %>% select(vat, year, revenue, proxy_tabachova)
rm(training_sample, syt)

panel <- panel %>% left_join(determ_df, by = c("vat", "year"))
rm(determ_df)

M <- min(M_sec, M_firm)
cat(sprintf("  Using M = %d repeats\n", M))

# ── Metric names ─────────────────────────────────────────────────────────────
metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")

extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

# ── Sector size filter ───────────────────────────────────────────────────────
firms_per_sector <- panel %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")

sectors_A <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_SECTOR_CV) %>%
  pull(primary_nace2d)
idx_A <- which(panel$primary_nace2d %in% sectors_A)

eligible_sectors <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_FIRM_CV) %>%
  pull(primary_nace2d)
idx_eligible <- which(panel$primary_nace2d %in% eligible_sectors)

cat(sprintf("  Panel A sectors: %d (>= %d firms), %d firm-years\n",
            length(sectors_A), MIN_FIRMS_SECTOR_CV, length(idx_A)))
cat(sprintf("  Panel B sectors: %d (>= %d firms), %d firm-years\n",
            length(eligible_sectors), MIN_FIRMS_FIRM_CV, length(idx_eligible)))

# ── Helper: tune alpha on training folds ─────────────────────────────────────
tune_alpha <- function(train_panel, en_proxy_train, revenue_train,
                       alpha_grid, K_inner, seed_inner) {
  set.seed(seed_inner)
  firms <- unique(train_panel[, c("vat", "primary_nace2d")])
  firms <- firms[order(firms$vat), ]
  firm_folds <- integer(nrow(firms))
  for (sec in unique(firms$primary_nace2d)) {
    idx <- which(firms$primary_nace2d == sec)
    firm_folds[idx] <- sample(rep(1:K_inner, length.out = length(idx)))
  }
  firms$inner_fold <- firm_folds
  inner_fold_k <- firms$inner_fold[match(train_panel$vat, firms$vat)]

  rmse_by_alpha <- numeric(length(alpha_grid))
  for (a_idx in seq_along(alpha_grid)) {
    alpha <- alpha_grid[a_idx]
    combined <- (en_proxy_train ^ alpha) * (revenue_train ^ (1 - alpha))
    yhat_inner <- calibrate_sector(train_panel, combined, inner_fold_k)
    rmse_by_alpha[a_idx] <- sqrt(mean((train_panel$y - yhat_inner)^2, na.rm = TRUE))
  }
  alpha_grid[which.min(rmse_by_alpha)]
}

# =============================================================================
# Panel A: Sector-level CV design
# =============================================================================
cat("\n── Panel A: Sector-level CV design ──\n")

models <- c("Revenue", "Elastic Net", "NACE", "Gated Rev", "Combined")
metrics_A <- array(NA_real_,
                   dim = c(M, length(metric_names), length(models)),
                   dimnames = list(NULL, metric_names, models))
alphas_A <- matrix(NA_real_, nrow = M, ncol = K_sec)

# Revenue and NACE: deterministic in sector CV
cat("  Revenue and NACE (deterministic)...\n")
fold_k_det <- assign_folds(panel, "sector", K_sec, BASE_SEED + 1L)

yhat_rev_A <- calibrate_sector(panel, panel$revenue, fold_k_det)
mA_rev <- calc_metrics(panel$y[idx_A], yhat_rev_A[idx_A], fp_threshold = 0,
                       nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
mA_rev_vec <- extract_metrics(mA_rev)

yhat_nace_A <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_det)
mA_nace <- calc_metrics(panel$y[idx_A], yhat_nace_A[idx_A], fp_threshold = 0,
                        nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
mA_nace_vec <- extract_metrics(mA_nace)

# Fill deterministic rows (same value every repeat, sd = 0)
for (r in seq_len(M)) {
  metrics_A[r, , "Revenue"] <- mA_rev_vec
  metrics_A[r, , "NACE"]    <- mA_nace_vec
}
cat("  Done.\n")

# EN, Gated Rev, Combined: loop over M repeats
cat(sprintf("  EN, Gated Rev, Combined (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])

  # EN
  yhat_en <- calibrate_sector(panel, en_levels, fold_k)
  m_en <- calc_metrics(panel$y[idx_A], yhat_en[idx_A], fp_threshold = 0,
                       nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Elastic Net"] <- extract_metrics(m_en)

  # Gated Revenue
  gated_score <- as.numeric(en_levels > 0) * panel$revenue
  yhat_gated <- calibrate_sector(panel, gated_score, fold_k)
  m_gated <- calc_metrics(panel$y[idx_A], yhat_gated[idx_A], fp_threshold = 0,
                          nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Gated Rev"] <- extract_metrics(m_gated)

  # Combined: tune alpha per fold, then calibrate
  yhat_combined_score <- rep(NA_real_, nrow(panel))
  for (k in 1:K_sec) {
    train_idx <- which(fold_k != k)
    held_idx  <- which(fold_k == k)

    alpha_k <- tune_alpha(
      train_panel    = panel[train_idx, ],
      en_proxy_train = en_levels[train_idx],
      revenue_train  = panel$revenue[train_idx],
      alpha_grid     = ALPHA_GRID,
      K_inner        = K_INNER,
      seed_inner     = BASE_SEED + r * 100L + k
    )
    alphas_A[r, k] <- alpha_k
    yhat_combined_score[held_idx] <- (en_levels[held_idx] ^ alpha_k) *
                                      (panel$revenue[held_idx] ^ (1 - alpha_k))
  }
  yhat_combined <- calibrate_sector(panel, yhat_combined_score, fold_k)
  m_combined <- calc_metrics(panel$y[idx_A], yhat_combined[idx_A], fp_threshold = 0,
                             nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Combined"] <- extract_metrics(m_combined)

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

# =============================================================================
# Panel B: Firm-level CV design
# =============================================================================
cat("\n── Panel B: Firm-level CV design ──\n")

metrics_B <- array(NA_real_,
                   dim = c(M, length(metric_names), length(models)),
                   dimnames = list(NULL, metric_names, models))
alphas_B <- matrix(NA_real_, nrow = M, ncol = K_firm)

cat(sprintf("  Computing all models (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "firm", K_firm, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_firm[, r])

  # Revenue
  yhat_rev <- calibrate_sector(panel, panel$revenue, fold_k)
  m_rev <- calc_metrics(panel$y[idx_eligible], yhat_rev[idx_eligible], fp_threshold = 0,
                        nace2d = panel$nace2d[idx_eligible], year = panel$year[idx_eligible])
  metrics_B[r, , "Revenue"] <- extract_metrics(m_rev)

  # EN
  yhat_en <- calibrate_sector(panel, en_levels, fold_k)
  m_en <- calc_metrics(panel$y[idx_eligible], yhat_en[idx_eligible], fp_threshold = 0,
                       nace2d = panel$nace2d[idx_eligible], year = panel$year[idx_eligible])
  metrics_B[r, , "Elastic Net"] <- extract_metrics(m_en)

  # NACE
  yhat_nace <- calibrate_sector(panel, panel$proxy_tabachova, fold_k)
  m_nace <- calc_metrics(panel$y[idx_eligible], yhat_nace[idx_eligible], fp_threshold = 0,
                         nace2d = panel$nace2d[idx_eligible], year = panel$year[idx_eligible])
  metrics_B[r, , "NACE"] <- extract_metrics(m_nace)

  # Gated Revenue
  gated_score <- as.numeric(en_levels > 0) * panel$revenue
  yhat_gated <- calibrate_sector(panel, gated_score, fold_k)
  m_gated <- calc_metrics(panel$y[idx_eligible], yhat_gated[idx_eligible], fp_threshold = 0,
                          nace2d = panel$nace2d[idx_eligible], year = panel$year[idx_eligible])
  metrics_B[r, , "Gated Rev"] <- extract_metrics(m_gated)

  # Combined
  yhat_combined_score <- rep(NA_real_, nrow(panel))
  for (k in 1:K_firm) {
    train_idx <- which(fold_k != k)
    held_idx  <- which(fold_k == k)

    alpha_k <- tune_alpha(
      train_panel    = panel[train_idx, ],
      en_proxy_train = en_levels[train_idx],
      revenue_train  = panel$revenue[train_idx],
      alpha_grid     = ALPHA_GRID,
      K_inner        = K_INNER,
      seed_inner     = BASE_SEED + r * 100L + k
    )
    alphas_B[r, k] <- alpha_k
    yhat_combined_score[held_idx] <- (en_levels[held_idx] ^ alpha_k) *
                                      (panel$revenue[held_idx] ^ (1 - alpha_k))
  }
  yhat_combined <- calibrate_sector(panel, yhat_combined_score, fold_k)
  m_combined <- calc_metrics(panel$y[idx_eligible], yhat_combined[idx_eligible], fp_threshold = 0,
                             nace2d = panel$nace2d[idx_eligible], year = panel$year[idx_eligible])
  metrics_B[r, , "Combined"] <- extract_metrics(m_combined)

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

# =============================================================================
# Assemble results
# =============================================================================
# Mean and sd across repeats
mean_A <- apply(metrics_A, c(2, 3), mean, na.rm = TRUE)
sd_A   <- apply(metrics_A, c(2, 3), sd,   na.rm = TRUE)
mean_B <- apply(metrics_B, c(2, 3), mean, na.rm = TRUE)
sd_B   <- apply(metrics_B, c(2, 3), sd,   na.rm = TRUE)

rmse_baseline_A <- mean_A["rmse", "Revenue"]
rmse_baseline_B <- mean_B["rmse", "Revenue"]

# ── Display helpers ──────────────────────────────────────────────────────────
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
cat("MAIN RESULTS TABLE (with Gated Rev and Combined)\n")
cat("======================================================================\n\n")

cat("Panel A: Sector-level CV design\n")
cat(hdr); cat(sep, "\n")
print_row("Revenue",     display_vals(mean_A[, "Revenue"],     sd_A[, "Revenue"],     rmse_baseline_A, deterministic = TRUE))
print_row("Elastic Net", display_vals(mean_A[, "Elastic Net"], sd_A[, "Elastic Net"], rmse_baseline_A))
print_row("NACE",        display_vals(mean_A[, "NACE"],        sd_A[, "NACE"],        rmse_baseline_A, deterministic = TRUE))
print_row("Gated Rev",   display_vals(mean_A[, "Gated Rev"],   sd_A[, "Gated Rev"],   rmse_baseline_A))
print_row("Combined",    display_vals(mean_A[, "Combined"],    sd_A[, "Combined"],    rmse_baseline_A))
cat(sprintf("  Revenue RMSE baseline: %.1f kt\n\n", rmse_baseline_A / 1e3))

cat("Panel B: Firm-level CV design\n")
cat(hdr); cat(sep, "\n")
print_row("Revenue",     display_vals(mean_B[, "Revenue"],     sd_B[, "Revenue"],     rmse_baseline_B))
print_row("Elastic Net", display_vals(mean_B[, "Elastic Net"], sd_B[, "Elastic Net"], rmse_baseline_B))
print_row("NACE",        display_vals(mean_B[, "NACE"],        sd_B[, "NACE"],        rmse_baseline_B))
print_row("Gated Rev",   display_vals(mean_B[, "Gated Rev"],   sd_B[, "Gated Rev"],   rmse_baseline_B))
print_row("Combined",    display_vals(mean_B[, "Combined"],    sd_B[, "Combined"],    rmse_baseline_B))
cat(sprintf("  Revenue RMSE baseline: %.1f kt\n\n", rmse_baseline_B / 1e3))

# Alpha distribution
cat("── Alpha distribution ──\n")
cat(sprintf("  Panel A: median = %.2f, IQR = [%.2f, %.2f], range = [%.2f, %.2f]\n",
            median(alphas_A), quantile(alphas_A, 0.25), quantile(alphas_A, 0.75),
            min(alphas_A), max(alphas_A)))
cat(sprintf("  Panel B: median = %.2f, IQR = [%.2f, %.2f], range = [%.2f, %.2f]\n",
            median(alphas_B), quantile(alphas_B, 0.25), quantile(alphas_B, 0.75),
            min(alphas_B), max(alphas_B)))

# ── Generate LaTeX table ─────────────────────────────────────────────────────
fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt1 <- function(x)             formatC(x, format = "f", digits = 1)

tex_row <- function(lbl, dv) {
  mean_line <- sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                       lbl,
                       fmt1(dv$rmse[1]), fmt(dv$nrmse[1]), fmt(dv$mapd[1]),
                       fmt(dv$pearson[1]), fmt(dv$spear[1]),
                       fmt(dv$fpr[1]), fmt(dv$tpr[1]), fmt(dv$p50[1]), fmt(dv$p99[1]))
  if (dv$deterministic) {
    sd_line <- " & & & & & & & & & \\\\"
  } else {
    sd_line <- sprintf(" & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} \\\\",
                       fmt1(dv$rmse[2]),
                       fmt(dv$nrmse[2]), fmt(dv$mapd[2]),
                       fmt(dv$pearson[2]), fmt(dv$spear[2]),
                       fmt(dv$fpr[2]), fmt(dv$tpr[2]), fmt(dv$p50[2]), fmt(dv$p99[2]))
  }
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
  tex_row("Revenue",              display_vals(mean_A[, "Revenue"],     sd_A[, "Revenue"],     rmse_baseline_A, deterministic = TRUE)),
  tex_row("Elastic Net",          display_vals(mean_A[, "Elastic Net"], sd_A[, "Elastic Net"], rmse_baseline_A)),
  tex_row("NACE",                 display_vals(mean_A[, "NACE"],        sd_A[, "NACE"],        rmse_baseline_A, deterministic = TRUE)),
  tex_row("Gated Revenue",        display_vals(mean_A[, "Gated Rev"],   sd_A[, "Gated Rev"],   rmse_baseline_A)),
  tex_row("Combined",             display_vals(mean_A[, "Combined"],    sd_A[, "Combined"],    rmse_baseline_A)),
  "\\addlinespace[4pt]",
  "\\multicolumn{10}{l}{\\textit{Panel B: Firm-level CV design}} \\\\",
  "\\addlinespace[2pt]",
  tex_row("Revenue",              display_vals(mean_B[, "Revenue"],     sd_B[, "Revenue"],     rmse_baseline_B)),
  tex_row("Elastic Net",          display_vals(mean_B[, "Elastic Net"], sd_B[, "Elastic Net"], rmse_baseline_B)),
  tex_row("NACE",                 display_vals(mean_B[, "NACE"],        sd_B[, "NACE"],        rmse_baseline_B)),
  tex_row("Gated Revenue",        display_vals(mean_B[, "Gated Rev"],   sd_B[, "Gated Rev"],   rmse_baseline_B)),
  tex_row("Combined",             display_vals(mean_B[, "Combined"],    sd_B[, "Combined"],    rmse_baseline_B)),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_path <- file.path(OUTPUT_DIR, "table_main_results_with_combined_models.tex")
writeLines(tex_lines, tex_path)
cat("LaTeX table written to:", tex_path, "\n")

# ── Save full results ────────────────────────────────────────────────────────
full_results <- list(
  metric_names = metric_names,
  models = models,
  metrics_A = metrics_A, metrics_B = metrics_B,
  mean_A = mean_A, sd_A = sd_A,
  mean_B = mean_B, sd_B = sd_B,
  rmse_baseline_A = rmse_baseline_A,
  rmse_baseline_B = rmse_baseline_B,
  alphas_A = alphas_A, alphas_B = alphas_B,
  M = M, K_sec = K_sec, K_firm = K_firm, BASE_SEED = BASE_SEED,
  K_INNER = K_INNER, ALPHA_GRID = ALPHA_GRID,
  MIN_FIRMS_FIRM_CV = MIN_FIRMS_FIRM_CV,
  eligible_sectors = eligible_sectors,
  firms_per_sector = firms_per_sector
)
rds_path <- file.path(OUTPUT_DIR, "table_main_results_with_combined_models.rds")
saveRDS(full_results, rds_path)
cat("Full results saved to:", rds_path, "\n")

cat("\nDone.\n")
