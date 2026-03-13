###############################################################################
# figures_tables/table_main_results.R
#
# PURPOSE
#   Generate the main results table (Table X) for Section 4.
#   Three rows, all using rank-and-calibrate with national-aggregate target:
#     1. EN, sector CV    — EN proxy built with sector-level K=5 CV
#     2. EN, firm CV      — EN proxy built with firm-level K=10 CV
#     3. NACE-based       — Tabachova (deterministic, NACE-based) proxy
#
#   Columns: nRMSE | Median APD | Rank corr. | FPR | TPR | FP sev. p50 | FP sev. p99
#   Column groups: "Prediction accuracy" (first 3) | "Extensive margin" (last 4)
#
#   Calibration is fold-aware national-aggregate:
#     For each held-out fold k, the national total to distribute is
#       E_nat_k = sum(all emissions) - sum(emissions used to train the EN in fold k)
#     This mimics deployment where we observe training-set emissions and must
#     allocate the remainder to held-out firms proportionally to their proxy rank.
#
# INPUT
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#     Contains: training_sample (with fold_specific_proxy_all_asinh,
#               proxy_tabachova_asinh, fold_k for sector-level folds)
#   {PROC_DATA}/firmfoldcv_proxy_asinh.RData
#     Contains: firmfoldcv_proxy_panel_asinh (with firmfoldcv_proxy_all_asinh,
#               fold_k for firm-level folds)
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

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading panel...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

cat("Loading firm-fold CV proxy...\n")
e_ff <- new.env()
load(file.path(PROC_DATA, "firmfoldcv_proxy_asinh.RData"), envir = e_ff)
ff_panel <- e_ff$firmfoldcv_proxy_panel_asinh
rm(e_ff)

# ── Helper: back-transform asinh proxy to levels ─────────────────────────────
proxy_to_levels <- function(proxy) pmax(sinh(proxy), 0)

# ── Helper: fold-aware national-aggregate calibration ────────────────────────
# For each fold k:
#   E_nat = sum of all emissions in the full sample
#   E_train_k = sum of emissions among firms in the training set for fold k
#   E_target_k = E_nat - E_train_k  (what's left to distribute to held-out firms)
#   Distribute E_target_k proportionally to proxy values among held-out firms,
#   separately per year.
calibrate_national <- function(df, proxy_col, fold_col) {
  # Total emissions by year (full sample)
  E_nat_by_year <- df %>%
    group_by(year) %>%
    summarise(E_nat = sum(y, na.rm = TRUE), .groups = "drop")

  folds <- sort(unique(df[[fold_col]]))
  result <- rep(NA_real_, nrow(df))

  for (k in folds) {
    held_out <- which(df[[fold_col]] == k)
    train_k  <- which(df[[fold_col]] != k)

    # Training-set emissions by year for fold k
    E_train_k <- df[train_k, ] %>%
      group_by(year) %>%
      summarise(E_train = sum(y, na.rm = TRUE), .groups = "drop")

    # Target for held-out firms by year
    targets <- E_nat_by_year %>%
      left_join(E_train_k, by = "year") %>%
      mutate(E_target = E_nat - coalesce(E_train, 0))

    # Distribute among held-out firms proportionally to proxy, per year
    for (yr in unique(df$year[held_out])) {
      idx <- held_out[df$year[held_out] == yr]
      raw <- df[[proxy_col]][idx]
      E_target_yr <- targets$E_target[targets$year == yr]

      if (length(E_target_yr) == 0 || E_target_yr <= 0) {
        result[idx] <- 0
        next
      }

      denom <- sum(raw, na.rm = TRUE)
      if (denom > 0) {
        result[idx] <- E_target_yr * (raw / denom)
      } else {
        # Fallback: equal split
        result[idx] <- E_target_yr / length(idx)
      }
    }
  }
  result
}

# ── Prepare sector-fold data ─────────────────────────────────────────────────
df_sec <- training_sample %>%
  mutate(
    proxy_raw = proxy_to_levels(fold_specific_proxy_all_asinh),
    tabachova_raw = proxy_to_levels(proxy_tabachova_asinh)
  )

# ── Prepare firm-fold data ───────────────────────────────────────────────────
# Merge firm-fold proxy into training_sample (use firm-fold's own fold_k)
df_firm <- training_sample %>%
  select(-fold_k) %>%
  left_join(
    ff_panel %>% select(vat, year, firmfoldcv_proxy_all_asinh, fold_k),
    by = c("vat", "year")
  ) %>%
  mutate(
    proxy_raw = proxy_to_levels(firmfoldcv_proxy_all_asinh)
  )

# ── Row 1: EN, sector CV ─────────────────────────────────────────────────────
cat("Computing Row 1: EN, sector CV...\n")
df_sec$yhat_cal <- calibrate_national(df_sec, "proxy_raw", "fold_k")
m1 <- calc_metrics(df_sec$y, df_sec$yhat_cal, fp_threshold = 0,
                    nace2d = df_sec$nace2d, year = df_sec$year)

# ── Row 2: EN, firm CV ───────────────────────────────────────────────────────
cat("Computing Row 2: EN, firm CV...\n")
df_firm$yhat_cal <- calibrate_national(df_firm, "proxy_raw", "fold_k")
m2 <- calc_metrics(df_firm$y, df_firm$yhat_cal, fp_threshold = 0,
                    nace2d = df_firm$nace2d, year = df_firm$year)

# ── Row 3: NACE-based ────────────────────────────────────────────────────────
# Tabachova is deterministic (no CV), so we calibrate using sector-fold structure
# as a conservative choice. The proxy itself doesn't depend on the fold.
cat("Computing Row 3: NACE-based...\n")
df_sec$tab_cal <- calibrate_national(df_sec, "tabachova_raw", "fold_k")
m3 <- calc_metrics(df_sec$y, df_sec$tab_cal, fp_threshold = 0,
                    nace2d = df_sec$nace2d, year = df_sec$year)

# ── Assemble results ─────────────────────────────────────────────────────────
rows <- list(m1, m2, m3)
row_labels <- c(
  "EN, sector CV",
  "EN, firm CV",
  "NACE-based"
)

results <- data.frame(
  row_label      = row_labels,
  nrmse_sd       = sapply(rows, `[[`, "nrmse_sd"),
  median_apd     = sapply(rows, `[[`, "median_apd"),
  rho_pooled_global = sapply(rows, `[[`, "rho_pooled_global"),
  fpr            = sapply(rows, `[[`, "fpr_nonemitters"),
  tpr            = sapply(rows, `[[`, "tpr_emitters"),
  fp_sev_p50     = sapply(rows, `[[`, "avg_nonemit_p50_rank"),
  fp_sev_p99     = sapply(rows, `[[`, "avg_nonemit_p99_rank"),
  stringsAsFactors = FALSE
)

# ── Print to console ─────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("MAIN RESULTS TABLE\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

cat(sprintf("%-20s %8s %10s %8s %8s %8s %8s %8s\n",
            "", "nRMSE", "Med.APD", "Rho", "FPR", "TPR", "FPsev50", "FPsev99"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in seq_len(nrow(results))) {
  cat(sprintf("%-20s %8.3f %10.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
              results$row_label[i],
              results$nrmse_sd[i],
              results$median_apd[i],
              results$rho_pooled_global[i],
              results$fpr[i],
              results$tpr[i],
              results$fp_sev_p50[i],
              results$fp_sev_p99[i]))
}
cat("\n")

# ── Generate LaTeX table ─────────────────────────────────────────────────────
fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "---", formatC(x, format = "f", digits = digits))
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

for (i in seq_len(nrow(results))) {
  tex_lines <- c(tex_lines, sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
    results$row_label[i],
    fmt(results$nrmse_sd[i]),
    fmt(results$median_apd[i]),
    fmt(results$rho_pooled_global[i]),
    fmt(results$fpr[i]),
    fmt(results$tpr[i]),
    fmt(results$fp_sev_p50[i]),
    fmt(results$fp_sev_p99[i])
  ))
}

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
  results_df = results,
  metrics = setNames(rows, row_labels),
  row_labels = row_labels
)
rds_path <- file.path(OUTPUT_DIR, "table_main_results.rds")
saveRDS(full_results, rds_path)
cat("Full results saved to:", rds_path, "\n")
