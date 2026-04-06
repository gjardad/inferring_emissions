###############################################################################
# figures_tables/table_main_mixed_cvthresh_crf.R
#
# PURPOSE
#   Panel B (mixed sectors) with CV-tuned percentile threshold on EN proxy.
#   CRF-group-level CV, proportional allocation.
#
#   For each of M repeats:
#     For each held-out mixed sector h in {paper, metals, refining}:
#       1. For each training sector s (the other two):
#            Find percentile p*_s among proxy > 0 that maximizes Youden's J
#       2. Average: p* = mean(p*_s1, p*_s2)
#       3. Apply p* to held-out sector h: within each (CRF group, year) cell,
#            zero out firms below the p*-th percentile of proxy > 0
#       4. Calibrate (proportional) on held-out sector h
#       5. Compute metrics on held-out sector h
#     Pool held-out predictions from all three sectors and compute pooled metrics.
#
#   Reports: EN (no thresh) vs EN (CV thresh) vs Revenue vs NACE,
#   all with proportional allocation, pooled across mixed sectors.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_crf_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_mixed_cvthresh_crf.tex
#   {OUTPUT_DIR}/table_main_mixed_cvthresh_crf.rds
#   Console output
#
# RUNS ON: local 1
###############################################################################

REPO_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
  error = function(e) normalizePath(getwd(), winslash = "/")
)
while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
source(file.path(UTILS_DIR, "calc_metrics.R"))

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED <- 2026L
K_crf     <- 5L
M         <- 20L
N_THRESH  <- 200L

MIXED_GROUPS <- list(
  paper    = c("17", "18", "17/18"),
  metals   = c("24", "25"),
  refining = c("19")
)
ALL_MIXED_CODES <- unlist(MIXED_GROUPS)

cat("================================================================\n")
cat("  MIXED SECTORS + CV THRESHOLD — CRF-GROUP CV\n")
cat("  M =", M, "repeats\n")
cat("================================================================\n\n")

# =============================================================================
# CRF GROUP MAP
# =============================================================================
crf_group_map <- data.frame(
  nace2d = c(
    "35", "19", "24", "25", "20", "21",
    "17", "18", "17/18",
    "10", "11", "12", "23",
    "05", "06", "07", "08", "09",
    "13", "14", "15", "16", "22",
    "26", "27", "28", "29", "30", "31", "32", "33",
    "41", "42", "43",
    "36", "37", "38", "39",
    "45", "46", "47", "52", "53",
    "55", "56", "58", "59", "60", "61", "62", "63",
    "64", "65", "66",
    "68", "69", "70", "71", "72", "73", "74", "75",
    "77", "78", "79", "80", "81", "82",
    "84", "85", "86", "87", "88",
    "90", "91", "92", "93", "94", "95", "96",
    "01", "02", "03", "49", "50", "51"
  ),
  crf_group = c(
    "energy", "refining", "metals", "metals", "chemicals", "chemicals",
    "paper", "paper", "paper",
    "food", "food", "food", "minerals",
    rep("mfg_other", 5), rep("mfg_other", 5), rep("mfg_other", 8), rep("mfg_other", 3),
    rep("commercial", 4), rep("commercial", 5), rep("commercial", 8),
    rep("commercial", 3), rep("commercial", 8), rep("commercial", 6),
    rep("commercial", 5), rep("commercial", 7),
    "agriculture", "agriculture", "agriculture",
    "transport", "transport", "transport"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading data...\n")
e_crf <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData"), envir = e_crf)
proxy_matrix_crf <- e_crf$proxy_matrix[, 1:M, drop = FALSE]
panel            <- e_crf$repeated_cv_proxy_panel
rm(e_crf)

panel$primary_nace2d[panel$primary_nace2d %in% c("17", "18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17", "18")] <- "17/18"

if (!"primary_crf_group" %in% names(panel)) {
  panel <- panel %>%
    left_join(crf_group_map %>% distinct(nace2d, .keep_all = TRUE),
              by = c("primary_nace2d" = "nace2d")) %>%
    rename(primary_crf_group = crf_group)
}

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
panel <- panel %>% left_join(
  training_sample %>% select(vat, year, revenue, proxy_tabachova),
  by = c("vat", "year")
)
rm(training_sample, syt)

N <- nrow(panel)
idx_mixed <- which(panel$nace2d %in% ALL_MIXED_CODES)
cat(sprintf("  Panel: %d firm-years, mixed: %d\n\n", N, length(idx_mixed)))

# =============================================================================
# HELPERS
# =============================================================================
proxy_to_levels <- function(proxy) pmax(sinh(proxy), 0)

# Proportional calibration within a single CRF group
calibrate_prop_group <- function(panel, ranking_signal, target_group) {
  idx_all <- which(panel$primary_crf_group == target_group)
  result <- rep(NA_real_, length(idx_all))
  sy_key <- paste(panel$primary_crf_group[idx_all], panel$year[idx_all])
  E_sy <- tapply(panel$y[idx_all], sy_key, sum, na.rm = TRUE)

  for (sy in unique(sy_key)) {
    idx_in <- which(sy_key == sy)
    E_target <- E_sy[sy]
    if (is.na(E_target) || E_target <= 0) { result[idx_in] <- 0; next }
    raw <- ranking_signal[idx_all[idx_in]]
    denom <- sum(raw, na.rm = TRUE)
    if (denom > 0) result[idx_in] <- E_target * (raw / denom)
    else result[idx_in] <- E_target / length(idx_in)
  }
  list(idx = idx_all, yhat = result)
}

# Youden-optimal percentile within one CRF group
youden_percentile <- function(proxy_raw, y, idx_group) {
  pr <- proxy_raw[idx_group]
  yt <- y[idx_group]
  is_emit <- (yt > 0)

  if (sum(is_emit) < 3 || sum(!is_emit) < 3) return(0)

  pos_vals <- pr[pr > 0]
  if (length(pos_vals) < 5) return(0)

  pct_grid <- seq(0, 0.95, length.out = N_THRESH)
  best_j <- -Inf
  best_pct <- 0

  for (pct in pct_grid) {
    tau <- quantile(pos_vals, probs = pct, names = FALSE)
    pred_pos <- (pr > tau)
    tp <- sum(pred_pos & is_emit)
    fp <- sum(pred_pos & !is_emit)
    fn <- sum(!pred_pos & is_emit)
    tn <- sum(!pred_pos & !is_emit)
    tpr <- tp / (tp + fn)
    fpr <- fp / (fp + tn)
    j <- tpr - fpr
    if (j > best_j) {
      best_j <- j
      best_pct <- pct
    }
  }
  best_pct
}

# Apply percentile threshold within each (group, year) cell
apply_pct_threshold <- function(proxy_raw, pct, idx_group, year) {
  result <- proxy_raw[idx_group]
  if (pct <= 0) return(result)

  yr <- year[idx_group]
  for (y_val in unique(yr)) {
    in_yr <- which(yr == y_val)
    pos_mask <- result[in_yr] > 0
    if (sum(pos_mask) < 2) next
    pos_vals <- result[in_yr[pos_mask]]
    tau <- quantile(pos_vals, probs = pct, names = FALSE)
    below <- in_yr[pos_mask][pos_vals <= tau]
    result[below] <- 0
  }
  result
}

# =============================================================================
# MAIN LOOP
# =============================================================================
metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")
extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

models <- c("Revenue", "EN", "EN (CV thresh)", "NACE")

metrics_mixed <- array(NA_real_,
  dim = c(M, length(metric_names), length(models)),
  dimnames = list(NULL, metric_names, models))

thresh_log <- matrix(NA_real_, M, 3,
  dimnames = list(NULL, names(MIXED_GROUPS)))

cat(sprintf("Running %d repeats...\n", M))

for (r in seq_len(M)) {
  en_levels <- proxy_to_levels(proxy_matrix_crf[, r])

  # Build predictions for each mixed group, then pool
  yhat_rev  <- rep(NA_real_, N)
  yhat_en   <- rep(NA_real_, N)
  yhat_enth <- rep(NA_real_, N)
  yhat_nace <- rep(NA_real_, N)

  for (g in names(MIXED_GROUPS)) {
    codes <- MIXED_GROUPS[[g]]
    idx_g <- which(panel$nace2d %in% codes)

    # Revenue
    res <- calibrate_prop_group(panel, panel$revenue, g)
    yhat_rev[res$idx] <- res$yhat

    # EN (no threshold)
    res <- calibrate_prop_group(panel, en_levels, g)
    yhat_en[res$idx] <- res$yhat

    # NACE
    res <- calibrate_prop_group(panel, panel$proxy_tabachova, g)
    yhat_nace[res$idx] <- res$yhat

    # EN (CV threshold): LOSO on mixed groups
    train_groups <- setdiff(names(MIXED_GROUPS), g)
    sector_pcts <- sapply(train_groups, function(tg) {
      idx_tg <- which(panel$primary_crf_group == tg)
      youden_percentile(en_levels, panel$y, idx_tg)
    })
    avg_pct <- mean(sector_pcts)
    thresh_log[r, g] <- avg_pct

    # Apply threshold and calibrate
    en_thresh <- en_levels
    en_thresh[idx_g] <- apply_pct_threshold(en_levels, avg_pct, idx_g, panel$year)
    res <- calibrate_prop_group(panel, en_thresh, g)
    yhat_enth[res$idx] <- res$yhat
  }

  # Pooled metrics across mixed sectors
  for (mod_name in models) {
    yhat <- switch(mod_name,
      "Revenue"        = yhat_rev,
      "EN"             = yhat_en,
      "EN (CV thresh)" = yhat_enth,
      "NACE"           = yhat_nace
    )
    m <- calc_metrics(panel$y[idx_mixed], yhat[idx_mixed], fp_threshold = 0,
                      nace2d = panel$nace2d[idx_mixed], year = panel$year[idx_mixed])
    metrics_mixed[r, , mod_name] <- extract_metrics(m)
  }

  if (r %% 5 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("Done.\n\n")

mean_m <- apply(metrics_mixed, c(2, 3), mean, na.rm = TRUE)
sd_m   <- apply(metrics_mixed, c(2, 3), sd,   na.rm = TRUE)
rmse_bl <- mean_m["rmse", "Revenue"]

# Threshold diagnostics
cat("CV threshold percentiles (avg across repeats):\n")
for (g in names(MIXED_GROUPS)) {
  cat(sprintf("  %s held out: avg pct = %.3f (sd %.3f)\n",
              g, mean(thresh_log[, g], na.rm = TRUE), sd(thresh_log[, g], na.rm = TRUE)))
}

# =============================================================================
# CONSOLE OUTPUT
# =============================================================================
cat("\n-- Mixed sectors (pooled, proportional allocation) --\n")
cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
            "", "RMSE(kt)", "nRMSE", "MAPD", "Pearson", "Spearman",
            "FPR", "TPR", "p50", "p99"))
cat(paste(rep("-", 110), collapse = ""), "\n")
for (mod in models) {
  mn <- mean_m[, mod]; s <- sd_m[, mod]
  cat(sprintf("%-20s %10.1f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f\n",
              mod, mn["rmse"]/1e3, mn["rmse"]/rmse_bl, mn["median_apd"],
              mn["pearson"], mn["spearman"],
              mn["fpr_nonemitters"], mn["tpr_emitters"],
              mn["avg_nonemit_p50_rank"], mn["avg_nonemit_p99_rank"]))
  cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
              "",
              sprintf("(%.1f)", s["rmse"]/1e3),
              sprintf("(%.3f)", s["rmse"]/rmse_bl),
              sprintf("(%.3f)", s["median_apd"]),
              sprintf("(%.3f)", s["pearson"]),
              sprintf("(%.3f)", s["spearman"]),
              sprintf("(%.3f)", s["fpr_nonemitters"]),
              sprintf("(%.3f)", s["tpr_emitters"]),
              sprintf("(%.3f)", s["avg_nonemit_p50_rank"]),
              sprintf("(%.3f)", s["avg_nonemit_p99_rank"])))
}

# =============================================================================
# LATEX TABLE
# =============================================================================
fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt1 <- function(x)             formatC(x, format = "f", digits = 1)

tex_row <- function(lbl, mn, s, bl) {
  mean_line <- sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
    lbl, fmt1(mn["rmse"]/1e3), fmt(mn["rmse"]/bl), fmt(mn["median_apd"]),
    fmt(mn["pearson"]), fmt(mn["spearman"]),
    fmt(mn["fpr_nonemitters"]), fmt(mn["tpr_emitters"]),
    fmt(mn["avg_nonemit_p50_rank"]), fmt(mn["avg_nonemit_p99_rank"]))
  sd_line <- sprintf(" & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} \\\\",
    fmt1(s["rmse"]/1e3), fmt(s["rmse"]/bl), fmt(s["median_apd"]),
    fmt(s["pearson"]), fmt(s["spearman"]),
    fmt(s["fpr_nonemitters"]), fmt(s["tpr_emitters"]),
    fmt(s["avg_nonemit_p50_rank"]), fmt(s["avg_nonemit_p99_rank"]))
  c(mean_line, sd_line)
}

tex <- c(
  "\\begin{tabular}{l ccc cc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction error} & \\multicolumn{2}{c}{Correlation} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-10}",
  " & RMSE & nRMSE & MAPD & Levels & Rank & FPR & TPR & p50 & p99 \\\\",
  "\\midrule",
  tex_row("Revenue",        mean_m[, "Revenue"],        sd_m[, "Revenue"],        rmse_bl),
  tex_row("Elastic Net",    mean_m[, "EN"],             sd_m[, "EN"],             rmse_bl),
  tex_row("EN (CV thresh)", mean_m[, "EN (CV thresh)"], sd_m[, "EN (CV thresh)"], rmse_bl),
  tex_row("NACE",           mean_m[, "NACE"],           sd_m[, "NACE"],           rmse_bl),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_path <- file.path(OUTPUT_DIR, "table_main_mixed_cvthresh_crf.tex")
writeLines(tex, tex_path)
cat("\nLaTeX table written to:", tex_path, "\n")

# Save
rds_path <- file.path(OUTPUT_DIR, "table_main_mixed_cvthresh_crf.rds")
saveRDS(list(
  metrics_mixed = metrics_mixed, mean_m = mean_m, sd_m = sd_m,
  rmse_bl = rmse_bl, models = models, thresh_log = thresh_log, M = M
), rds_path)
cat("Results saved to:", rds_path, "\n")
