###############################################################################
# figures_tables/table_pareto_all_models_cvthresh.R
#
# PURPOSE
#   Same as table_pareto_all_models.R but with cross-validated percentile
#   thresholds for the three mixed sectors (17/18, 19, 24).
#
#   For each held-out mixed sector h:
#     1. For each of the two other mixed sectors s, find the percentile p*_s
#        of proxy > 0 values that maximizes within-sector Youden's J = TPR - FPR
#     2. Average: p*_h = mean(p*_s1, p*_s2)
#     3. Within each (sector, year) cell of sector h, zero out firms below
#        the p*_h-th percentile of proxy > 0 values
#   For all other sectors: threshold = proxy > 0 (no change).
#
#   The threshold applies to every ranking signal (Revenue, EN, NACE, Gated Rev)
#   since it's about the extensive margin, not the ranking.
#
#   Wait — the threshold should be model-specific. The Youden-optimal percentile
#   depends on which ranking signal we're thresholding. A firm with revenue > 0
#   but EN proxy = 0 should be zeroed out under EN but not under Revenue.
#   The threshold is tuned per model.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_results_sector_cv_pareto_cvthresh.tex
#   {OUTPUT_DIR}/table_pareto_all_models_cvthresh.rds
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
library(lmom)
source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration_helpers.R"))

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED           <- 2026L
K_sec               <- 5L
M                   <- 20L
MIN_FIRMS_SECTOR_CV <- 3L
N_THRESH            <- 200L
MIXED_SECTORS       <- c("17/18", "19", "24")
SECTOR_GROUPS       <- list("17/18" = "17/18", "19" = "19", "24" = "24")

cat("================================================================\n")
cat("  PARETO + CV THRESHOLD — ALL MODELS\n")
cat("  M =", M, "repeats, K =", K_sec, "folds\n")
cat("================================================================\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading data...\n")
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix[, 1:M, drop = FALSE]
panel            <- e_sec$repeated_cv_proxy_panel
rm(e_sec)

panel$primary_nace2d[panel$primary_nace2d %in% c("17", "18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17", "18")] <- "17/18"

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
panel <- panel %>% left_join(
  training_sample %>% select(vat, year, revenue, proxy_tabachova),
  by = c("vat", "year")
)
rm(training_sample, syt)

N <- nrow(panel)
firms_per_sector <- panel %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")
sectors_A <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_SECTOR_CV) %>%
  pull(primary_nace2d)
idx_A <- which(panel$primary_nace2d %in% sectors_A)

cat(sprintf("  Panel: %d firm-years, %d in eval set\n", N, length(idx_A)))

# =============================================================================
# PARETO CALIBRATION
# =============================================================================
build_reference_dist <- function(panel, fold_k, k) {
  train_idx <- which(fold_k != k & panel$y > 0)
  if (length(train_idx) < 20) return(NULL)
  df <- data.frame(
    log_y  = log(panel$y[train_idx]),
    year   = panel$year[train_idx],
    nace2d = panel$nace2d[train_idx],
    stringsAsFactors = FALSE
  )
  mu_t <- tapply(df$log_y, df$year, mean)
  df$tilde <- df$log_y - mu_t[as.character(df$year)]
  mu_s <- tapply(df$tilde, df$nace2d, mean)
  df$d <- df$tilde - mu_s[df$nace2d]
  sort(df$d)
}

calibrate_pareto <- function(panel, ranking_signal, fold_k) {
  result <- rep(NA_real_, nrow(panel))
  sy_key <- paste(panel$nace2d, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)
  folds <- sort(unique(fold_k))

  for (k in folds) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)

    ref_k <- build_reference_dist(panel, fold_k, k)
    gpa_params <- if (!is.null(ref_k) && length(ref_k) >= 20) {
      tryCatch(pelgpa(samlmu(ref_k, nmom = 3)), error = function(e) NULL)
    } else NULL

    sy_train <- paste(panel$nace2d[train_idx], panel$year[train_idx])
    E_train_sy <- tapply(panel$y[train_idx], sy_train, sum, na.rm = TRUE)

    ho_sy <- sy_key[held_out]

    for (sy in unique(ho_sy)) {
      idx_in_ho <- which(ho_sy == sy)
      idx <- held_out[idx_in_ho]

      E_total <- E_sy[sy]
      E_train <- ifelse(is.na(E_train_sy[sy]), 0, E_train_sy[sy])
      E_target <- E_total - E_train

      if (is.na(E_target) || E_target <= 0) {
        result[idx] <- 0
        next
      }

      emitter_mask <- ranking_signal[idx] > 0
      n_emit <- sum(emitter_mask)

      if (n_emit < 2 || is.null(gpa_params)) {
        raw <- ranking_signal[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) result[idx] <- E_target * (raw / denom)
        else result[idx] <- E_target / length(idx)
        next
      }

      result[idx[!emitter_mask]] <- 0

      emit_idx <- idx[emitter_mask]
      ranks <- rank(ranking_signal[emit_idx], ties.method = "average")
      p_i <- (ranks - 0.5) / n_emit

      w_i <- tryCatch(quagpa(p_i, gpa_params), error = function(e) rep(NA, n_emit))
      if (any(is.na(w_i)) || any(!is.finite(w_i))) {
        raw <- ranking_signal[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) result[idx] <- E_target * (raw / denom)
        else result[idx] <- E_target / length(idx)
        next
      }

      exp_w <- exp(w_i - max(w_i))
      result[emit_idx] <- E_target * exp_w / sum(exp_w)
    }
  }
  result
}

# =============================================================================
# THRESHOLD FUNCTIONS
# =============================================================================

# Youden-optimal percentile for one sector, given a ranking signal
youden_pct_sector <- function(signal, y, nace2d, sector) {
  idx <- which(nace2d == sector)
  pr <- signal[idx]; yt <- y[idx]
  is_emit <- (yt > 0)
  if (sum(is_emit) < 3 || sum(!is_emit) < 3) return(0)

  pos_vals <- pr[pr > 0]
  if (length(pos_vals) < 5) return(0)

  pct_grid <- seq(0, 0.95, length.out = N_THRESH)
  best_j <- -Inf; best_pct <- 0

  for (pct in pct_grid) {
    tau <- quantile(pos_vals, probs = pct, names = FALSE)
    pred_pos <- (pr > tau)
    tp <- sum(pred_pos & is_emit); fp <- sum(pred_pos & !is_emit)
    fn <- sum(!pred_pos & is_emit); tn <- sum(!pred_pos & !is_emit)
    j <- tp/(tp+fn) - fp/(fp+tn)
    if (j > best_j) { best_j <- j; best_pct <- pct }
  }

  best_pct
}

# For held-out sector h: tune on the other two, average, apply within cells
apply_cv_threshold <- function(signal, panel, held_sector) {
  train_sectors <- setdiff(MIXED_SECTORS, held_sector)

  # Per-sector Youden-optimal percentile
  pcts <- sapply(train_sectors, function(sec)
    youden_pct_sector(signal, panel$y, panel$nace2d, sec))
  avg_pct <- mean(pcts)

  if (avg_pct <= 0) return(signal)  # no threshold

  # Apply within each (sector, year) cell of the held-out sector
  result <- signal
  idx_ho <- which(panel$nace2d == held_sector)
  yr_ho <- panel$year[idx_ho]

  for (yr in unique(yr_ho)) {
    in_yr <- which(yr_ho == yr)
    cell_idx <- idx_ho[in_yr]
    pos_mask <- result[cell_idx] > 0
    if (sum(pos_mask) < 2) next
    pos_vals <- result[cell_idx[pos_mask]]
    tau <- quantile(pos_vals, probs = avg_pct, names = FALSE)
    below <- cell_idx[pos_mask][pos_vals <= tau]
    result[below] <- 0
  }

  result
}

# Apply CV thresholds to ALL mixed sectors (each using LOSO on other two)
apply_all_cv_thresholds <- function(signal, panel) {
  result <- signal
  for (sec in MIXED_SECTORS) {
    result <- apply_cv_threshold(result, panel, sec)
  }
  result
}

# =============================================================================
# TABLE 1: Main results — all models, Pareto + CV threshold
# =============================================================================
cat("\n── TABLE 1: Main results (Pareto + CV threshold) ──\n")

metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")
extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

models <- c("Revenue", "Elastic Net", "NACE", "Gated Rev")
metrics_A <- array(NA_real_,
                   dim = c(M, length(metric_names), length(models)),
                   dimnames = list(NULL, metric_names, models))

# Store threshold diagnostics
thresh_diag <- array(NA_real_,
  dim = c(M, length(MIXED_SECTORS), length(models)),
  dimnames = list(NULL, MIXED_SECTORS, models))

cat(sprintf("  All models (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])
  gated_score <- as.numeric(en_levels > 0) * panel$revenue

  signals <- list(
    "Revenue"     = panel$revenue,
    "Elastic Net" = en_levels,
    "NACE"        = panel$proxy_tabachova,
    "Gated Rev"   = gated_score
  )

  for (mod in models) {
    sig <- signals[[mod]]

    # Apply CV threshold to mixed sectors
    sig_thresh <- apply_all_cv_thresholds(sig, panel)

    # Store threshold percentiles for diagnostics (EN only)
    if (mod == "Elastic Net") {
      for (sec in MIXED_SECTORS) {
        train_secs <- setdiff(MIXED_SECTORS, sec)
        pcts <- sapply(train_secs, function(s)
          youden_pct_sector(sig, panel$y, panel$nace2d, s))
        thresh_diag[r, sec, mod] <- mean(pcts)
      }
    }

    # Calibrate with Pareto
    yhat <- calibrate_pareto(panel, sig_thresh, fold_k)
    m_out <- calc_metrics(panel$y[idx_A], yhat[idx_A], fp_threshold = 0,
                          nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
    metrics_A[r, , mod] <- extract_metrics(m_out)
  }

  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

mean_A <- apply(metrics_A, c(2, 3), mean, na.rm = TRUE)
sd_A   <- apply(metrics_A, c(2, 3), sd,   na.rm = TRUE)
rmse_baseline <- mean_A["rmse", "Revenue"]

# Console output
cat("\nPanel A: Sector-level CV, Pareto + CV threshold\n")
cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
            "", "RMSE(kt)", "nRMSE", "MAPD", "Pearson", "Spearman",
            "FPR", "TPR", "p50", "p99"))
cat(paste(rep("-", 110), collapse = ""), "\n")
for (mod in models) {
  mn <- mean_A[, mod]; s <- sd_A[, mod]
  cat(sprintf("%-20s %10.1f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f\n",
              mod, mn["rmse"]/1e3, mn["rmse"]/rmse_baseline, mn["median_apd"],
              mn["pearson"], mn["spearman"],
              mn["fpr_nonemitters"], mn["tpr_emitters"],
              mn["avg_nonemit_p50_rank"], mn["avg_nonemit_p99_rank"]))
  cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
              "",
              sprintf("(%.1f)", s["rmse"]/1e3),
              sprintf("(%.3f)", s["rmse"]/rmse_baseline),
              sprintf("(%.3f)", s["median_apd"]),
              sprintf("(%.3f)", s["pearson"]),
              sprintf("(%.3f)", s["spearman"]),
              sprintf("(%.3f)", s["fpr_nonemitters"]),
              sprintf("(%.3f)", s["tpr_emitters"]),
              sprintf("(%.3f)", s["avg_nonemit_p50_rank"]),
              sprintf("(%.3f)", s["avg_nonemit_p99_rank"])))
}

# Threshold diagnostics (EN only)
cat("\n── EN threshold diagnostics ──\n")
for (sec in MIXED_SECTORS) {
  mn <- mean(thresh_diag[, sec, "Elastic Net"], na.rm = TRUE)
  s  <- sd(thresh_diag[, sec, "Elastic Net"], na.rm = TRUE)
  cat(sprintf("  %s: avg CV percentile = %.3f (%.3f)\n", sec, mn, s))
}

# =============================================================================
# TABLE 2: Zero-emitter sectors — all models, Pareto + CV threshold
# =============================================================================
cat("\n── TABLE 2: Zero-emitter sectors (Pareto + CV threshold) ──\n")

# Helpers
mapd_emitters <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes & y > 0)
  if (length(idx) < 1) return(NA_real_)
  median(abs(y[idx] - yhat[idx]) / y[idx])
}
fp_severity_rank <- function(y, yhat, nace2d, year, codes, stat = "max") {
  idx <- which(nace2d %in% codes)
  y_s <- y[idx]; yhat_s <- yhat[idx]; year_s <- year[idx]
  is_emit <- (y_s > 0); is_nonemit <- !is_emit
  cell_vals <- numeric(0)
  for (yr in sort(unique(year_s))) {
    in_yr <- (year_s == yr)
    yr_emit <- (in_yr & is_emit); yr_nonemit <- (in_yr & is_nonemit)
    if (sum(yr_emit) < 3 || sum(yr_nonemit) < 1) next
    emitter_ecdf <- ecdf(y_s[yr_emit])
    ne_preds <- yhat_s[yr_nonemit]
    val <- if (stat == "max") max(ne_preds) else median(ne_preds)
    cell_vals <- c(cell_vals, emitter_ecdf(val))
  }
  if (length(cell_vals) > 0) mean(cell_vals) else NA_real_
}
spearman_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  y_s <- y[idx]; yhat_s <- yhat[idx]
  if (length(y_s) >= 3 && sd(y_s) > 0 && sd(yhat_s) > 0)
    suppressWarnings(cor(y_s, yhat_s, method = "spearman", use = "complete.obs"))
  else NA_real_
}
fpr_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  is_nonemit <- (y[idx] == 0)
  if (sum(is_nonemit) > 0) sum(yhat[idx][is_nonemit] > 0) / sum(is_nonemit) else NA_real_
}
tpr_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  is_emit <- (y[idx] > 0)
  if (sum(is_emit) > 0) sum(yhat[idx][is_emit] > 0) / sum(is_emit) else NA_real_
}
pearson_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  y_s <- y[idx]; yhat_s <- yhat[idx]
  if (length(y_s) >= 3 && sd(y_s) > 0 && sd(yhat_s) > 0)
    suppressWarnings(cor(y_s, yhat_s, method = "pearson", use = "complete.obs"))
  else NA_real_
}
rmse_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  sqrt(mean((y[idx] - yhat[idx])^2))
}

grp_names <- names(SECTOR_GROUPS)
t2_metric_names <- c("nrmse", "mapd", "pear", "spear", "fpr", "tpr", "fpp50", "fpmax")

rev_rmse_by_grp <- matrix(NA_real_, M, 3, dimnames = list(NULL, grp_names))
t2_results <- array(NA_real_,
  dim = c(M, length(t2_metric_names), 3, length(models)),
  dimnames = list(NULL, t2_metric_names, grp_names, models))

# Also store per-sector threshold details for summary table
thresh_detail <- array(NA_real_,
  dim = c(M, length(MIXED_SECTORS), 4),
  dimnames = list(NULL, MIXED_SECTORS,
                  c("avg_pct", "pct_train1", "pct_train2", "n_train")))

cat(sprintf("  All models (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])
  gated_score <- as.numeric(en_levels > 0) * panel$revenue

  signals <- list(
    "Revenue"     = panel$revenue,
    "Elastic Net" = en_levels,
    "NACE"        = panel$proxy_tabachova,
    "Gated Rev"   = gated_score
  )

  for (mod in models) {
    sig <- signals[[mod]]
    sig_thresh <- apply_all_cv_thresholds(sig, panel)

    # Store detailed threshold info for EN
    if (mod == "Elastic Net") {
      for (sec in MIXED_SECTORS) {
        train_secs <- setdiff(MIXED_SECTORS, sec)
        pcts <- sapply(train_secs, function(s)
          youden_pct_sector(sig, panel$y, panel$nace2d, s))
        thresh_detail[r, sec, "avg_pct"] <- mean(pcts)
        thresh_detail[r, sec, "pct_train1"] <- pcts[1]
        thresh_detail[r, sec, "pct_train2"] <- pcts[2]
        thresh_detail[r, sec, "n_train"] <- length(train_secs)
      }
    }

    yhat <- calibrate_pareto(panel, sig_thresh, fold_k)

    for (g in grp_names) {
      codes <- SECTOR_GROUPS[[g]]
      t2_results[r, "nrmse", g, mod] <- rmse_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "mapd",  g, mod] <- mapd_emitters(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "pear",  g, mod] <- pearson_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "spear", g, mod] <- spearman_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "fpr",   g, mod] <- fpr_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "tpr",   g, mod] <- tpr_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "fpp50", g, mod] <- fp_severity_rank(panel$y, yhat, panel$nace2d, panel$year, codes, "median")
      t2_results[r, "fpmax", g, mod] <- fp_severity_rank(panel$y, yhat, panel$nace2d, panel$year, codes, "max")
    }
  }

  for (g in grp_names) rev_rmse_by_grp[r, g] <- t2_results[r, "nrmse", g, "Revenue"]

  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

# Normalize nrmse
for (mod in models) {
  for (g in grp_names) {
    t2_results[, "nrmse", g, mod] <- t2_results[, "nrmse", g, mod] / rev_rmse_by_grp[, g]
  }
}

# Console output
cat("\n=== Zero-emitter sectors: all models, Pareto + CV threshold ===\n")
row_labels <- c("nRMSE", "MAPD", "Levels corr", "Rank corr", "FPR", "TPR", "Med FP rank", "Max FP rank")
for (i in seq_along(t2_metric_names)) {
  cat(sprintf("\n%-15s", row_labels[i]))
  for (g in grp_names) {
    cat(sprintf("\n  %s:", g))
    for (mod in models) {
      mn <- mean(t2_results[, t2_metric_names[i], g, mod], na.rm = TRUE)
      s  <- sd(t2_results[, t2_metric_names[i], g, mod], na.rm = TRUE)
      cat(sprintf("  %s %.3f(%.3f)", substr(mod, 1, 3), mn, s))
    }
  }
}
cat("\n")

# =============================================================================
# TABLE 3: Threshold summary
# =============================================================================
cat("\n── TABLE 3: CV threshold summary (EN proxy) ──\n\n")

SECTOR_LABELS_PLAIN <- c("17/18" = "Paper & printing",
                          "19" = "Petroleum refining",
                          "24" = "Iron & steel")
TRAIN_PAIRS <- list(
  "17/18" = c("19", "24"),
  "19"    = c("17/18", "24"),
  "24"    = c("17/18", "19")
)

cat(sprintf("%-22s %12s %12s %12s %8s %8s\n",
            "Held-out sector", "Train sec. 1", "Train sec. 2", "Avg p*", "OOS TPR", "OOS FPR"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (sec in MIXED_SECTORS) {
  tp <- TRAIN_PAIRS[[sec]]
  # Mean across repeats of each training sector's optimal percentile
  p1 <- mean(thresh_detail[, sec, "pct_train1"], na.rm = TRUE)
  p2 <- mean(thresh_detail[, sec, "pct_train2"], na.rm = TRUE)
  avg_p <- mean(thresh_detail[, sec, "avg_pct"], na.rm = TRUE)

  # OOS TPR and FPR from Table 2 results (EN model)
  oos_tpr <- mean(t2_results[, "tpr", sec, "Elastic Net"], na.rm = TRUE)
  oos_fpr <- mean(t2_results[, "fpr", sec, "Elastic Net"], na.rm = TRUE)

  cat(sprintf("%-22s %s: %.3f   %s: %.3f   %12.3f %8.3f %8.3f\n",
              SECTOR_LABELS_PLAIN[sec],
              tp[1], p1, tp[2], p2, avg_p, oos_tpr, oos_fpr))
}

# =============================================================================
# SAVE
# =============================================================================
all_results <- list(
  metrics_A = metrics_A, mean_A = mean_A, sd_A = sd_A,
  rmse_baseline = rmse_baseline, models = models,
  thresh_diag = thresh_diag, thresh_detail = thresh_detail,
  t2_results = t2_results, rev_rmse_by_grp = rev_rmse_by_grp,
  M = M, K_sec = K_sec
)
rds_path <- file.path(OUTPUT_DIR, "table_pareto_all_models_cvthresh.rds")
saveRDS(all_results, rds_path)
cat("\nResults saved to:", rds_path, "\n")

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
  tex_row("Revenue",     mean_A[, "Revenue"],     sd_A[, "Revenue"],     rmse_baseline),
  tex_row("Elastic Net", mean_A[, "Elastic Net"], sd_A[, "Elastic Net"], rmse_baseline),
  tex_row("NACE",        mean_A[, "NACE"],        sd_A[, "NACE"],        rmse_baseline),
  tex_row("Gated Rev.",  mean_A[, "Gated Rev"],   sd_A[, "Gated Rev"],   rmse_baseline),
  "\\bottomrule",
  "\\end{tabular}"
)
tex_path <- file.path(OUTPUT_DIR, "table_main_results_sector_cv_pareto_cvthresh.tex")
writeLines(tex, tex_path)
cat("LaTeX Table 1 written to:", tex_path, "\n")

# ── Table 2: Zero-emitter sectors, Pareto + CV threshold ─────────────────────
SECTOR_LABELS_TEX <- c("Paper \\& printing", "Petroleum refining", "Iron \\& steel")
row_names_t2 <- c("nRMSE", "MAPD", "Levels corr.", "Rank corr.",
                   "FPR", "TPR", "Med.\\ FP rank", "Max FP rank")

tex_t2 <- c(
  "\\begin{tabular}{l cccc cccc cccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} \\\\",
          SECTOR_LABELS_TEX[1], SECTOR_LABELS_TEX[2], SECTOR_LABELS_TEX[3]),
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9} \\cmidrule(lr){10-13}",
  paste0(" & ", paste(rep("Rev. & EN & NACE & Gat.", 3), collapse = " & "), " \\\\"),
  "\\midrule"
)

model_order <- c("Revenue", "Elastic Net", "NACE", "Gated Rev")
for (i in seq_along(t2_metric_names)) {
  parts <- character(0)
  sd_parts <- character(0)
  for (g in grp_names) {
    for (mod in model_order) {
      mn <- mean(t2_results[, t2_metric_names[i], g, mod], na.rm = TRUE)
      s  <- sd(t2_results[, t2_metric_names[i], g, mod], na.rm = TRUE)
      parts <- c(parts, fmt(mn))
      sd_parts <- c(sd_parts, sprintf("{\\scriptsize(%s)}", fmt(s)))
    }
  }
  tex_t2 <- c(tex_t2,
    sprintf("%s & %s \\\\", row_names_t2[i], paste(parts, collapse = " & ")),
    sprintf(" & %s \\\\", paste(sd_parts, collapse = " & ")))
}
tex_t2 <- c(tex_t2, "\\bottomrule", "\\end{tabular}")

tex_path_t2 <- file.path(OUTPUT_DIR, "table_zero_emitters_sector_cv_pareto_cvthresh.tex")
writeLines(tex_t2, tex_path_t2)
cat("LaTeX Table 2 written to:", tex_path_t2, "\n")

# ── Table 3: Threshold summary ───────────────────────────────────────────────
SECTOR_LABELS_TEX_FULL <- c("17/18" = "Paper \\& printing (17/18)",
                             "19" = "Petroleum refining (19)",
                             "24" = "Iron \\& steel (24)")
TRAIN_LABELS <- list(
  "17/18" = c("19", "24"),
  "19"    = c("17/18", "24"),
  "24"    = c("17/18", "19")
)

tex_t3 <- c(
  "\\begin{tabular}{l cc c cc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Training percentiles} & & \\multicolumn{2}{c}{Held-out performance} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){5-6}",
  "Held-out sector & Sector 1 & Sector 2 & $\\bar{p}^*$ & TPR & FPR \\\\",
  "\\midrule"
)

for (sec in MIXED_SECTORS) {
  tp <- TRAIN_PAIRS[[sec]]
  p1 <- mean(thresh_detail[, sec, "pct_train1"], na.rm = TRUE)
  p2 <- mean(thresh_detail[, sec, "pct_train2"], na.rm = TRUE)
  s1 <- sd(thresh_detail[, sec, "pct_train1"], na.rm = TRUE)
  s2 <- sd(thresh_detail[, sec, "pct_train2"], na.rm = TRUE)
  avg_p <- mean(thresh_detail[, sec, "avg_pct"], na.rm = TRUE)
  s_avg <- sd(thresh_detail[, sec, "avg_pct"], na.rm = TRUE)
  oos_tpr <- mean(t2_results[, "tpr", sec, "Elastic Net"], na.rm = TRUE)
  oos_fpr <- mean(t2_results[, "fpr", sec, "Elastic Net"], na.rm = TRUE)
  s_tpr <- sd(t2_results[, "tpr", sec, "Elastic Net"], na.rm = TRUE)
  s_fpr <- sd(t2_results[, "fpr", sec, "Elastic Net"], na.rm = TRUE)

  mean_line <- sprintf("%s & %s & %s & %s & %s & %s \\\\",
    SECTOR_LABELS_TEX_FULL[sec],
    sprintf("%s: %s", tp[1], fmt(p1)),
    sprintf("%s: %s", tp[2], fmt(p2)),
    fmt(avg_p), fmt(oos_tpr), fmt(oos_fpr))
  sd_line <- sprintf(" & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} \\\\",
    fmt(s1), fmt(s2), fmt(s_avg), fmt(s_tpr), fmt(s_fpr))
  tex_t3 <- c(tex_t3, mean_line, sd_line)
}

tex_t3 <- c(tex_t3, "\\bottomrule", "\\end{tabular}")

tex_path_t3 <- file.path(OUTPUT_DIR, "table_cv_threshold_summary.tex")
writeLines(tex_t3, tex_path_t3)
cat("LaTeX Table 3 written to:", tex_path_t3, "\n")

cat("\nDone.\n")
