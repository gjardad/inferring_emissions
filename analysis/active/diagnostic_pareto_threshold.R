###############################################################################
# analysis/active/diagnostic_pareto_threshold.R
#
# PURPOSE
#   Test whether a cross-validated percentile threshold on the EN proxy
#   improves the extensive margin (FPR, FP severity) in the three sectors
#   with both emitters and non-emitters: 17/18, 19, 24.
#
# APPROACH
#   Leave-one-sector-out on the three mixed sectors:
#     For each held-out sector h in {17/18, 19, 24}:
#       1. For each of the two training sectors s:
#            Find the percentile p*_s of proxy > 0 that maximizes Youden's J
#       2. Average: p* = mean(p*_s1, p*_s2)
#       3. Apply p* to held-out sector h: within each (sector, year) cell,
#            zero out firms below the p*-th percentile of proxy > 0
#       4. Run Pareto redistribution on sector h
#       5. Compute performance metrics on sector h
#
#   Repeats over M random fold assignments (which affect the EN proxy
#   but not the sector assignment — sectors are fixed).
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/diagnostic_pareto_threshold.rds
#   Console: per-sector metrics for 3 methods × 3 sectors
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
source(file.path(UTILS_DIR, "calibration_helpers.R"))

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED <- 2026L
K_sec     <- 5L
M         <- 20L
N_THRESH  <- 200L

SECTOR_GROUPS <- c("17/18", "19", "24")

cat("================================================================\n")
cat("  PARETO + CV PERCENTILE THRESHOLD (mixed sectors only)\n")
cat("  M =", M, "repeats, LOSO on sectors:", paste(SECTOR_GROUPS, collapse=", "), "\n")
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

N <- nrow(panel)
cat(sprintf("  Panel: %d firm-years\n", N))
for (g in SECTOR_GROUPS) {
  idx <- which(panel$nace2d == g)
  cat(sprintf("    %s: %d firms, %d emitters, %d non-emitters\n",
              g, length(idx), sum(panel$y[idx] > 0), sum(panel$y[idx] == 0)))
}

# =============================================================================
# PARETO CALIBRATION (sector-specific: calibrates only within target sector)
# =============================================================================

build_reference_dist <- function(panel, exclude_sector) {
  # Build reference distribution from ALL emitters EXCEPT those in exclude_sector
  train_idx <- which(panel$y > 0 & panel$nace2d != exclude_sector)
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

calibrate_pareto_sector <- function(panel, ranking_signal, target_sector) {
  # Calibrate only firms in target_sector, using all other sectors as reference
  idx_all <- which(panel$nace2d == target_sector)
  result <- rep(NA_real_, length(idx_all))

  ref_dist <- build_reference_dist(panel, target_sector)
  gpa_params <- if (!is.null(ref_dist) && length(ref_dist) >= 20) {
    tryCatch(pelgpa(samlmu(ref_dist, nmom = 3)), error = function(e) NULL)
  } else NULL

  sy_key <- paste(panel$nace2d[idx_all], panel$year[idx_all])
  E_sy <- tapply(panel$y[idx_all], sy_key, sum, na.rm = TRUE)

  for (sy in unique(sy_key)) {
    idx_in <- which(sy_key == sy)
    idx <- idx_all[idx_in]

    E_target <- E_sy[sy]
    if (is.na(E_target) || E_target <= 0) {
      result[idx_in] <- 0
      next
    }

    emitter_mask <- ranking_signal[idx] > 0
    n_emit <- sum(emitter_mask)

    if (n_emit < 2 || is.null(gpa_params)) {
      raw <- ranking_signal[idx]
      denom <- sum(raw, na.rm = TRUE)
      if (denom > 0) result[idx_in] <- E_target * (raw / denom)
      else result[idx_in] <- E_target / length(idx_in)
      next
    }

    result[idx_in[!emitter_mask]] <- 0

    emit_local <- which(emitter_mask)
    emit_idx <- idx[emit_local]
    ranks <- rank(ranking_signal[emit_idx], ties.method = "average")
    p_i <- (ranks - 0.5) / n_emit

    w_i <- tryCatch(quagpa(p_i, gpa_params), error = function(e) rep(NA, n_emit))
    if (any(is.na(w_i)) || any(!is.finite(w_i))) {
      raw <- ranking_signal[idx]
      denom <- sum(raw, na.rm = TRUE)
      if (denom > 0) result[idx_in] <- E_target * (raw / denom)
      else result[idx_in] <- E_target / length(idx_in)
      next
    }

    exp_w <- exp(w_i - max(w_i))
    result[idx_in[emit_local]] <- E_target * exp_w / sum(exp_w)
  }

  result
}

# Sinh-calibrated within a sector (proportional allocation)
calibrate_sinh_sector <- function(panel, proxy_raw, target_sector) {
  idx_all <- which(panel$nace2d == target_sector)
  result <- rep(NA_real_, length(idx_all))

  sy_key <- paste(panel$nace2d[idx_all], panel$year[idx_all])
  E_sy <- tapply(panel$y[idx_all], sy_key, sum, na.rm = TRUE)

  for (sy in unique(sy_key)) {
    idx_in <- which(sy_key == sy)
    idx <- idx_all[idx_in]

    E_target <- E_sy[sy]
    if (is.na(E_target) || E_target <= 0) {
      result[idx_in] <- 0
      next
    }

    raw <- proxy_raw[idx]
    denom <- sum(raw, na.rm = TRUE)
    if (denom > 0) result[idx_in] <- E_target * (raw / denom)
    else result[idx_in] <- E_target / length(idx_in)
  }

  result
}

# =============================================================================
# THRESHOLD TUNING FUNCTIONS
# =============================================================================

# Find Youden-optimal percentile within one sector
youden_percentile <- function(proxy_raw, y, idx_sector) {
  pr <- proxy_raw[idx_sector]
  yt <- y[idx_sector]
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

# Apply percentile threshold within each (sector, year) cell
apply_pct_threshold <- function(proxy_raw, pct, idx_sector, year) {
  result <- proxy_raw[idx_sector]
  if (pct <= 0) return(result)

  yr <- year[idx_sector]
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
# METRIC FUNCTIONS
# =============================================================================
fpr_fn <- function(y, yhat) {
  ne <- (y == 0)
  if (sum(ne) > 0) sum(yhat[ne] > 0) / sum(ne) else NA_real_
}
tpr_fn <- function(y, yhat) {
  em <- (y > 0)
  if (sum(em) > 0) sum(yhat[em] > 0) / sum(em) else NA_real_
}
rmse_fn <- function(y, yhat) sqrt(mean((y - yhat)^2))
pearson_fn <- function(y, yhat) {
  if (sd(y) > 0 && sd(yhat) > 0) cor(y, yhat, use = "complete.obs") else NA_real_
}
spearman_fn <- function(y, yhat) {
  if (sd(y) > 0 && sd(yhat) > 0) cor(y, yhat, method = "spearman", use = "complete.obs") else NA_real_
}
mapd_emitters_fn <- function(y, yhat) {
  em <- (y > 0)
  if (sum(em) > 0) median(abs(y[em] - yhat[em]) / y[em]) else NA_real_
}
fp_severity_fn <- function(y, yhat, year, stat = "max") {
  cell_vals <- numeric(0)
  for (yr in unique(year)) {
    in_yr <- (year == yr)
    em <- (y > 0 & in_yr); ne <- (y == 0 & in_yr)
    if (sum(em) < 3 || sum(ne) < 1) next
    ecdf_em <- ecdf(y[em])
    ne_preds <- yhat[ne]
    val <- if (stat == "max") max(ne_preds) else median(ne_preds)
    cell_vals <- c(cell_vals, ecdf_em(val))
  }
  if (length(cell_vals) > 0) mean(cell_vals) else NA_real_
}

compute_all_metrics <- function(y, yhat, year) {
  c(fpr     = fpr_fn(y, yhat),
    tpr     = tpr_fn(y, yhat),
    rmse    = rmse_fn(y, yhat),
    pearson = pearson_fn(y, yhat),
    spearman = spearman_fn(y, yhat),
    mapd    = mapd_emitters_fn(y, yhat),
    fpp50   = fp_severity_fn(y, yhat, year, "median"),
    fpmax   = fp_severity_fn(y, yhat, year, "max"))
}

# =============================================================================
# MAIN LOOP: LOSO on 3 mixed sectors × M repeats
# =============================================================================
methods <- c("Sinh (no thresh)", "Pareto (no thresh)", "Pareto (CV thresh)")
metric_names <- c("fpr", "tpr", "rmse", "pearson", "spearman", "mapd", "fpp50", "fpmax")

# Storage: M × metrics × sectors × methods
results <- array(NA_real_,
  dim = c(M, length(metric_names), length(SECTOR_GROUPS), length(methods)),
  dimnames = list(NULL, metric_names, SECTOR_GROUPS, methods))

# Also store threshold diagnostics
thresh_pcts <- array(NA_real_,
  dim = c(M, length(SECTOR_GROUPS), 2),
  dimnames = list(NULL, SECTOR_GROUPS, c("avg_pct", "n_training_sectors")))

cat(sprintf("Running %d repeats × 3 held-out sectors...\n", M))

for (r in seq_len(M)) {
  cat(sprintf("  Repeat %2d/%d ...", r, M))

  # The EN proxy varies across repeats (different fold → different model)
  # But for this LOSO exercise, the sector assignment is fixed.
  # We use the proxy from repeat r.
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])

  for (hi in seq_along(SECTOR_GROUPS)) {
    held_out_sector <- SECTOR_GROUPS[hi]
    train_sectors <- setdiff(SECTOR_GROUPS, held_out_sector)

    idx_ho <- which(panel$nace2d == held_out_sector)
    y_ho <- panel$y[idx_ho]
    yr_ho <- panel$year[idx_ho]

    # ── Method 1: Sinh (no threshold) ──
    yhat_sinh <- calibrate_sinh_sector(panel, en_levels, held_out_sector)
    results[r, , held_out_sector, "Sinh (no thresh)"] <-
      compute_all_metrics(y_ho, yhat_sinh, yr_ho)

    # ── Method 2: Pareto (no threshold) ──
    yhat_pareto <- calibrate_pareto_sector(panel, en_levels, held_out_sector)
    results[r, , held_out_sector, "Pareto (no thresh)"] <-
      compute_all_metrics(y_ho, yhat_pareto, yr_ho)

    # ── Method 3: Pareto + CV percentile threshold ──
    # Tune per-sector percentile on training sectors, average
    sector_pcts <- sapply(train_sectors, function(sec) {
      idx_sec <- which(panel$nace2d == sec)
      youden_percentile(en_levels, panel$y, idx_sec)
    })
    avg_pct <- mean(sector_pcts)
    thresh_pcts[r, held_out_sector, "avg_pct"] <- avg_pct
    thresh_pcts[r, held_out_sector, "n_training_sectors"] <- length(train_sectors)

    # Apply percentile threshold to held-out sector
    en_thresholded <- en_levels
    en_thresholded[idx_ho] <- apply_pct_threshold(en_levels, avg_pct, idx_ho, panel$year)

    yhat_thresh <- calibrate_pareto_sector(panel, en_thresholded, held_out_sector)
    results[r, , held_out_sector, "Pareto (CV thresh)"] <-
      compute_all_metrics(y_ho, yhat_thresh, yr_ho)
  }

  cat(" done.\n")
}

# =============================================================================
# DISPLAY RESULTS
# =============================================================================
cat("\n── RESULTS BY HELD-OUT SECTOR ──────────────────────────────────\n")

for (g in SECTOR_GROUPS) {
  cat(sprintf("\n  === Sector %s (held out) ===\n", g))
  cat(sprintf("  %-25s %8s %8s %8s %8s %8s %8s %8s %8s\n",
              "", "FPR", "TPR", "RMSE", "Pearson", "Spearman", "MAPD", "p50", "pMax"))
  cat(sprintf("  %s\n", paste(rep("-", 95), collapse = "")))

  for (meth in methods) {
    mn <- apply(results[, , g, meth, drop = FALSE], 2, mean, na.rm = TRUE)
    s  <- apply(results[, , g, meth, drop = FALSE], 2, sd, na.rm = TRUE)
    cat(sprintf("  %-25s %8.3f %8.3f %8.0f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
                meth, mn["fpr"], mn["tpr"], mn["rmse"],
                mn["pearson"], mn["spearman"], mn["mapd"],
                mn["fpp50"], mn["fpmax"]))
    cat(sprintf("  %-25s %8s %8s %8s %8s %8s %8s %8s %8s\n",
                "",
                sprintf("(%.3f)", s["fpr"]), sprintf("(%.3f)", s["tpr"]),
                sprintf("(%.0f)", s["rmse"]),
                sprintf("(%.3f)", s["pearson"]), sprintf("(%.3f)", s["spearman"]),
                sprintf("(%.3f)", s["mapd"]),
                sprintf("(%.3f)", s["fpp50"]), sprintf("(%.3f)", s["fpmax"])))
  }

  # Threshold info
  avg_p <- mean(thresh_pcts[, g, "avg_pct"], na.rm = TRUE)
  sd_p  <- sd(thresh_pcts[, g, "avg_pct"], na.rm = TRUE)
  cat(sprintf("  CV percentile: %.3f (%.3f)\n", avg_p, sd_p))
}

# ── Summary across sectors ───────────────────────────────────────────────────
cat("\n── SUMMARY (mean across 3 sectors, equal weight) ──────────────\n")
cat(sprintf("  %-25s %8s %8s %8s %8s %8s %8s %8s %8s\n",
            "", "FPR", "TPR", "RMSE", "Pearson", "Spearman", "MAPD", "p50", "pMax"))
cat(sprintf("  %s\n", paste(rep("-", 95), collapse = "")))

for (meth in methods) {
  # Mean across repeats for each sector, then mean across sectors
  sector_means <- sapply(SECTOR_GROUPS, function(g)
    apply(results[, , g, meth, drop = FALSE], 2, mean, na.rm = TRUE))
  overall <- rowMeans(sector_means, na.rm = TRUE)
  cat(sprintf("  %-25s %8.3f %8.3f %8.0f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
              meth, overall["fpr"], overall["tpr"], overall["rmse"],
              overall["pearson"], overall["spearman"], overall["mapd"],
              overall["fpp50"], overall["fpmax"]))
}

# =============================================================================
# SAVE
# =============================================================================
all_results <- list(
  results = results, thresh_pcts = thresh_pcts,
  methods = methods, metric_names = metric_names,
  SECTOR_GROUPS = SECTOR_GROUPS, M = M
)
rds_path <- file.path(OUTPUT_DIR, "diagnostic_pareto_threshold.rds")
saveRDS(all_results, rds_path)
cat("\nResults saved to:", rds_path, "\n")

cat("\nDone.\n")
