###############################################################################
# analysis/active/diagnostic_redistribution_comparison.R
#
# PURPOSE
#   Compare six redistribution methods for assigning firm-level emissions
#   within sectors: (A) sinh-calibrated (status quo), (B) quantile mapping
#   with pooled reference distribution, (C) GPA, (D) GEV, (E) GLO,
#   (F) Log-normal — all using parametric quantile functions.
#
#   All three methods preserve sector-year totals and the ranking implied by
#   the EN proxy. They differ in the within-sector distributional shape they
#   impose.
#
# APPROACH
#   Uses the existing repeated-CV framework (M=20 repeats, sector-level CV,
#   K=5 folds). For each repeat and fold, computes calibrated predictions
#   under each method, then evaluates within-sector distributional metrics
#   (p90/p10, Gini, L-moments) against actuals.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#
# OUTPUT
#   {OUTPUT_DIR}/redistribution_comparison.RData
#   {OUTPUT_DIR}/redistribution_comparison.csv
#   Console: summary table
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))
source(file.path(UTILS_DIR, "calibration_helpers.R"))

library(dplyr)
library(lmom)

# ── Parameters ───────────────────────────────────────────────────────────────
CV_TYPE   <- "sector"
M         <- 200L
K         <- 5L
BASE_SEED <- 2026L
MIN_CELL  <- 10L   # minimum emitters with proxy > 0 for distributional stats

cat("================================================================\n")
cat("  REDISTRIBUTION METHOD COMPARISON\n")
cat("  M =", M, "repeats, K =", K, "folds, MIN_CELL =", MIN_CELL, "\n")
cat("================================================================\n\n")

# =============================================================================
# SECTION 1: Load data
# =============================================================================
cat("Loading repeated CV proxy data (sector design)...\n")
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"))

panel <- repeated_cv_proxy_panel
proxy_matrix <- proxy_matrix[, 1:M, drop = FALSE]
N <- nrow(panel)

cat(sprintf("  Panel: %d firm-years, %d sectors, %d emitters\n",
            N, length(unique(panel$nace2d)), sum(panel$y > 0)))
cat(sprintf("  Proxy matrix: %d x %d\n", nrow(proxy_matrix), ncol(proxy_matrix)))

# =============================================================================
# SECTION 2: Helper functions
# =============================================================================

# ── Gini coefficient (non-negative vector) ───────────────────────────────────
gini_coef <- function(x) {
  x <- sort(x[x > 0])
  n <- length(x)
  if (n < 2) return(NA_real_)
  (2 * sum(x * seq_len(n))) / (n * sum(x)) - (n + 1) / n
}

# ── Build reference distribution for quantile mapping ────────────────────────
# Returns sorted vector of year-demeaned, sector-demeaned log deviations
# from training emitters (excluding fold k sectors).
build_reference_dist <- function(panel, fold_k, k) {
  train_idx <- which(fold_k != k & panel$y > 0)
  if (length(train_idx) < 20) return(NULL)

  df <- data.frame(
    log_y  = log(panel$y[train_idx]),
    year   = panel$year[train_idx],
    nace2d = panel$nace2d[train_idx],
    stringsAsFactors = FALSE
  )

  # Year-demean
  mu_t <- tapply(df$log_y, df$year, mean)
  df$tilde <- df$log_y - mu_t[as.character(df$year)]

  # Sector-demean
  mu_s <- tapply(df$tilde, df$nace2d, mean)
  df$d <- df$tilde - mu_s[df$nace2d]

  sort(df$d)
}

# ── Fit distribution to reference distribution ──────────────────────────────
fit_dist <- function(ref_dist, pel_fn) {
  if (is.null(ref_dist) || length(ref_dist) < 20) return(NULL)
  lmoms <- samlmu(ref_dist, nmom = 3)
  tryCatch(pel_fn(lmoms), error = function(e) NULL)
}

# ── Compute E_target for a (sector, year) cell ──────────────────────────────
# Replicates calibrate_sector() logic exactly.
compute_e_target <- function(panel, fold_k, k) {
  sy_key <- paste(panel$nace2d, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)

  train_idx <- which(fold_k != k)
  sy_train <- paste(panel$nace2d[train_idx], panel$year[train_idx])
  E_train_sy <- tapply(panel$y[train_idx], sy_train, sum, na.rm = TRUE)

  list(E_sy = E_sy, E_train_sy = E_train_sy)
}

# ── Quantile mapping calibration ─────────────────────────────────────────────
calibrate_qmap <- function(panel, proxy_raw, fold_k, ref_dist_sorted) {
  result <- rep(NA_real_, nrow(panel))
  folds <- sort(unique(fold_k))

  for (k in folds) {
    held_out <- which(fold_k == k)
    etgt <- compute_e_target(panel, fold_k, k)

    # Build fold-specific reference distribution
    ref_k <- build_reference_dist(panel, fold_k, k)
    if (is.null(ref_k)) {
      # Fallback to sinh-calibrated
      result[held_out] <- calibrate_sector_subset(panel, proxy_raw, fold_k, k, etgt)
      next
    }

    ho_sy <- paste(panel$nace2d[held_out], panel$year[held_out])

    for (sy in unique(ho_sy)) {
      idx_in_ho <- which(ho_sy == sy)
      idx <- held_out[idx_in_ho]

      E_total <- etgt$E_sy[sy]
      E_train <- ifelse(is.na(etgt$E_train_sy[sy]), 0, etgt$E_train_sy[sy])
      E_target <- E_total - E_train

      if (is.na(E_target) || E_target <= 0) {
        result[idx] <- 0
        next
      }

      # Identify emitters (proxy > 0)
      emitter_mask <- proxy_raw[idx] > 0
      n_emit <- sum(emitter_mask)

      if (n_emit < 2) {
        # Fallback: proportional allocation
        raw <- proxy_raw[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) {
          result[idx] <- E_target * (raw / denom)
        } else {
          result[idx] <- E_target / length(idx)
        }
        next
      }

      # Non-emitters get zero
      result[idx[!emitter_mask]] <- 0

      # Rank emitters by proxy
      emit_idx <- idx[emitter_mask]
      emit_proxy <- proxy_raw[emit_idx]
      ranks <- rank(emit_proxy, ties.method = "average")
      p_i <- (ranks - 0.5) / n_emit

      # Look up deviations from reference distribution
      d_i <- quantile(ref_k, probs = p_i, type = 7, names = FALSE)

      # Solve for c: sum(exp(c + d_i)) = E_target
      # Allocate E_target to emitters only (non-emitters already set to 0)
      E_emit_target <- E_target  # full target goes to emitters

      f <- function(cc) sum(exp(cc + d_i)) - E_emit_target
      c_sol <- tryCatch({
        uniroot(f,
                lower = log(E_emit_target) - max(d_i) - 30,
                upper = log(E_emit_target) - min(d_i) + 30,
                tol = 1e-10)$root
      }, error = function(e) NA)

      if (is.na(c_sol)) {
        # Fallback: proportional allocation for this cell
        raw <- proxy_raw[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) {
          result[idx] <- E_target * (raw / denom)
        } else {
          result[idx] <- E_target / length(idx)
        }
        next
      }

      result[emit_idx] <- exp(c_sol + d_i)
    }
  }
  result
}

# ── Parametric calibration (generic) ────────────────────────────────────────
calibrate_parametric <- function(panel, proxy_raw, fold_k, pel_fn, qua_fn) {
  result <- rep(NA_real_, nrow(panel))
  folds <- sort(unique(fold_k))

  for (k in folds) {
    held_out <- which(fold_k == k)
    etgt <- compute_e_target(panel, fold_k, k)

    # Build fold-specific reference and fit distribution
    ref_k <- build_reference_dist(panel, fold_k, k)
    dist_params <- fit_dist(ref_k, pel_fn)

    ho_sy <- paste(panel$nace2d[held_out], panel$year[held_out])

    for (sy in unique(ho_sy)) {
      idx_in_ho <- which(ho_sy == sy)
      idx <- held_out[idx_in_ho]

      E_total <- etgt$E_sy[sy]
      E_train <- ifelse(is.na(etgt$E_train_sy[sy]), 0, etgt$E_train_sy[sy])
      E_target <- E_total - E_train

      if (is.na(E_target) || E_target <= 0) {
        result[idx] <- 0
        next
      }

      emitter_mask <- proxy_raw[idx] > 0
      n_emit <- sum(emitter_mask)

      if (n_emit < 2 || is.null(dist_params)) {
        # Fallback: proportional allocation
        raw <- proxy_raw[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) {
          result[idx] <- E_target * (raw / denom)
        } else {
          result[idx] <- E_target / length(idx)
        }
        next
      }

      result[idx[!emitter_mask]] <- 0

      emit_idx <- idx[emitter_mask]
      emit_proxy <- proxy_raw[emit_idx]
      ranks <- rank(emit_proxy, ties.method = "average")
      p_i <- (ranks - 0.5) / n_emit

      # Parametric quantile function for deviations
      w_i <- tryCatch(
        qua_fn(p_i, dist_params),
        error = function(e) rep(NA, length(p_i))
      )

      if (any(is.na(w_i)) || any(!is.finite(w_i))) {
        # Fallback: proportional allocation
        raw <- proxy_raw[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) {
          result[idx] <- E_target * (raw / denom)
        } else {
          result[idx] <- E_target / length(idx)
        }
        next
      }

      # Calibrate: yhat_i = E_target * exp(w_i) / sum(exp(w_i))
      E_emit_target <- E_target
      exp_w <- exp(w_i - max(w_i))  # numerical stability
      result[emit_idx] <- E_emit_target * exp_w / sum(exp_w)
    }
  }
  result
}

# ── Proportional fallback for a single fold ─────────────────────────────────
calibrate_sector_subset <- function(panel, proxy_raw, fold_k, k, etgt) {
  held_out <- which(fold_k == k)
  result <- rep(NA_real_, length(held_out))
  ho_sy <- paste(panel$nace2d[held_out], panel$year[held_out])

  for (sy in unique(ho_sy)) {
    idx_in_ho <- which(ho_sy == sy)

    E_total <- etgt$E_sy[sy]
    E_train <- ifelse(is.na(etgt$E_train_sy[sy]), 0, etgt$E_train_sy[sy])
    E_target <- E_total - E_train

    if (is.na(E_target) || E_target <= 0) {
      result[idx_in_ho] <- 0
      next
    }

    raw <- proxy_raw[held_out[idx_in_ho]]
    denom <- sum(raw, na.rm = TRUE)
    if (denom > 0) {
      result[idx_in_ho] <- E_target * (raw / denom)
    } else {
      result[idx_in_ho] <- E_target / length(idx_in_ho)
    }
  }
  result
}

# ── Cell-level distributional statistics ─────────────────────────────────────
cell_dist_stats <- function(x) {
  x <- x[x > 0]
  n <- length(x)
  if (n < MIN_CELL) return(rep(NA_real_, 5))

  q90 <- quantile(x, 0.9, names = FALSE)
  q10 <- quantile(x, 0.1, names = FALSE)
  p90p10 <- q90 / q10
  log_gap <- log(q90) - log(q10)
  gi <- gini_coef(x)

  lm_ratios <- tryCatch({
    lm <- samlmu(log(x), nmom = 4)
    c(lm[3], lm[4])
  }, error = function(e) c(NA_real_, NA_real_))

  c(p90p10 = p90p10, log_gap = log_gap, gini = gi,
    tau3 = lm_ratios[1], tau4 = lm_ratios[2])
}

# =============================================================================
# SECTION 3: Main loop
# =============================================================================
METHOD_NAMES <- c("Sinh-calibrated", "Quantile mapping",
                   "GPA", "GEV", "GLO", "Log-normal")
N_METHODS <- length(METHOD_NAMES)
STAT_NAMES <- c("p90p10", "log_gap", "gini", "tau3", "tau4")
N_STATS <- length(STAT_NAMES)

# Storage: per-repeat summary of distributional metric deviations
# For each repeat: mean bias and RMSE across eligible cells
repeat_bias <- array(NA_real_, dim = c(M, N_STATS, N_METHODS),
                     dimnames = list(NULL, STAT_NAMES, METHOD_NAMES))
repeat_rmse <- array(NA_real_, dim = c(M, N_STATS, N_METHODS),
                     dimnames = list(NULL, STAT_NAMES, METHOD_NAMES))

# Firm-level metrics per repeat
repeat_pearson  <- matrix(NA_real_, nrow = M, ncol = N_METHODS,
                          dimnames = list(NULL, METHOD_NAMES))
repeat_spearman <- matrix(NA_real_, nrow = M, ncol = N_METHODS,
                          dimnames = list(NULL, METHOD_NAMES))
repeat_rmse_lev <- matrix(NA_real_, nrow = M, ncol = N_METHODS,
                          dimnames = list(NULL, METHOD_NAMES))

cat("Running", M, "repeats...\n")
t0 <- Sys.time()

for (r in seq_len(M)) {
  cat(sprintf("  Repeat %2d/%d ...", r, M))

  # Step 1: Fold assignments
  fold_k_r <- assign_folds(panel, cv_type = CV_TYPE, K = K, seed = BASE_SEED + r)

  # Step 2: Proxy in levels
  proxy_raw_r <- proxy_to_levels(proxy_matrix[, r])

  # Step 3: Compute predictions under each method
  yhat_A <- calibrate_sector(panel, proxy_raw_r, fold_k_r)
  yhat_B <- calibrate_qmap(panel, proxy_raw_r, fold_k_r, NULL)
  yhat_C <- calibrate_parametric(panel, proxy_raw_r, fold_k_r, pelgpa, quagpa)
  yhat_D <- calibrate_parametric(panel, proxy_raw_r, fold_k_r, pelgev, quagev)
  yhat_E <- calibrate_parametric(panel, proxy_raw_r, fold_k_r, pelglo, quaglo)
  yhat_F <- calibrate_parametric(panel, proxy_raw_r, fold_k_r, pelln3, qualn3)

  yhats <- list(yhat_A, yhat_B, yhat_C, yhat_D, yhat_E, yhat_F)

  # Step 4: Cell-level distributional stats
  sy_key <- paste(panel$nace2d, panel$year)
  cells <- unique(sy_key)

  # Collect per-cell deviations (imputed stat - actual stat)
  cell_devs <- vector("list", N_METHODS)
  for (m in seq_len(N_METHODS)) cell_devs[[m]] <- list()

  n_eligible <- 0L

  for (cell in cells) {
    idx <- which(sy_key == cell)

    # Actual stats
    actual_stats <- cell_dist_stats(panel$y[idx])
    if (any(is.na(actual_stats))) next

    # Check eligible: enough emitters with proxy > 0
    n_emit_proxy <- sum(proxy_raw_r[idx] > 0)
    if (n_emit_proxy < MIN_CELL) next

    n_eligible <- n_eligible + 1L

    for (m in seq_len(N_METHODS)) {
      imp_stats <- cell_dist_stats(yhats[[m]][idx])
      cell_devs[[m]][[length(cell_devs[[m]]) + 1]] <- imp_stats - actual_stats
    }
  }

  # Aggregate: bias and RMSE across eligible cells
  for (m in seq_len(N_METHODS)) {
    dev_mat <- do.call(rbind, cell_devs[[m]])
    if (is.null(dev_mat) || nrow(dev_mat) == 0) next
    repeat_bias[r, , m] <- colMeans(dev_mat, na.rm = TRUE)
    repeat_rmse[r, , m] <- sqrt(colMeans(dev_mat^2, na.rm = TRUE))
  }

  # Step 5: Firm-level metrics (emitters only)
  emit_mask <- panel$y > 0
  for (m in seq_len(N_METHODS)) {
    yh <- yhats[[m]]
    repeat_pearson[r, m]  <- cor(panel$y[emit_mask], yh[emit_mask], use = "complete.obs")
    repeat_spearman[r, m] <- cor(panel$y[emit_mask], yh[emit_mask],
                                 method = "spearman", use = "complete.obs")
    repeat_rmse_lev[r, m] <- sqrt(mean((panel$y[emit_mask] - yh[emit_mask])^2, na.rm = TRUE))
  }

  cat(sprintf(" %d eligible cells, done.\n", n_eligible))
}

elapsed <- difftime(Sys.time(), t0, units = "secs")
cat(sprintf("\nCompleted %d repeats in %.1f seconds.\n\n", M, as.numeric(elapsed)))

# =============================================================================
# SECTION 4: Aggregate and display results
# =============================================================================

# ── Distributional metrics ───────────────────────────────────────────────────
cat("── DISTRIBUTIONAL METRICS (bias: imputed − actual) ────────────\n\n")

bias_summary <- data.frame(Method = METHOD_NAMES, stringsAsFactors = FALSE)
rmse_summary <- data.frame(Method = METHOD_NAMES, stringsAsFactors = FALSE)

for (s in STAT_NAMES) {
  for (m in seq_len(N_METHODS)) {
    bias_summary[m, paste0(s, "_bias")] <- sprintf("%.3f (%.3f)",
      mean(repeat_bias[, s, m], na.rm = TRUE),
      sd(repeat_bias[, s, m], na.rm = TRUE) / sqrt(M))
    rmse_summary[m, paste0(s, "_rmse")] <- sprintf("%.3f (%.3f)",
      mean(repeat_rmse[, s, m], na.rm = TRUE),
      sd(repeat_rmse[, s, m], na.rm = TRUE) / sqrt(M))
  }
}

cat("Bias (mean across cells, SE across repeats):\n")
print(bias_summary, row.names = FALSE)
cat("\nRMSE (mean across cells, SE across repeats):\n")
print(rmse_summary, row.names = FALSE)

# ── Firm-level metrics ───────────────────────────────────────────────────────
cat("\n── FIRM-LEVEL METRICS (emitters only) ──────────────────────────\n\n")

firm_summary <- data.frame(
  Method   = METHOD_NAMES,
  Pearson  = sprintf("%.3f (%.3f)",
                     colMeans(repeat_pearson, na.rm = TRUE),
                     apply(repeat_pearson, 2, sd, na.rm = TRUE) / sqrt(M)),
  Spearman = sprintf("%.3f (%.3f)",
                     colMeans(repeat_spearman, na.rm = TRUE),
                     apply(repeat_spearman, 2, sd, na.rm = TRUE) / sqrt(M)),
  RMSE     = sprintf("%.0f (%.0f)",
                     colMeans(repeat_rmse_lev, na.rm = TRUE),
                     apply(repeat_rmse_lev, 2, sd, na.rm = TRUE) / sqrt(M)),
  stringsAsFactors = FALSE
)
print(firm_summary, row.names = FALSE)

# =============================================================================
# SECTION 5: Save outputs
# =============================================================================

# Numeric summary for CSV (means only)
csv_out <- data.frame(Method = METHOD_NAMES, stringsAsFactors = FALSE)
for (s in STAT_NAMES) {
  csv_out[, paste0(s, "_bias")]   <- sapply(seq_len(N_METHODS), function(m)
    mean(repeat_bias[, s, m], na.rm = TRUE))
  csv_out[, paste0(s, "_rmse")]   <- sapply(seq_len(N_METHODS), function(m)
    mean(repeat_rmse[, s, m], na.rm = TRUE))
}
csv_out$pearson  <- colMeans(repeat_pearson, na.rm = TRUE)
csv_out$spearman <- colMeans(repeat_spearman, na.rm = TRUE)
csv_out$rmse_levels <- colMeans(repeat_rmse_lev, na.rm = TRUE)

write.csv(csv_out, file.path(OUTPUT_DIR, "redistribution_comparison.csv"),
          row.names = FALSE)

save(repeat_bias, repeat_rmse, repeat_pearson, repeat_spearman, repeat_rmse_lev,
     METHOD_NAMES, STAT_NAMES, M, K, MIN_CELL,
     file = file.path(OUTPUT_DIR, "redistribution_comparison.RData"))

cat(sprintf("\nSaved: %s\n", file.path(OUTPUT_DIR, "redistribution_comparison.csv")))
cat(sprintf("Saved: %s\n", file.path(OUTPUT_DIR, "redistribution_comparison.RData")))

# =============================================================================
# SECTION 6: Verification (first repeat only)
# =============================================================================
cat("\n── VERIFICATION (repeat 1) ─────────────────────────────────────\n")

fold_k_1 <- assign_folds(panel, cv_type = CV_TYPE, K = K, seed = BASE_SEED + 1L)
proxy_raw_1 <- proxy_to_levels(proxy_matrix[, 1])

yhat_A1 <- calibrate_sector(panel, proxy_raw_1, fold_k_1)
yhat_B1 <- calibrate_qmap(panel, proxy_raw_1, fold_k_1, NULL)
yhat_C1 <- calibrate_parametric(panel, proxy_raw_1, fold_k_1, pelgpa, quagpa)
yhat_D1 <- calibrate_parametric(panel, proxy_raw_1, fold_k_1, pelgev, quagev)
yhat_E1 <- calibrate_parametric(panel, proxy_raw_1, fold_k_1, pelglo, quaglo)
yhat_F1 <- calibrate_parametric(panel, proxy_raw_1, fold_k_1, pelln3, qualn3)

# Check cell totals
sy_key <- paste(panel$nace2d, panel$year)
verif_yhats <- list(A = yhat_A1, B = yhat_B1, C = yhat_C1,
                    D = yhat_D1, E = yhat_E1, F = yhat_F1)
for (k in 1:K) {
  held_out <- which(fold_k_1 == k)
  ho_sy <- unique(sy_key[held_out])
  for (sy in ho_sy) {
    idx <- held_out[sy_key[held_out] == sy]
    actual_total <- sum(panel$y[idx])
    for (nm in names(verif_yhats)) {
      pred_total <- sum(verif_yhats[[nm]][idx], na.rm = TRUE)
      if (abs(pred_total - actual_total) > 0.01 * actual_total & actual_total > 0) {
        cat(sprintf("  WARNING: Method %s, cell %s: predicted %.0f vs actual %.0f\n",
                    nm, sy, pred_total, actual_total))
      }
    }
  }
}
cat("  Cell total verification complete.\n")

# Check ranking preservation (Methods B-F)
n_rank_violations <- 0L
for (k in 1:K) {
  held_out <- which(fold_k_1 == k)
  ho_sy <- unique(sy_key[held_out])
  for (sy in ho_sy) {
    idx <- held_out[sy_key[held_out] == sy]
    emit_mask <- proxy_raw_1[idx] > 0
    if (sum(emit_mask) < 2) next
    emit_idx <- idx[emit_mask]
    proxy_rank <- rank(proxy_raw_1[emit_idx])
    for (yh in verif_yhats[c("B", "C", "D", "E", "F")]) {
      pred_rank <- rank(yh[emit_idx])
      if (!all(proxy_rank == pred_rank)) n_rank_violations <- n_rank_violations + 1L
    }
  }
}
cat(sprintf("  Ranking violations (B-F): %d cells\n", n_rank_violations))

cat("\nDone.\n")
