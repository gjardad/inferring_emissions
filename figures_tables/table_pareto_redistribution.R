###############################################################################
# figures_tables/table_pareto_redistribution.R
#
# PURPOSE
#   Generate three tables comparing redistribution methods:
#
#   1. Main results table (sector CV) with Pareto row added alongside
#      existing models (Revenue, EN, NACE, Gated Rev, Combined)
#   2. Zero-emitter sector table (17/18, 19, 24) with Pareto row added
#   3. Comparison table: Sinh-calibrated vs Quantile mapping vs Pareto
#      for EN proxy only (distributional + firm-level metrics)
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_results_sector_cv_pareto.tex
#   {OUTPUT_DIR}/table_zero_emitters_sector_cv_pareto.tex
#   {OUTPUT_DIR}/table_redistribution_comparison.tex
#   {OUTPUT_DIR}/table_pareto_redistribution.rds
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
library(lmom)
source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration_helpers.R"))

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED           <- 2026L
K_sec               <- 5L
M                   <- 20L       # 20 repeats (sufficient for distributional stats)
MIN_FIRMS_SECTOR_CV <- 3L
MIN_CELL            <- 10L       # for distributional stats in Table 3
SECTOR_GROUPS       <- list("17/18" = "17/18", "19" = "19", "24" = "24")

cat("================================================================\n")
cat("  PARETO REDISTRIBUTION TABLES\n")
cat("  M =", M, "repeats, K =", K_sec, "folds\n")
cat("================================================================\n\n")

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading EN proxy (sector CV)...\n")
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix[, 1:M, drop = FALSE]
panel            <- e_sec$repeated_cv_proxy_panel
rm(e_sec)

# Combine NACE 17/18
panel$primary_nace2d[panel$primary_nace2d %in% c("17", "18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17", "18")] <- "17/18"

cat("Loading training sample (for revenue and NACE proxies)...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
panel <- panel %>% left_join(
  training_sample %>% select(vat, year, revenue, proxy_tabachova),
  by = c("vat", "year")
)
rm(training_sample, syt)

N <- nrow(panel)
cat(sprintf("  Panel: %d firm-years, %d sectors\n", N, length(unique(panel$nace2d))))

# Sector size filter (Table 1)
firms_per_sector <- panel %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")
sectors_A <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_SECTOR_CV) %>%
  pull(primary_nace2d)
idx_A <- which(panel$primary_nace2d %in% sectors_A)

# =============================================================================
# PARETO/GPA CALIBRATION FUNCTIONS (from diagnostic_redistribution_comparison.R)
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

calibrate_pareto <- function(panel, proxy_raw, fold_k) {
  result <- rep(NA_real_, nrow(panel))
  sy_key <- paste(panel$nace2d, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)
  folds <- sort(unique(fold_k))

  for (k in folds) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)

    # Fold-specific reference distribution and GPA fit
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

      emitter_mask <- proxy_raw[idx] > 0
      n_emit <- sum(emitter_mask)

      if (n_emit < 2 || is.null(gpa_params)) {
        # Fallback: proportional
        raw <- proxy_raw[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) result[idx] <- E_target * (raw / denom)
        else result[idx] <- E_target / length(idx)
        next
      }

      result[idx[!emitter_mask]] <- 0

      emit_idx <- idx[emitter_mask]
      ranks <- rank(proxy_raw[emit_idx], ties.method = "average")
      p_i <- (ranks - 0.5) / n_emit

      w_i <- tryCatch(quagpa(p_i, gpa_params), error = function(e) rep(NA, n_emit))
      if (any(is.na(w_i)) || any(!is.finite(w_i))) {
        raw <- proxy_raw[idx]
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

calibrate_qmap <- function(panel, proxy_raw, fold_k) {
  result <- rep(NA_real_, nrow(panel))
  sy_key <- paste(panel$nace2d, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)
  folds <- sort(unique(fold_k))

  for (k in folds) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)

    ref_k <- build_reference_dist(panel, fold_k, k)

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

      emitter_mask <- proxy_raw[idx] > 0
      n_emit <- sum(emitter_mask)

      if (n_emit < 2 || is.null(ref_k)) {
        raw <- proxy_raw[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) result[idx] <- E_target * (raw / denom)
        else result[idx] <- E_target / length(idx)
        next
      }

      result[idx[!emitter_mask]] <- 0

      emit_idx <- idx[emitter_mask]
      ranks <- rank(proxy_raw[emit_idx], ties.method = "average")
      p_i <- (ranks - 0.5) / n_emit
      d_i <- quantile(ref_k, probs = p_i, type = 7, names = FALSE)

      f <- function(cc) sum(exp(cc + d_i)) - E_target
      c_sol <- tryCatch(
        uniroot(f,
                lower = log(E_target) - max(d_i) - 30,
                upper = log(E_target) - min(d_i) + 30,
                tol = 1e-10)$root,
        error = function(e) NA
      )

      if (is.na(c_sol)) {
        raw <- proxy_raw[idx]
        denom <- sum(raw, na.rm = TRUE)
        if (denom > 0) result[idx] <- E_target * (raw / denom)
        else result[idx] <- E_target / length(idx)
        next
      }

      result[emit_idx] <- exp(c_sol + d_i)
    }
  }
  result
}

# =============================================================================
# TABLE 1: Main results (sector CV) — add EN (Pareto) row
# =============================================================================
cat("\n── TABLE 1: Main results (sector CV, Panel A) ──\n")

metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")
extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

models_t1 <- c("Revenue", "EN (sinh)", "EN (Pareto)", "NACE", "Gated Rev")
metrics_t1 <- array(NA_real_,
                    dim = c(M, length(metric_names), length(models_t1)),
                    dimnames = list(NULL, metric_names, models_t1))

# Revenue and NACE: deterministic
cat("  Revenue and NACE (deterministic)...\n")
fold_k_det <- assign_folds(panel, "sector", K_sec, BASE_SEED + 1L)

yhat_rev <- calibrate_sector(panel, panel$revenue, fold_k_det)
mA_rev <- calc_metrics(panel$y[idx_A], yhat_rev[idx_A], fp_threshold = 0,
                       nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
mA_rev_vec <- extract_metrics(mA_rev)

yhat_nace <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_det)
mA_nace <- calc_metrics(panel$y[idx_A], yhat_nace[idx_A], fp_threshold = 0,
                        nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
mA_nace_vec <- extract_metrics(mA_nace)

for (r in seq_len(M)) {
  metrics_t1[r, , "Revenue"] <- mA_rev_vec
  metrics_t1[r, , "NACE"]    <- mA_nace_vec
}

# EN (sinh), EN (Pareto), Gated Rev: loop over M repeats
cat(sprintf("  EN sinh, EN Pareto, Gated Rev (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])

  # EN (sinh)
  yhat_en <- calibrate_sector(panel, en_levels, fold_k)
  m_en <- calc_metrics(panel$y[idx_A], yhat_en[idx_A], fp_threshold = 0,
                       nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_t1[r, , "EN (sinh)"] <- extract_metrics(m_en)

  # EN (Pareto)
  yhat_par <- calibrate_pareto(panel, en_levels, fold_k)
  m_par <- calc_metrics(panel$y[idx_A], yhat_par[idx_A], fp_threshold = 0,
                        nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_t1[r, , "EN (Pareto)"] <- extract_metrics(m_par)

  # Gated Revenue
  gated_score <- as.numeric(en_levels > 0) * panel$revenue
  yhat_gated <- calibrate_sector(panel, gated_score, fold_k)
  m_gated <- calc_metrics(panel$y[idx_A], yhat_gated[idx_A], fp_threshold = 0,
                          nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_t1[r, , "Gated Rev"] <- extract_metrics(m_gated)

  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

mean_t1 <- apply(metrics_t1, c(2, 3), mean, na.rm = TRUE)
sd_t1   <- apply(metrics_t1, c(2, 3), sd,   na.rm = TRUE)
rmse_baseline <- mean_t1["rmse", "Revenue"]

# Console output
cat("\nPanel A: Sector-level CV design\n")
cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
            "", "RMSE(kt)", "nRMSE", "MAPD", "Pearson", "Spearman",
            "FPR", "TPR", "p50", "p99"))
cat(paste(rep("-", 110), collapse = ""), "\n")
for (mod in models_t1) {
  determ <- mod %in% c("Revenue", "NACE")
  mn <- mean_t1[, mod]; s <- sd_t1[, mod]
  cat(sprintf("%-20s %10.1f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f\n",
              mod, mn["rmse"]/1e3, mn["rmse"]/rmse_baseline, mn["median_apd"],
              mn["pearson"], mn["spearman"],
              mn["fpr_nonemitters"], mn["tpr_emitters"],
              mn["avg_nonemit_p50_rank"], mn["avg_nonemit_p99_rank"]))
  if (!determ) {
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
}

# =============================================================================
# TABLE 2: Zero-emitter sectors (17/18, 19, 24) — add EN (Pareto) row
# =============================================================================
cat("\n── TABLE 2: Zero-emitter sectors (sector CV) ──\n")

# Helpers (from table_zero_emitters_concise.R)
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

# Revenue (deterministic)
yhat_rev_t2 <- calibrate_sector(panel, panel$revenue, fold_k_det)
rev_rmse_t2 <- setNames(sapply(grp_names, function(g)
  rmse_grp(panel$y, yhat_rev_t2, panel$nace2d, SECTOR_GROUPS[[g]])), grp_names)

# NACE (deterministic)
yhat_nace_t2 <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_det)

# EN sinh and Pareto: M repeats
t2_metric_names <- c("nrmse", "mapd", "pear", "spear", "fpr", "tpr", "fpp50", "fpmax")
en_sinh_t2 <- en_par_t2 <- array(NA_real_,
  dim = c(M, length(t2_metric_names), 3),
  dimnames = list(NULL, t2_metric_names, grp_names))

cat(sprintf("  Computing EN sinh and Pareto (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])

  yhat_sinh <- calibrate_sector(panel, en_levels, fold_k)
  yhat_par  <- calibrate_pareto(panel, en_levels, fold_k)

  for (g in grp_names) {
    codes <- SECTOR_GROUPS[[g]]
    en_sinh_t2[r, "nrmse", g] <- rmse_grp(panel$y, yhat_sinh, panel$nace2d, codes) / rev_rmse_t2[g]
    en_sinh_t2[r, "mapd",  g] <- mapd_emitters(panel$y, yhat_sinh, panel$nace2d, codes)
    en_sinh_t2[r, "pear",  g] <- pearson_grp(panel$y, yhat_sinh, panel$nace2d, codes)
    en_sinh_t2[r, "spear", g] <- spearman_grp(panel$y, yhat_sinh, panel$nace2d, codes)
    en_sinh_t2[r, "fpr",   g] <- fpr_grp(panel$y, yhat_sinh, panel$nace2d, codes)
    en_sinh_t2[r, "tpr",   g] <- tpr_grp(panel$y, yhat_sinh, panel$nace2d, codes)
    en_sinh_t2[r, "fpp50", g] <- fp_severity_rank(panel$y, yhat_sinh, panel$nace2d, panel$year, codes, "median")
    en_sinh_t2[r, "fpmax", g] <- fp_severity_rank(panel$y, yhat_sinh, panel$nace2d, panel$year, codes, "max")

    en_par_t2[r, "nrmse", g] <- rmse_grp(panel$y, yhat_par, panel$nace2d, codes) / rev_rmse_t2[g]
    en_par_t2[r, "mapd",  g] <- mapd_emitters(panel$y, yhat_par, panel$nace2d, codes)
    en_par_t2[r, "pear",  g] <- pearson_grp(panel$y, yhat_par, panel$nace2d, codes)
    en_par_t2[r, "spear", g] <- spearman_grp(panel$y, yhat_par, panel$nace2d, codes)
    en_par_t2[r, "fpr",   g] <- fpr_grp(panel$y, yhat_par, panel$nace2d, codes)
    en_par_t2[r, "tpr",   g] <- tpr_grp(panel$y, yhat_par, panel$nace2d, codes)
    en_par_t2[r, "fpp50", g] <- fp_severity_rank(panel$y, yhat_par, panel$nace2d, panel$year, codes, "median")
    en_par_t2[r, "fpmax", g] <- fp_severity_rank(panel$y, yhat_par, panel$nace2d, panel$year, codes, "max")
  }
  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

# Console output for Table 2
cat("\n=== Zero-emitter sectors: Revenue vs NACE vs EN(sinh) vs EN(Pareto) ===\n")
row_labels <- c("nRMSE", "MAPD", "Levels corr", "Rank corr", "FPR", "TPR", "Med FP rank", "Max FP rank")
for (i in seq_along(t2_metric_names)) {
  cat(sprintf("\n%-15s", row_labels[i]))
  for (g in grp_names) {
    codes <- SECTOR_GROUPS[[g]]
    # Revenue
    rv <- switch(t2_metric_names[i],
      nrmse = 1.000,
      mapd  = mapd_emitters(panel$y, yhat_rev_t2, panel$nace2d, codes),
      pear  = pearson_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      spear = spearman_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      fpr   = fpr_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      tpr   = tpr_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      fpp50 = fp_severity_rank(panel$y, yhat_rev_t2, panel$nace2d, panel$year, codes, "median"),
      fpmax = fp_severity_rank(panel$y, yhat_rev_t2, panel$nace2d, panel$year, codes, "max")
    )
    nv <- switch(t2_metric_names[i],
      nrmse = rmse_grp(panel$y, yhat_nace_t2, panel$nace2d, codes) / rev_rmse_t2[g],
      mapd  = mapd_emitters(panel$y, yhat_nace_t2, panel$nace2d, codes),
      pear  = pearson_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      spear = spearman_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      fpr   = fpr_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      tpr   = tpr_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      fpp50 = fp_severity_rank(panel$y, yhat_nace_t2, panel$nace2d, panel$year, codes, "median"),
      fpmax = fp_severity_rank(panel$y, yhat_nace_t2, panel$nace2d, panel$year, codes, "max")
    )
    es_m <- mean(en_sinh_t2[, t2_metric_names[i], g], na.rm = TRUE)
    es_s <- sd(en_sinh_t2[, t2_metric_names[i], g], na.rm = TRUE)
    ep_m <- mean(en_par_t2[, t2_metric_names[i], g], na.rm = TRUE)
    ep_s <- sd(en_par_t2[, t2_metric_names[i], g], na.rm = TRUE)
    cat(sprintf("  %s: R %.3f N %.3f S %.3f(%.3f) P %.3f(%.3f)",
                g, rv, nv, es_m, es_s, ep_m, ep_s))
  }
}
cat("\n")

# =============================================================================
# TABLE 3: Three-method comparison (EN proxy only)
# =============================================================================
cat("\n── TABLE 3: Redistribution method comparison (EN only) ──\n")

gini_coef <- function(x) {
  x <- sort(x[x > 0])
  n <- length(x)
  if (n < 2) return(NA_real_)
  (2 * sum(x * seq_len(n))) / (n * sum(x)) - (n + 1) / n
}

cell_dist_stats <- function(x) {
  x <- x[x > 0]
  n <- length(x)
  if (n < MIN_CELL) return(rep(NA_real_, 5))
  q90 <- quantile(x, 0.9, names = FALSE)
  q10 <- quantile(x, 0.1, names = FALSE)
  lm_ratios <- tryCatch({
    lm <- samlmu(log(x), nmom = 4)
    c(lm[3], lm[4])
  }, error = function(e) c(NA_real_, NA_real_))
  c(p90p10 = q90/q10, log_gap = log(q90) - log(q10), gini = gini_coef(x),
    tau3 = lm_ratios[1], tau4 = lm_ratios[2])
}

METHOD_NAMES <- c("Sinh-calibrated", "Quantile mapping", "Pareto (GPA)")
STAT_NAMES <- c("p90p10", "log_gap", "gini", "tau3", "tau4")
N_METHODS <- 3L; N_STATS <- 5L

repeat_bias <- array(NA_real_, dim = c(M, N_STATS, N_METHODS),
                     dimnames = list(NULL, STAT_NAMES, METHOD_NAMES))
repeat_rmse_dist <- array(NA_real_, dim = c(M, N_STATS, N_METHODS),
                          dimnames = list(NULL, STAT_NAMES, METHOD_NAMES))
repeat_pearson  <- matrix(NA_real_, M, N_METHODS, dimnames = list(NULL, METHOD_NAMES))
repeat_spearman <- matrix(NA_real_, M, N_METHODS, dimnames = list(NULL, METHOD_NAMES))
repeat_rmse_lev <- matrix(NA_real_, M, N_METHODS, dimnames = list(NULL, METHOD_NAMES))

cat(sprintf("  Running %d repeats...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])

  yhat_A <- calibrate_sector(panel, en_levels, fold_k)
  yhat_B <- calibrate_qmap(panel, en_levels, fold_k)
  yhat_C <- calibrate_pareto(panel, en_levels, fold_k)
  yhats <- list(yhat_A, yhat_B, yhat_C)

  # Cell-level distributional stats
  sy_key <- paste(panel$nace2d, panel$year)
  cells <- unique(sy_key)
  cell_devs <- lapply(1:N_METHODS, function(m) list())

  for (cell in cells) {
    idx <- which(sy_key == cell)
    actual_stats <- cell_dist_stats(panel$y[idx])
    if (any(is.na(actual_stats))) next
    if (sum(en_levels[idx] > 0) < MIN_CELL) next
    for (m in 1:N_METHODS) {
      imp_stats <- cell_dist_stats(yhats[[m]][idx])
      cell_devs[[m]][[length(cell_devs[[m]]) + 1]] <- imp_stats - actual_stats
    }
  }

  for (m in 1:N_METHODS) {
    dev_mat <- do.call(rbind, cell_devs[[m]])
    if (is.null(dev_mat) || nrow(dev_mat) == 0) next
    repeat_bias[r, , m] <- colMeans(dev_mat, na.rm = TRUE)
    repeat_rmse_dist[r, , m] <- sqrt(colMeans(dev_mat^2, na.rm = TRUE))
  }

  emit <- panel$y > 0
  for (m in 1:N_METHODS) {
    repeat_pearson[r, m]  <- cor(panel$y[emit], yhats[[m]][emit], use = "complete.obs")
    repeat_spearman[r, m] <- cor(panel$y[emit], yhats[[m]][emit], method = "spearman", use = "complete.obs")
    repeat_rmse_lev[r, m] <- sqrt(mean((panel$y[emit] - yhats[[m]][emit])^2, na.rm = TRUE))
  }

  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

# Console output for Table 3
cat("\n── Distributional bias (imputed − actual) ──\n")
for (s in STAT_NAMES) {
  cat(sprintf("  %-10s", s))
  for (m in 1:N_METHODS) {
    cat(sprintf("  %s: %7.3f (%.3f)", METHOD_NAMES[m],
                mean(repeat_bias[, s, m], na.rm = TRUE),
                sd(repeat_bias[, s, m], na.rm = TRUE) / sqrt(M)))
  }
  cat("\n")
}
cat("\n── Distributional RMSE ──\n")
for (s in STAT_NAMES) {
  cat(sprintf("  %-10s", s))
  for (m in 1:N_METHODS) {
    cat(sprintf("  %s: %7.3f (%.3f)", METHOD_NAMES[m],
                mean(repeat_rmse_dist[, s, m], na.rm = TRUE),
                sd(repeat_rmse_dist[, s, m], na.rm = TRUE) / sqrt(M)))
  }
  cat("\n")
}
cat("\n── Firm-level metrics (emitters) ──\n")
cat(sprintf("  %-10s", "Pearson"))
for (m in 1:N_METHODS) cat(sprintf("  %s: %.3f (%.3f)", METHOD_NAMES[m],
  mean(repeat_pearson[, m], na.rm=TRUE), sd(repeat_pearson[, m], na.rm=TRUE)/sqrt(M)))
cat("\n")
cat(sprintf("  %-10s", "Spearman"))
for (m in 1:N_METHODS) cat(sprintf("  %s: %.3f (%.3f)", METHOD_NAMES[m],
  mean(repeat_spearman[, m], na.rm=TRUE), sd(repeat_spearman[, m], na.rm=TRUE)/sqrt(M)))
cat("\n")
cat(sprintf("  %-10s", "RMSE"))
for (m in 1:N_METHODS) cat(sprintf("  %s: %.0f (%.0f)", METHOD_NAMES[m],
  mean(repeat_rmse_lev[, m], na.rm=TRUE), sd(repeat_rmse_lev[, m], na.rm=TRUE)/sqrt(M)))
cat("\n")

# =============================================================================
# SAVE ALL RESULTS
# =============================================================================
all_results <- list(
  # Table 1
  metrics_t1 = metrics_t1, mean_t1 = mean_t1, sd_t1 = sd_t1,
  rmse_baseline = rmse_baseline, models_t1 = models_t1,
  # Table 2
  en_sinh_t2 = en_sinh_t2, en_par_t2 = en_par_t2,
  rev_rmse_t2 = rev_rmse_t2,
  # Table 3
  repeat_bias = repeat_bias, repeat_rmse_dist = repeat_rmse_dist,
  repeat_pearson = repeat_pearson, repeat_spearman = repeat_spearman,
  repeat_rmse_lev = repeat_rmse_lev,
  METHOD_NAMES = METHOD_NAMES, STAT_NAMES = STAT_NAMES,
  M = M, K_sec = K_sec
)
rds_path <- file.path(OUTPUT_DIR, "table_pareto_redistribution.rds")
saveRDS(all_results, rds_path)
cat("\nAll results saved to:", rds_path, "\n")

# =============================================================================
# LATEX TABLES
# =============================================================================
fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt1 <- function(x)             formatC(x, format = "f", digits = 1)

# ── Table 1: Main results with Pareto ────────────────────────────────────────
tex_row_t1 <- function(lbl, mn, s, bl, determ = FALSE) {
  mean_line <- sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
    lbl, fmt1(mn["rmse"]/1e3), fmt(mn["rmse"]/bl), fmt(mn["median_apd"]),
    fmt(mn["pearson"]), fmt(mn["spearman"]),
    fmt(mn["fpr_nonemitters"]), fmt(mn["tpr_emitters"]),
    fmt(mn["avg_nonemit_p50_rank"]), fmt(mn["avg_nonemit_p99_rank"]))
  if (determ) {
    sd_line <- " & & & & & & & & & \\\\"
  } else {
    sd_line <- sprintf(" & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} \\\\",
      fmt1(s["rmse"]/1e3), fmt(s["rmse"]/bl), fmt(s["median_apd"]),
      fmt(s["pearson"]), fmt(s["spearman"]),
      fmt(s["fpr_nonemitters"]), fmt(s["tpr_emitters"]),
      fmt(s["avg_nonemit_p50_rank"]), fmt(s["avg_nonemit_p99_rank"]))
  }
  c(mean_line, sd_line)
}

tex_t1 <- c(
  "\\begin{tabular}{l ccc cc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction error} & \\multicolumn{2}{c}{Correlation} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-10}",
  " & RMSE & nRMSE & MAPD & Levels & Rank & FPR & TPR & p50 & p99 \\\\",
  "\\midrule",
  tex_row_t1("Revenue",       mean_t1[, "Revenue"],     sd_t1[, "Revenue"],     rmse_baseline, determ = TRUE),
  tex_row_t1("Elastic Net",   mean_t1[, "EN (sinh)"],   sd_t1[, "EN (sinh)"],   rmse_baseline),
  tex_row_t1("EN (Pareto)",   mean_t1[, "EN (Pareto)"], sd_t1[, "EN (Pareto)"], rmse_baseline),
  tex_row_t1("NACE",          mean_t1[, "NACE"],        sd_t1[, "NACE"],        rmse_baseline, determ = TRUE),
  tex_row_t1("Gated Rev.",    mean_t1[, "Gated Rev"],   sd_t1[, "Gated Rev"],   rmse_baseline),
  "\\bottomrule",
  "\\end{tabular}"
)
tex_path_t1 <- file.path(OUTPUT_DIR, "table_main_results_sector_cv_pareto.tex")
writeLines(tex_t1, tex_path_t1)
cat("LaTeX Table 1 written to:", tex_path_t1, "\n")

# ── Table 2: Zero-emitter sectors with Pareto ────────────────────────────────
SECTOR_LABELS <- c("Paper \\& printing", "Petroleum refining", "Iron \\& steel")
row_names_t2 <- c("nRMSE", "MAPD", "Levels corr.", "Rank corr.",
                   "FPR", "TPR", "Med.\\ FP rank", "Max FP rank")

tex_t2 <- c(
  "\\begin{tabular}{l cccc cccc cccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} \\\\",
          SECTOR_LABELS[1], SECTOR_LABELS[2], SECTOR_LABELS[3]),
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9} \\cmidrule(lr){10-13}",
  paste0(" & ", paste(rep("Rev. & NACE & EN & Par.", 3), collapse = " & "), " \\\\"),
  "\\midrule"
)

for (i in seq_along(t2_metric_names)) {
  parts <- character(0)
  sd_parts <- character(0)
  for (g in grp_names) {
    codes <- SECTOR_GROUPS[[g]]
    rv <- switch(t2_metric_names[i],
      nrmse = 1.000,
      mapd  = mapd_emitters(panel$y, yhat_rev_t2, panel$nace2d, codes),
      pear  = pearson_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      spear = spearman_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      fpr   = fpr_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      tpr   = tpr_grp(panel$y, yhat_rev_t2, panel$nace2d, codes),
      fpp50 = fp_severity_rank(panel$y, yhat_rev_t2, panel$nace2d, panel$year, codes, "median"),
      fpmax = fp_severity_rank(panel$y, yhat_rev_t2, panel$nace2d, panel$year, codes, "max"))
    nv <- switch(t2_metric_names[i],
      nrmse = rmse_grp(panel$y, yhat_nace_t2, panel$nace2d, codes) / rev_rmse_t2[g],
      mapd  = mapd_emitters(panel$y, yhat_nace_t2, panel$nace2d, codes),
      pear  = pearson_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      spear = spearman_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      fpr   = fpr_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      tpr   = tpr_grp(panel$y, yhat_nace_t2, panel$nace2d, codes),
      fpp50 = fp_severity_rank(panel$y, yhat_nace_t2, panel$nace2d, panel$year, codes, "median"),
      fpmax = fp_severity_rank(panel$y, yhat_nace_t2, panel$nace2d, panel$year, codes, "max"))
    es_m <- mean(en_sinh_t2[, t2_metric_names[i], g], na.rm = TRUE)
    es_s <- sd(en_sinh_t2[, t2_metric_names[i], g], na.rm = TRUE)
    ep_m <- mean(en_par_t2[, t2_metric_names[i], g], na.rm = TRUE)
    ep_s <- sd(en_par_t2[, t2_metric_names[i], g], na.rm = TRUE)
    parts <- c(parts, fmt(rv), fmt(nv), fmt(es_m), fmt(ep_m))
    sd_parts <- c(sd_parts, "", "", sprintf("{\\scriptsize(%s)}", fmt(es_s)),
                  sprintf("{\\scriptsize(%s)}", fmt(ep_s)))
  }
  tex_t2 <- c(tex_t2,
    sprintf("%s & %s \\\\", row_names_t2[i], paste(parts, collapse = " & ")),
    sprintf(" & %s \\\\", paste(sd_parts, collapse = " & ")))
}
tex_t2 <- c(tex_t2, "\\bottomrule", "\\end{tabular}")

tex_path_t2 <- file.path(OUTPUT_DIR, "table_zero_emitters_sector_cv_pareto.tex")
writeLines(tex_t2, tex_path_t2)
cat("LaTeX Table 2 written to:", tex_path_t2, "\n")

# ── Table 3: Method comparison ───────────────────────────────────────────────
tex_t3 <- c(
  "\\begin{tabular}{l ccc}",
  "\\toprule",
  " & Sinh-calibrated & Quantile mapping & Pareto (GPA) \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Firm-level metrics (emitters only)}} \\\\"
)
for (metric in c("Pearson", "Spearman", "RMSE")) {
  mat <- switch(metric, Pearson = repeat_pearson, Spearman = repeat_spearman, RMSE = repeat_rmse_lev)
  f <- if (metric == "RMSE") function(x) formatC(x, format = "f", digits = 0) else fmt
  vals <- paste(sapply(1:N_METHODS, function(m) {
    mn <- mean(mat[, m], na.rm = TRUE)
    s  <- sd(mat[, m], na.rm = TRUE) / sqrt(M)
    sprintf("%s {\\scriptsize(%s)}", f(mn), f(s))
  }), collapse = " & ")
  tex_t3 <- c(tex_t3, sprintf("%s & %s \\\\", metric, vals))
}

tex_t3 <- c(tex_t3,
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Distributional bias (imputed $-$ actual, mean across cells)}} \\\\"
)
stat_labels <- c("$p_{90}/p_{10}$", "$\\log p_{90} - \\log p_{10}$", "Gini",
                 "$\\tau_3$ (L-skew)", "$\\tau_4$ (L-kurt)")
for (si in seq_along(STAT_NAMES)) {
  vals <- paste(sapply(1:N_METHODS, function(m) {
    mn <- mean(repeat_bias[, STAT_NAMES[si], m], na.rm = TRUE)
    s  <- sd(repeat_bias[, STAT_NAMES[si], m], na.rm = TRUE) / sqrt(M)
    sprintf("%s {\\scriptsize(%s)}", fmt(mn), fmt(s))
  }), collapse = " & ")
  tex_t3 <- c(tex_t3, sprintf("%s & %s \\\\", stat_labels[si], vals))
}

tex_t3 <- c(tex_t3,
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Distributional RMSE (across cells)}} \\\\"
)
for (si in seq_along(STAT_NAMES)) {
  vals <- paste(sapply(1:N_METHODS, function(m) {
    mn <- mean(repeat_rmse_dist[, STAT_NAMES[si], m], na.rm = TRUE)
    s  <- sd(repeat_rmse_dist[, STAT_NAMES[si], m], na.rm = TRUE) / sqrt(M)
    sprintf("%s {\\scriptsize(%s)}", fmt(mn), fmt(s))
  }), collapse = " & ")
  tex_t3 <- c(tex_t3, sprintf("%s & %s \\\\", stat_labels[si], vals))
}

tex_t3 <- c(tex_t3, "\\bottomrule", "\\end{tabular}")

tex_path_t3 <- file.path(OUTPUT_DIR, "table_redistribution_comparison.tex")
writeLines(tex_t3, tex_path_t3)
cat("LaTeX Table 3 written to:", tex_path_t3, "\n")

cat("\nDone.\n")
