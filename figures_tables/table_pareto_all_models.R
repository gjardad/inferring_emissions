###############################################################################
# figures_tables/table_pareto_all_models.R
#
# PURPOSE
#   Replicate table_main_results_sector_cv_design and
#   table_zero_emitters_sector_cv_design_concise with ALL models using
#   Pareto (GPA) redistribution. This isolates the ranking signal
#   (Revenue, EN, NACE, Gated Rev) from the redistribution method.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_results_sector_cv_pareto_all.tex
#   {OUTPUT_DIR}/table_zero_emitters_sector_cv_pareto_all.tex
#   {OUTPUT_DIR}/table_pareto_all_models.rds
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
M                   <- 20L
MIN_FIRMS_SECTOR_CV <- 3L
SECTOR_GROUPS       <- list("17/18" = "17/18", "19" = "19", "24" = "24")

cat("================================================================\n")
cat("  PARETO REDISTRIBUTION — ALL MODELS\n")
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

# Sector size filter
firms_per_sector <- panel %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")
sectors_A <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_SECTOR_CV) %>%
  pull(primary_nace2d)
idx_A <- which(panel$primary_nace2d %in% sectors_A)

# =============================================================================
# PARETO CALIBRATION (generic — works with any ranking signal)
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
  # ranking_signal: non-negative vector; used only for ranking within cells
  # Firms with ranking_signal == 0 are treated as non-emitters (get 0)
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
        # Fallback: proportional
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
# TABLE 1: Main results — all models with Pareto redistribution
# =============================================================================
cat("\n── TABLE 1: Main results (all models, Pareto redistribution) ──\n")

metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")
extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

models <- c("Revenue", "Elastic Net", "NACE", "Gated Rev")
metrics_A <- array(NA_real_,
                   dim = c(M, length(metric_names), length(models)),
                   dimnames = list(NULL, metric_names, models))

# Revenue and NACE: deterministic ranking, but Pareto redistribution
# still varies with fold assignment (reference distribution changes).
# However, Revenue uses ALL firms (all have revenue > 0), so the
# extensive margin is trivially 1.0 FPR / 1.0 TPR.
# NACE proxy_tabachova: some firms have 0, some > 0.

cat(sprintf("  All models (%d repeats, Pareto redistribution)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])

  # Revenue — ranking by revenue, Pareto shape
  yhat_rev <- calibrate_pareto(panel, panel$revenue, fold_k)
  m_rev <- calc_metrics(panel$y[idx_A], yhat_rev[idx_A], fp_threshold = 0,
                        nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Revenue"] <- extract_metrics(m_rev)

  # Elastic Net — ranking by EN proxy, Pareto shape
  yhat_en <- calibrate_pareto(panel, en_levels, fold_k)
  m_en <- calc_metrics(panel$y[idx_A], yhat_en[idx_A], fp_threshold = 0,
                       nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Elastic Net"] <- extract_metrics(m_en)

  # NACE — ranking by NACE proxy, Pareto shape
  yhat_nace <- calibrate_pareto(panel, panel$proxy_tabachova, fold_k)
  m_nace <- calc_metrics(panel$y[idx_A], yhat_nace[idx_A], fp_threshold = 0,
                         nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "NACE"] <- extract_metrics(m_nace)

  # Gated Revenue — ranking by 1(EN>0)*revenue, Pareto shape
  gated_score <- as.numeric(en_levels > 0) * panel$revenue
  yhat_gated <- calibrate_pareto(panel, gated_score, fold_k)
  m_gated <- calc_metrics(panel$y[idx_A], yhat_gated[idx_A], fp_threshold = 0,
                          nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Gated Rev"] <- extract_metrics(m_gated)

  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

mean_A <- apply(metrics_A, c(2, 3), mean, na.rm = TRUE)
sd_A   <- apply(metrics_A, c(2, 3), sd,   na.rm = TRUE)
rmse_baseline <- mean_A["rmse", "Revenue"]

# Console output
cat("\nPanel A: Sector-level CV, Pareto redistribution\n")
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

# =============================================================================
# TABLE 2: Zero-emitter sectors — all models with Pareto redistribution
# =============================================================================
cat("\n── TABLE 2: Zero-emitter sectors (all models, Pareto) ──\n")

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

# Revenue RMSE baseline (Pareto, deterministic fold for normalization)
# Use mean across repeats for consistency
rev_rmse_by_grp <- matrix(NA_real_, M, 3, dimnames = list(NULL, grp_names))

# Storage: M repeats × 8 metrics × 3 groups × 4 models
t2_results <- array(NA_real_,
  dim = c(M, length(t2_metric_names), 3, length(models)),
  dimnames = list(NULL, t2_metric_names, grp_names, models))

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
    yhat <- calibrate_pareto(panel, signals[[mod]], fold_k)

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

  # Store revenue RMSE for normalization
  for (g in grp_names) rev_rmse_by_grp[r, g] <- t2_results[r, "nrmse", g, "Revenue"]

  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

# Normalize nrmse by revenue RMSE (per repeat, per group)
for (mod in models) {
  for (g in grp_names) {
    t2_results[, "nrmse", g, mod] <- t2_results[, "nrmse", g, mod] / rev_rmse_by_grp[, g]
  }
}

# Console output
cat("\n=== Zero-emitter sectors: all models with Pareto redistribution ===\n")
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
# SAVE RESULTS
# =============================================================================
all_results <- list(
  metrics_A = metrics_A, mean_A = mean_A, sd_A = sd_A,
  rmse_baseline = rmse_baseline, models = models,
  t2_results = t2_results, rev_rmse_by_grp = rev_rmse_by_grp,
  M = M, K_sec = K_sec
)
rds_path <- file.path(OUTPUT_DIR, "table_pareto_all_models.rds")
saveRDS(all_results, rds_path)
cat("\nResults saved to:", rds_path, "\n")

# =============================================================================
# LATEX TABLES
# =============================================================================
fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt1 <- function(x)             formatC(x, format = "f", digits = 1)

# ── Table 1: Main results, all Pareto ────────────────────────────────────────
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

tex_t1 <- c(
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
tex_path_t1 <- file.path(OUTPUT_DIR, "table_main_results_sector_cv_pareto_all.tex")
writeLines(tex_t1, tex_path_t1)
cat("LaTeX Table 1 written to:", tex_path_t1, "\n")

# ── Table 2: Zero-emitter sectors, all Pareto ────────────────────────────────
SECTOR_LABELS <- c("Paper \\& printing", "Petroleum refining", "Iron \\& steel")
row_names_t2 <- c("nRMSE", "MAPD", "Levels corr.", "Rank corr.",
                   "FPR", "TPR", "Med.\\ FP rank", "Max FP rank")

tex_t2 <- c(
  "\\begin{tabular}{l cccc cccc cccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} \\\\",
          SECTOR_LABELS[1], SECTOR_LABELS[2], SECTOR_LABELS[3]),
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

tex_path_t2 <- file.path(OUTPUT_DIR, "table_zero_emitters_sector_cv_pareto_all.tex")
writeLines(tex_t2, tex_path_t2)
cat("LaTeX Table 2 written to:", tex_path_t2, "\n")

cat("\nDone.\n")
