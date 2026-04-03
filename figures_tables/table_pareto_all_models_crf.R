###############################################################################
# figures_tables/table_pareto_all_models_crf.R
#
# PURPOSE
#   Evaluate prediction performance with CRF-group-level folds and
#   CRF-group-level calibration. Produces two tables:
#     1. Main results pooled across all CRF groups (Panel A)
#     2. Zero-emitter CRF groups: paper (17/18), metals (24/25),
#        refining (19) — the groups with structural non-emitters
#
#   Uses Pareto (GPA) redistribution for all models.
#
#   Key differences from table_pareto_all_models.R:
#     - Folds are defined over CRF groups, not NACE 2d sectors
#     - Calibration distributes within CRF-group x year cells
#     - EN proxy comes from repeated_cv_proxy_crf_asinh.RData
#       (estimated with CRF-level holdouts on RMD)
#     - Metrics are computed within CRF groups
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_crf_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_results_crf_cv_pareto.tex
#   {OUTPUT_DIR}/table_zero_emitters_crf_cv_pareto.tex
#   {OUTPUT_DIR}/table_pareto_all_models_crf.rds
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

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED           <- 2026L
K_crf               <- 5L
M                   <- 20L
MIN_FIRMS_GROUP     <- 3L

# CRF groups with structural non-emitters (for Table 2)
ZERO_EMITTER_GROUPS <- list(
  "paper"    = c("17", "18", "17/18"),
  "metals"   = c("24", "25"),
  "refining" = c("19")
)

cat("================================================================\n")
cat("  PARETO REDISTRIBUTION — ALL MODELS (CRF-group CV)\n")
cat("  M =", M, "repeats, K =", K_crf, "folds\n")
cat("================================================================\n\n")

# =============================================================================
# CRF GROUP DEFINITIONS (must match build_repeated_cv_proxy_crf.R)
# =============================================================================
crf_group_map <- data.frame(
  nace2d = c(
    "35",
    "19",
    "24", "25",
    "20", "21",
    "17", "18", "17/18",
    "10", "11", "12",
    "23",
    "05", "06", "07", "08", "09",
    "13", "14", "15", "16", "22",
    "26", "27", "28", "29", "30", "31", "32", "33",
    "41", "42", "43",
    "36", "37", "38", "39",
    "45", "46", "47",
    "52", "53",
    "55", "56",
    "58", "59", "60", "61", "62", "63",
    "64", "65", "66",
    "68", "69", "70", "71", "72", "73", "74", "75",
    "77", "78", "79", "80", "81", "82",
    "84", "85", "86", "87", "88",
    "90", "91", "92", "93", "94", "95", "96",
    "01", "02", "03",
    "49", "50", "51"
  ),
  crf_group = c(
    "energy",
    "refining",
    "metals", "metals",
    "chemicals", "chemicals",
    "paper", "paper", "paper",
    "food", "food", "food",
    "minerals",
    rep("mfg_other", 5),
    rep("mfg_other", 5),
    rep("mfg_other", 8),
    rep("mfg_other", 3),
    rep("commercial", 4),
    rep("commercial", 3),
    rep("commercial", 2),
    rep("commercial", 2),
    rep("commercial", 6),
    rep("commercial", 3),
    rep("commercial", 8),
    rep("commercial", 6),
    rep("commercial", 5),
    rep("commercial", 7),
    "agriculture", "agriculture", "agriculture",
    "transport", "transport", "transport"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading EN proxy (CRF-group CV)...\n")
e_crf <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData"), envir = e_crf)
proxy_matrix_crf <- e_crf$proxy_matrix[, 1:M, drop = FALSE]
panel            <- e_crf$repeated_cv_proxy_panel
rm(e_crf)

# Merge 17/18
panel$primary_nace2d[panel$primary_nace2d %in% c("17", "18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17", "18")] <- "17/18"

# Assign CRF group (use the column from the proxy panel if available,
# otherwise derive from nace2d)
if (!"primary_crf_group" %in% names(panel)) {
  panel <- panel %>%
    left_join(crf_group_map %>% distinct(nace2d, .keep_all = TRUE),
              by = c("primary_nace2d" = "nace2d")) %>%
    rename(primary_crf_group = crf_group)
}

cat("Loading training sample (for revenue and NACE proxies)...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
panel <- panel %>% left_join(
  training_sample %>% select(vat, year, revenue, proxy_tabachova),
  by = c("vat", "year")
)
rm(training_sample, syt)

N <- nrow(panel)
cat(sprintf("  Panel: %d firm-years, %d CRF groups\n", N,
            length(unique(panel$primary_crf_group))))

# CRF group size filter
firms_per_group <- panel %>%
  distinct(vat, primary_crf_group) %>%
  count(primary_crf_group, name = "n_firms")
groups_A <- firms_per_group %>%
  filter(n_firms >= MIN_FIRMS_GROUP) %>%
  pull(primary_crf_group)
idx_A <- which(panel$primary_crf_group %in% groups_A)

cat(sprintf("  CRF groups with >= %d firms: %d of %d\n",
            MIN_FIRMS_GROUP, length(groups_A),
            length(unique(panel$primary_crf_group))))

# =============================================================================
# BACK-TRANSFORM HELPER
# =============================================================================
proxy_to_levels <- function(proxy) pmax(sinh(proxy), 0)

# =============================================================================
# CRF-GROUP-LEVEL FOLD ASSIGNMENT
# Must match the logic in build_repeated_cv_proxy_crf.R
# =============================================================================
assign_folds_crf <- function(panel, K, seed) {
  set.seed(seed)
  crf_groups <- sort(unique(panel$primary_crf_group[!is.na(panel$primary_crf_group)]))
  group_folds <- sample(rep(1:K, length.out = length(crf_groups)))
  gfm <- data.frame(primary_crf_group = crf_groups, fold_k = group_folds,
                     stringsAsFactors = FALSE)
  fold_k <- gfm$fold_k[match(panel$primary_crf_group, gfm$primary_crf_group)]
  fold_k
}

# =============================================================================
# PARETO CALIBRATION (CRF-group x year cells)
# =============================================================================

build_reference_dist <- function(panel, fold_k, k) {
  train_idx <- which(fold_k != k & panel$y > 0)
  if (length(train_idx) < 20) return(NULL)
  df <- data.frame(
    log_y      = log(panel$y[train_idx]),
    year       = panel$year[train_idx],
    crf_group  = panel$primary_crf_group[train_idx],
    stringsAsFactors = FALSE
  )
  mu_t <- tapply(df$log_y, df$year, mean)
  df$tilde <- df$log_y - mu_t[as.character(df$year)]
  mu_s <- tapply(df$tilde, df$crf_group, mean)
  df$d <- df$tilde - mu_s[df$crf_group]
  sort(df$d)
}

calibrate_pareto_crf <- function(panel, ranking_signal, fold_k) {
  result <- rep(NA_real_, nrow(panel))
  # Calibration cells: CRF group x year
  sy_key <- paste(panel$primary_crf_group, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)
  folds <- sort(unique(fold_k))

  for (k in folds) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)

    ref_k <- build_reference_dist(panel, fold_k, k)
    gpa_params <- if (!is.null(ref_k) && length(ref_k) >= 20) {
      tryCatch(pelgpa(samlmu(ref_k, nmom = 3)), error = function(e) NULL)
    } else NULL

    sy_train <- paste(panel$primary_crf_group[train_idx], panel$year[train_idx])
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
# METRICS HELPERS (for CRF-group-level Table 2)
# =============================================================================
# These compute metrics within a CRF group, identified by the NACE 2d codes
# that belong to it.

mapd_emitters <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes & y > 0)
  if (length(idx) < 1) return(NA_real_)
  median(abs(y[idx] - yhat[idx]) / y[idx])
}
rmse_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  sqrt(mean((y[idx] - yhat[idx])^2))
}
pearson_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  y_s <- y[idx]; yhat_s <- yhat[idx]
  if (length(y_s) >= 3 && sd(y_s) > 0 && sd(yhat_s) > 0)
    suppressWarnings(cor(y_s, yhat_s, method = "pearson", use = "complete.obs"))
  else NA_real_
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

# =============================================================================
# POOLED METRICS (calc_metrics uses nace2d for FP severity; here we pass
# nace2d as-is since the FP severity sectors 19, 24, 17/18 are unchanged)
# =============================================================================
metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")
extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

# =============================================================================
# TABLE 1: Main results — all models with CRF-group CV and Pareto
# =============================================================================
cat("\n-- TABLE 1: Main results (all models, CRF-group CV, Pareto) --\n")

models <- c("Revenue", "Elastic Net", "NACE", "Gated Rev")
metrics_A <- array(NA_real_,
                   dim = c(M, length(metric_names), length(models)),
                   dimnames = list(NULL, metric_names, models))

cat(sprintf("  All models (%d repeats, CRF-group Pareto calibration)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds_crf(panel, K_crf, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_crf[, r])

  # Revenue
  yhat_rev <- calibrate_pareto_crf(panel, panel$revenue, fold_k)
  m_rev <- calc_metrics(panel$y[idx_A], yhat_rev[idx_A], fp_threshold = 0,
                        nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Revenue"] <- extract_metrics(m_rev)

  # Elastic Net
  yhat_en <- calibrate_pareto_crf(panel, en_levels, fold_k)
  m_en <- calc_metrics(panel$y[idx_A], yhat_en[idx_A], fp_threshold = 0,
                       nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "Elastic Net"] <- extract_metrics(m_en)

  # NACE
  yhat_nace <- calibrate_pareto_crf(panel, panel$proxy_tabachova, fold_k)
  m_nace <- calc_metrics(panel$y[idx_A], yhat_nace[idx_A], fp_threshold = 0,
                         nace2d = panel$nace2d[idx_A], year = panel$year[idx_A])
  metrics_A[r, , "NACE"] <- extract_metrics(m_nace)

  # Gated Revenue
  gated_score <- as.numeric(en_levels > 0) * panel$revenue
  yhat_gated <- calibrate_pareto_crf(panel, gated_score, fold_k)
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
cat("\nPanel A: CRF-group-level CV, Pareto redistribution\n")
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
# TABLE 2: Zero-emitter CRF groups — all models with Pareto
# =============================================================================
cat("\n-- TABLE 2: Zero-emitter CRF groups (all models, Pareto) --\n")

grp_names <- names(ZERO_EMITTER_GROUPS)
t2_metric_names <- c("nrmse", "mapd", "pear", "spear", "fpr", "tpr", "fpp50", "fpmax")

rev_rmse_by_grp <- matrix(NA_real_, M, 3, dimnames = list(NULL, grp_names))

t2_results <- array(NA_real_,
  dim = c(M, length(t2_metric_names), 3, length(models)),
  dimnames = list(NULL, t2_metric_names, grp_names, models))

cat(sprintf("  All models (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds_crf(panel, K_crf, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_crf[, r])
  gated_score <- as.numeric(en_levels > 0) * panel$revenue

  signals <- list(
    "Revenue"     = panel$revenue,
    "Elastic Net" = en_levels,
    "NACE"        = panel$proxy_tabachova,
    "Gated Rev"   = gated_score
  )

  for (mod in models) {
    yhat <- calibrate_pareto_crf(panel, signals[[mod]], fold_k)

    for (g in grp_names) {
      codes <- ZERO_EMITTER_GROUPS[[g]]
      t2_results[r, "nrmse", g, mod] <- rmse_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "mapd",  g, mod] <- mapd_emitters(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "pear",  g, mod] <- pearson_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "spear", g, mod] <- spearman_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "fpr",   g, mod] <- fpr_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "tpr",   g, mod] <- tpr_grp(panel$y, yhat, panel$nace2d, codes)
      t2_results[r, "fpp50", g, mod] <- fp_severity_rank(panel$y, yhat, panel$nace2d,
                                                          panel$year, codes, "median")
      t2_results[r, "fpmax", g, mod] <- fp_severity_rank(panel$y, yhat, panel$nace2d,
                                                          panel$year, codes, "max")
    }
  }

  for (g in grp_names) rev_rmse_by_grp[r, g] <- t2_results[r, "nrmse", g, "Revenue"]

  if (r %% 5 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Done.\n")

# Normalize nrmse by revenue RMSE
for (mod in models) {
  for (g in grp_names) {
    t2_results[, "nrmse", g, mod] <- t2_results[, "nrmse", g, mod] / rev_rmse_by_grp[, g]
  }
}

# Console output
cat("\n=== Zero-emitter CRF groups: all models with CRF-group CV Pareto ===\n")
row_labels <- c("nRMSE", "MAPD", "Levels corr", "Rank corr", "FPR", "TPR",
                "Med FP rank", "Max FP rank")
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
  M = M, K_crf = K_crf,
  ZERO_EMITTER_GROUPS = ZERO_EMITTER_GROUPS
)
rds_path <- file.path(OUTPUT_DIR, "table_pareto_all_models_crf.rds")
saveRDS(all_results, rds_path)
cat("\nResults saved to:", rds_path, "\n")

# =============================================================================
# LATEX TABLES
# =============================================================================
fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt1 <- function(x)             formatC(x, format = "f", digits = 1)

# ── Table 1: Main results ───────────────────────────────────────────────────
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
tex_path_t1 <- file.path(OUTPUT_DIR, "table_main_results_crf_cv_pareto.tex")
writeLines(tex_t1, tex_path_t1)
cat("LaTeX Table 1 written to:", tex_path_t1, "\n")

# ── Table 2: Zero-emitter CRF groups ────────────────────────────────────────
GROUP_LABELS <- c("Paper \\& printing", "Metals", "Petroleum refining")
row_names_t2 <- c("nRMSE", "MAPD", "Levels corr.", "Rank corr.",
                   "FPR", "TPR", "Med.\\ FP rank", "Max FP rank")

tex_t2 <- c(
  "\\begin{tabular}{l cccc cccc cccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} \\\\",
          GROUP_LABELS[1], GROUP_LABELS[2], GROUP_LABELS[3]),
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

tex_path_t2 <- file.path(OUTPUT_DIR, "table_zero_emitters_crf_cv_pareto.tex")
writeLines(tex_t2, tex_path_t2)
cat("LaTeX Table 2 written to:", tex_path_t2, "\n")

cat("\nDone.\n")
