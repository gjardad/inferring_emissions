###############################################################################
# analysis/active/combined_proxy_evaluation.R
#
# PURPOSE
#   Evaluate a combined proxy: score_i = EN_proxy_i^alpha * revenue_i^(1-alpha)
#   where alpha is cross-validated (tuned on training folds, applied to held-out).
#
#   The EN proxy gates who emits; revenue scales how much. The geometric mean
#   combines both signals. Alpha controls the weight on each.
#
# DESIGN
#   For each repeat r, for each outer fold k:
#     1. Training = folds != k (with cross-fitted EN proxies + true y)
#     2. Inner CV (K_inner=5) on training to select alpha from grid
#     3. Apply selected alpha to outer fold k held-out firms
#     4. Calibrate held-out firms as usual
#   Alpha is never evaluated on the data used to select it.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/repeated_cv_proxy_firm_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   Console: comparison table (Revenue, EN, NACE, Combined)
#   Console: alpha distribution across folds and repeats
#
# RUNS ON: local 1
###############################################################################

# ── Paths ──────────────────────────────────────────────────────────────────
REPO_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
  error = function(e) normalizePath(getwd(), winslash = "/")
)
while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration_helpers.R"))

# ── Parameters ─────────────────────────────────────────────────────────────
BASE_SEED           <- 2026L
K_sec               <- 5L
K_firm              <- 5L
K_INNER             <- 5L
MIN_FIRMS_SECTOR_CV <- 3L
MIN_FIRMS_FIRM_CV   <- 15L
ALPHA_GRID          <- seq(0, 1, by = 0.1)
M_USE               <- 50L   # use first 50 repeats for speed

# ── Load data ──────────────────────────────────────────────────────────────
cat("Loading data...\n")

e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel <- e_sec$repeated_cv_proxy_panel
rm(e_sec)

e_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_firm_asinh.RData"), envir = e_firm)
proxy_matrix_firm <- e_firm$proxy_matrix
rm(e_firm)

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
determ_df <- training_sample %>% select(vat, year, revenue, proxy_tabachova)
rm(training_sample, syt)

panel <- panel %>% left_join(determ_df, by = c("vat", "year"))
rm(determ_df)

M <- min(ncol(proxy_matrix_sec), ncol(proxy_matrix_firm), M_USE)
cat(sprintf("  Using M = %d repeats, alpha grid: %s\n", M, paste(ALPHA_GRID, collapse = ", ")))

# ── Metric names ───────────────────────────────────────────────────────────
metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")

extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

# ── Sector size filter ─────────────────────────────────────────────────────
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

# ── Helper: tune alpha on training folds ───────────────────────────────────
tune_alpha <- function(train_panel, en_proxy_train, revenue_train,
                       alpha_grid, K_inner, seed_inner) {
  # Assign inner folds (firm-level stratified by sector)
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

  # For each alpha, compute inner-CV RMSE
  rmse_by_alpha <- numeric(length(alpha_grid))

  for (a_idx in seq_along(alpha_grid)) {
    alpha <- alpha_grid[a_idx]

    # Compute combined score
    combined <- (en_proxy_train ^ alpha) * (revenue_train ^ (1 - alpha))

    # Inner CV calibration
    yhat_inner <- calibrate_sector(train_panel, combined, inner_fold_k)

    # RMSE on all training firms (each evaluated in their inner held-out fold)
    rmse_by_alpha[a_idx] <- sqrt(mean((train_panel$y - yhat_inner)^2, na.rm = TRUE))
  }

  # Return best alpha
  alpha_grid[which.min(rmse_by_alpha)]
}

# ── Helper: run one repeat for a given CV design ──────────────────────────
run_repeat <- function(r, cv_type, K, proxy_matrix, idx_eval) {
  fold_k <- assign_folds(panel, cv_type, K, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix[, r])

  # Storage for combined predictions and chosen alphas
  yhat_combined <- rep(NA_real_, nrow(panel))
  alphas_chosen <- numeric(K)

  for (k in 1:K) {
    held_out <- which(fold_k == k)
    train    <- which(fold_k != k)

    # Tune alpha on training folds
    alpha_k <- tune_alpha(
      train_panel      = panel[train, ],
      en_proxy_train   = en_levels[train],
      revenue_train    = panel$revenue[train],
      alpha_grid       = ALPHA_GRID,
      K_inner          = K_INNER,
      seed_inner       = BASE_SEED + r * 100L + k
    )
    alphas_chosen[k] <- alpha_k

    # Apply alpha_k to held-out fold
    # (We still need to calibrate using the full-panel calibrate_sector)
    # Store the combined score for held-out firms
    yhat_combined[held_out] <- (en_levels[held_out] ^ alpha_k) *
                                (panel$revenue[held_out] ^ (1 - alpha_k))
  }

  # Now calibrate: use combined score as the proxy for the full panel
  # But we need fold-aware calibration — use calibrate_sector with the outer folds
  yhat_calibrated <- calibrate_sector(panel, yhat_combined, fold_k)

  # Gated revenue: 1(EN > 0) * revenue (parameter-free)
  gated_score <- as.numeric(en_levels > 0) * panel$revenue
  yhat_gated <- calibrate_sector(panel, gated_score, fold_k)

  # Also compute the three baseline models for comparison
  yhat_rev  <- calibrate_sector(panel, panel$revenue, fold_k)
  yhat_en   <- calibrate_sector(panel, en_levels, fold_k)
  yhat_nace <- calibrate_sector(panel, panel$proxy_tabachova, fold_k)

  # Compute metrics on evaluation subset
  m_combined <- calc_metrics(panel$y[idx_eval], yhat_calibrated[idx_eval],
                             fp_threshold = 0,
                             nace2d = panel$nace2d[idx_eval],
                             year = panel$year[idx_eval])
  m_rev  <- calc_metrics(panel$y[idx_eval], yhat_rev[idx_eval],
                         fp_threshold = 0,
                         nace2d = panel$nace2d[idx_eval],
                         year = panel$year[idx_eval])
  m_en   <- calc_metrics(panel$y[idx_eval], yhat_en[idx_eval],
                         fp_threshold = 0,
                         nace2d = panel$nace2d[idx_eval],
                         year = panel$year[idx_eval])
  m_nace <- calc_metrics(panel$y[idx_eval], yhat_nace[idx_eval],
                         fp_threshold = 0,
                         nace2d = panel$nace2d[idx_eval],
                         year = panel$year[idx_eval])
  m_gated <- calc_metrics(panel$y[idx_eval], yhat_gated[idx_eval],
                          fp_threshold = 0,
                          nace2d = panel$nace2d[idx_eval],
                          year = panel$year[idx_eval])

  list(
    combined = extract_metrics(m_combined),
    gated    = extract_metrics(m_gated),
    revenue  = extract_metrics(m_rev),
    en       = extract_metrics(m_en),
    nace     = extract_metrics(m_nace),
    alphas   = alphas_chosen
  )
}

# ==========================================================================
# Panel A: Sector-level CV
# ==========================================================================
cat("\n── Panel A: Sector-level CV ──\n")
cat(sprintf("  Sectors: %d (>= %d firms)\n", length(sectors_A), MIN_FIRMS_SECTOR_CV))

models <- c("Revenue", "Elastic Net", "NACE", "Gated Rev", "Combined")
metrics_A <- array(NA_real_,
                   dim = c(M, length(metric_names), length(models)),
                   dimnames = list(NULL, metric_names, models))
alphas_A <- matrix(NA_real_, nrow = M, ncol = K_sec)

for (r in seq_len(M)) {
  res <- run_repeat(r, "sector", K_sec, proxy_matrix_sec, idx_A)
  metrics_A[r, , "Revenue"]     <- res$revenue
  metrics_A[r, , "Elastic Net"] <- res$en
  metrics_A[r, , "NACE"]        <- res$nace
  metrics_A[r, , "Gated Rev"]   <- res$gated
  metrics_A[r, , "Combined"]    <- res$combined
  alphas_A[r, ] <- res$alphas

  if (r %% 10 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("  Done.\n")

# ==========================================================================
# Panel B: Firm-level CV
# ==========================================================================
cat("\n── Panel B: Firm-level CV ──\n")
cat(sprintf("  Sectors: %d (>= %d firms)\n", length(eligible_sectors), MIN_FIRMS_FIRM_CV))

metrics_B <- array(NA_real_,
                   dim = c(M, length(metric_names), length(models)),
                   dimnames = list(NULL, metric_names, models))
alphas_B <- matrix(NA_real_, nrow = M, ncol = K_firm)

for (r in seq_len(M)) {
  res <- run_repeat(r, "firm", K_firm, proxy_matrix_firm, idx_eligible)
  metrics_B[r, , "Revenue"]     <- res$revenue
  metrics_B[r, , "Elastic Net"] <- res$en
  metrics_B[r, , "NACE"]        <- res$nace
  metrics_B[r, , "Gated Rev"]   <- res$gated
  metrics_B[r, , "Combined"]    <- res$combined
  alphas_B[r, ] <- res$alphas

  if (r %% 10 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("  Done.\n")

# ==========================================================================
# Results
# ==========================================================================
print_panel <- function(panel_name, metrics_arr, rmse_baseline_name = "Revenue") {
  cat(sprintf("\n%s\n", panel_name))
  cat(sprintf("%-15s %8s %8s %8s %8s %8s %8s %8s %8s\n",
              "", "nRMSE", "MedAPD", "rho", "rho_S", "FPR", "TPR", "p50", "p99"))
  cat(paste(rep("-", 85), collapse = ""), "\n")

  rmse_base <- mean(metrics_arr[, "rmse", rmse_baseline_name], na.rm = TRUE)

  for (m in models) {
    mn <- colMeans(metrics_arr[, , m], na.rm = TRUE)
    sd_v <- apply(metrics_arr[, , m], 2, sd, na.rm = TRUE)

    cat(sprintf("%-15s %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f\n",
                m,
                mn["rmse"] / rmse_base,
                mn["median_apd"], mn["pearson"], mn["spearman"],
                mn["fpr_nonemitters"], mn["tpr_emitters"],
                mn["avg_nonemit_p50_rank"], mn["avg_nonemit_p99_rank"]))
    cat(sprintf("%-15s %8s %8s %8s %8s %8s %8s %8s %8s\n",
                "",
                sprintf("(%.3f)", sd_v["rmse"] / rmse_base),
                sprintf("(%.3f)", sd_v["median_apd"]),
                sprintf("(%.3f)", sd_v["pearson"]),
                sprintf("(%.3f)", sd_v["spearman"]),
                sprintf("(%.3f)", sd_v["fpr_nonemitters"]),
                sprintf("(%.3f)", sd_v["tpr_emitters"]),
                sprintf("(%.3f)", sd_v["avg_nonemit_p50_rank"]),
                sprintf("(%.3f)", sd_v["avg_nonemit_p99_rank"])))
  }
}

cat("\n======================================================================\n")
cat("COMBINED PROXY EVALUATION: EN^alpha * Revenue^(1-alpha)\n")
cat("======================================================================\n")

print_panel("Panel A: Sector-level CV", metrics_A)
print_panel("Panel B: Firm-level CV", metrics_B)

# Alpha distribution
cat("\n── Alpha distribution ──\n")
cat(sprintf("  Panel A: median = %.2f, IQR = [%.2f, %.2f], range = [%.2f, %.2f]\n",
            median(alphas_A, na.rm = TRUE),
            quantile(alphas_A, 0.25, na.rm = TRUE),
            quantile(alphas_A, 0.75, na.rm = TRUE),
            min(alphas_A, na.rm = TRUE),
            max(alphas_A, na.rm = TRUE)))
cat(sprintf("  Panel B: median = %.2f, IQR = [%.2f, %.2f], range = [%.2f, %.2f]\n",
            median(alphas_B, na.rm = TRUE),
            quantile(alphas_B, 0.25, na.rm = TRUE),
            quantile(alphas_B, 0.75, na.rm = TRUE),
            min(alphas_B, na.rm = TRUE),
            max(alphas_B, na.rm = TRUE)))

cat("\n  Alpha frequency (Panel A):\n")
print(table(alphas_A))
cat("\n  Alpha frequency (Panel B):\n")
print(table(alphas_B))

cat("\nDone.\n")
