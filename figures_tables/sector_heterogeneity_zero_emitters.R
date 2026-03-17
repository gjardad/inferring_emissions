###############################################################################
# figures_tables/sector_heterogeneity_zero_emitters.R
#
# PURPOSE
#   Sector-specific performance table for the three sector groups that contain
#   both emitters and non-emitters in the training sample:
#     17/18 (Paper, pulp & printing), 19 (Petroleum refining), 24 (Iron & steel)
#
#   Table layout:
#     3 sector-group columns × 4 metrics = 12 data columns
#     Metrics: nRMSE, Spearman, FPR, FP severity p99
#     6 rows in 2 panels (sector-level CV, firm-level CV) × 3 models
#
#   Panel A (sector-level CV):
#     Revenue, NACE: deterministic (no sd)
#     EN: averaged across M repeats
#
#   Panel B (firm-level CV):
#     All three models: averaged across M repeats
#
#   nRMSE normalized within each sector group by the corresponding
#   Revenue RMSE (separately for Panel A and Panel B).
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/repeated_cv_proxy_firm_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_sector_zero_emitters.tex
#   {OUTPUT_DIR}/table_sector_zero_emitters.rds
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
source(file.path(UTILS_DIR, "calibration_helpers.R"))

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED         <- 2026L
K_sec             <- 5L
K_firm            <- 5L
K_INNER           <- 5L
ALPHA_GRID        <- seq(0, 1, by = 0.1)

# Sector groups: name → vector of NACE 2-digit codes
SECTOR_GROUPS <- list(
  "17/18" = c("17", "18"),
  "19"    = "19",
  "24"    = "24"
)
SECTOR_LABELS <- c("Paper, pulp \\& printing", "Petroleum refining",
                    "Iron \\& steel")

# Metric names for storage
METRIC_NAMES <- c("rmse", "spearman", "fpr", "fp_sev_p99")

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading data...\n")

e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel_sec        <- e_sec$repeated_cv_proxy_panel
M_sec <- ncol(proxy_matrix_sec)
rm(e_sec)

e_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_firm_asinh.RData"), envir = e_firm)
proxy_matrix_firm <- e_firm$proxy_matrix
panel_firm        <- e_firm$repeated_cv_proxy_panel
M_firm <- ncol(proxy_matrix_firm)
rm(e_firm)

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
determ_df <- training_sample %>% select(vat, year, revenue, proxy_tabachova)
rm(training_sample, syt)

panel_sec  <- panel_sec  %>% left_join(determ_df, by = c("vat", "year"))
panel_firm <- panel_firm %>% left_join(determ_df, by = c("vat", "year"))
rm(determ_df)

stopifnot(all(panel_sec$vat == panel_firm$vat),
          all(panel_sec$year == panel_firm$year))

M <- min(M_sec, M_firm)
cat(sprintf("  Using M = %d repeats\n", M))

# ── Helper: compute 4 metrics for a sector group ────────────────────────────
# Returns named vector: rmse, spearman, fpr, fp_sev_p99
compute_sector_group_metrics <- function(y, yhat, nace2d, year, group_codes) {
  idx <- which(nace2d %in% group_codes)
  y_s    <- y[idx]
  yhat_s <- yhat[idx]
  nace_s <- nace2d[idx]
  year_s <- year[idx]

  # RMSE
  rmse <- sqrt(mean((y_s - yhat_s)^2))

  # Spearman rho
  spearman <- if (length(y_s) >= 3 && sd(y_s) > 0 && sd(yhat_s) > 0) {
    suppressWarnings(cor(y_s, yhat_s, method = "spearman", use = "complete.obs"))
  } else {
    NA_real_
  }

  # FPR: share of true non-emitters predicted as positive
  is_nonemit <- (y_s == 0)
  fpr <- if (sum(is_nonemit) > 0) {
    sum(yhat_s[is_nonemit] > 0) / sum(is_nonemit)
  } else {
    NA_real_
  }

  # FP severity p99: average across year cells
  # For each year: p99 of non-emitter predictions ranked in emitter ecdf
  is_emit <- (y_s > 0)
  all_yrs <- sort(unique(year_s))
  cell_p99_ranks <- numeric(0)

  for (yr in all_yrs) {
    in_yr <- (year_s == yr)
    yr_emit    <- (in_yr & is_emit)
    yr_nonemit <- (in_yr & is_nonemit)

    if (sum(yr_emit) < 3 || sum(yr_nonemit) < 1) next

    emitter_ecdf <- ecdf(y_s[yr_emit])
    ne_preds <- yhat_s[yr_nonemit]
    p99_ne   <- quantile(ne_preds, 0.99, na.rm = TRUE, names = FALSE, type = 7)
    cell_p99_ranks <- c(cell_p99_ranks, emitter_ecdf(p99_ne))
  }

  fp_sev_p99 <- if (length(cell_p99_ranks) > 0) mean(cell_p99_ranks) else NA_real_

  c(rmse = rmse, spearman = spearman, fpr = fpr, fp_sev_p99 = fp_sev_p99)
}

# ── Helper: compute metrics for all 3 sector groups ─────────────────────────
# Returns matrix: 3 rows (sector groups) × 4 cols (metrics)
compute_all_groups <- function(y, yhat, nace2d, year) {
  out <- matrix(NA_real_, nrow = length(SECTOR_GROUPS), ncol = length(METRIC_NAMES),
                dimnames = list(names(SECTOR_GROUPS), METRIC_NAMES))
  for (g in names(SECTOR_GROUPS)) {
    out[g, ] <- compute_sector_group_metrics(y, yhat, nace2d, year,
                                              SECTOR_GROUPS[[g]])
  }
  out
}


# ── Helper: tune alpha on training folds (same as combined_proxy_evaluation.R)
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

# Revenue and NACE-based: deterministic
cat("  Revenue and NACE-based (deterministic)...\n")
fold_k_A <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + 1L)

yhat_rev_A  <- calibrate_sector(panel_sec, panel_sec$revenue, fold_k_A)
mA_rev <- compute_all_groups(panel_sec$y, yhat_rev_A, panel_sec$nace2d, panel_sec$year)

yhat_nace_A <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_A)
mA_nace <- compute_all_groups(panel_sec$y, yhat_nace_A, panel_sec$nace2d, panel_sec$year)
cat("  Done.\n")

# EN, Gated Rev, Combined: loop over M repeats
cat(sprintf("  EN, Gated Rev, Combined (%d repeats)...\n", M))
mA_en_all      <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                        dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))
mA_gated_all   <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                        dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))
mA_combined_all <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                         dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  en_levels_r <- proxy_to_levels(proxy_matrix_sec[, r])

  # EN
  yhat_r <- calibrate_sector(panel_sec, en_levels_r, fold_k_r)
  mA_en_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                          panel_sec$nace2d, panel_sec$year)

  # Gated revenue: 1(EN > 0) * revenue
  gated_score <- as.numeric(en_levels_r > 0) * panel_sec$revenue
  yhat_r <- calibrate_sector(panel_sec, gated_score, fold_k_r)
  mA_gated_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                             panel_sec$nace2d, panel_sec$year)

  # Combined: EN^alpha * rev^(1-alpha) with cross-validated alpha
  yhat_combined <- rep(NA_real_, nrow(panel_sec))
  for (k in 1:K_sec) {
    held_out <- which(fold_k_r == k)
    train    <- which(fold_k_r != k)
    alpha_k <- tune_alpha(panel_sec[train, ], en_levels_r[train],
                          panel_sec$revenue[train], ALPHA_GRID, K_INNER,
                          BASE_SEED + r * 100L + k)
    yhat_combined[held_out] <- (en_levels_r[held_out] ^ alpha_k) *
                                (panel_sec$revenue[held_out] ^ (1 - alpha_k))
  }
  yhat_r <- calibrate_sector(panel_sec, yhat_combined, fold_k_r)
  mA_combined_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                                panel_sec$nace2d, panel_sec$year)

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}
mA_en_mean       <- apply(mA_en_all,       c(2, 3), mean, na.rm = TRUE)
mA_en_sd         <- apply(mA_en_all,       c(2, 3), sd,   na.rm = TRUE)
mA_gated_mean    <- apply(mA_gated_all,    c(2, 3), mean, na.rm = TRUE)
mA_gated_sd      <- apply(mA_gated_all,    c(2, 3), sd,   na.rm = TRUE)
mA_combined_mean <- apply(mA_combined_all, c(2, 3), mean, na.rm = TRUE)
mA_combined_sd   <- apply(mA_combined_all, c(2, 3), sd,   na.rm = TRUE)
cat("  Done.\n")


# =============================================================================
# Panel B: Firm-level CV design
# =============================================================================
cat("\n── Panel B: Firm-level CV design ──\n")

mB_rev_all      <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                         dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))
mB_en_all       <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                         dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))
mB_nace_all     <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                         dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))
mB_gated_all    <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                         dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))
mB_combined_all <- array(NA_real_, dim = c(M, length(SECTOR_GROUPS), length(METRIC_NAMES)),
                         dimnames = list(NULL, names(SECTOR_GROUPS), METRIC_NAMES))

cat(sprintf("  Computing all models (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel_sec, "firm", K_firm, BASE_SEED + r)
  en_levels_r <- proxy_to_levels(proxy_matrix_firm[, r])

  # Revenue
  yhat_r <- calibrate_sector(panel_sec, panel_sec$revenue, fold_k_r)
  mB_rev_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                           panel_sec$nace2d, panel_sec$year)

  # EN
  yhat_r <- calibrate_sector(panel_sec, en_levels_r, fold_k_r)
  mB_en_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                          panel_sec$nace2d, panel_sec$year)

  # NACE
  yhat_r <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_r)
  mB_nace_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                            panel_sec$nace2d, panel_sec$year)

  # Gated revenue
  gated_score <- as.numeric(en_levels_r > 0) * panel_sec$revenue
  yhat_r <- calibrate_sector(panel_sec, gated_score, fold_k_r)
  mB_gated_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                             panel_sec$nace2d, panel_sec$year)

  # Combined (cross-validated alpha)
  yhat_combined <- rep(NA_real_, nrow(panel_sec))
  for (k in 1:K_firm) {
    held_out <- which(fold_k_r == k)
    train    <- which(fold_k_r != k)
    alpha_k <- tune_alpha(panel_sec[train, ], en_levels_r[train],
                          panel_sec$revenue[train], ALPHA_GRID, K_INNER,
                          BASE_SEED + r * 100L + k)
    yhat_combined[held_out] <- (en_levels_r[held_out] ^ alpha_k) *
                                (panel_sec$revenue[held_out] ^ (1 - alpha_k))
  }
  yhat_r <- calibrate_sector(panel_sec, yhat_combined, fold_k_r)
  mB_combined_all[r, , ] <- compute_all_groups(panel_sec$y, yhat_r,
                                                panel_sec$nace2d, panel_sec$year)

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}

mB_rev_mean      <- apply(mB_rev_all,      c(2, 3), mean, na.rm = TRUE)
mB_rev_sd        <- apply(mB_rev_all,      c(2, 3), sd,   na.rm = TRUE)
mB_en_mean       <- apply(mB_en_all,       c(2, 3), mean, na.rm = TRUE)
mB_en_sd         <- apply(mB_en_all,       c(2, 3), sd,   na.rm = TRUE)
mB_nace_mean     <- apply(mB_nace_all,     c(2, 3), mean, na.rm = TRUE)
mB_nace_sd       <- apply(mB_nace_all,     c(2, 3), sd,   na.rm = TRUE)
mB_gated_mean    <- apply(mB_gated_all,    c(2, 3), mean, na.rm = TRUE)
mB_gated_sd      <- apply(mB_gated_all,    c(2, 3), sd,   na.rm = TRUE)
mB_combined_mean <- apply(mB_combined_all, c(2, 3), mean, na.rm = TRUE)
mB_combined_sd   <- apply(mB_combined_all, c(2, 3), sd,   na.rm = TRUE)
cat("  Done.\n")


# =============================================================================
# Compute nRMSE (normalize RMSE by Revenue RMSE within each panel × sector)
# =============================================================================
grp_names <- names(SECTOR_GROUPS)

# Panel A: revenue RMSE is deterministic (single value per sector group)
nrmse_A_rev      <- setNames(rep(1, length(grp_names)), grp_names)
nrmse_A_en       <- mA_en_mean[, "rmse"]       / mA_rev[, "rmse"]
nrmse_A_nace     <- mA_nace[, "rmse"]          / mA_rev[, "rmse"]
nrmse_A_gated    <- mA_gated_mean[, "rmse"]    / mA_rev[, "rmse"]
nrmse_A_combined <- mA_combined_mean[, "rmse"] / mA_rev[, "rmse"]

# Panel A nRMSE sd: revenue baseline is fixed, so sd(nRMSE) = sd(RMSE) / baseline
nrmse_A_en_sd       <- mA_en_sd[, "rmse"]       / mA_rev[, "rmse"]
nrmse_A_gated_sd    <- mA_gated_sd[, "rmse"]    / mA_rev[, "rmse"]
nrmse_A_combined_sd <- mA_combined_sd[, "rmse"] / mA_rev[, "rmse"]

# Panel B: compute nRMSE per repeat, then take mean/sd
# nRMSE_r = RMSE_model_r / RMSE_revenue_r
nrmse_B_rev_all      <- matrix(NA_real_, nrow = M, ncol = length(grp_names),
                               dimnames = list(NULL, grp_names))
nrmse_B_en_all       <- nrmse_B_rev_all
nrmse_B_nace_all     <- nrmse_B_rev_all
nrmse_B_gated_all    <- nrmse_B_rev_all
nrmse_B_combined_all <- nrmse_B_rev_all

for (r in seq_len(M)) {
  rev_rmse_r <- mB_rev_all[r, , "rmse"]
  nrmse_B_rev_all[r, ]      <- mB_rev_all[r, , "rmse"]      / rev_rmse_r
  nrmse_B_en_all[r, ]       <- mB_en_all[r, , "rmse"]       / rev_rmse_r
  nrmse_B_nace_all[r, ]     <- mB_nace_all[r, , "rmse"]     / rev_rmse_r
  nrmse_B_gated_all[r, ]    <- mB_gated_all[r, , "rmse"]    / rev_rmse_r
  nrmse_B_combined_all[r, ] <- mB_combined_all[r, , "rmse"] / rev_rmse_r
}

nrmse_B_rev      <- colMeans(nrmse_B_rev_all,      na.rm = TRUE)
nrmse_B_en       <- colMeans(nrmse_B_en_all,       na.rm = TRUE)
nrmse_B_nace     <- colMeans(nrmse_B_nace_all,     na.rm = TRUE)
nrmse_B_gated    <- colMeans(nrmse_B_gated_all,    na.rm = TRUE)
nrmse_B_combined <- colMeans(nrmse_B_combined_all, na.rm = TRUE)

nrmse_B_rev_sd      <- apply(nrmse_B_rev_all,      2, sd, na.rm = TRUE)
nrmse_B_en_sd       <- apply(nrmse_B_en_all,       2, sd, na.rm = TRUE)
nrmse_B_nace_sd     <- apply(nrmse_B_nace_all,     2, sd, na.rm = TRUE)
nrmse_B_gated_sd    <- apply(nrmse_B_gated_all,    2, sd, na.rm = TRUE)
nrmse_B_combined_sd <- apply(nrmse_B_combined_all, 2, sd, na.rm = TRUE)


# =============================================================================
# Console output
# =============================================================================
fmt3 <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "---")

print_row <- function(lbl, nrmse_vec, spear_vec, fpr_vec, p99_vec,
                      nrmse_sd = NULL, spear_sd = NULL, fpr_sd = NULL, p99_sd = NULL) {
  parts <- character(0)
  for (g in grp_names) {
    parts <- c(parts, sprintf("%8s %8s %8s %8s",
                              fmt3(nrmse_vec[g]), fmt3(spear_vec[g]),
                              fmt3(fpr_vec[g]), fmt3(p99_vec[g])))
  }
  cat(sprintf("%-18s %s\n", lbl, paste(parts, collapse = "  ")))

  # Print sd line if provided
  if (!is.null(nrmse_sd)) {
    sd_parts <- character(0)
    for (g in grp_names) {
      sd_parts <- c(sd_parts, sprintf("%8s %8s %8s %8s",
                                      sprintf("(%.3f)", nrmse_sd[g]),
                                      sprintf("(%.3f)", spear_sd[g]),
                                      sprintf("(%.3f)", fpr_sd[g]),
                                      sprintf("(%.3f)", p99_sd[g])))
    }
    cat(sprintf("%-18s %s\n", "", paste(sd_parts, collapse = "  ")))
  }
}

cat("\n======================================================================\n")
cat("SECTOR-SPECIFIC TABLE: NACE 17/18, 19, 24\n")
cat("  nRMSE normalized within each sector by Revenue RMSE\n")
cat("======================================================================\n\n")

hdr_parts <- character(0)
for (g in grp_names) {
  hdr_parts <- c(hdr_parts, sprintf("%8s %8s %8s %8s", "nRMSE", "Spear", "FPR", "p99"))
}
cat(sprintf("%-18s %s\n", "", paste(hdr_parts, collapse = "  ")))

grp_hdr <- character(0)
for (g in grp_names) {
  grp_hdr <- c(grp_hdr, sprintf("%-35s", paste0("NACE ", g)))
}
cat(sprintf("%-18s %s\n", "", paste(grp_hdr, collapse = "")))
cat(paste(rep("-", 120), collapse = ""), "\n")

cat("Panel A: Sector-level CV\n")
print_row("  Revenue",     nrmse_A_rev,      mA_rev[, "spearman"],           mA_rev[, "fpr"],           mA_rev[, "fp_sev_p99"])
print_row("  Elastic Net", nrmse_A_en,       mA_en_mean[, "spearman"],       mA_en_mean[, "fpr"],       mA_en_mean[, "fp_sev_p99"],
          nrmse_A_en_sd,                      mA_en_sd[, "spearman"],         mA_en_sd[, "fpr"],         mA_en_sd[, "fp_sev_p99"])
print_row("  NACE",        nrmse_A_nace,     mA_nace[, "spearman"],          mA_nace[, "fpr"],          mA_nace[, "fp_sev_p99"])
print_row("  Gated Rev",   nrmse_A_gated,    mA_gated_mean[, "spearman"],    mA_gated_mean[, "fpr"],    mA_gated_mean[, "fp_sev_p99"],
          nrmse_A_gated_sd,                   mA_gated_sd[, "spearman"],      mA_gated_sd[, "fpr"],      mA_gated_sd[, "fp_sev_p99"])
print_row("  Combined",    nrmse_A_combined, mA_combined_mean[, "spearman"], mA_combined_mean[, "fpr"], mA_combined_mean[, "fp_sev_p99"],
          nrmse_A_combined_sd,                mA_combined_sd[, "spearman"],   mA_combined_sd[, "fpr"],   mA_combined_sd[, "fp_sev_p99"])

cat("\nPanel B: Firm-level CV\n")
print_row("  Revenue",     nrmse_B_rev,      mB_rev_mean[, "spearman"],      mB_rev_mean[, "fpr"],      mB_rev_mean[, "fp_sev_p99"],
          nrmse_B_rev_sd,                     mB_rev_sd[, "spearman"],        mB_rev_sd[, "fpr"],        mB_rev_sd[, "fp_sev_p99"])
print_row("  Elastic Net", nrmse_B_en,       mB_en_mean[, "spearman"],       mB_en_mean[, "fpr"],       mB_en_mean[, "fp_sev_p99"],
          nrmse_B_en_sd,                      mB_en_sd[, "spearman"],         mB_en_sd[, "fpr"],         mB_en_sd[, "fp_sev_p99"])
print_row("  NACE",        nrmse_B_nace,     mB_nace_mean[, "spearman"],     mB_nace_mean[, "fpr"],     mB_nace_mean[, "fp_sev_p99"],
          nrmse_B_nace_sd,                    mB_nace_sd[, "spearman"],       mB_nace_sd[, "fpr"],       mB_nace_sd[, "fp_sev_p99"])
print_row("  Gated Rev",   nrmse_B_gated,    mB_gated_mean[, "spearman"],    mB_gated_mean[, "fpr"],    mB_gated_mean[, "fp_sev_p99"],
          nrmse_B_gated_sd,                   mB_gated_sd[, "spearman"],      mB_gated_sd[, "fpr"],      mB_gated_sd[, "fp_sev_p99"])
print_row("  Combined",    nrmse_B_combined, mB_combined_mean[, "spearman"], mB_combined_mean[, "fpr"], mB_combined_mean[, "fp_sev_p99"],
          nrmse_B_combined_sd,                mB_combined_sd[, "spearman"],   mB_combined_sd[, "fpr"],   mB_combined_sd[, "fp_sev_p99"])

# Print Revenue RMSE baselines for reference
cat("\n  Revenue RMSE baselines (kt):\n")
for (g in grp_names) {
  cat(sprintf("    NACE %s:  Panel A = %.1f,  Panel B = %.1f\n",
              g, mA_rev[g, "rmse"] / 1e3, mB_rev_mean[g, "rmse"] / 1e3))
}


# =============================================================================
# LaTeX table (tabular only — landscape wrapper goes in the paper)
# =============================================================================
fmt  <- function(x, d = 3) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "---")

tex_row <- function(lbl, nrmse_vec, spear_vec, fpr_vec, p99_vec,
                    nrmse_sd = NULL, spear_sd = NULL, fpr_sd = NULL, p99_sd = NULL) {
  parts <- character(0)
  for (g in grp_names) {
    parts <- c(parts, sprintf("%s & %s & %s & %s",
                              fmt(nrmse_vec[g]), fmt(spear_vec[g]),
                              fmt(fpr_vec[g]), fmt(p99_vec[g])))
  }
  mean_line <- sprintf("%s & %s \\\\", lbl, paste(parts, collapse = " & "))

  if (is.null(nrmse_sd)) return(mean_line)

  # sd line
  sd_parts <- character(0)
  for (g in grp_names) {
    sd_parts <- c(sd_parts, sprintf("{\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)} & {\\scriptsize(%s)}",
                                    fmt(nrmse_sd[g]), fmt(spear_sd[g]),
                                    fmt(fpr_sd[g]), fmt(p99_sd[g])))
  }
  sd_line <- sprintf(" & %s \\\\", paste(sd_parts, collapse = " & "))
  c(mean_line, sd_line)
}

tex_lines <- c(
  "\\begin{tabular}{l cccc cccc cccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} & \\multicolumn{4}{c}{%s} \\\\",
          SECTOR_LABELS[1], SECTOR_LABELS[2], SECTOR_LABELS[3]),
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9} \\cmidrule(lr){10-13}",
  paste0(" & ", paste(rep("nRMSE & $\\rho_S$ & FPR & p99", 3), collapse = " & "), " \\\\"),
  "\\midrule",
  "\\multicolumn{13}{l}{\\textit{Panel A: Sector-level CV design}} \\\\",
  "\\addlinespace[2pt]",
  tex_row("Revenue", nrmse_A_rev, mA_rev[, "spearman"], mA_rev[, "fpr"], mA_rev[, "fp_sev_p99"]),
  tex_row("Elastic Net", nrmse_A_en, mA_en_mean[, "spearman"], mA_en_mean[, "fpr"], mA_en_mean[, "fp_sev_p99"],
          nrmse_A_en_sd, mA_en_sd[, "spearman"], mA_en_sd[, "fpr"], mA_en_sd[, "fp_sev_p99"]),
  tex_row("NACE", nrmse_A_nace, mA_nace[, "spearman"], mA_nace[, "fpr"], mA_nace[, "fp_sev_p99"]),
  tex_row("Gated Rev", nrmse_A_gated, mA_gated_mean[, "spearman"], mA_gated_mean[, "fpr"], mA_gated_mean[, "fp_sev_p99"],
          nrmse_A_gated_sd, mA_gated_sd[, "spearman"], mA_gated_sd[, "fpr"], mA_gated_sd[, "fp_sev_p99"]),
  tex_row("Combined", nrmse_A_combined, mA_combined_mean[, "spearman"], mA_combined_mean[, "fpr"], mA_combined_mean[, "fp_sev_p99"],
          nrmse_A_combined_sd, mA_combined_sd[, "spearman"], mA_combined_sd[, "fpr"], mA_combined_sd[, "fp_sev_p99"]),
  "\\addlinespace[4pt]",
  "\\multicolumn{13}{l}{\\textit{Panel B: Firm-level CV design}} \\\\",
  "\\addlinespace[2pt]",
  tex_row("Revenue", nrmse_B_rev, mB_rev_mean[, "spearman"], mB_rev_mean[, "fpr"], mB_rev_mean[, "fp_sev_p99"],
          nrmse_B_rev_sd, mB_rev_sd[, "spearman"], mB_rev_sd[, "fpr"], mB_rev_sd[, "fp_sev_p99"]),
  tex_row("Elastic Net", nrmse_B_en, mB_en_mean[, "spearman"], mB_en_mean[, "fpr"], mB_en_mean[, "fp_sev_p99"],
          nrmse_B_en_sd, mB_en_sd[, "spearman"], mB_en_sd[, "fpr"], mB_en_sd[, "fp_sev_p99"]),
  tex_row("NACE", nrmse_B_nace, mB_nace_mean[, "spearman"], mB_nace_mean[, "fpr"], mB_nace_mean[, "fp_sev_p99"],
          nrmse_B_nace_sd, mB_nace_sd[, "spearman"], mB_nace_sd[, "fpr"], mB_nace_sd[, "fp_sev_p99"]),
  tex_row("Gated Rev", nrmse_B_gated, mB_gated_mean[, "spearman"], mB_gated_mean[, "fpr"], mB_gated_mean[, "fp_sev_p99"],
          nrmse_B_gated_sd, mB_gated_sd[, "spearman"], mB_gated_sd[, "fpr"], mB_gated_sd[, "fp_sev_p99"]),
  tex_row("Combined", nrmse_B_combined, mB_combined_mean[, "spearman"], mB_combined_mean[, "fpr"], mB_combined_mean[, "fp_sev_p99"],
          nrmse_B_combined_sd, mB_combined_sd[, "spearman"], mB_combined_sd[, "fpr"], mB_combined_sd[, "fp_sev_p99"]),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_path <- file.path(OUTPUT_DIR, "table_sector_zero_emitters.tex")
writeLines(tex_lines, tex_path)
cat("\nLaTeX table written to:", tex_path, "\n")


# =============================================================================
# Save results
# =============================================================================
results <- list(
  # Panel A
  mA_rev = mA_rev, mA_nace = mA_nace,
  mA_en_mean = mA_en_mean, mA_en_sd = mA_en_sd,
  mA_en_all = mA_en_all,
  nrmse_A_rev = nrmse_A_rev, nrmse_A_en = nrmse_A_en, nrmse_A_nace = nrmse_A_nace,
  # Panel B
  mB_rev_mean = mB_rev_mean, mB_rev_sd = mB_rev_sd, mB_rev_all = mB_rev_all,
  mB_en_mean = mB_en_mean, mB_en_sd = mB_en_sd, mB_en_all = mB_en_all,
  mB_nace_mean = mB_nace_mean, mB_nace_sd = mB_nace_sd, mB_nace_all = mB_nace_all,
  nrmse_B_rev = nrmse_B_rev, nrmse_B_en = nrmse_B_en, nrmse_B_nace = nrmse_B_nace,
  # Metadata
  SECTOR_GROUPS = SECTOR_GROUPS, METRIC_NAMES = METRIC_NAMES,
  M = M, K_sec = K_sec, K_firm = K_firm, BASE_SEED = BASE_SEED
)
rds_path <- file.path(OUTPUT_DIR, "table_sector_zero_emitters.rds")
saveRDS(results, rds_path)
cat("Results saved:", rds_path, "\n")
