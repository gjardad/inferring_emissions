###############################################################################
# figures_tables/sector_heterogeneity_all.R
#
# PURPOSE
#   Cross-sector distribution of within-sector prediction quality.
#   For each sector, compute year-demeaned Spearman rho and Median APD
#   (among emitters). Report the distribution across sectors for each model.
#
#   Panel A (sector-level CV, all sectors):
#     Revenue, NACE-based: deterministic (computed once)
#     EN, Gated Revenue, Combined: averaged across M repeats
#
#   Panel B (firm-level CV, sectors with >= MIN_FIRMS firms):
#     All three models: averaged across M repeats
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/repeated_cv_proxy_firm_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   Figure 1 (eligible sectors, sector vs firm CV — apples-to-apples):
#     {OUTPUT_DIR}/figure_sector_heterogeneity_v1.pdf  (no separator)
#     {OUTPUT_DIR}/figure_sector_heterogeneity_v2.pdf  (dashed separator)
#   Figure 2 (all sectors, sector-level CV only):
#     {OUTPUT_DIR}/figure_sector_heterogeneity_all_sectors.pdf
#   {OUTPUT_DIR}/sector_heterogeneity_all.rds
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
BASE_SEED           <- 2026L
K_sec               <- 5L
K_firm              <- 5L
K_INNER             <- 5L
MIN_FIRMS_SECTOR_CV <- 3L    # all-sectors figure: exclude tiny sectors
MIN_FIRMS_FIRM_CV   <- 15L
ALPHA_GRID          <- seq(0, 1, by = 0.1)

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading data...\n")

# EN proxy matrices
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

# Deterministic proxies
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
determ_df <- training_sample %>% select(vat, year, revenue, proxy_tabachova)
rm(training_sample, syt)

panel_sec  <- panel_sec  %>% left_join(determ_df, by = c("vat", "year"))
panel_firm <- panel_firm %>% left_join(determ_df, by = c("vat", "year"))
rm(determ_df)

# Verify panels match
stopifnot(all(panel_sec$vat == panel_firm$vat),
          all(panel_sec$year == panel_firm$year))

M <- min(M_sec, M_firm)
cat(sprintf("  EN sector CV: %d obs x %d repeats\n", nrow(proxy_matrix_sec), M_sec))
cat(sprintf("  EN firm CV:   %d obs x %d repeats\n", nrow(proxy_matrix_firm), M_firm))
cat(sprintf("  Using M = %d repeats\n", M))

# ── Helper: tune alpha on training folds ─────────────────────────────────────
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

# ── Sector lists ─────────────────────────────────────────────────────────────
all_sectors <- sort(unique(panel_sec$nace2d))

firms_per_sector <- panel_sec %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")

eligible_sectors <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_FIRM_CV) %>%
  pull(primary_nace2d)

cat(sprintf("  All sectors: %d | Eligible for firm CV: %d\n",
            length(all_sectors), length(eligible_sectors)))

# ── Helper: compute within-sector metrics from calibrated predictions ────────
compute_sector_metrics <- function(y, yhat, nace2d, year, sectors) {
  # Year-demean y and yhat
  all_yrs <- sort(unique(year))
  y_dm    <- numeric(length(y))
  yhat_dm <- numeric(length(y))
  for (yr in all_yrs) {
    idx <- which(year == yr)
    y_dm[idx]    <- y[idx] - mean(y[idx])
    yhat_dm[idx] <- yhat[idx] - mean(yhat[idx])
  }

  rho_vec <- setNames(rep(NA_real_, length(sectors)), sectors)
  apd_vec <- setNames(rep(NA_real_, length(sectors)), sectors)

  for (sec in sectors) {
    idx <- which(nace2d == sec)

    # Demeaned Spearman rho (pooled across years)
    if (length(idx) >= 3 && sd(y_dm[idx]) > 0 && sd(yhat_dm[idx]) > 0) {
      rho_vec[sec] <- suppressWarnings(
        stats::cor(y_dm[idx], yhat_dm[idx], method = "spearman", use = "complete.obs")
      )
    }

    # Median APD among emitters (non-demeaned)
    emit_idx <- idx[y[idx] > 0]
    if (length(emit_idx) >= 1) {
      apd_vec[sec] <- median(abs(y[emit_idx] - yhat[emit_idx]) / y[emit_idx],
                             na.rm = TRUE)
    }
  }

  list(rho = rho_vec, apd = apd_vec)
}

# =============================================================================
# Panel A: Sector-level CV design
# =============================================================================
cat("\n── Panel A: Sector-level CV design ──\n")

# Revenue and NACE-based: deterministic (one fold assignment suffices)
cat("  Revenue and NACE-based (deterministic)...\n")
fold_k_A <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + 1L)

yhat_rev  <- calibrate_sector(panel_sec, panel_sec$revenue, fold_k_A)
sm_rev_A_all  <- compute_sector_metrics(panel_sec$y, yhat_rev,
                                         panel_sec$nace2d, panel_sec$year, all_sectors)
sm_rev_A_elig <- compute_sector_metrics(panel_sec$y, yhat_rev,
                                         panel_sec$nace2d, panel_sec$year, eligible_sectors)

yhat_nace <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_A)
sm_nace_A_all  <- compute_sector_metrics(panel_sec$y, yhat_nace,
                                          panel_sec$nace2d, panel_sec$year, all_sectors)
sm_nace_A_elig <- compute_sector_metrics(panel_sec$y, yhat_nace,
                                          panel_sec$nace2d, panel_sec$year, eligible_sectors)
cat("  Done.\n")

# EN, Gated Revenue, Combined: loop over M repeats
cat(sprintf("  EN, Gated Rev, Combined sector CV (%d repeats)...\n", M))
rho_en_A <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                   dimnames = list(NULL, all_sectors))
apd_en_A <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                   dimnames = list(NULL, all_sectors))
rho_en_A_elig <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                        dimnames = list(NULL, eligible_sectors))
apd_en_A_elig <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                        dimnames = list(NULL, eligible_sectors))

rho_gated_A <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                      dimnames = list(NULL, all_sectors))
apd_gated_A <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                      dimnames = list(NULL, all_sectors))
rho_combined_A <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                         dimnames = list(NULL, all_sectors))
apd_combined_A <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                         dimnames = list(NULL, all_sectors))

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])

  # EN
  yhat_r   <- calibrate_sector(panel_sec, en_levels, fold_k_r)
  sm_all  <- compute_sector_metrics(panel_sec$y, yhat_r,
                                     panel_sec$nace2d, panel_sec$year, all_sectors)
  sm_elig <- compute_sector_metrics(panel_sec$y, yhat_r,
                                     panel_sec$nace2d, panel_sec$year, eligible_sectors)
  rho_en_A[r, ]      <- sm_all$rho[all_sectors]
  apd_en_A[r, ]      <- sm_all$apd[all_sectors]
  rho_en_A_elig[r, ] <- sm_elig$rho[eligible_sectors]
  apd_en_A_elig[r, ] <- sm_elig$apd[eligible_sectors]

  # Gated Revenue
  gated_score <- as.numeric(en_levels > 0) * panel_sec$revenue
  yhat_gated  <- calibrate_sector(panel_sec, gated_score, fold_k_r)
  sm_gated    <- compute_sector_metrics(panel_sec$y, yhat_gated,
                                         panel_sec$nace2d, panel_sec$year, all_sectors)
  rho_gated_A[r, ] <- sm_gated$rho[all_sectors]
  apd_gated_A[r, ] <- sm_gated$apd[all_sectors]

  # Combined: tune alpha per fold, then calibrate
  combined_score <- rep(NA_real_, nrow(panel_sec))
  for (k in 1:K_sec) {
    train_idx <- which(fold_k_r != k)
    held_idx  <- which(fold_k_r == k)
    alpha_k <- tune_alpha(
      train_panel    = panel_sec[train_idx, ],
      en_proxy_train = en_levels[train_idx],
      revenue_train  = panel_sec$revenue[train_idx],
      alpha_grid     = ALPHA_GRID,
      K_inner        = K_INNER,
      seed_inner     = BASE_SEED + r * 100L + k
    )
    combined_score[held_idx] <- (en_levels[held_idx] ^ alpha_k) *
                                 (panel_sec$revenue[held_idx] ^ (1 - alpha_k))
  }
  yhat_combined <- calibrate_sector(panel_sec, combined_score, fold_k_r)
  sm_combined   <- compute_sector_metrics(panel_sec$y, yhat_combined,
                                           panel_sec$nace2d, panel_sec$year, all_sectors)
  rho_combined_A[r, ] <- sm_combined$rho[all_sectors]
  apd_combined_A[r, ] <- sm_combined$apd[all_sectors]

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}
rho_en_A_mean      <- colMeans(rho_en_A, na.rm = TRUE)
apd_en_A_mean      <- colMeans(apd_en_A, na.rm = TRUE)
rho_en_A_elig_mean <- colMeans(rho_en_A_elig, na.rm = TRUE)
apd_en_A_elig_mean <- colMeans(apd_en_A_elig, na.rm = TRUE)
rho_gated_A_mean    <- colMeans(rho_gated_A, na.rm = TRUE)
apd_gated_A_mean    <- colMeans(apd_gated_A, na.rm = TRUE)
rho_combined_A_mean <- colMeans(rho_combined_A, na.rm = TRUE)
apd_combined_A_mean <- colMeans(apd_combined_A, na.rm = TRUE)
cat("  Done.\n")


# =============================================================================
# Panel B: Firm-level CV design (eligible sectors only)
# =============================================================================
cat("\n── Panel B: Firm-level CV design ──\n")
cat(sprintf("  Eligible sectors (%d): %s\n",
            length(eligible_sectors), paste(eligible_sectors, collapse = ", ")))

rho_rev_B  <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                     dimnames = list(NULL, eligible_sectors))
apd_rev_B  <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                     dimnames = list(NULL, eligible_sectors))
rho_en_B   <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                     dimnames = list(NULL, eligible_sectors))
apd_en_B   <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                     dimnames = list(NULL, eligible_sectors))
rho_nace_B <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                     dimnames = list(NULL, eligible_sectors))
apd_nace_B <- matrix(NA_real_, nrow = M, ncol = length(eligible_sectors),
                     dimnames = list(NULL, eligible_sectors))

cat(sprintf("  Computing all models (%d repeats)...\n", M))

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel_sec, "firm", K_firm, BASE_SEED + r)

  # Revenue
  yhat_r <- calibrate_sector(panel_sec, panel_sec$revenue, fold_k_r)
  sm <- compute_sector_metrics(panel_sec$y, yhat_r,
                                panel_sec$nace2d, panel_sec$year, eligible_sectors)
  rho_rev_B[r, ] <- sm$rho[eligible_sectors]
  apd_rev_B[r, ] <- sm$apd[eligible_sectors]

  # EN firm CV
  yhat_r <- calibrate_sector(panel_sec,
                              proxy_to_levels(proxy_matrix_firm[, r]),
                              fold_k_r)
  sm <- compute_sector_metrics(panel_sec$y, yhat_r,
                                panel_sec$nace2d, panel_sec$year, eligible_sectors)
  rho_en_B[r, ] <- sm$rho[eligible_sectors]
  apd_en_B[r, ] <- sm$apd[eligible_sectors]

  # NACE-based
  yhat_r <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_r)
  sm <- compute_sector_metrics(panel_sec$y, yhat_r,
                                panel_sec$nace2d, panel_sec$year, eligible_sectors)
  rho_nace_B[r, ] <- sm$rho[eligible_sectors]
  apd_nace_B[r, ] <- sm$apd[eligible_sectors]

  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}

rho_rev_B_mean  <- colMeans(rho_rev_B,  na.rm = TRUE)
apd_rev_B_mean  <- colMeans(apd_rev_B,  na.rm = TRUE)
rho_en_B_mean   <- colMeans(rho_en_B,   na.rm = TRUE)
apd_en_B_mean   <- colMeans(apd_en_B,   na.rm = TRUE)
rho_nace_B_mean <- colMeans(rho_nace_B, na.rm = TRUE)
apd_nace_B_mean <- colMeans(apd_nace_B, na.rm = TRUE)
cat("  Done.\n")


# =============================================================================
# Console summary
# =============================================================================
print_summary <- function(lbl, vals) {
  v <- vals[is.finite(vals)]
  cat(sprintf("%-35s %8.3f %8.3f %8.3f %8.3f %8.3f  %d\n",
              lbl, median(v), quantile(v, 0.25), quantile(v, 0.75),
              min(v), max(v), length(v)))
}

hdr <- function() {
  cat(sprintf("%-35s %8s %8s %8s %8s %8s  %s\n",
              "", "Median", "p25", "p75", "Min", "Max", "N"))
  cat(paste(rep("-", 95), collapse = ""), "\n")
}

cat("\n── Within-sector Spearman rho: ALL sectors ──\n"); hdr()
print_summary("A (all): Revenue",    sm_rev_A_all$rho)
print_summary("A (all): Elastic Net",rho_en_A_mean)
print_summary("A (all): NACE-based", sm_nace_A_all$rho)
print_summary("A (all): Gated Rev",  rho_gated_A_mean)
print_summary("A (all): Combined",   rho_combined_A_mean)

cat("\n── Within-sector Spearman rho: eligible sectors only ──\n"); hdr()
print_summary("A (elig): Revenue",    sm_rev_A_elig$rho)
print_summary("A (elig): Elastic Net",rho_en_A_elig_mean)
print_summary("A (elig): NACE-based", sm_nace_A_elig$rho)
print_summary("B (elig): Revenue",    rho_rev_B_mean)
print_summary("B (elig): Elastic Net",rho_en_B_mean)
print_summary("B (elig): NACE-based", rho_nace_B_mean)

cat("\n── Within-sector Median APD: ALL sectors ──\n"); hdr()
print_summary("A (all): Revenue",    sm_rev_A_all$apd)
print_summary("A (all): Elastic Net",apd_en_A_mean)
print_summary("A (all): NACE-based", sm_nace_A_all$apd)
print_summary("A (all): Gated Rev",  apd_gated_A_mean)
print_summary("A (all): Combined",   apd_combined_A_mean)

cat("\n── Within-sector Median APD: eligible sectors only ──\n"); hdr()
print_summary("A (elig): Revenue",    sm_rev_A_elig$apd)
print_summary("A (elig): Elastic Net",apd_en_A_elig_mean)
print_summary("A (elig): NACE-based", sm_nace_A_elig$apd)
print_summary("B (elig): Revenue",    apd_rev_B_mean)
print_summary("B (elig): Elastic Net",apd_en_B_mean)
print_summary("B (elig): NACE-based", apd_nace_B_mean)


# =============================================================================
# Figure helpers
# =============================================================================

# Sector sizes for dot scaling (used by both figures)
sector_sizes_all <- as.numeric(table(panel_sec$nace2d)[all_sectors])
names(sector_sizes_all) <- all_sectors
sector_sizes_elig <- sector_sizes_all[eligible_sectors]

# ── Figure 1: Apples-to-apples comparison (eligible sectors only, both CVs) ──
# Shows whether firm-level CV genuinely improves on sector-level CV
# by restricting both to the same set of sectors.
draw_figure_eligible <- function(filepath, separator = FALSE) {

  if (separator) {
    pos <- c(1, 2, 3, 5, 6, 7)
    xlim <- c(0.3, 7.7)
    sep_x <- 4
  } else {
    pos <- c(1, 2, 3, 4.5, 5.5, 6.5)
    xlim <- c(0.3, 7.2)
    sep_x <- 3.75
  }

  tick_labels <- c("Revenue", "Elastic Net", "NACE", "Revenue", "Elastic Net", "NACE")

  # Both panels use eligible sectors only
  rho_list <- list(
    sm_rev_A_elig$rho[is.finite(sm_rev_A_elig$rho)],
    rho_en_A_elig_mean[is.finite(rho_en_A_elig_mean)],
    sm_nace_A_elig$rho[is.finite(sm_nace_A_elig$rho)],
    rho_rev_B_mean[is.finite(rho_rev_B_mean)],
    rho_en_B_mean[is.finite(rho_en_B_mean)],
    rho_nace_B_mean[is.finite(rho_nace_B_mean)]
  )

  apd_list <- list(
    sm_rev_A_elig$apd[is.finite(sm_rev_A_elig$apd)],
    apd_en_A_elig_mean[is.finite(apd_en_A_elig_mean)],
    sm_nace_A_elig$apd[is.finite(sm_nace_A_elig$apd)],
    apd_rev_B_mean[is.finite(apd_rev_B_mean)],
    apd_en_B_mean[is.finite(apd_en_B_mean)],
    apd_nace_B_mean[is.finite(apd_nace_B_mean)]
  )

  col_A <- adjustcolor("steelblue", 0.6)
  col_B <- adjustcolor("firebrick", 0.6)
  cols <- c(col_A, col_A, col_A, col_B, col_B, col_B)

  pdf(filepath, width = 10, height = 6)
  par(mfrow = c(1, 2), mar = c(5, 4, 4, 1), oma = c(2, 0, 0, 0))

  # ── Sub-panel: Spearman rho ──
  rho_all_vals <- unlist(rho_list)
  ylim_rho <- range(rho_all_vals, na.rm = TRUE)
  ylim_rho <- ylim_rho + c(-0.05, 0.05) * diff(ylim_rho)

  plot(NULL, xlim = xlim, ylim = ylim_rho,
       xlab = "", ylab = "Rank correlation",
       xaxt = "n", main = "(a)")
  axis(1, at = pos, labels = tick_labels, las = 2, cex.axis = 0.75)
  abline(h = 0, lty = 2, col = "grey60")
  if (separator) abline(v = sep_x, lty = 2, col = "grey40", lwd = 1)

  for (m in 1:6) {
    vals <- rho_list[[m]]
    if (length(vals) == 0) next
    sec_names <- names(rho_list[[m]])
    sizes_raw <- sector_sizes_elig[sec_names]
    sizes <- log(as.numeric(sizes_raw)) / max(log(sector_sizes_all))
    sizes[is.na(sizes)] <- 0.5
    jx <- pos[m] + runif(length(vals), -0.15, 0.15)
    points(jx, vals, pch = 19, col = cols[m], cex = 0.5 + 1.5 * sizes)
    segments(pos[m] - 0.25, median(vals, na.rm = TRUE),
             pos[m] + 0.25, median(vals, na.rm = TRUE), col = "red", lwd = 2)
  }

  # ── Sub-panel: Median APD ──
  apd_all_vals <- unlist(apd_list)
  ylim_apd <- range(apd_all_vals, na.rm = TRUE)
  ylim_apd <- ylim_apd + c(-0.05, 0.05) * diff(ylim_apd)

  plot(NULL, xlim = xlim, ylim = ylim_apd,
       xlab = "", ylab = "Median APD",
       xaxt = "n", main = "(b)")
  axis(1, at = pos, labels = tick_labels, las = 2, cex.axis = 0.75)
  if (separator) abline(v = sep_x, lty = 2, col = "grey40", lwd = 1)

  for (m in 1:6) {
    vals <- apd_list[[m]]
    if (length(vals) == 0) next
    sec_names <- names(apd_list[[m]])
    sizes_raw <- sector_sizes_elig[sec_names]
    sizes <- log(as.numeric(sizes_raw)) / max(log(sector_sizes_all))
    sizes[is.na(sizes)] <- 0.5
    jx <- pos[m] + runif(length(vals), -0.15, 0.15)
    points(jx, vals, pch = 19, col = cols[m], cex = 0.5 + 1.5 * sizes)
    segments(pos[m] - 0.25, median(vals, na.rm = TRUE),
             pos[m] + 0.25, median(vals, na.rm = TRUE), col = "red", lwd = 2)
  }

  # ── Legend (centered between the two panels) ──
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", horiz = TRUE, inset = c(0, 0),
         legend = c("Sector-level CV", "Firm-level CV", "Median"),
         col = c(col_A, col_B, "red"),
         pch = c(19, 19, NA),
         lty = c(NA, NA, 1), lwd = c(NA, NA, 2),
         pt.cex = 1.2, cex = 0.85, bty = "n")

  dev.off()
  cat("Figure saved:", filepath, "\n")
}

# ── Figure 2: Full cross-sector distribution (sector-level CV, all sectors) ──
# Shows heterogeneity across all ~34 sectors under sector-level CV only.
draw_figure_all_sectors <- function(filepath) {

  # Filter to sectors with >= MIN_FIRMS_SECTOR_CV firms
  fig_sectors <- firms_per_sector %>%
    filter(n_firms >= MIN_FIRMS_SECTOR_CV) %>%
    pull(primary_nace2d)

  pos <- c(1, 2, 3, 4, 5)
  xlim <- c(0.3, 5.7)
  tick_labels <- c("Revenue", "Elastic Net", "NACE", "Gated Rev", "Combined")

  # Subset to fig_sectors only
  rho_rev      <- sm_rev_A_all$rho[fig_sectors];       rho_rev      <- rho_rev[is.finite(rho_rev)]
  rho_en       <- rho_en_A_mean[fig_sectors];           rho_en       <- rho_en[is.finite(rho_en)]
  rho_nace     <- sm_nace_A_all$rho[fig_sectors];       rho_nace     <- rho_nace[is.finite(rho_nace)]
  rho_gated    <- rho_gated_A_mean[fig_sectors];        rho_gated    <- rho_gated[is.finite(rho_gated)]
  rho_combined <- rho_combined_A_mean[fig_sectors];     rho_combined <- rho_combined[is.finite(rho_combined)]
  rho_list <- list(rho_rev, rho_en, rho_nace, rho_gated, rho_combined)

  apd_rev      <- sm_rev_A_all$apd[fig_sectors];       apd_rev      <- apd_rev[is.finite(apd_rev)]
  apd_en       <- apd_en_A_mean[fig_sectors];           apd_en       <- apd_en[is.finite(apd_en)]
  apd_nace     <- sm_nace_A_all$apd[fig_sectors];       apd_nace     <- apd_nace[is.finite(apd_nace)]
  apd_gated    <- apd_gated_A_mean[fig_sectors];        apd_gated    <- apd_gated[is.finite(apd_gated)]
  apd_combined <- apd_combined_A_mean[fig_sectors];     apd_combined <- apd_combined[is.finite(apd_combined)]
  apd_list <- list(apd_rev, apd_en, apd_nace, apd_gated, apd_combined)

  col_A <- adjustcolor("steelblue", 0.6)

  pdf(filepath, width = 10, height = 5)
  par(mfrow = c(1, 2), mar = c(6, 4, 3, 1), oma = c(0, 0, 0, 0))

  # ── Sub-panel: Spearman rho ──
  rho_all_vals <- unlist(rho_list)
  ylim_rho <- range(rho_all_vals, na.rm = TRUE)
  ylim_rho <- ylim_rho + c(-0.05, 0.05) * diff(ylim_rho)

  plot(NULL, xlim = xlim, ylim = ylim_rho,
       xlab = "", ylab = "Rank correlation",
       xaxt = "n", main = "(a)")
  axis(1, at = pos, labels = tick_labels, las = 2, cex.axis = 0.75)
  abline(h = 0, lty = 2, col = "grey60")

  for (m in seq_along(pos)) {
    vals <- rho_list[[m]]
    if (length(vals) == 0) next
    sec_names <- names(rho_list[[m]])
    sizes_raw <- sector_sizes_all[sec_names]
    sizes <- log(as.numeric(sizes_raw)) / max(log(sector_sizes_all))
    sizes[is.na(sizes)] <- 0.5
    jx <- pos[m] + runif(length(vals), -0.15, 0.15)
    points(jx, vals, pch = 19, col = col_A, cex = 0.5 + 1.5 * sizes)
    segments(pos[m] - 0.25, median(vals, na.rm = TRUE),
             pos[m] + 0.25, median(vals, na.rm = TRUE), col = "red", lwd = 2)
  }

  # ── Sub-panel: Median APD ──
  apd_all_vals <- unlist(apd_list)
  ylim_apd <- range(apd_all_vals, na.rm = TRUE)
  ylim_apd <- ylim_apd + c(-0.05, 0.05) * diff(ylim_apd)

  plot(NULL, xlim = xlim, ylim = ylim_apd,
       xlab = "", ylab = "Median APD",
       xaxt = "n", main = "(b)")
  axis(1, at = pos, labels = tick_labels, las = 2, cex.axis = 0.75)

  for (m in seq_along(pos)) {
    vals <- apd_list[[m]]
    if (length(vals) == 0) next
    sec_names <- names(apd_list[[m]])
    sizes_raw <- sector_sizes_all[sec_names]
    sizes <- log(as.numeric(sizes_raw)) / max(log(sector_sizes_all))
    sizes[is.na(sizes)] <- 0.5
    jx <- pos[m] + runif(length(vals), -0.15, 0.15)
    points(jx, vals, pch = 19, col = col_A, cex = 0.5 + 1.5 * sizes)
    segments(pos[m] - 0.25, median(vals, na.rm = TRUE),
             pos[m] + 0.25, median(vals, na.rm = TRUE), col = "red", lwd = 2)
  }

  dev.off()
  cat("Figure saved:", filepath, "\n")
}


# =============================================================================
# Generate figures
# =============================================================================
# Figure 1: eligible sectors, sector vs firm CV (two aesthetic versions)
draw_figure_eligible(file.path(OUTPUT_DIR, "figure_sector_heterogeneity_v1.pdf"),
                     separator = FALSE)
draw_figure_eligible(file.path(OUTPUT_DIR, "figure_sector_heterogeneity_v2.pdf"),
                     separator = TRUE)

# Figure 2: all sectors, sector-level CV only
draw_figure_all_sectors(file.path(OUTPUT_DIR, "figure_sector_heterogeneity_all_sectors.pdf"))


# =============================================================================
# Save results
# =============================================================================
results <- list(
  # Panel A — all sectors
  rho_rev_A_all      = sm_rev_A_all$rho,    apd_rev_A_all      = sm_rev_A_all$apd,
  rho_en_A_all       = rho_en_A_mean,       apd_en_A_all       = apd_en_A_mean,
  rho_nace_A_all     = sm_nace_A_all$rho,   apd_nace_A_all     = sm_nace_A_all$apd,
  rho_gated_A_all    = rho_gated_A_mean,    apd_gated_A_all    = apd_gated_A_mean,
  rho_combined_A_all = rho_combined_A_mean, apd_combined_A_all = apd_combined_A_mean,
  # Panel A — eligible sectors only
  rho_rev_A_elig  = sm_rev_A_elig$rho,  apd_rev_A_elig  = sm_rev_A_elig$apd,
  rho_en_A_elig   = rho_en_A_elig_mean, apd_en_A_elig   = apd_en_A_elig_mean,
  rho_nace_A_elig = sm_nace_A_elig$rho, apd_nace_A_elig = sm_nace_A_elig$apd,
  # Panel A repeat-level arrays
  rho_en_A_repeats      = rho_en_A,      apd_en_A_repeats      = apd_en_A,
  rho_en_A_elig_repeats = rho_en_A_elig, apd_en_A_elig_repeats = apd_en_A_elig,
  rho_gated_A_repeats    = rho_gated_A,    apd_gated_A_repeats    = apd_gated_A,
  rho_combined_A_repeats = rho_combined_A, apd_combined_A_repeats = apd_combined_A,
  # Panel B — eligible sectors only
  rho_rev_B  = rho_rev_B_mean,  apd_rev_B  = apd_rev_B_mean,
  rho_en_B   = rho_en_B_mean,   apd_en_B   = apd_en_B_mean,
  rho_nace_B = rho_nace_B_mean, apd_nace_B = apd_nace_B_mean,
  # Panel B repeat-level arrays
  rho_rev_B_repeats  = rho_rev_B,  apd_rev_B_repeats  = apd_rev_B,
  rho_en_B_repeats   = rho_en_B,   apd_en_B_repeats   = apd_en_B,
  rho_nace_B_repeats = rho_nace_B, apd_nace_B_repeats = apd_nace_B,
  # Metadata
  all_sectors = all_sectors, eligible_sectors = eligible_sectors,
  firms_per_sector = firms_per_sector,
  M = M, K_sec = K_sec, K_firm = K_firm,
  BASE_SEED = BASE_SEED, MIN_FIRMS_FIRM_CV = MIN_FIRMS_FIRM_CV
)
rds_path <- file.path(OUTPUT_DIR, "sector_heterogeneity_all.rds")
saveRDS(results, rds_path)
cat("Results saved:", rds_path, "\n")
