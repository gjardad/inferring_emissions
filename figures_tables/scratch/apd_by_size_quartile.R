###############################################################################
# figures_tables/scratch/apd_by_size_quartile.R
#
# PURPOSE
#   Investigate whether EN performs better for smaller firms within sectors.
#   For each proxy (Revenue, EN, NACE), compute median APD by within-sector
#   revenue quartile, then plot the cross-quartile profile.
#
#   Two outputs:
#     1. Pooled figure: median APD by quartile across all eligible sectors
#     2. Sector-specific panels for focal sectors (17/18, 19, 24)
#
#   Both sector-level and firm-level CV designs.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/repeated_cv_proxy_firm_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/scratch/apd_by_size_quartile.pdf
#   Console output
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
source(file.path(UTILS_DIR, "calibration_helpers.R"))

# ── Parameters ─────────────────────────────────────────────────────────────
BASE_SEED         <- 2026L
K_sec             <- 5L
K_firm            <- 5L
MIN_FIRMS         <- 3L
N_QUARTILES       <- 4L
M_USE             <- 50L   # use first 50 repeats (speed vs precision tradeoff)

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
cat(sprintf("  Using M = %d repeats\n", M))

# ── Assign within-sector revenue quartiles ─────────────────────────────────
# Quartile assignment is fixed (based on revenue rank within sector, pooled
# across years). Using primary_nace2d for consistency with fold assignment.
panel <- panel %>%
  group_by(primary_nace2d) %>%
  mutate(
    rev_quartile = ntile(revenue, N_QUARTILES)
  ) %>%
  ungroup()

# Filter to emitters only for APD (APD undefined for y = 0)
emitter_idx <- which(panel$y > 0)

# Filter sectors with >= MIN_FIRMS firms
firms_per_sector <- panel %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")

eligible_sectors <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS) %>%
  pull(primary_nace2d)

cat(sprintf("  Eligible sectors (>= %d firms): %d\n", MIN_FIRMS, length(eligible_sectors)))
cat(sprintf("  Emitters: %d firm-years\n", length(emitter_idx)))

# ── Helper: compute APD by quartile for a given yhat vector ────────────────
apd_by_quartile <- function(y, yhat, quartile, eligible_nace, nace) {
  # APD = |y - yhat| / y, only for emitters in eligible sectors
  keep <- y > 0 & nace %in% eligible_nace
  apd <- abs(y[keep] - yhat[keep]) / y[keep]
  q <- quartile[keep]
  out <- rep(NA_real_, N_QUARTILES)
  names(out) <- 1:N_QUARTILES
  tab <- tapply(apd, q, median, na.rm = TRUE)
  out[names(tab)] <- tab
  out
}

# ── Compute APD by quartile for each model and CV design ──────────────────

# Storage: rows = quartiles, cols = models, for each CV design
# Average across M repeats

models <- c("Revenue", "Elastic Net", "NACE")
designs <- c("Sector CV", "Firm CV")

results <- array(NA_real_,
                 dim = c(N_QUARTILES, length(models), length(designs), M),
                 dimnames = list(
                   paste0("Q", 1:N_QUARTILES),
                   models,
                   designs,
                   NULL
                 ))

cat("\nComputing APD by quartile...\n")

for (r in seq_len(M)) {
  # --- Sector CV ---
  fold_k_sec <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)

  yhat_rev  <- calibrate_sector(panel, panel$revenue, fold_k_sec)
  yhat_en   <- calibrate_sector(panel, proxy_to_levels(proxy_matrix_sec[, r]), fold_k_sec)
  yhat_nace <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_sec)

  results[, "Revenue",     "Sector CV", r] <- apd_by_quartile(panel$y, yhat_rev,  panel$rev_quartile, eligible_sectors, panel$primary_nace2d)
  results[, "Elastic Net", "Sector CV", r] <- apd_by_quartile(panel$y, yhat_en,   panel$rev_quartile, eligible_sectors, panel$primary_nace2d)
  results[, "NACE",        "Sector CV", r] <- apd_by_quartile(panel$y, yhat_nace, panel$rev_quartile, eligible_sectors, panel$primary_nace2d)

  # --- Firm CV ---
  fold_k_firm <- assign_folds(panel, "firm", K_firm, BASE_SEED + r)

  yhat_rev  <- calibrate_sector(panel, panel$revenue, fold_k_firm)
  yhat_en   <- calibrate_sector(panel, proxy_to_levels(proxy_matrix_firm[, r]), fold_k_firm)
  yhat_nace <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_firm)

  results[, "Revenue",     "Firm CV", r] <- apd_by_quartile(panel$y, yhat_rev,  panel$rev_quartile, eligible_sectors, panel$primary_nace2d)
  results[, "Elastic Net", "Firm CV", r] <- apd_by_quartile(panel$y, yhat_en,   panel$rev_quartile, eligible_sectors, panel$primary_nace2d)
  results[, "NACE",        "Firm CV", r] <- apd_by_quartile(panel$y, yhat_nace, panel$rev_quartile, eligible_sectors, panel$primary_nace2d)

  if (r %% 10 == 0) cat(sprintf("  %d/%d\n", r, M))
}

# Average across repeats
mean_results <- apply(results, c(1, 2, 3), mean, na.rm = TRUE)
sd_results   <- apply(results, c(1, 2, 3), sd, na.rm = TRUE)

# ── Print results ──────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════════\n")
cat("Median APD by within-sector revenue quartile (Q1 = smallest)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

for (d in designs) {
  cat(sprintf("--- %s ---\n", d))
  cat(sprintf("%-15s %8s %8s %8s %8s\n", "", "Q1", "Q2", "Q3", "Q4"))
  for (m in models) {
    cat(sprintf("%-15s %8.3f %8.3f %8.3f %8.3f\n",
                m,
                mean_results["Q1", m, d],
                mean_results["Q2", m, d],
                mean_results["Q3", m, d],
                mean_results["Q4", m, d]))
    cat(sprintf("%-15s %8s %8s %8s %8s\n",
                "",
                sprintf("(%.3f)", sd_results["Q1", m, d]),
                sprintf("(%.3f)", sd_results["Q2", m, d]),
                sprintf("(%.3f)", sd_results["Q3", m, d]),
                sprintf("(%.3f)", sd_results["Q4", m, d])))
  }
  cat("\n")
}

# ── Figure: pooled APD by quartile ─────────────────────────────────────────
scratch_dir <- file.path(OUTPUT_DIR, "scratch")
if (!dir.exists(scratch_dir)) dir.create(scratch_dir, recursive = TRUE)

col_rev  <- "grey40"
col_en   <- "steelblue"
col_nace <- "firebrick"

pdf(file.path(scratch_dir, "apd_by_size_quartile.pdf"), width = 9, height = 5)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), oma = c(2, 0, 0, 0))

for (d in designs) {
  qs <- 1:N_QUARTILES
  ylim <- c(0, max(mean_results[, , d], na.rm = TRUE) * 1.1)

  plot(qs, mean_results[, "Revenue", d], type = "b", pch = 19, col = col_rev,
       ylim = ylim, xlab = "Revenue quartile (within sector-year)",
       ylab = "Median APD", main = d, xaxt = "n", lwd = 2)
  axis(1, at = qs, labels = paste0("Q", qs))
  lines(qs, mean_results[, "Elastic Net", d], type = "b", pch = 17, col = col_en, lwd = 2)
  lines(qs, mean_results[, "NACE", d], type = "b", pch = 15, col = col_nace, lwd = 2)

  # Error bars (±1 sd)
  for (m in models) {
    col_m <- switch(m, Revenue = col_rev, `Elastic Net` = col_en, NACE = col_nace)
    arrows(qs, mean_results[, m, d] - sd_results[, m, d],
           qs, mean_results[, m, d] + sd_results[, m, d],
           angle = 90, code = 3, length = 0.03, col = col_m, lwd = 1)
  }
}

# Legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", horiz = TRUE, inset = c(0, 0),
       legend = models,
       col = c(col_rev, col_en, col_nace),
       pch = c(19, 17, 15),
       lty = 1, lwd = 2,
       pt.cex = 1.2, cex = 0.85, bty = "n")

dev.off()
cat("Figure saved to:", file.path(scratch_dir, "apd_by_size_quartile.pdf"), "\n")

# ── Also compute for focal sectors individually ────────────────────────────
focal_groups <- list(
  "17/18" = c(17, 18),
  "19"    = 19,
  "24"    = 24
)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("Focal sector breakdown\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

apd_by_quartile_focal <- function(y, yhat, quartile, focal_nace, nace) {
  keep <- y > 0 & nace %in% focal_nace
  if (sum(keep) < 10) return(rep(NA_real_, N_QUARTILES))
  apd <- abs(y[keep] - yhat[keep]) / y[keep]
  q <- quartile[keep]
  out <- rep(NA_real_, N_QUARTILES)
  names(out) <- 1:N_QUARTILES
  tab <- tapply(apd, q, median, na.rm = TRUE)
  out[names(tab)] <- tab
  out
}

for (fg_name in names(focal_groups)) {
  fg_nace <- focal_groups[[fg_name]]
  cat(sprintf("\n=== NACE %s ===\n", fg_name))

  # Count emitters per quartile
  fg_emitters <- panel %>%
    filter(primary_nace2d %in% fg_nace, y > 0)
  cat(sprintf("  Emitters: %d firm-years\n", nrow(fg_emitters)))
  cat("  Per quartile: ")
  cat(paste(table(fg_emitters$rev_quartile), collapse = " / "), "\n")

  focal_res <- array(NA_real_,
                     dim = c(N_QUARTILES, length(models), length(designs), M),
                     dimnames = list(paste0("Q", 1:N_QUARTILES), models, designs, NULL))

  for (r in seq_len(M)) {
    fold_k_sec  <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
    fold_k_firm <- assign_folds(panel, "firm", K_firm, BASE_SEED + r)

    yhat_rev_s  <- calibrate_sector(panel, panel$revenue, fold_k_sec)
    yhat_en_s   <- calibrate_sector(panel, proxy_to_levels(proxy_matrix_sec[, r]), fold_k_sec)
    yhat_nace_s <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_sec)

    yhat_rev_f  <- calibrate_sector(panel, panel$revenue, fold_k_firm)
    yhat_en_f   <- calibrate_sector(panel, proxy_to_levels(proxy_matrix_firm[, r]), fold_k_firm)
    yhat_nace_f <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_firm)

    focal_res[, "Revenue",     "Sector CV", r] <- apd_by_quartile_focal(panel$y, yhat_rev_s,  panel$rev_quartile, fg_nace, panel$primary_nace2d)
    focal_res[, "Elastic Net", "Sector CV", r] <- apd_by_quartile_focal(panel$y, yhat_en_s,   panel$rev_quartile, fg_nace, panel$primary_nace2d)
    focal_res[, "NACE",        "Sector CV", r] <- apd_by_quartile_focal(panel$y, yhat_nace_s, panel$rev_quartile, fg_nace, panel$primary_nace2d)

    focal_res[, "Revenue",     "Firm CV", r] <- apd_by_quartile_focal(panel$y, yhat_rev_f,  panel$rev_quartile, fg_nace, panel$primary_nace2d)
    focal_res[, "Elastic Net", "Firm CV", r] <- apd_by_quartile_focal(panel$y, yhat_en_f,   panel$rev_quartile, fg_nace, panel$primary_nace2d)
    focal_res[, "NACE",        "Firm CV", r] <- apd_by_quartile_focal(panel$y, yhat_nace_f, panel$rev_quartile, fg_nace, panel$primary_nace2d)
  }

  focal_mean <- apply(focal_res, c(1, 2, 3), mean, na.rm = TRUE)

  for (d in designs) {
    cat(sprintf("  --- %s ---\n", d))
    cat(sprintf("  %-15s %8s %8s %8s %8s\n", "", "Q1", "Q2", "Q3", "Q4"))
    for (m in models) {
      vals <- focal_mean[, m, d]
      cat(sprintf("  %-15s %8.3f %8.3f %8.3f %8.3f\n",
                  m, vals[1], vals[2], vals[3], vals[4]))
    }
  }
}

cat("\nDone.\n")
