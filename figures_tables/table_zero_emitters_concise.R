###############################################################################
# figures_tables/table_zero_emitters_concise.R
#
# PURPOSE
#   Concise sector-specific table (sector-level CV only) for NACE 17/18, 19, 24.
#   7 rows: nRMSE, MAPD, Levels corr, Rank corr, FPR, TPR, Worst-case FP rank
#   9 columns: Revenue, NACE & EN for each of the 3 sector groups
#
# OUTPUT
#   {OUTPUT_DIR}/table_zero_emitters_sector_cv_design_concise.tex
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
source(file.path(UTILS_DIR, "calibration_helpers.R"))

BASE_SEED <- 2026L
K_sec     <- 5L

SECTOR_GROUPS <- list("17/18" = "17/18", "19" = "19", "24" = "24")

# ── Load data ────────────────────────────────────────────────────────────────
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel <- e_sec$repeated_cv_proxy_panel
M <- ncol(proxy_matrix_sec)
rm(e_sec)

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
panel <- panel %>% left_join(
  training_sample %>% select(vat, year, revenue, proxy_tabachova),
  by = c("vat", "year")
)
rm(training_sample, syt)

# ── Helpers ──────────────────────────────────────────────────────────────────
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
  if (length(y_s) >= 3 && sd(y_s) > 0 && sd(yhat_s) > 0) {
    suppressWarnings(cor(y_s, yhat_s, method = "spearman", use = "complete.obs"))
  } else NA_real_
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
  if (length(y_s) >= 3 && sd(y_s) > 0 && sd(yhat_s) > 0) {
    suppressWarnings(cor(y_s, yhat_s, method = "pearson", use = "complete.obs"))
  } else NA_real_
}

rmse_grp <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  sqrt(mean((y[idx] - yhat[idx])^2))
}

# ── Combine NACE 17 and 18 into a single sector for calibration ──────────────
panel$primary_nace2d[panel$primary_nace2d %in% c("17", "18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17", "18")] <- "17/18"

# ── Panel A: Sector-level CV ─────────────────────────────────────────────────
cat("Computing sector-level CV metrics...\n")

grp_names <- names(SECTOR_GROUPS)

# Revenue (deterministic)
fold_k_A <- assign_folds(panel, "sector", K_sec, BASE_SEED + 1L)
yhat_rev <- calibrate_sector(panel, panel$revenue, fold_k_A)

rev_rmse <- rev_nrmse <- rev_mapd <- rev_pear <- rev_spear <- rev_fpr <- rev_tpr <- rev_fpmax <- rev_fpp50 <- setNames(numeric(3), grp_names)
for (g in grp_names) {
  codes <- SECTOR_GROUPS[[g]]
  rev_rmse[g]  <- rmse_grp(panel$y, yhat_rev, panel$nace2d, codes)
  rev_mapd[g]  <- mapd_emitters(panel$y, yhat_rev, panel$nace2d, codes)
  rev_pear[g]  <- pearson_grp(panel$y, yhat_rev, panel$nace2d, codes)
  rev_spear[g] <- spearman_grp(panel$y, yhat_rev, panel$nace2d, codes)
  rev_fpr[g]   <- fpr_grp(panel$y, yhat_rev, panel$nace2d, codes)
  rev_tpr[g]   <- tpr_grp(panel$y, yhat_rev, panel$nace2d, codes)
  rev_fpmax[g] <- fp_severity_rank(panel$y, yhat_rev, panel$nace2d, panel$year, codes, "max")
  rev_fpp50[g] <- fp_severity_rank(panel$y, yhat_rev, panel$nace2d, panel$year, codes, "median")
}
rev_nrmse <- rep(1, 3); names(rev_nrmse) <- grp_names

# NACE model (deterministic)
yhat_nace <- calibrate_sector(panel, panel$proxy_tabachova, fold_k_A)

nace_rmse <- nace_nrmse <- nace_mapd <- nace_pear <- nace_spear <- nace_fpr <- nace_tpr <- nace_fpmax <- nace_fpp50 <- setNames(numeric(3), grp_names)
for (g in grp_names) {
  codes <- SECTOR_GROUPS[[g]]
  nace_rmse[g]  <- rmse_grp(panel$y, yhat_nace, panel$nace2d, codes)
  nace_mapd[g]  <- mapd_emitters(panel$y, yhat_nace, panel$nace2d, codes)
  nace_pear[g]  <- pearson_grp(panel$y, yhat_nace, panel$nace2d, codes)
  nace_spear[g] <- spearman_grp(panel$y, yhat_nace, panel$nace2d, codes)
  nace_fpr[g]   <- fpr_grp(panel$y, yhat_nace, panel$nace2d, codes)
  nace_tpr[g]   <- tpr_grp(panel$y, yhat_nace, panel$nace2d, codes)
  nace_fpmax[g] <- fp_severity_rank(panel$y, yhat_nace, panel$nace2d, panel$year, codes, "max")
  nace_fpp50[g] <- fp_severity_rank(panel$y, yhat_nace, panel$nace2d, panel$year, codes, "median")
}
nace_nrmse <- nace_rmse / rev_rmse

# EN (M repeats)
en_nrmse <- en_mapd <- en_pear <- en_spear <- en_fpr <- en_tpr <- en_fpmax <- en_fpp50 <- matrix(
  NA_real_, nrow = M, ncol = 3, dimnames = list(NULL, grp_names)
)

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels_r <- proxy_to_levels(proxy_matrix_sec[, r])
  yhat_r <- calibrate_sector(panel, en_levels_r, fold_k_r)
  for (g in grp_names) {
    codes <- SECTOR_GROUPS[[g]]
    en_nrmse[r, g] <- rmse_grp(panel$y, yhat_r, panel$nace2d, codes) / rev_rmse[g]
    en_mapd[r, g]  <- mapd_emitters(panel$y, yhat_r, panel$nace2d, codes)
    en_pear[r, g]  <- pearson_grp(panel$y, yhat_r, panel$nace2d, codes)
    en_spear[r, g] <- spearman_grp(panel$y, yhat_r, panel$nace2d, codes)
    en_fpr[r, g]   <- fpr_grp(panel$y, yhat_r, panel$nace2d, codes)
    en_tpr[r, g]   <- tpr_grp(panel$y, yhat_r, panel$nace2d, codes)
    en_fpmax[r, g] <- fp_severity_rank(panel$y, yhat_r, panel$nace2d, panel$year, codes, "max")
    en_fpp50[r, g] <- fp_severity_rank(panel$y, yhat_r, panel$nace2d, panel$year, codes, "median")
  }
  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M))
}

en_mean <- list(
  nrmse = colMeans(en_nrmse, na.rm = TRUE),
  mapd  = colMeans(en_mapd,  na.rm = TRUE),
  pear  = colMeans(en_pear,  na.rm = TRUE),
  spear = colMeans(en_spear, na.rm = TRUE),
  fpr   = colMeans(en_fpr,   na.rm = TRUE),
  tpr   = colMeans(en_tpr,   na.rm = TRUE),
  fpp50 = colMeans(en_fpp50, na.rm = TRUE),
  fpmax = colMeans(en_fpmax, na.rm = TRUE)
)
en_sd <- list(
  nrmse = apply(en_nrmse, 2, sd, na.rm = TRUE),
  mapd  = apply(en_mapd,  2, sd, na.rm = TRUE),
  pear  = apply(en_pear,  2, sd, na.rm = TRUE),
  spear = apply(en_spear, 2, sd, na.rm = TRUE),
  fpr   = apply(en_fpr,   2, sd, na.rm = TRUE),
  tpr   = apply(en_tpr,   2, sd, na.rm = TRUE),
  fpp50 = apply(en_fpp50, 2, sd, na.rm = TRUE),
  fpmax = apply(en_fpmax, 2, sd, na.rm = TRUE)
)

# ── Console output ───────────────────────────────────────────────────────────
cat("\n=== Sector-level CV: Revenue vs NACE vs Elastic Net ===\n\n")
metrics <- c("nRMSE", "MAPD", "Levels corr", "Spearman", "FPR", "TPR", "Med. NE FP rank", "Worst-case FP rank")
rev_vals  <- list(rev_nrmse, rev_mapd, rev_pear, rev_spear, rev_fpr, rev_tpr, rev_fpp50, rev_fpmax)
nace_vals <- list(nace_nrmse, nace_mapd, nace_pear, nace_spear, nace_fpr, nace_tpr, nace_fpp50, nace_fpmax)
en_m <- list(en_mean$nrmse, en_mean$mapd, en_mean$pear, en_mean$spear, en_mean$fpr, en_mean$tpr, en_mean$fpp50, en_mean$fpmax)
en_s <- list(en_sd$nrmse, en_sd$mapd, en_sd$pear, en_sd$spear, en_sd$fpr, en_sd$tpr, en_sd$fpp50, en_sd$fpmax)

for (i in seq_along(metrics)) {
  cat(sprintf("%-15s", metrics[i]))
  for (g in grp_names) {
    cat(sprintf("  %s: Rev %.3f | NACE %.3f | EN %.3f (%.3f)", g, rev_vals[[i]][g], nace_vals[[i]][g], en_m[[i]][g], en_s[[i]][g]))
  }
  cat("\n")
}

# ── LaTeX table ──────────────────────────────────────────────────────────────
fmt <- function(x) ifelse(is.finite(x), formatC(x, format = "f", digits = 3), "---")

SECTOR_LABELS <- c("Paper \\& printing", "Petroleum refining", "Iron \\& steel")

tex <- c(
  "\\begin{tabular}{l ccc ccc ccc}",
  "\\toprule",
  sprintf(" & \\multicolumn{3}{c}{%s} & \\multicolumn{3}{c}{%s} & \\multicolumn{3}{c}{%s} \\\\",
          SECTOR_LABELS[1], SECTOR_LABELS[2], SECTOR_LABELS[3]),
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}",
  paste0(" & ", paste(rep("Rev. & NACE & EN", 3), collapse = " & "), " \\\\"),
  "\\midrule"
)

row_names_tex <- c("nRMSE", "MAPD", "Levels corr.", "Rank corr.",
                    "FPR", "TPR", "Med.\\ FP rank", "Max FP rank")

for (i in seq_along(metrics)) {
  parts <- character(0)
  for (g in grp_names) {
    parts <- c(parts, fmt(rev_vals[[i]][g]), fmt(nace_vals[[i]][g]), fmt(en_m[[i]][g]))
  }
  mean_line <- sprintf("%s & %s \\\\", row_names_tex[i], paste(parts, collapse = " & "))

  # sd line (EN only; Revenue and NACE are deterministic in sector CV)
  sd_parts <- character(0)
  for (g in grp_names) {
    sd_parts <- c(sd_parts, "", "", sprintf("{\\scriptsize(%s)}", fmt(en_s[[i]][g])))
  }
  sd_line <- sprintf(" & %s \\\\", paste(sd_parts, collapse = " & "))

  tex <- c(tex, mean_line, sd_line)
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")

tex_path <- file.path(OUTPUT_DIR, "table_zero_emitters_sector_cv_design_concise.tex")
writeLines(tex, tex_path)
cat("\nLaTeX written to:", tex_path, "\n")
cat("Done.\n")
