###############################################################################
# figures_tables/sector_heterogeneity_crf.R
#
# PURPOSE
#   Sector-by-sector breakdown of prediction quality under CRF-group-level
#   CV. For each NACE 2d sector, compute within-sector Spearman rho and
#   Median APD (among emitters) for 3 ranking signals (Revenue, EN, NACE)
#   x 2 redistribution methods (proportional, GPA) = 6 models.
#
#   Produces a bubble chart analogous to
#   figure_sector_heterogeneity_all_sectors.pdf but with CRF-group folds
#   and CRF-group x year calibration cells.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_crf_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/figure_sector_heterogeneity_crf.pdf
#   {OUTPUT_DIR}/sector_heterogeneity_crf.rds
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

# ── Parameters ───────────────────────────────────────────────────────────────
BASE_SEED       <- 2026L
K_crf           <- 5L
M               <- 20L
MIN_FIRMS       <- 3L

cat("================================================================\n")
cat("  SECTOR HETEROGENEITY — CRF-GROUP CV\n")
cat("  M =", M, "repeats, K =", K_crf, "folds\n")
cat("================================================================\n\n")

# =============================================================================
# CRF GROUP MAP
# =============================================================================
crf_group_map <- data.frame(
  nace2d = c(
    "35", "19", "24", "25", "20", "21",
    "17", "18", "17/18",
    "10", "11", "12", "23",
    "05", "06", "07", "08", "09",
    "13", "14", "15", "16", "22",
    "26", "27", "28", "29", "30", "31", "32", "33",
    "41", "42", "43",
    "36", "37", "38", "39",
    "45", "46", "47", "52", "53",
    "55", "56", "58", "59", "60", "61", "62", "63",
    "64", "65", "66",
    "68", "69", "70", "71", "72", "73", "74", "75",
    "77", "78", "79", "80", "81", "82",
    "84", "85", "86", "87", "88",
    "90", "91", "92", "93", "94", "95", "96",
    "01", "02", "03", "49", "50", "51"
  ),
  crf_group = c(
    "energy", "refining", "metals", "metals", "chemicals", "chemicals",
    "paper", "paper", "paper",
    "food", "food", "food", "minerals",
    rep("mfg_other", 5), rep("mfg_other", 5), rep("mfg_other", 8), rep("mfg_other", 3),
    rep("commercial", 4), rep("commercial", 5), rep("commercial", 8),
    rep("commercial", 3), rep("commercial", 8), rep("commercial", 6),
    rep("commercial", 5), rep("commercial", 7),
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

panel$primary_nace2d[panel$primary_nace2d %in% c("17", "18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17", "18")] <- "17/18"

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

# Sectors with enough firms
firms_per_sector <- panel %>%
  distinct(vat, primary_nace2d) %>%
  count(primary_nace2d, name = "n_firms")
all_sectors <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS) %>%
  pull(primary_nace2d) %>%
  sort()

sector_sizes <- as.numeric(table(panel$nace2d)[all_sectors])
names(sector_sizes) <- all_sectors

cat(sprintf("  Panel: %d firm-years, %d sectors with >= %d firms\n",
            N, length(all_sectors), MIN_FIRMS))

# =============================================================================
# HELPERS
# =============================================================================
proxy_to_levels <- function(proxy) pmax(sinh(proxy), 0)

assign_folds_crf <- function(panel, K, seed) {
  set.seed(seed)
  crf_groups <- sort(unique(panel$primary_crf_group[!is.na(panel$primary_crf_group)]))
  group_folds <- sample(rep(1:K, length.out = length(crf_groups)))
  gfm <- data.frame(primary_crf_group = crf_groups, fold_k = group_folds,
                     stringsAsFactors = FALSE)
  gfm$fold_k[match(panel$primary_crf_group, gfm$primary_crf_group)]
}

# Proportional calibration (CRF group x year cells)
calibrate_proportional_crf <- function(panel, ranking_signal, fold_k) {
  result <- rep(NA_real_, nrow(panel))
  sy_key <- paste(panel$primary_crf_group, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)
  folds <- sort(unique(fold_k))

  for (k in folds) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)
    sy_train <- paste(panel$primary_crf_group[train_idx], panel$year[train_idx])
    E_train_sy <- tapply(panel$y[train_idx], sy_train, sum, na.rm = TRUE)
    ho_sy <- sy_key[held_out]

    for (sy in unique(ho_sy)) {
      idx_in_ho <- which(ho_sy == sy)
      idx <- held_out[idx_in_ho]
      E_total <- E_sy[sy]
      E_train <- ifelse(is.na(E_train_sy[sy]), 0, E_train_sy[sy])
      E_target <- E_total - E_train
      if (is.na(E_target) || E_target <= 0) { result[idx] <- 0; next }
      raw <- ranking_signal[idx]
      denom <- sum(raw, na.rm = TRUE)
      if (denom > 0) result[idx] <- E_target * (raw / denom)
      else result[idx] <- E_target / length(idx)
    }
  }
  result
}

# GPA calibration (CRF group x year cells)
build_reference_dist <- function(panel, fold_k, k) {
  train_idx <- which(fold_k != k & panel$y > 0)
  if (length(train_idx) < 20) return(NULL)
  df <- data.frame(
    log_y = log(panel$y[train_idx]),
    year = panel$year[train_idx],
    crf_group = panel$primary_crf_group[train_idx],
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
      if (is.na(E_target) || E_target <= 0) { result[idx] <- 0; next }

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

# Within-sector metrics
compute_sector_metrics <- function(y, yhat, nace2d, year, sectors) {
  all_yrs <- sort(unique(year))
  y_dm <- numeric(length(y))
  yhat_dm <- numeric(length(y))
  for (yr in all_yrs) {
    idx <- which(year == yr)
    y_dm[idx] <- y[idx] - mean(y[idx])
    yhat_dm[idx] <- yhat[idx] - mean(yhat[idx])
  }

  rho_vec <- setNames(rep(NA_real_, length(sectors)), sectors)
  apd_vec <- setNames(rep(NA_real_, length(sectors)), sectors)

  for (sec in sectors) {
    idx <- which(nace2d == sec)
    if (length(idx) >= 3 && sd(y_dm[idx]) > 0 && sd(yhat_dm[idx]) > 0) {
      rho_vec[sec] <- suppressWarnings(
        stats::cor(y_dm[idx], yhat_dm[idx], method = "spearman", use = "complete.obs")
      )
    }
    emit_idx <- idx[y[idx] > 0]
    if (length(emit_idx) >= 1) {
      apd_vec[sec] <- median(abs(y[emit_idx] - yhat[emit_idx]) / y[emit_idx], na.rm = TRUE)
    }
  }
  list(rho = rho_vec, apd = apd_vec)
}

# =============================================================================
# COMPUTE SECTOR METRICS: 3 signals x 2 redistributions x M repeats
# =============================================================================
model_names <- c("Rev (prop)", "Rev (GPA)", "EN (prop)", "EN (GPA)",
                 "NACE (prop)", "NACE (GPA)")

rho_arrays <- list()
apd_arrays <- list()
for (mod in model_names) {
  rho_arrays[[mod]] <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                               dimnames = list(NULL, all_sectors))
  apd_arrays[[mod]] <- matrix(NA_real_, nrow = M, ncol = length(all_sectors),
                               dimnames = list(NULL, all_sectors))
}

cat(sprintf("Computing sector metrics (%d repeats)...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds_crf(panel, K_crf, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_crf[, r])

  signals <- list(Rev = panel$revenue, EN = en_levels, NACE = panel$proxy_tabachova)

  for (sig_name in names(signals)) {
    sig <- signals[[sig_name]]

    yhat_prop <- calibrate_proportional_crf(panel, sig, fold_k)
    sm_prop <- compute_sector_metrics(panel$y, yhat_prop, panel$nace2d, panel$year, all_sectors)
    rho_arrays[[paste0(sig_name, " (prop)")]][r, ] <- sm_prop$rho[all_sectors]
    apd_arrays[[paste0(sig_name, " (prop)")]][r, ] <- sm_prop$apd[all_sectors]

    yhat_gpa <- calibrate_pareto_crf(panel, sig, fold_k)
    sm_gpa <- compute_sector_metrics(panel$y, yhat_gpa, panel$nace2d, panel$year, all_sectors)
    rho_arrays[[paste0(sig_name, " (GPA)")]][r, ] <- sm_gpa$rho[all_sectors]
    apd_arrays[[paste0(sig_name, " (GPA)")]][r, ] <- sm_gpa$apd[all_sectors]
  }

  if (r %% 5 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("Done.\n")

# Average across repeats
rho_means <- lapply(rho_arrays, colMeans, na.rm = TRUE)
apd_means <- lapply(apd_arrays, colMeans, na.rm = TRUE)

# =============================================================================
# CONSOLE SUMMARY
# =============================================================================
print_summary <- function(lbl, vals) {
  v <- vals[is.finite(vals)]
  cat(sprintf("%-20s %8.3f %8.3f %8.3f %8.3f %8.3f  %d\n",
              lbl, median(v), quantile(v, 0.25), quantile(v, 0.75),
              min(v), max(v), length(v)))
}
hdr <- function() {
  cat(sprintf("%-20s %8s %8s %8s %8s %8s  %s\n",
              "", "Median", "p25", "p75", "Min", "Max", "N"))
  cat(paste(rep("-", 75), collapse = ""), "\n")
}

cat("\n-- Within-sector Spearman rho --\n"); hdr()
for (mod in model_names) print_summary(mod, rho_means[[mod]])

cat("\n-- Within-sector Median APD --\n"); hdr()
for (mod in model_names) print_summary(mod, apd_means[[mod]])

# =============================================================================
# FIGURE: 4-panel bubble chart (rho and APD, prop and GPA)
# =============================================================================
cat("\nGenerating figure...\n")

cols <- c("Rev" = "grey60", "EN" = "black", "NACE" = "steelblue3")

fig_path <- file.path(OUTPUT_DIR, "figure_sector_heterogeneity_crf.pdf")
pdf(fig_path, width = 14, height = 10)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (redist in c("prop", "GPA")) {
  for (metric in c("rho", "apd")) {
    means_list <- if (metric == "rho") rho_means else apd_means
    mod_suffix <- paste0(" (", redist, ")")

    # Data for 3 models
    rev_vals  <- means_list[[paste0("Rev", mod_suffix)]]
    en_vals   <- means_list[[paste0("EN", mod_suffix)]]
    nace_vals <- means_list[[paste0("NACE", mod_suffix)]]

    # Scale dot sizes
    sz <- sqrt(sector_sizes / max(sector_sizes)) * 4

    if (metric == "rho") {
      ylim <- range(c(rev_vals, en_vals, nace_vals), na.rm = TRUE)
      ylim[1] <- min(ylim[1], -0.5)
      ylim[2] <- max(ylim[2], 1.0)
      ylab <- "Spearman rho"
    } else {
      ylim <- c(0, max(c(rev_vals, en_vals, nace_vals), na.rm = TRUE) * 1.1)
      ylim[2] <- min(ylim[2], 2.0)
      ylab <- "Median APD"
    }

    n_sec <- length(all_sectors)
    x_base <- seq_len(n_sec)

    plot(NA, xlim = c(0.5, n_sec + 0.5), ylim = ylim,
         xlab = "", ylab = ylab, xaxt = "n",
         main = paste0(ifelse(metric == "rho", "Rank correlation", "Median APD"),
                       " — ", ifelse(redist == "prop", "Proportional", "GPA")))
    axis(1, at = x_base, labels = all_sectors, las = 2, cex.axis = 0.7)
    abline(h = ifelse(metric == "rho", 0, 1), lty = 2, col = "grey80")

    offset <- c(-0.2, 0, 0.2)
    for (j in seq_along(c("Rev", "EN", "NACE"))) {
      mod_name <- paste0(c("Rev", "EN", "NACE")[j], mod_suffix)
      vals <- means_list[[mod_name]]
      points(x_base + offset[j], vals, pch = 16,
             cex = sz, col = adjustcolor(cols[j], alpha.f = 0.7))
    }

    legend("topright", legend = c("Revenue", "EN", "NACE"),
           col = unname(cols), pch = 16, pt.cex = 1.5, bty = "n", cex = 0.9)
  }
}
mtext("Sector Heterogeneity — CRF-Group CV", outer = TRUE, cex = 1.2)
dev.off()
cat("Figure saved to:", fig_path, "\n")

# =============================================================================
# PER-SECTOR WINNER TABLE
# =============================================================================
cat("\n-- Per-sector best model --\n")
cat(sprintf("%-10s | %-20s %-20s | %-20s %-20s\n",
            "Sector", "Best rho (prop)", "Best rho (GPA)",
            "Best APD (prop)", "Best APD (GPA)"))
cat(paste(rep("-", 95), collapse = ""), "\n")

for (sec in all_sectors) {
  best <- character(4)
  for (i_redist in 1:2) {
    redist <- c("prop", "GPA")[i_redist]
    for (i_met in 1:2) {
      metric <- c("rho", "apd")[i_met]
      means_list <- if (metric == "rho") rho_means else apd_means
      vals <- sapply(c("Rev", "EN", "NACE"), function(s) {
        means_list[[paste0(s, " (", redist, ")")]][sec]
      })
      if (all(is.na(vals))) { best[(i_redist-1)*2 + i_met] <- "NA"; next }
      winner <- if (metric == "rho") {
        c("Rev", "EN", "NACE")[which.max(vals)]
      } else {
        c("Rev", "EN", "NACE")[which.min(vals)]
      }
      best[(i_redist-1)*2 + i_met] <- winner
    }
  }
  cat(sprintf("%-10s | %-20s %-20s | %-20s %-20s\n",
              sec, best[1], best[2], best[3], best[4]))
}

# =============================================================================
# SAVE
# =============================================================================
all_results <- list(
  rho_means = rho_means, apd_means = apd_means,
  rho_arrays = rho_arrays, apd_arrays = apd_arrays,
  all_sectors = all_sectors, sector_sizes = sector_sizes,
  model_names = model_names, M = M, K_crf = K_crf
)
rds_path <- file.path(OUTPUT_DIR, "sector_heterogeneity_crf.rds")
saveRDS(all_results, rds_path)
cat("\nResults saved to:", rds_path, "\n")
cat("Done.\n")
