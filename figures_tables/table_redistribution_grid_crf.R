###############################################################################
# figures_tables/table_redistribution_grid_crf.R
#
# PURPOSE
#   4 ranking signals x 5 redistribution methods grid of RMSE values,
#   all under CRF-group-level CV. For the appendix.
#
#   Ranking signals: Revenue, EN, NACE, Gated Revenue
#   Redistribution:  Proportional, GPA, GEV, GLO, Log-normal
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_crf_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_redistribution_grid_crf.tex
#   {OUTPUT_DIR}/table_redistribution_grid_crf.rds
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
BASE_SEED <- 2026L
K_crf     <- 5L
M         <- 20L

cat("================================================================\n")
cat("  REDISTRIBUTION GRID — CRF-GROUP CV\n")
cat("  M =", M, "repeats, K =", K_crf, "folds\n")
cat("  4 rankings x 5 redistributions\n")
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
cat(sprintf("  Panel: %d firm-years, %d CRF groups\n\n", N,
            length(unique(panel$primary_crf_group))))

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

# Reference distribution: demeaned log-emissions, pooled across CRF groups
build_reference_dist_crf <- function(panel, fold_k, k) {
  train_idx <- which(fold_k != k & panel$y > 0)
  if (length(train_idx) < 20) return(NULL)
  df <- data.frame(
    log_y     = log(panel$y[train_idx]),
    year      = panel$year[train_idx],
    crf_group = panel$primary_crf_group[train_idx],
    stringsAsFactors = FALSE
  )
  mu_t <- tapply(df$log_y, df$year, mean)
  df$tilde <- df$log_y - mu_t[as.character(df$year)]
  mu_s <- tapply(df$tilde, df$crf_group, mean)
  df$d <- df$tilde - mu_s[df$crf_group]
  sort(df$d)
}

fit_dist <- function(ref_dist, pel_fn) {
  if (is.null(ref_dist) || length(ref_dist) < 20) return(NULL)
  lmoms <- samlmu(ref_dist, nmom = 3)
  tryCatch(pel_fn(lmoms), error = function(e) NULL)
}

# Proportional calibration (CRF group x year)
calibrate_proportional_crf <- function(panel, ranking_signal, fold_k) {
  result <- rep(NA_real_, nrow(panel))
  sy_key <- paste(panel$primary_crf_group, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)

  for (k in sort(unique(fold_k))) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)
    sy_train <- paste(panel$primary_crf_group[train_idx], panel$year[train_idx])
    E_train_sy <- tapply(panel$y[train_idx], sy_train, sum, na.rm = TRUE)
    ho_sy <- sy_key[held_out]

    for (sy in unique(ho_sy)) {
      idx <- held_out[which(ho_sy == sy)]
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

# Parametric calibration (CRF group x year) — generic for GPA/GEV/GLO/LN3
calibrate_parametric_crf <- function(panel, ranking_signal, fold_k, pel_fn, qua_fn) {
  result <- rep(NA_real_, nrow(panel))
  sy_key <- paste(panel$primary_crf_group, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)

  for (k in sort(unique(fold_k))) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)

    ref_k <- build_reference_dist_crf(panel, fold_k, k)
    dist_params <- fit_dist(ref_k, pel_fn)

    sy_train <- paste(panel$primary_crf_group[train_idx], panel$year[train_idx])
    E_train_sy <- tapply(panel$y[train_idx], sy_train, sum, na.rm = TRUE)
    ho_sy <- sy_key[held_out]

    for (sy in unique(ho_sy)) {
      idx <- held_out[which(ho_sy == sy)]
      E_total <- E_sy[sy]
      E_train <- ifelse(is.na(E_train_sy[sy]), 0, E_train_sy[sy])
      E_target <- E_total - E_train
      if (is.na(E_target) || E_target <= 0) { result[idx] <- 0; next }

      emitter_mask <- ranking_signal[idx] > 0
      n_emit <- sum(emitter_mask)

      if (n_emit < 2 || is.null(dist_params)) {
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

      w_i <- tryCatch(qua_fn(p_i, dist_params),
                       error = function(e) rep(NA, n_emit))
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
# MAIN LOOP
# =============================================================================
ranking_names <- c("Revenue", "Elastic Net", "NACE", "Gated Rev.")
redist_names  <- c("Proportional", "GPA", "GEV", "GLO", "Log-normal")

# Storage: M x 4 rankings x 5 redistributions x 3 metrics
metric_list <- c("rmse", "mapd", "spearman")
rmse_array <- array(NA_real_,
  dim = c(M, length(ranking_names), length(redist_names)),
  dimnames = list(NULL, ranking_names, redist_names))
mapd_array <- array(NA_real_,
  dim = c(M, length(ranking_names), length(redist_names)),
  dimnames = list(NULL, ranking_names, redist_names))
spearman_array <- array(NA_real_,
  dim = c(M, length(ranking_names), length(redist_names)),
  dimnames = list(NULL, ranking_names, redist_names))

# Helper: compute metrics from y and yhat
compute_three <- function(y, yhat) {
  ok <- is.finite(y) & is.finite(yhat)
  y <- y[ok]; yhat <- yhat[ok]
  rmse_val <- sqrt(mean((y - yhat)^2))
  emit <- (y > 0)
  mapd_val <- if (any(emit)) median(abs(y[emit] - yhat[emit]) / y[emit]) else NA_real_
  spear_val <- if (length(y) >= 3 && sd(y) > 0 && sd(yhat) > 0) {
    suppressWarnings(cor(y, yhat, method = "spearman", use = "complete.obs"))
  } else NA_real_
  c(rmse = rmse_val, mapd = mapd_val, spearman = spear_val)
}

cat(sprintf("Running %d repeats x 4 rankings x 5 redistributions...\n", M))

redist_fns <- list(
  "Proportional" = NULL,
  "GPA"          = list(pel = pelgpa, qua = quagpa),
  "GEV"          = list(pel = pelgev, qua = quagev),
  "GLO"          = list(pel = pelglo, qua = quaglo),
  "Log-normal"   = list(pel = pelln3, qua = qualn3)
)

for (r in seq_len(M)) {
  fold_k <- assign_folds_crf(panel, K_crf, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_crf[, r])
  gated_score <- as.numeric(en_levels > 0) * panel$revenue

  signals <- list(
    "Revenue"     = panel$revenue,
    "Elastic Net" = en_levels,
    "NACE"        = panel$proxy_tabachova,
    "Gated Rev."  = gated_score
  )

  for (sig_name in ranking_names) {
    sig <- signals[[sig_name]]

    for (rd_name in redist_names) {
      if (rd_name == "Proportional") {
        yhat <- calibrate_proportional_crf(panel, sig, fold_k)
      } else {
        fns <- redist_fns[[rd_name]]
        yhat <- calibrate_parametric_crf(panel, sig, fold_k, fns$pel, fns$qua)
      }

      m <- compute_three(panel$y, yhat)
      rmse_array[r, sig_name, rd_name]     <- m["rmse"]
      mapd_array[r, sig_name, rd_name]     <- m["mapd"]
      spearman_array[r, sig_name, rd_name] <- m["spearman"]
    }
  }

  if (r %% 5 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("Done.\n\n")

# =============================================================================
# RESULTS
# =============================================================================
mean_rmse     <- apply(rmse_array, c(2, 3), mean, na.rm = TRUE)
sd_rmse       <- apply(rmse_array, c(2, 3), sd,   na.rm = TRUE)
mean_mapd     <- apply(mapd_array, c(2, 3), mean, na.rm = TRUE)
sd_mapd       <- apply(mapd_array, c(2, 3), sd,   na.rm = TRUE)
mean_spearman <- apply(spearman_array, c(2, 3), mean, na.rm = TRUE)
sd_spearman   <- apply(spearman_array, c(2, 3), sd,   na.rm = TRUE)

# Console
print_grid <- function(title, mean_mat, sd_mat, fmt_fn) {
  cat(title, "— mean (sd) across M =", M, "repeats\n\n")
  cat(sprintf("%-15s", ""))
  for (rd in redist_names) cat(sprintf("%18s", rd))
  cat("\n")
  cat(paste(rep("-", 15 + 18 * length(redist_names)), collapse = ""), "\n")
  for (rk in ranking_names) {
    cat(sprintf("%-15s", rk))
    for (rd in redist_names) {
      cat(sprintf("  %s (%s)", fmt_fn(mean_mat[rk, rd]), fmt_fn(sd_mat[rk, rd])))
    }
    cat("\n")
  }
  cat("\n")
}

fmt0 <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt3 <- function(x) formatC(x, format = "f", digits = 3)

print_grid("RMSE (tCO2)", mean_rmse, sd_rmse, fmt0)
print_grid("Median APD (emitters)", mean_mapd, sd_mapd, fmt3)
print_grid("Spearman rho", mean_spearman, sd_spearman, fmt3)

# =============================================================================
# LATEX TABLES
# =============================================================================
make_tex_table <- function(mean_mat, sd_mat, fmt_fn, caption_suffix) {
  tex <- c(
    "\\begin{tabular}{l ccccc}",
    "\\toprule",
    sprintf(" & %s \\\\", paste(redist_names, collapse = " & ")),
    "\\midrule"
  )
  for (rk in ranking_names) {
    mean_parts <- character(0)
    sd_parts   <- character(0)
    for (rd in redist_names) {
      mean_parts <- c(mean_parts, fmt_fn(mean_mat[rk, rd]))
      sd_parts   <- c(sd_parts, sprintf("{\\scriptsize(%s)}", fmt_fn(sd_mat[rk, rd])))
    }
    tex <- c(tex,
      sprintf("%s & %s \\\\", rk, paste(mean_parts, collapse = " & ")),
      sprintf(" & %s \\\\", paste(sd_parts, collapse = " & ")))
  }
  tex <- c(tex, "\\bottomrule", "\\end{tabular}")
  tex
}

# RMSE table
tex_rmse <- make_tex_table(mean_rmse, sd_rmse, fmt0, "RMSE")
tex_path_rmse <- file.path(OUTPUT_DIR, "table_redistribution_grid_crf_rmse.tex")
writeLines(tex_rmse, tex_path_rmse)
cat("RMSE table written to:", tex_path_rmse, "\n")

# MAPD table
tex_mapd <- make_tex_table(mean_mapd, sd_mapd, fmt3, "MAPD")
tex_path_mapd <- file.path(OUTPUT_DIR, "table_redistribution_grid_crf_mapd.tex")
writeLines(tex_mapd, tex_path_mapd)
cat("MAPD table written to:", tex_path_mapd, "\n")

# Spearman table
tex_spear <- make_tex_table(mean_spearman, sd_spearman, fmt3, "Spearman")
tex_path_spear <- file.path(OUTPUT_DIR, "table_redistribution_grid_crf_spearman.tex")
writeLines(tex_spear, tex_path_spear)
cat("Spearman table written to:", tex_path_spear, "\n")

# Save all
rds_path <- file.path(OUTPUT_DIR, "table_redistribution_grid_crf.rds")
saveRDS(list(
  rmse_array = rmse_array, mapd_array = mapd_array, spearman_array = spearman_array,
  mean_rmse = mean_rmse, sd_rmse = sd_rmse,
  mean_mapd = mean_mapd, sd_mapd = sd_mapd,
  mean_spearman = mean_spearman, sd_spearman = sd_spearman,
  ranking_names = ranking_names, redist_names = redist_names, M = M
), rds_path)
cat("Results saved to:", rds_path, "\n")
