###############################################################################
# figures_tables/table_redistribution_grid_crf_zero_emitters.R
#
# PURPOSE
#   For each of the three mixed sectors (paper 17/18, metals 24/25,
#   refining 19), report RMSE, Median APD, and Spearman rho for:
#     4 ranking signals x 5 redistribution methods
#   All under CRF-group-level CV.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_crf_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_redistribution_grid_crf_zero_emitters.rds
#   {OUTPUT_DIR}/table_redistribution_grid_crf_zero_emitters_paper.tex
#   {OUTPUT_DIR}/table_redistribution_grid_crf_zero_emitters_metals.tex
#   {OUTPUT_DIR}/table_redistribution_grid_crf_zero_emitters_refining.tex
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

ZERO_EMITTER_GROUPS <- list(
  paper    = c("17", "18", "17/18"),
  metals   = c("24", "25"),
  refining = c("19")
)

cat("================================================================\n")
cat("  REDISTRIBUTION GRID — ZERO-EMITTER SECTORS — CRF-GROUP CV\n")
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
cat(sprintf("  Panel: %d firm-years\n\n", N))

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

build_reference_dist_crf <- function(panel, fold_k, k) {
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

fit_dist <- function(ref_dist, pel_fn) {
  if (is.null(ref_dist) || length(ref_dist) < 20) return(NULL)
  lmoms <- samlmu(ref_dist, nmom = 3)
  tryCatch(pel_fn(lmoms), error = function(e) NULL)
}

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
      w_i <- tryCatch(qua_fn(p_i, dist_params), error = function(e) rep(NA, n_emit))
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

# Sector-specific metrics
compute_grp_metrics <- function(y, yhat, nace2d, codes) {
  idx <- which(nace2d %in% codes)
  y_s <- y[idx]; yhat_s <- yhat[idx]
  rmse_val <- sqrt(mean((y_s - yhat_s)^2))
  emit <- (y_s > 0)
  mapd_val <- if (any(emit)) median(abs(y_s[emit] - yhat_s[emit]) / y_s[emit]) else NA_real_
  spear_val <- if (length(y_s) >= 3 && sd(y_s) > 0 && sd(yhat_s) > 0) {
    suppressWarnings(cor(y_s, yhat_s, method = "spearman", use = "complete.obs"))
  } else NA_real_
  c(rmse = rmse_val, mapd = mapd_val, spearman = spear_val)
}

# =============================================================================
# MAIN LOOP
# =============================================================================
ranking_names <- c("Revenue", "Elastic Net", "NACE", "Gated Rev.")
redist_names  <- c("Proportional", "GPA", "GEV", "GLO", "Log-normal")
grp_names     <- names(ZERO_EMITTER_GROUPS)
met_names     <- c("rmse", "mapd", "spearman")

# M x 4 rankings x 5 redists x 3 groups x 3 metrics
results <- array(NA_real_,
  dim = c(M, length(ranking_names), length(redist_names),
          length(grp_names), length(met_names)),
  dimnames = list(NULL, ranking_names, redist_names, grp_names, met_names))

redist_fns <- list(
  "Proportional" = NULL,
  "GPA"          = list(pel = pelgpa, qua = quagpa),
  "GEV"          = list(pel = pelgev, qua = quagev),
  "GLO"          = list(pel = pelglo, qua = quaglo),
  "Log-normal"   = list(pel = pelln3, qua = qualn3)
)

cat(sprintf("Running %d repeats...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds_crf(panel, K_crf, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_crf[, r])
  gated_score <- as.numeric(en_levels > 0) * panel$revenue

  signals <- list(
    "Revenue" = panel$revenue, "Elastic Net" = en_levels,
    "NACE" = panel$proxy_tabachova, "Gated Rev." = gated_score
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
      for (g in grp_names) {
        codes <- ZERO_EMITTER_GROUPS[[g]]
        m <- compute_grp_metrics(panel$y, yhat, panel$nace2d, codes)
        results[r, sig_name, rd_name, g, ] <- m
      }
    }
  }
  if (r %% 5 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("Done.\n\n")

# =============================================================================
# CONSOLE OUTPUT
# =============================================================================
for (g in grp_names) {
  cat(sprintf("=== %s ===\n", toupper(g)))
  for (met in met_names) {
    fmt_fn <- if (met == "rmse") function(x) sprintf("%8.0f", x) else function(x) sprintf("%8.3f", x)
    cat(sprintf("\n  %s:\n", met))
    cat(sprintf("  %-15s", ""))
    for (rd in redist_names) cat(sprintf("%16s", rd))
    cat("\n")
    for (rk in ranking_names) {
      cat(sprintf("  %-15s", rk))
      for (rd in redist_names) {
        mn <- mean(results[, rk, rd, g, met], na.rm = TRUE)
        s  <- sd(results[, rk, rd, g, met], na.rm = TRUE)
        cat(sprintf("  %s(%s)", fmt_fn(mn), fmt_fn(s)))
      }
      cat("\n")
    }
  }
  cat("\n")
}

# =============================================================================
# LATEX TABLES (one per sector, metrics stacked as panels)
# =============================================================================
fmt0 <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt3 <- function(x) formatC(x, format = "f", digits = 3)

GROUP_LABELS <- c(paper = "Paper \\& printing (17/18)",
                  metals = "Metals (24/25)",
                  refining = "Petroleum refining (19)")
MET_LABELS <- c(rmse = "RMSE (tCO$_2$)", mapd = "Median APD", spearman = "Spearman $\\rho$")

for (g in grp_names) {
  tex <- c(
    "\\begin{tabular}{l ccccc}",
    "\\toprule",
    sprintf(" & %s \\\\", paste(redist_names, collapse = " & ")),
    "\\midrule"
  )

  for (met in met_names) {
    fmt_fn <- if (met == "rmse") fmt0 else fmt3
    tex <- c(tex,
      sprintf("\\multicolumn{6}{l}{\\textit{%s}} \\\\", MET_LABELS[met]))

    for (rk in ranking_names) {
      mean_parts <- character(0)
      sd_parts   <- character(0)
      for (rd in redist_names) {
        mn <- mean(results[, rk, rd, g, met], na.rm = TRUE)
        s  <- sd(results[, rk, rd, g, met], na.rm = TRUE)
        mean_parts <- c(mean_parts, fmt_fn(mn))
        sd_parts   <- c(sd_parts, sprintf("{\\scriptsize(%s)}", fmt_fn(s)))
      }
      tex <- c(tex,
        sprintf("%s & %s \\\\", rk, paste(mean_parts, collapse = " & ")),
        sprintf(" & %s \\\\", paste(sd_parts, collapse = " & ")))
    }
    if (met != met_names[length(met_names)]) tex <- c(tex, "\\midrule")
  }

  tex <- c(tex, "\\bottomrule", "\\end{tabular}")

  tex_path <- file.path(OUTPUT_DIR,
    sprintf("table_redistribution_grid_crf_zero_emitters_%s.tex", g))
  writeLines(tex, tex_path)
  cat("Written:", tex_path, "\n")
}

# Save
rds_path <- file.path(OUTPUT_DIR, "table_redistribution_grid_crf_zero_emitters.rds")
saveRDS(list(results = results, ranking_names = ranking_names,
             redist_names = redist_names, grp_names = grp_names,
             met_names = met_names, M = M), rds_path)
cat("\nResults saved to:", rds_path, "\n")
