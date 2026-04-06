###############################################################################
# figures_tables/table_main_with_mixed_crf.R
#
# PURPOSE
#   Replicate table_main_results_crf_cv.tex but add a panel for the
#   average across the three mixed sectors (paper 17/18, metals 24/25,
#   refining 19). Two redistribution methods: proportional and GPA.
#   Three ranking signals: Revenue, EN, NACE.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_crf_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_main_with_mixed_crf_cv.tex
#   {OUTPUT_DIR}/table_main_with_mixed_crf_cv.rds
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
BASE_SEED       <- 2026L
K_crf           <- 5L
M               <- 20L
MIN_FIRMS_GROUP <- 3L

ZERO_EMITTER_GROUPS <- list(
  paper    = c("17", "18", "17/18"),
  metals   = c("24", "25"),
  refining = c("19")
)
MIXED_CODES <- unlist(ZERO_EMITTER_GROUPS)

cat("================================================================\n")
cat("  MAIN TABLE + MIXED SECTORS — CRF-GROUP CV\n")
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
cat("Loading data...\n")
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

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
panel <- panel %>% left_join(
  training_sample %>% select(vat, year, revenue, proxy_tabachova),
  by = c("vat", "year")
)
rm(training_sample, syt)

N <- nrow(panel)

# Indices for pooled (groups with >= 3 firms) and mixed sectors
firms_per_group <- panel %>%
  distinct(vat, primary_crf_group) %>%
  count(primary_crf_group, name = "n_firms")
groups_A <- firms_per_group %>%
  filter(n_firms >= MIN_FIRMS_GROUP) %>%
  pull(primary_crf_group)
idx_pooled <- which(panel$primary_crf_group %in% groups_A)
idx_mixed  <- which(panel$nace2d %in% MIXED_CODES)

cat(sprintf("  Panel: %d firm-years\n", N))
cat(sprintf("  Pooled: %d firm-years (%d CRF groups)\n",
            length(idx_pooled), length(groups_A)))
cat(sprintf("  Mixed sectors: %d firm-years\n\n", length(idx_mixed)))

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

calibrate_pareto_crf <- function(panel, ranking_signal, fold_k) {
  result <- rep(NA_real_, nrow(panel))
  sy_key <- paste(panel$primary_crf_group, panel$year)
  E_sy <- tapply(panel$y, sy_key, sum, na.rm = TRUE)
  for (k in sort(unique(fold_k))) {
    held_out <- which(fold_k == k)
    train_idx <- which(fold_k != k)
    ref_k <- build_reference_dist_crf(panel, fold_k, k)
    gpa_params <- if (!is.null(ref_k) && length(ref_k) >= 20) {
      tryCatch(pelgpa(samlmu(ref_k, nmom = 3)), error = function(e) NULL)
    } else NULL
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
# METRICS
# =============================================================================
metric_names <- c("rmse", "median_apd", "pearson", "spearman",
                  "fpr_nonemitters", "tpr_emitters",
                  "avg_nonemit_p50_rank", "avg_nonemit_p99_rank")
extract_metrics <- function(m) sapply(metric_names, function(nm) m[[nm]])

# =============================================================================
# MAIN LOOP: 3 rankings x 2 redistributions, pooled + mixed
# =============================================================================
models <- c("Rev (prop)", "Rev (GPA)",
            "EN (prop)",  "EN (GPA)",
            "NACE (prop)", "NACE (GPA)")

metrics_pooled <- array(NA_real_,
  dim = c(M, length(metric_names), length(models)),
  dimnames = list(NULL, metric_names, models))
metrics_mixed <- array(NA_real_,
  dim = c(M, length(metric_names), length(models)),
  dimnames = list(NULL, metric_names, models))

cat(sprintf("Running %d repeats...\n", M))
for (r in seq_len(M)) {
  fold_k <- assign_folds_crf(panel, K_crf, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_crf[, r])

  signals <- list(Rev = panel$revenue, EN = en_levels, NACE = panel$proxy_tabachova)

  for (sig_name in names(signals)) {
    sig <- signals[[sig_name]]

    yhat_prop <- calibrate_proportional_crf(panel, sig, fold_k)
    yhat_gpa  <- calibrate_pareto_crf(panel, sig, fold_k)

    for (redist in c("prop", "GPA")) {
      yhat <- if (redist == "prop") yhat_prop else yhat_gpa
      mod <- paste0(sig_name, " (", redist, ")")

      # Pooled
      m_p <- calc_metrics(panel$y[idx_pooled], yhat[idx_pooled], fp_threshold = 0,
                          nace2d = panel$nace2d[idx_pooled], year = panel$year[idx_pooled])
      metrics_pooled[r, , mod] <- extract_metrics(m_p)

      # Mixed sectors
      m_m <- calc_metrics(panel$y[idx_mixed], yhat[idx_mixed], fp_threshold = 0,
                          nace2d = panel$nace2d[idx_mixed], year = panel$year[idx_mixed])
      metrics_mixed[r, , mod] <- extract_metrics(m_m)
    }
  }
  if (r %% 5 == 0) cat(sprintf("  %d/%d\n", r, M))
}
cat("Done.\n\n")

mean_pooled <- apply(metrics_pooled, c(2, 3), mean, na.rm = TRUE)
sd_pooled   <- apply(metrics_pooled, c(2, 3), sd,   na.rm = TRUE)
mean_mixed  <- apply(metrics_mixed, c(2, 3), mean, na.rm = TRUE)
sd_mixed    <- apply(metrics_mixed, c(2, 3), sd,   na.rm = TRUE)

rmse_bl_pooled <- mean_pooled["rmse", "Rev (prop)"]
rmse_bl_mixed  <- mean_mixed["rmse", "Rev (prop)"]

# =============================================================================
# CONSOLE OUTPUT
# =============================================================================
print_panel <- function(title, mean_mat, sd_mat, bl) {
  cat(sprintf("\n%s\n", title))
  cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
              "", "RMSE(kt)", "nRMSE", "MAPD", "Pearson", "Spearman",
              "FPR", "TPR", "p50", "p99"))
  cat(paste(rep("-", 110), collapse = ""), "\n")
  for (mod in models) {
    mn <- mean_mat[, mod]; s <- sd_mat[, mod]
    cat(sprintf("%-20s %10.1f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f %10.3f\n",
                mod, mn["rmse"]/1e3, mn["rmse"]/bl, mn["median_apd"],
                mn["pearson"], mn["spearman"],
                mn["fpr_nonemitters"], mn["tpr_emitters"],
                mn["avg_nonemit_p50_rank"], mn["avg_nonemit_p99_rank"]))
    cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s %10s %10s\n",
                "",
                sprintf("(%.1f)", s["rmse"]/1e3),
                sprintf("(%.3f)", s["rmse"]/bl),
                sprintf("(%.3f)", s["median_apd"]),
                sprintf("(%.3f)", s["pearson"]),
                sprintf("(%.3f)", s["spearman"]),
                sprintf("(%.3f)", s["fpr_nonemitters"]),
                sprintf("(%.3f)", s["tpr_emitters"]),
                sprintf("(%.3f)", s["avg_nonemit_p50_rank"]),
                sprintf("(%.3f)", s["avg_nonemit_p99_rank"])))
  }
}

print_panel("POOLED (all CRF groups)", mean_pooled, sd_pooled, rmse_bl_pooled)
print_panel("MIXED SECTORS (paper, metals, refining)", mean_mixed, sd_mixed, rmse_bl_mixed)

# =============================================================================
# LATEX TABLE
# =============================================================================
fmt  <- function(x, digits = 3) formatC(x, format = "f", digits = digits)
fmt1 <- function(x)             formatC(x, format = "f", digits = 1)

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

make_panel <- function(mean_mat, sd_mat, bl, panel_title) {
  lines <- c(
    sprintf("\\multicolumn{10}{l}{\\textit{%s}} \\\\", panel_title),
    "\\multicolumn{10}{l}{\\textit{\\quad Proportional redistribution}} \\\\",
    tex_row("\\quad Revenue",     mean_mat[, "Rev (prop)"],  sd_mat[, "Rev (prop)"],  bl),
    tex_row("\\quad Elastic Net", mean_mat[, "EN (prop)"],   sd_mat[, "EN (prop)"],   bl),
    tex_row("\\quad NACE",        mean_mat[, "NACE (prop)"], sd_mat[, "NACE (prop)"], bl),
    "\\addlinespace",
    "\\multicolumn{10}{l}{\\textit{\\quad GPA redistribution}} \\\\",
    tex_row("\\quad Revenue",     mean_mat[, "Rev (GPA)"],  sd_mat[, "Rev (GPA)"],  bl),
    tex_row("\\quad Elastic Net", mean_mat[, "EN (GPA)"],   sd_mat[, "EN (GPA)"],   bl),
    tex_row("\\quad NACE",        mean_mat[, "NACE (GPA)"], sd_mat[, "NACE (GPA)"], bl)
  )
  lines
}

tex <- c(
  "\\begin{tabular}{l ccc cc cccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Prediction error} & \\multicolumn{2}{c}{Correlation} & \\multicolumn{4}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-10}",
  " & RMSE & nRMSE & MAPD & Levels & Rank & FPR & TPR & p50 & p99 \\\\",
  "\\midrule",
  make_panel(mean_pooled, sd_pooled, rmse_bl_pooled, "Panel A: All sectors"),
  "\\midrule",
  make_panel(mean_mixed, sd_mixed, rmse_bl_mixed, "Panel B: Mixed sectors (17/18, 19, 24/25)"),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_path <- file.path(OUTPUT_DIR, "table_main_with_mixed_crf_cv.tex")
writeLines(tex, tex_path)
cat("\nLaTeX table written to:", tex_path, "\n")

# Save
rds_path <- file.path(OUTPUT_DIR, "table_main_with_mixed_crf_cv.rds")
saveRDS(list(
  metrics_pooled = metrics_pooled, metrics_mixed = metrics_mixed,
  mean_pooled = mean_pooled, sd_pooled = sd_pooled,
  mean_mixed = mean_mixed, sd_mixed = sd_mixed,
  rmse_bl_pooled = rmse_bl_pooled, rmse_bl_mixed = rmse_bl_mixed,
  models = models, M = M, K_crf = K_crf
), rds_path)
cat("Results saved to:", rds_path, "\n")
