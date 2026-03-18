###############################################################################
# figures_tables/table_sector_comparison_en_vs_benchmarks.R
#
# PURPOSE
#   Sector-by-sector comparison of EN vs Revenue vs NACE under sector-level CV.
#   Reports within-sector Spearman rho and Median APD for each model,
#   plus a tally of how many sectors EN > Revenue, EN > NACE, etc.
#
#   NACE 17 and 18 are combined into a single "17/18" group (paper, pulp &
#   printing) consistent with the training sample design.
#
# INPUT
#   {PROC_DATA}/repeated_cv_proxy_sector_asinh.RData
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   {OUTPUT_DIR}/table_sector_comparison_en_vs_benchmarks.tex
#   {OUTPUT_DIR}/table_sector_comparison_en_vs_benchmarks.rds
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
MIN_FIRMS_SECTOR_CV <- 3L

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading data...\n")

e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel_sec        <- e_sec$repeated_cv_proxy_panel
M <- ncol(proxy_matrix_sec)
rm(e_sec)

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
determ_df <- training_sample %>% select(vat, year, revenue, proxy_tabachova)
rm(training_sample, syt)

panel_sec <- panel_sec %>% left_join(determ_df, by = c("vat", "year"))
rm(determ_df)

# ── Create sector_group column (combine 17 + 18 → "17/18") ──────────────────
panel_sec$sector_group <- ifelse(panel_sec$nace2d %in% c("17", "18"),
                                  "17/18", panel_sec$nace2d)

cat(sprintf("  %d obs, %d repeats\n", nrow(panel_sec), M))

# ── Sector list (>= MIN_FIRMS_SECTOR_CV firms) ──────────────────────────────
firms_per_sector <- panel_sec %>%
  distinct(vat, sector_group) %>%
  count(sector_group, name = "n_firms")

fig_sectors <- firms_per_sector %>%
  filter(n_firms >= MIN_FIRMS_SECTOR_CV) %>%
  arrange(sector_group) %>%
  pull(sector_group)

cat(sprintf("  %d sector groups with >= %d firms\n",
            length(fig_sectors), MIN_FIRMS_SECTOR_CV))

# ── Helper: within-sector-group metrics ──────────────────────────────────────
compute_group_metrics <- function(y, yhat, sector_group, year, groups) {
  # Year-demean
  all_yrs <- sort(unique(year))
  y_dm    <- numeric(length(y))
  yhat_dm <- numeric(length(y))
  for (yr in all_yrs) {
    idx <- which(year == yr)
    y_dm[idx]    <- y[idx] - mean(y[idx])
    yhat_dm[idx] <- yhat[idx] - mean(yhat[idx])
  }

  rho_vec <- setNames(rep(NA_real_, length(groups)), groups)
  apd_vec <- setNames(rep(NA_real_, length(groups)), groups)

  for (grp in groups) {
    idx <- which(sector_group == grp)

    # Demeaned Spearman rho
    if (length(idx) >= 3 && sd(y_dm[idx]) > 0 && sd(yhat_dm[idx]) > 0) {
      rho_vec[grp] <- suppressWarnings(
        stats::cor(y_dm[idx], yhat_dm[idx], method = "spearman", use = "complete.obs")
      )
    }

    # Median APD among emitters
    emit_idx <- idx[y[idx] > 0]
    if (length(emit_idx) >= 1) {
      apd_vec[grp] <- median(abs(y[emit_idx] - yhat[emit_idx]) / y[emit_idx],
                             na.rm = TRUE)
    }
  }

  list(rho = rho_vec, apd = apd_vec)
}

# ── Revenue & NACE (deterministic) ──────────────────────────────────────────
cat("Computing Revenue and NACE (deterministic)...\n")
fold_k_det <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + 1L)

yhat_rev  <- calibrate_sector(panel_sec, panel_sec$revenue, fold_k_det)
sm_rev    <- compute_group_metrics(panel_sec$y, yhat_rev,
                                    panel_sec$sector_group, panel_sec$year,
                                    fig_sectors)

yhat_nace <- calibrate_sector(panel_sec, panel_sec$proxy_tabachova, fold_k_det)
sm_nace   <- compute_group_metrics(panel_sec$y, yhat_nace,
                                    panel_sec$sector_group, panel_sec$year,
                                    fig_sectors)

# ── EN: average over M repeats ──────────────────────────────────────────────
cat(sprintf("Computing EN (%d repeats)...\n", M))
rho_en_mat <- matrix(NA_real_, nrow = M, ncol = length(fig_sectors),
                     dimnames = list(NULL, fig_sectors))
apd_en_mat <- matrix(NA_real_, nrow = M, ncol = length(fig_sectors),
                     dimnames = list(NULL, fig_sectors))

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel_sec, "sector", K_sec, BASE_SEED + r)
  en_levels <- proxy_to_levels(proxy_matrix_sec[, r])
  yhat_r    <- calibrate_sector(panel_sec, en_levels, fold_k_r)
  sm_r      <- compute_group_metrics(panel_sec$y, yhat_r,
                                      panel_sec$sector_group, panel_sec$year,
                                      fig_sectors)
  rho_en_mat[r, ] <- sm_r$rho[fig_sectors]
  apd_en_mat[r, ] <- sm_r$apd[fig_sectors]
  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M))
}
rho_en <- colMeans(rho_en_mat, na.rm = TRUE)
apd_en <- colMeans(apd_en_mat, na.rm = TRUE)
cat("Done.\n")

# ── Emitter counts per sector group ──────────────────────────────────────────
emitters_per_sector <- panel_sec %>%
  filter(sector_group %in% fig_sectors, y > 0) %>%
  distinct(vat, sector_group) %>%
  count(sector_group, name = "n_emitters") %>%
  arrange(sector_group)

# ── Build comparison data frame ──────────────────────────────────────────────
comp <- data.frame(
  sector     = fig_sectors,
  n_firms    = firms_per_sector$n_firms[match(fig_sectors, firms_per_sector$sector_group)],
  rho_rev    = sm_rev$rho[fig_sectors],
  rho_nace   = sm_nace$rho[fig_sectors],
  rho_en     = rho_en[fig_sectors],
  apd_rev    = sm_rev$apd[fig_sectors],
  apd_nace   = sm_nace$apd[fig_sectors],
  apd_en     = apd_en[fig_sectors],
  stringsAsFactors = FALSE
)

comp <- comp %>%
  left_join(emitters_per_sector, by = c("sector" = "sector_group"))
comp$n_emitters[is.na(comp$n_emitters)] <- 0L

# ── Winners (Spearman rho: higher is better; Median APD: lower is better) ───
comp <- comp %>%
  mutate(
    rho_en_vs_rev  = case_when(
      is.na(rho_en) | is.na(rho_rev)  ~ "—",
      rho_en > rho_rev                 ~ "EN",
      rho_en < rho_rev                 ~ "Rev",
      TRUE                             ~ "Tie"
    ),
    rho_en_vs_nace = case_when(
      is.na(rho_en) | is.na(rho_nace) ~ "—",
      rho_en > rho_nace                ~ "EN",
      rho_en < rho_nace                ~ "NACE",
      TRUE                             ~ "Tie"
    ),
    apd_en_vs_rev  = case_when(
      is.na(apd_en) | is.na(apd_rev)  ~ "—",
      apd_en < apd_rev                 ~ "EN",
      apd_en > apd_rev                 ~ "Rev",
      TRUE                             ~ "Tie"
    ),
    apd_en_vs_nace = case_when(
      is.na(apd_en) | is.na(apd_nace) ~ "—",
      apd_en < apd_nace                ~ "EN",
      apd_en > apd_nace                ~ "NACE",
      TRUE                             ~ "Tie"
    )
  )

# ── Console summary ─────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════════════\n")
cat("Sector-by-sector comparison (sector-level CV)\n")
cat("══════════════════════════════════════════════════════════════════════\n\n")

cat("── Spearman rho (higher = better) ──\n")
cat(sprintf("  EN > Revenue: %d / %d\n",
            sum(comp$rho_en_vs_rev == "EN"), sum(comp$rho_en_vs_rev != "—")))
cat(sprintf("  EN > NACE:    %d / %d\n",
            sum(comp$rho_en_vs_nace == "EN"), sum(comp$rho_en_vs_nace != "—")))

cat("\n── Median APD (lower = better) ──\n")
cat(sprintf("  EN < Revenue: %d / %d\n",
            sum(comp$apd_en_vs_rev == "EN"), sum(comp$apd_en_vs_rev != "—")))
cat(sprintf("  EN < NACE:    %d / %d\n",
            sum(comp$apd_en_vs_nace == "EN"), sum(comp$apd_en_vs_nace != "—")))

cat("\n── Full table ──\n")
print(comp %>% select(sector, n_firms, n_emitters,
                       rho_rev, rho_nace, rho_en, rho_en_vs_rev, rho_en_vs_nace,
                       apd_rev, apd_nace, apd_en, apd_en_vs_rev, apd_en_vs_nace),
      digits = 3, row.names = FALSE)

# ── LaTeX table ──────────────────────────────────────────────────────────────
fmt <- function(x, d = 3) ifelse(is.na(x), "---", formatC(x, format = "f", digits = d))

bold_winner_rho <- function(rev, nace, en) {
  vals <- c(rev, nace, en)
  strs <- fmt(vals)
  if (all(is.na(vals))) return(strs)
  best <- which.max(vals)
  if (length(best) == 1 && !is.na(vals[best])) {
    strs[best] <- paste0("\\textbf{", strs[best], "}")
  }
  strs
}

bold_winner_apd <- function(rev, nace, en) {
  vals <- c(rev, nace, en)
  strs <- fmt(vals)
  if (all(is.na(vals))) return(strs)
  best <- which.min(vals)
  if (length(best) == 1 && !is.na(vals[best])) {
    strs[best] <- paste0("\\textbf{", strs[best], "}")
  }
  strs
}

lines <- character()
lines <- c(lines, "\\begin{table}[htbp]")
lines <- c(lines, "\\centering")
lines <- c(lines, "\\caption{Sector-by-sector model comparison (sector-level CV)}")
lines <- c(lines, "\\label{tab:sector_comparison}")
lines <- c(lines, "\\small")
lines <- c(lines, "\\begin{tabular}{lrr ccc ccc}")
lines <- c(lines, "\\toprule")
lines <- c(lines, " & & & \\multicolumn{3}{c}{Spearman $\\rho$} & \\multicolumn{3}{c}{Median APD} \\\\")
lines <- c(lines, "\\cmidrule(lr){4-6} \\cmidrule(lr){7-9}")
lines <- c(lines, "NACE & Firms & Emitters & Revenue & NACE & EN & Revenue & NACE & EN \\\\")
lines <- c(lines, "\\midrule")

for (i in seq_len(nrow(comp))) {
  r <- comp[i, ]
  rho_strs <- bold_winner_rho(r$rho_rev, r$rho_nace, r$rho_en)
  apd_strs <- bold_winner_apd(r$apd_rev, r$apd_nace, r$apd_en)

  lines <- c(lines, sprintf("%s & %d & %d & %s & %s & %s & %s & %s & %s \\\\",
                             r$sector, r$n_firms, r$n_emitters,
                             rho_strs[1], rho_strs[2], rho_strs[3],
                             apd_strs[1], apd_strs[2], apd_strs[3]))
}

lines <- c(lines, "\\midrule")

# Tally rows — exclude ties from denominator
n_rho_comp   <- sum(comp$rho_en_vs_rev %in% c("EN", "Rev"))
n_rho_comp_n <- sum(comp$rho_en_vs_nace %in% c("EN", "NACE"))
n_apd_comp   <- sum(comp$apd_en_vs_rev %in% c("EN", "Rev"))
n_apd_comp_n <- sum(comp$apd_en_vs_nace %in% c("EN", "NACE"))

lines <- c(lines, sprintf(
  "\\multicolumn{3}{l}{EN wins} & %d/%d & %d/%d & & %d/%d & %d/%d & \\\\",
  sum(comp$rho_en_vs_rev == "EN"), n_rho_comp,
  sum(comp$rho_en_vs_nace == "EN"), n_rho_comp_n,
  sum(comp$apd_en_vs_rev == "EN"), n_apd_comp,
  sum(comp$apd_en_vs_nace == "EN"), n_apd_comp_n))
lines <- c(lines, sprintf(
  "\\multicolumn{3}{l}{Benchmark wins} & %d/%d & %d/%d & & %d/%d & %d/%d & \\\\",
  sum(comp$rho_en_vs_rev == "Rev"), n_rho_comp,
  sum(comp$rho_en_vs_nace == "NACE"), n_rho_comp_n,
  sum(comp$apd_en_vs_rev == "Rev"), n_apd_comp,
  sum(comp$apd_en_vs_nace == "NACE"), n_apd_comp_n))

lines <- c(lines, "\\bottomrule")
lines <- c(lines, "\\end{tabular}")
lines <- c(lines, "\\begin{tablenotes}")
lines <- c(lines, "\\footnotesize")
lines <- c(lines, paste0("\\item Bold indicates the best-performing model in each row. ",
                          "Spearman $\\rho$: higher is better (year-demeaned, pooled across years). ",
                          "Median APD: lower is better (among emitters). ",
                          "Sectors with $\\geq$ ", MIN_FIRMS_SECTOR_CV, " firms shown. ",
                          "NACE 17 and 18 combined into 17/18 (paper, pulp \\& printing). ",
                          "EN values averaged across ", M, " repeated fold assignments."))
lines <- c(lines, "\\end{tablenotes}")
lines <- c(lines, "\\end{table}")

tex_path <- file.path(OUTPUT_DIR, "table_sector_comparison_en_vs_benchmarks.tex")
writeLines(lines, tex_path)
cat("\nLaTeX table saved:", tex_path, "\n")

# ── Save RDS ─────────────────────────────────────────────────────────────────
rds_path <- file.path(OUTPUT_DIR, "table_sector_comparison_en_vs_benchmarks.rds")
saveRDS(comp, rds_path)
cat("RDS saved:", rds_path, "\n")
