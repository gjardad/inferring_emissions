###############################################################################
# figures_tables/table_hungarian_comparison.R
#
# PURPOSE
#   Generate the 2x2 Hungarian comparison appendix table, decomposing our
#   value-added along two dimensions:
#     Rows:    Proportional allocation (Tabachova method) vs. Hybrid + calibration (ours)
#     Columns: Pre-selected NACE codes (Tabachova proxy) vs. Data-driven (our proxy)
#
#   Proportional allocation cells are computed on the full sample (no CV) since
#   the method has no parameters to fit. Hybrid cells use LOFOCV (group k-fold).
#
# INPUT
#   {PROC_DATA}/training_sample.RData  (must include proxy_tabachova)
#
# OUTPUT
#   {OUTPUT_DIR}/hungarian_comparison.tex
#   {OUTPUT_DIR}/hungarian_comparison.csv
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(mgcv)

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

stopifnot("proxy_tabachova" %in% names(panel))

panel <- panel %>%
  mutate(
    year_f   = factor(year),
    nace2d_f = factor(nace2d),
    nace5d_f = factor(nace5d),
    emit     = as.integer(y > 0),
    log_revenue = ifelse(is.na(log_revenue), 0, log_revenue)
  )

# Recreate foldid if absent
if (!exists("K_FOLDS")) K_FOLDS <- 10L
if (!exists("foldid")) {
  set.seed(42)
  unique_firms <- unique(panel$vat)
  firm_folds <- sample(rep(1:K_FOLDS, length.out = length(unique_firms)))
  names(firm_folds) <- unique_firms
  foldid <- unname(firm_folds[panel$vat])
  cat("(foldid recreated from seed)\n")
}
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Proxy coverage (tabachova > 0):", sum(panel$proxy_tabachova > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_tabachova > 0)))
cat("Proxy coverage (weighted > 0): ", sum(panel$proxy_weighted > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_weighted > 0)))


# ===========================================================================
# CELL A & B: Proportional allocation (full sample, no CV)
# ===========================================================================
# yhat_i = (proxy_i / sum_j(proxy_j)) * E_total_st  per (sector, year)
# Firms with proxy = 0 get yhat = 0.

proportional_allocate <- function(proxy, nace2d, year, syt) {
  df <- data.frame(proxy = proxy, nace2d = nace2d, year = year,
                   stringsAsFactors = FALSE)
  df <- df %>%
    left_join(syt, by = c("nace2d", "year"))

  yhat <- rep(0, nrow(df))

  cells <- unique(df[, c("nace2d", "year"), drop = FALSE])
  for (r in seq_len(nrow(cells))) {
    sec <- cells$nace2d[r]
    yr  <- cells$year[r]
    idx <- which(df$nace2d == sec & df$year == yr)
    E   <- df$E_total[idx[1]]
    if (is.na(E) || E == 0) next
    p <- df$proxy[idx]
    denom <- sum(p, na.rm = TRUE)
    if (denom > 0) {
      yhat[idx] <- E * p / denom
    }
  }
  yhat
}

cat("\n── Cell A: Tabachova proxy + proportional allocation ──\n")
yhat_A <- proportional_allocate(panel$proxy_tabachova, panel$nace2d, panel$year, syt)
m_A <- calc_metrics(panel$y, yhat_A, nace2d = panel$nace2d, year = panel$year)
cat("  nRMSE:", round(m_A$nrmse_sd, 3), "\n")

cat("\n── Cell B: Our proxy + proportional allocation ──\n")
yhat_B <- proportional_allocate(panel$proxy_weighted, panel$nace2d, panel$year, syt)
m_B <- calc_metrics(panel$y, yhat_B, nace2d = panel$nace2d, year = panel$year)
cat("  nRMSE:", round(m_B$nrmse_sd, 3), "\n")


# ===========================================================================
# CELL C: Tabachova proxy + hybrid + calibration (LOFOCV)
# ===========================================================================
cat("\n── Cell C: Tabachova proxy + hybrid + calibration (LOFOCV) ──\n")

re_base <- "year_f + s(nace2d_f, bs = 're')"
THRESHOLDS <- seq(0.01, 0.60, by = 0.01)

# Pre-allocate (only extensive margin needed for hybrid)
panel$phat_tab <- NA_real_

cat("Running LOFOCV for Tabachova hybrid (K =", K_FOLDS, ")...\n")
for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)

  # Extensive margin: logit
  fit_ext <- tryCatch(
    gam(as.formula(paste("emit ~ log_revenue + I(proxy_tabachova > 0) + asinh(proxy_tabachova) +", re_base)),
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit_ext)) {
    phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
    panel$phat_tab[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" done (%.1fs)\n", elapsed))
}

# Threshold search: optimize calibrated_clipped nRMSE
# Hybrid: yhat_raw = I(phat > thr) * proxy_tabachova
cat("Searching optimal threshold...\n")
best_thr_C <- 0.5
best_nrmse_C <- Inf

for (thr in THRESHOLDS) {
  yhat_raw <- ifelse(panel$phat_tab > thr, 1, 0) * panel$proxy_tabachova
  ok <- !is.na(yhat_raw) & !is.na(panel$y)
  if (sum(ok) == 0) next
  yhat_cal <- calibrate_with_cap(yhat_raw[ok], panel$emit[ok], panel$y[ok],
                                  panel$nace2d[ok], panel$year[ok], syt)
  nrmse <- sqrt(mean((panel$y[ok] - yhat_cal)^2)) / sd(panel$y[ok])
  if (nrmse < best_nrmse_C) {
    best_nrmse_C <- nrmse
    best_thr_C <- thr
  }
}
cat("  Best threshold:", best_thr_C, "-> nRMSE:", round(best_nrmse_C, 3), "\n")

# Final predictions at best threshold
yhat_C_raw <- ifelse(panel$phat_tab > best_thr_C, 1, 0) * panel$proxy_tabachova
ok_C <- !is.na(yhat_C_raw) & !is.na(panel$y)
yhat_C <- rep(NA_real_, nrow(panel))
yhat_C[ok_C] <- calibrate_with_cap(yhat_C_raw[ok_C], panel$emit[ok_C], panel$y[ok_C],
                                    panel$nace2d[ok_C], panel$year[ok_C], syt)
m_C <- calc_metrics(panel$y[ok_C], yhat_C[ok_C], nace2d = panel$nace2d[ok_C], year = panel$year[ok_C])
cat("  nRMSE:", round(m_C$nrmse_sd, 3), "\n")


# ===========================================================================
# CELL D: Our proxy + hybrid + calibration (LOFOCV)
# ===========================================================================
cat("\n── Cell D: Our proxy + hybrid + calibration (LOFOCV) ──\n")

panel$phat_ours <- NA_real_

cat("Running LOFOCV for our hybrid (K =", K_FOLDS, ")...\n")
for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)

  fit_ext <- tryCatch(
    gam(as.formula(paste("emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +", re_base)),
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit_ext)) {
    phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
    panel$phat_ours[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" done (%.1fs)\n", elapsed))
}

# Threshold search
# Hybrid: yhat_raw = I(phat > thr) * proxy_weighted
cat("Searching optimal threshold...\n")
best_thr_D <- 0.5
best_nrmse_D <- Inf

for (thr in THRESHOLDS) {
  yhat_raw <- ifelse(panel$phat_ours > thr, 1, 0) * panel$proxy_weighted
  ok <- !is.na(yhat_raw) & !is.na(panel$y)
  if (sum(ok) == 0) next
  yhat_cal <- calibrate_with_cap(yhat_raw[ok], panel$emit[ok], panel$y[ok],
                                  panel$nace2d[ok], panel$year[ok], syt)
  nrmse <- sqrt(mean((panel$y[ok] - yhat_cal)^2)) / sd(panel$y[ok])
  if (nrmse < best_nrmse_D) {
    best_nrmse_D <- nrmse
    best_thr_D <- thr
  }
}
cat("  Best threshold:", best_thr_D, "-> nRMSE:", round(best_nrmse_D, 3), "\n")

# Final predictions at best threshold
yhat_D_raw <- ifelse(panel$phat_ours > best_thr_D, 1, 0) * panel$proxy_weighted
ok_D <- !is.na(yhat_D_raw) & !is.na(panel$y)
yhat_D <- rep(NA_real_, nrow(panel))
yhat_D[ok_D] <- calibrate_with_cap(yhat_D_raw[ok_D], panel$emit[ok_D], panel$y[ok_D],
                                    panel$nace2d[ok_D], panel$year[ok_D], syt)
m_D <- calc_metrics(panel$y[ok_D], yhat_D[ok_D], nace2d = panel$nace2d[ok_D], year = panel$year[ok_D])
cat("  nRMSE:", round(m_D$nrmse_sd, 3), "\n")


# ===========================================================================
# Collect results and generate table
# ===========================================================================
cat("\n── Results summary ──\n")

collect <- function(m, label) {
  data.frame(
    label       = label,
    nRMSE       = m$nrmse_sd,
    median_apd  = m$median_apd,
    apd_q25     = m$apd_q25,
    apd_q75     = m$apd_q75,
    spearman    = m$spearman,
    rho_pooled  = m$rho_pooled,
    rho_pooled_min = m$rho_pooled_min,
    rho_pooled_max = m$rho_pooled_max,
    fpr         = m$fpr_nonemitters,
    tpr         = m$tpr_emitters,
    avg_p50     = m$avg_nonemit_p50_rank,
    avg_p99     = m$avg_nonemit_p99_rank,
    stringsAsFactors = FALSE
  )
}

results <- rbind(
  collect(m_A, "Prop. alloc. + Fuel-linked NACE"),
  collect(m_B, "Prop. alloc. + Elastic net"),
  collect(m_C, "Hybrid + cal. + Fuel-linked NACE"),
  collect(m_D, "Hybrid + cal. + Elastic net")
)

print(results %>% select(label, nRMSE, median_apd, spearman, rho_pooled, fpr, tpr))


# ── Save CSV ─────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
csv_path <- file.path(OUTPUT_DIR, "hungarian_comparison.csv")
write.csv(results, csv_path, row.names = FALSE)
cat("\nSaved CSV:", csv_path, "\n")


# ── Generate LaTeX table ─────────────────────────────────────────────────────
fmt3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))
fmt_pct <- function(x) ifelse(is.na(x), "---", sprintf("%.0f", x * 100))

make_row <- function(m, label) {
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s \\\\",
    label,
    fmt3(m$nrmse_sd), fmt3(m$median_apd),
    fmt3(m$spearman), fmt3(m$rho_pooled),
    fmt3(m$fpr_nonemitters), fmt3(m$tpr_emitters)
  )
}

tex <- c(
  "\\begin{tabular}{l cccc cc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Prediction accuracy} & \\multicolumn{2}{c}{Extensive margin} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-7}",
  "Model & nRMSE & Med.~APD & $\\rho$ & $\\rho_s$ & FPR & TPR \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel A: Proportional allocation}} \\\\",
  "\\addlinespace",
  make_row(m_A, "Fuel-linked NACE"),
  "\\addlinespace",
  make_row(m_B, "Elastic net"),
  "\\addlinespace",
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Panel B: Hybrid + calibration}} \\\\",
  "\\addlinespace",
  make_row(m_C, "Fuel-linked NACE"),
  "\\addlinespace",
  make_row(m_D, "Elastic net"),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_path <- file.path(OUTPUT_DIR, "hungarian_comparison.tex")
writeLines(tex, tex_path)
cat("Saved LaTeX table:", tex_path, "\n")
