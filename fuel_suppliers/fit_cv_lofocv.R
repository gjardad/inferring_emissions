###############################################################################
# fuel_suppliers/fit_cv_lofocv.R
#
# PURPOSE
#   Leave-firms-out cross-validation (group k-fold, K=10, grouped by firm).
#   Loads training_sample.RData (produced by build_proxy.R) and evaluates
#   PPML and hurdle models across all proxy variants.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#     Contains: training_sample, foldid, K_FOLDS, syt
#
# OUTPUT
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance_lofocv.csv
#   {OUTPUT_DIR}/within_sy_rho_*.csv          (per-model rho detail)
#   {OUTPUT_DIR}/cell_fp_severity_*.csv       (per-cell FP severity)
#   {OUTPUT_DIR}/firm_preds_*.csv             (firm-level predictions)
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

source(file.path(REPO_DIR, "fuel_proxy", "utils", "calc_metrics.R"))
source(file.path(REPO_DIR, "fuel_suppliers", "utils", "calibration.R"))


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

panel <- panel %>%
  mutate(
    year_f   = factor(year),
    nace2d_f = factor(nace2d),
    nace4d_f = factor(substr(nace5d, 1, 4)),
    nace5d_f = factor(nace5d),
    emit     = as.integer(y > 0),
    log_revenue = ifelse(is.na(log_revenue), 0, log_revenue)
  )

# foldid, K_FOLDS, syt are saved by build_proxy.R; recreate if absent
# (e.g. when using an older training_sample.RData from before the refactor)
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
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("Folds: K =", K_FOLDS, "\n\n")


# ── Model specifications ────────────────────────────────────────────────────
# All models are run with two RE variants:
#   "nested": s(nace2d_f, bs = "re") + s(nace5d_f, bs = "re")
#   "base":   s(nace2d_f, bs = "re") only
# Base variants get a "_base" name suffix. Both go into the output CSV.
# A comparison of within-sector-year rho is printed at the end.

re_nested    <- "year_f + s(nace2d_f, bs = 're') + s(nace5d_f, bs = 're')"
re_base      <- "year_f + s(nace2d_f, bs = 're')"
re_year_only <- "year_f"

make_ppml_spec <- function(name, rhs, re) {
  list(name = name, formula = as.formula(paste("y ~", rhs, "+", re)))
}

make_hurdle_spec <- function(name, rhs, re) {
  list(
    name        = name,
    ext_formula = as.formula(paste("emit ~", rhs, "+", re)),
    int_formula = as.formula(paste("y ~", rhs, "+", re))
  )
}

# PPML specs (nested + base)
ppml_specs <- list(
  make_ppml_spec("benchmark",               "log_revenue", re_nested),
  make_ppml_spec("benchmark_base",          "log_revenue", re_base),
  make_ppml_spec("proxy_pooled",            "log_revenue + asinh(proxy_pooled)", re_nested),
  make_ppml_spec("proxy_pooled_base",       "log_revenue + asinh(proxy_pooled)", re_base),
  make_ppml_spec("proxy_within_buyer",      "log_revenue + asinh(proxy_fe)", re_nested),
  make_ppml_spec("proxy_within_buyer_base", "log_revenue + asinh(proxy_fe)", re_base),
  make_ppml_spec("proxy_weighted",          "log_revenue + asinh(proxy_weighted)", re_nested),
  make_ppml_spec("proxy_weighted_base",     "log_revenue + asinh(proxy_weighted)", re_base)
)

# Hurdle specs (nested + base)
hurdle_specs <- list(
  make_hurdle_spec("hurdle_benchmark",               "log_revenue", re_nested),
  make_hurdle_spec("hurdle_benchmark_base",          "log_revenue", re_base),
  make_hurdle_spec("hurdle_proxy_pooled",            "log_revenue + asinh(proxy_pooled)", re_nested),
  make_hurdle_spec("hurdle_proxy_pooled_base",       "log_revenue + asinh(proxy_pooled)", re_base),
  make_hurdle_spec("hurdle_proxy_pooled_ind",        "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_nested),
  make_hurdle_spec("hurdle_proxy_pooled_ind_base",   "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_base),
  make_hurdle_spec("hurdle_proxy_within_buyer",      "log_revenue + asinh(proxy_fe)", re_nested),
  make_hurdle_spec("hurdle_proxy_within_buyer_base", "log_revenue + asinh(proxy_fe)", re_base),
  make_hurdle_spec("hurdle_proxy_weighted",          "log_revenue + asinh(proxy_weighted)", re_nested),
  make_hurdle_spec("hurdle_proxy_weighted_base",     "log_revenue + asinh(proxy_weighted)", re_base),

  # Phase 2A: hurdle specs for new proxy variants (indicator, nested + base)
  make_hurdle_spec("hurdle_proxy_weighted_ind",      "log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted)", re_nested),
  make_hurdle_spec("hurdle_proxy_weighted_ind_base", "log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted)", re_base),
  make_hurdle_spec("hurdle_proxy_robust3_ind",       "log_revenue + I(proxy_robust3 > 0) + asinh(proxy_robust3)", re_nested),
  make_hurdle_spec("hurdle_proxy_robust3_ind_base",  "log_revenue + I(proxy_robust3 > 0) + asinh(proxy_robust3)", re_base),
  make_hurdle_spec("hurdle_proxy_robust3w_ind",      "log_revenue + I(proxy_robust3w > 0) + asinh(proxy_robust3w)", re_nested),
  make_hurdle_spec("hurdle_proxy_robust3w_ind_base", "log_revenue + I(proxy_robust3w > 0) + asinh(proxy_robust3w)", re_base),
  make_hurdle_spec("hurdle_proxy_1se_ind",           "log_revenue + I(proxy_1se > 0) + asinh(proxy_1se)", re_nested),
  make_hurdle_spec("hurdle_proxy_1se_ind_base",      "log_revenue + I(proxy_1se > 0) + asinh(proxy_1se)", re_base)
)

# Threshold grid for hurdle
THRESHOLDS <- seq(0.10, 0.50, by = 0.05)


# ── Pre-allocate prediction columns ─────────────────────────────────────────
for (sp in ppml_specs) {
  panel[[paste0("yhat_ppml_", sp$name)]] <- NA_real_
}
for (sp in hurdle_specs) {
  panel[[paste0("phat_", sp$name)]]  <- NA_real_
  panel[[paste0("muhat_", sp$name)]] <- NA_real_
}


# ── Group k-fold CV ──────────────────────────────────────────────────────────
cat("Running group k-fold CV (K =", K_FOLDS, ")...\n\n")

for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)

  # ── PPML models ──────────────────────────────────────────────────────────
  for (sp in ppml_specs) {
    fit <- tryCatch(
      gam(sp$formula, data = train, family = poisson(link = "log"), method = "REML"),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      preds <- pmax(as.numeric(predict(fit, newdata = test, type = "response")), 0)
      panel[[paste0("yhat_ppml_", sp$name)]][test_idx] <- preds
    }
  }

  # ── Hurdle models ────────────────────────────────────────────────────────
  train_emit <- train[train$emit == 1, ]

  for (sp in hurdle_specs) {
    # Step 1: extensive margin (logit on all training data)
    fit_ext <- tryCatch(
      gam(sp$ext_formula, data = train, family = binomial(link = "logit"), method = "REML"),
      error = function(e) NULL
    )
    if (!is.null(fit_ext)) {
      phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
      phat <- pmin(pmax(phat, 0), 1)
      panel[[paste0("phat_", sp$name)]][test_idx] <- phat
    }

    # Step 2: intensive margin (Poisson on emitters only)
    if (nrow(train_emit) > 0) {
      fit_int <- tryCatch(
        gam(sp$int_formula, data = train_emit, family = poisson(link = "log"), method = "REML"),
        error = function(e) NULL
      )
      if (!is.null(fit_int)) {
        muhat <- pmax(as.numeric(predict(fit_int, newdata = test, type = "response")), 0)
        panel[[paste0("muhat_", sp$name)]][test_idx] <- muhat
      }
    }
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" done (%.1fs)\n", elapsed))
}


# ── Compute metrics ──────────────────────────────────────────────────────────
cat("\nComputing performance metrics...\n")

results <- list()

make_result_row <- function(nm, variant, thr, m) {
  data.frame(
    model = nm, variant = variant, threshold = thr,
    n = m$n, nRMSE = m$nrmse_sd,
    rmse = m$rmse, mae = m$mae,
    mapd_emitters = m$mapd_emitters, spearman = m$spearman,
    fpr_nonemitters = m$fpr_nonemitters,
    tpr_emitters = m$tpr_emitters,
    emitter_mass_captured = m$emitter_mass_captured,
    nonemit_p50_rank_19 = m$nonemit_p50_rank_19,
    nonemit_p90_rank_19 = m$nonemit_p90_rank_19,
    nonemit_p99_rank_19 = m$nonemit_p99_rank_19,
    nonemit_p50_rank_24 = m$nonemit_p50_rank_24,
    nonemit_p90_rank_24 = m$nonemit_p90_rank_24,
    nonemit_p99_rank_24 = m$nonemit_p99_rank_24,
    avg_nonemit_p50_rank = m$avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = m$avg_nonemit_p99_rank,
    within_sy_rho_med = m$within_sy_rho_med,
    within_sy_rho_min = m$within_sy_rho_min,
    within_sy_rho_max = m$within_sy_rho_max,
    stringsAsFactors = FALSE
  )
}

# ── PPML metrics (raw + calibrated + calibrated_clipped) ─────────────────────
for (sp in ppml_specs) {
  nm <- sp$name
  yhat_raw <- panel[[paste0("yhat_ppml_", nm)]]

  ok <- !is.na(yhat_raw) & !is.na(panel$y)
  if (sum(ok) == 0) next

  # Raw
  m_raw <- calc_metrics(panel$y[ok], yhat_raw[ok], nace2d = panel$nace2d[ok], year = panel$year[ok])

  # Capture per-cell FP severity and within-sector-year rho detail
  if (nm == "benchmark") {
    benchmark_cell_fp_raw <- m_raw$cell_fp_severity
    benchmark_sy_rho_raw  <- m_raw$within_sy_rho_detail
  } else if (nm == "proxy_pooled") {
    proxy_pooled_cell_fp_raw <- m_raw$cell_fp_severity
    proxy_pooled_sy_rho_raw  <- m_raw$within_sy_rho_detail
  }

  # Calibrated
  yhat_cal <- rep(NA_real_, nrow(panel))
  yhat_cal[ok] <- calibrate_predictions(
    yhat_raw[ok], panel$nace2d[ok], panel$year[ok], syt
  )
  m_cal <- calc_metrics(panel$y[ok], yhat_cal[ok], nace2d = panel$nace2d[ok], year = panel$year[ok])

  results[[paste0(nm, "_raw")]] <- make_result_row(nm, "raw", NA_real_, m_raw)
  results[[paste0(nm, "_cal")]] <- make_result_row(nm, "calibrated", NA_real_, m_cal)

  # Joint calibration + cap
  yhat_cap <- calibrate_with_cap(
    yhat_raw[ok], panel$emit[ok], panel$y[ok],
    panel$nace2d[ok], panel$year[ok], syt
  )
  m_cap <- calc_metrics(panel$y[ok], yhat_cap, nace2d = panel$nace2d[ok], year = panel$year[ok])

  results[[paste0(nm, "_clip")]] <- make_result_row(nm, "calibrated_clipped", NA_real_, m_cap)
}

# ── Hurdle metrics (threshold search, raw + calibrated + clipped) ────────────
cat("Searching over hurdle thresholds:", paste(THRESHOLDS, collapse = ", "), "\n")

for (sp in hurdle_specs) {
  nm <- sp$name
  phat  <- panel[[paste0("phat_", nm)]]
  muhat <- panel[[paste0("muhat_", nm)]]

  ok <- !is.na(phat) & !is.na(muhat) & !is.na(panel$y)
  if (sum(ok) == 0) next

  # Step 1: find best threshold on raw predictions
  best_raw  <- list(rmse = Inf)
  best_thr_raw <- NA_real_
  best_m_raw   <- NULL

  for (thr in THRESHOLDS) {
    yhat_hard <- pmax(as.numeric(phat[ok] > thr) * muhat[ok], 0)
    m_raw <- calc_metrics(panel$y[ok], yhat_hard, nace2d = panel$nace2d[ok], year = panel$year[ok])

    if (!is.na(m_raw$rmse) && m_raw$rmse < best_raw$rmse) {
      best_raw <- m_raw
      best_thr_raw <- thr
      best_m_raw <- m_raw
    }
  }

  # Step 2: apply calibration + cap at the raw-optimal threshold
  yhat_hard <- pmax(as.numeric(phat[ok] > best_thr_raw) * muhat[ok], 0)

  yhat_cal <- calibrate_predictions(
    yhat_hard, panel$nace2d[ok], panel$year[ok], syt
  )
  best_m_cal <- calc_metrics(panel$y[ok], yhat_cal, nace2d = panel$nace2d[ok], year = panel$year[ok])

  yhat_cap <- calibrate_with_cap(
    yhat_hard, panel$emit[ok], panel$y[ok],
    panel$nace2d[ok], panel$year[ok], syt
  )
  best_m_clip <- calc_metrics(panel$y[ok], yhat_cap, nace2d = panel$nace2d[ok], year = panel$year[ok])

  cat(sprintf("  %s: best thr=%.2f\n", nm, best_thr_raw))

  # Capture per-cell FP severity and within-sector-year rho detail
  if (nm == "hurdle_proxy_pooled") {
    if (!is.null(best_m_raw)) {
      hurdle_proxy_pooled_cell_fp_raw <- best_m_raw$cell_fp_severity
      hurdle_proxy_pooled_sy_rho_raw  <- best_m_raw$within_sy_rho_detail
    }
  }
  if (nm == "hurdle_proxy_pooled_ind") {
    if (!is.null(best_m_clip)) {
      hurdle_proxy_pooled_ind_cell_fp     <- best_m_clip$cell_fp_severity
      hurdle_proxy_pooled_ind_sy_rho_clip <- best_m_clip$within_sy_rho_detail
    }
    if (!is.null(best_m_raw)) {
      hurdle_proxy_pooled_ind_cell_fp_raw <- best_m_raw$cell_fp_severity
      hurdle_proxy_pooled_ind_sy_rho_raw  <- best_m_raw$within_sy_rho_detail
    }
    # Save firm-level predictions for pooled within-sector rho diagnostic
    firm_preds_nested <- data.frame(
      vat = panel$vat[ok], nace2d = panel$nace2d[ok], year = panel$year[ok],
      y = panel$y[ok], yhat_clip = yhat_cap, stringsAsFactors = FALSE
    )
  }
  if (nm == "hurdle_proxy_pooled_ind_base") {
    if (!is.null(best_m_clip)) {
      hurdle_ind_base_sy_rho_clip <- best_m_clip$within_sy_rho_detail
    }
    firm_preds_base <- data.frame(
      vat = panel$vat[ok], nace2d = panel$nace2d[ok], year = panel$year[ok],
      y = panel$y[ok], yhat_clip = yhat_cap, stringsAsFactors = FALSE
    )
  }
  if (nm == "hurdle_proxy_weighted_ind_base") {
    if (!is.null(best_m_clip)) {
      hurdle_weighted_ind_base_sy_rho_clip <- best_m_clip$within_sy_rho_detail
    }
    firm_preds_weighted_base <- data.frame(
      vat = panel$vat[ok], nace2d = panel$nace2d[ok], year = panel$year[ok],
      y = panel$y[ok], yhat_clip = yhat_cap, stringsAsFactors = FALSE
    )
  }
  if (!is.null(best_m_raw)) {
    results[[paste0(nm, "_raw")]] <- make_result_row(nm, "raw", best_thr_raw, best_m_raw)
  }
  if (!is.null(best_m_cal)) {
    results[[paste0(nm, "_cal")]] <- make_result_row(nm, "calibrated", best_thr_raw, best_m_cal)
  }
  if (!is.null(best_m_clip)) {
    results[[paste0(nm, "_clip")]] <- make_result_row(nm, "calibrated_clipped", best_thr_raw, best_m_clip)
  }
}


# ── Combine k-fold results ─────────────────────────────────────────────────
cv_performance <- bind_rows(results)

cat("\n═══ Group k-fold CV performance ═══\n")
print(cv_performance, row.names = FALSE)


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance_lofocv.csv")
write.csv(cv_performance, out_path, row.names = FALSE)

# Save per-cell FP severity CSVs
if (exists("benchmark_cell_fp_raw")) {
  write.csv(benchmark_cell_fp_raw,
            file.path(OUTPUT_DIR, "cell_fp_severity_benchmark_raw.csv"),
            row.names = FALSE)
}
if (exists("proxy_pooled_cell_fp_raw")) {
  write.csv(proxy_pooled_cell_fp_raw,
            file.path(OUTPUT_DIR, "cell_fp_severity_proxy_pooled_raw.csv"),
            row.names = FALSE)
}
if (exists("hurdle_proxy_pooled_cell_fp_raw")) {
  write.csv(hurdle_proxy_pooled_cell_fp_raw,
            file.path(OUTPUT_DIR, "cell_fp_severity_hurdle_proxy_pooled_raw.csv"),
            row.names = FALSE)
}
if (exists("hurdle_proxy_pooled_ind_cell_fp")) {
  write.csv(hurdle_proxy_pooled_ind_cell_fp,
            file.path(OUTPUT_DIR, "cell_fp_severity_hurdle_proxy_pooled_ind.csv"),
            row.names = FALSE)
}
if (exists("hurdle_proxy_pooled_ind_cell_fp_raw")) {
  write.csv(hurdle_proxy_pooled_ind_cell_fp_raw,
            file.path(OUTPUT_DIR, "cell_fp_severity_hurdle_proxy_pooled_ind_raw.csv"),
            row.names = FALSE)
}

# Save within-sector-year rho detail CSVs
sy_rho_saves <- list(
  benchmark_sy_rho_raw                  = "within_sy_rho_benchmark_raw.csv",
  proxy_pooled_sy_rho_raw               = "within_sy_rho_proxy_pooled_raw.csv",
  hurdle_proxy_pooled_sy_rho_raw        = "within_sy_rho_hurdle_proxy_pooled_raw.csv",
  hurdle_proxy_pooled_ind_sy_rho_raw    = "within_sy_rho_hurdle_proxy_pooled_ind_raw.csv",
  hurdle_proxy_pooled_ind_sy_rho_clip   = "within_sy_rho_hurdle_proxy_pooled_ind_cal_clip.csv",
  hurdle_ind_base_sy_rho_clip           = "within_sy_rho_hurdle_proxy_pooled_ind_base_cal_clip.csv",
  hurdle_weighted_ind_base_sy_rho_clip  = "within_sy_rho_hurdle_proxy_weighted_ind_base_cal_clip.csv"
)
for (v in names(sy_rho_saves)) {
  if (exists(v) && !is.null(get(v)) && nrow(get(v)) > 0) {
    write.csv(get(v),
              file.path(OUTPUT_DIR, sy_rho_saves[[v]]),
              row.names = FALSE)
  }
}

# Save firm-level predictions for diagnostics
for (obj_name in c("firm_preds_nested", "firm_preds_base",
                    "firm_preds_weighted_base")) {
  if (exists(obj_name)) {
    write.csv(get(obj_name),
              file.path(OUTPUT_DIR, paste0(obj_name, ".csv")),
              row.names = FALSE)
  }
}

cat("\n══════════════════════════════════════════════\n")
cat("LOFOCV performance saved to:", out_path, "\n")
cat("══════════════════════════════════════════════\n")


# ── Nested vs Base RE comparison ──────────────────────────────────────────
compare_rho_variants <- function(nested_rho, base_rho, label) {
  merged <- merge(base_rho, nested_rho,
                  by = c("nace2d", "year"), suffixes = c("_base", "_nested"))

  sector_summary <- merged %>%
    group_by(nace2d) %>%
    summarise(
      n_years    = n(),
      n_firms    = median(n_firms_base),
      rho_base   = median(rho_base),
      rho_nested = median(rho_nested),
      delta      = median(rho_nested) - median(rho_base),
      .groups    = "drop"
    ) %>%
    arrange(nace2d)

  cat(sprintf("\n── %s ──\n", label))
  cat(sprintf("%-6s  %5s  %5s  %8s  %8s  %8s\n",
              "NACE", "Yrs", "Firms", "Base", "Nested", "Delta"))
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(sector_summary))) {
    r <- sector_summary[i, ]
    cat(sprintf("%-6s  %5d  %5.0f  %8.3f  %8.3f  %+8.3f\n",
                r$nace2d, r$n_years, r$n_firms,
                r$rho_base, r$rho_nested, r$delta))
  }
  cat(sprintf("\n%-6s  %5d  %5s  %8.3f  %8.3f  %+8.3f\n",
              "ALL", nrow(merged), "",
              median(merged$rho_base), median(merged$rho_nested),
              median(merged$rho_nested) - median(merged$rho_base)))

  # Cell-level wins/losses
  cat(sprintf("\n  Cells: %d | Nested wins: %d (%.0f%%) | Base wins: %d (%.0f%%) | Ties: %d\n\n",
              nrow(merged),
              sum(merged$rho_nested > merged$rho_base),
              100 * mean(merged$rho_nested > merged$rho_base),
              sum(merged$rho_nested < merged$rho_base),
              100 * mean(merged$rho_nested < merged$rho_base),
              sum(merged$rho_nested == merged$rho_base)))
}

cat("\n\n═══ Nested vs Base RE: within-sector-year rho comparison ═══\n")

if (exists("hurdle_proxy_pooled_ind_sy_rho_clip") &&
    exists("hurdle_ind_base_sy_rho_clip")) {
  compare_rho_variants(
    hurdle_proxy_pooled_ind_sy_rho_clip,
    hurdle_ind_base_sy_rho_clip,
    "Leave-firms-out: hurdle_proxy_pooled_ind (calibrated_clipped)"
  )
}


# ── Pooled within-sector rho (across years, demeaned by sector-year) ────────
pooled_within_sector_rho <- function(preds, label) {
  # For each sector-year cell, demean y and yhat
  preds <- preds %>%
    group_by(nace2d, year) %>%
    mutate(
      y_dm    = y - mean(y),
      yhat_dm = yhat_clip - mean(yhat_clip)
    ) %>%
    ungroup()

  # For each sector, pool all firm-years and compute Spearman rho
  sector_rho <- preds %>%
    group_by(nace2d) %>%
    summarise(
      n_firmyears = n(),
      n_years     = n_distinct(year),
      rho_pooled  = suppressWarnings(
        cor(y_dm, yhat_dm, method = "spearman", use = "complete.obs")
      ),
      .groups = "drop"
    ) %>%
    arrange(nace2d)

  cat(sprintf("\n── %s ──\n", label))
  cat(sprintf("%-6s  %8s  %5s  %10s\n", "NACE", "FirmYrs", "Years", "rho_pooled"))
  cat(strrep("-", 40), "\n")
  for (i in seq_len(nrow(sector_rho))) {
    r <- sector_rho[i, ]
    cat(sprintf("%-6s  %8d  %5d  %10.3f\n",
                r$nace2d, r$n_firmyears, r$n_years, r$rho_pooled))
  }
  overall <- suppressWarnings(cor(preds$y_dm, preds$yhat_dm,
                                   method = "spearman", use = "complete.obs"))
  cat(sprintf("\n%-6s  %8d  %5s  %10.3f\n", "ALL", nrow(preds), "", overall))

  sector_rho
}

cat("\n\n═══ Pooled within-sector rho (demeaned by sector-year, pooled across years) ═══\n")

if (exists("firm_preds_nested") && exists("firm_preds_base")) {
  rho_nest <- pooled_within_sector_rho(firm_preds_nested,
    "Leave-firms-out NESTED: hurdle_proxy_pooled_ind (cal_clip)")
  rho_base <- pooled_within_sector_rho(firm_preds_base,
    "Leave-firms-out BASE: hurdle_proxy_pooled_ind (cal_clip)")

  comp <- merge(rho_base, rho_nest, by = "nace2d", suffixes = c("_base", "_nested"))
  cat("\n── Leave-firms-out: pooled rho comparison ──\n")
  cat(sprintf("%-6s  %8s  %10s  %10s  %10s\n",
              "NACE", "FirmYrs", "Base", "Nested", "Delta"))
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(comp))) {
    r <- comp[i, ]
    cat(sprintf("%-6s  %8d  %10.3f  %10.3f  %+10.3f\n",
                r$nace2d, r$n_firmyears_base,
                r$rho_pooled_base, r$rho_pooled_nested,
                r$rho_pooled_nested - r$rho_pooled_base))
  }
}

# Leave-firms-out: pooled vs weighted proxy
if (exists("firm_preds_base") && exists("firm_preds_weighted_base")) {
  rho_pooled_p <- pooled_within_sector_rho(firm_preds_base,
    "Leave-firms-out: proxy_POOLED_ind_base (cal_clip)")
  rho_weighted_p <- pooled_within_sector_rho(firm_preds_weighted_base,
    "Leave-firms-out: proxy_WEIGHTED_ind_base (cal_clip)")

  pw_comp <- merge(rho_pooled_p, rho_weighted_p, by = "nace2d",
                   suffixes = c("_pooled", "_weighted"))
  cat("\n── Leave-firms-out: pooled vs weighted proxy (pooled rho) ──\n")
  cat(sprintf("%-6s  %8s  %10s  %10s  %10s\n",
              "NACE", "FirmYrs", "Pooled", "Weighted", "Delta"))
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(pw_comp))) {
    r <- pw_comp[i, ]
    cat(sprintf("%-6s  %8d  %10.3f  %10.3f  %+10.3f\n",
                r$nace2d, r$n_firmyears_pooled,
                r$rho_pooled_pooled, r$rho_pooled_weighted,
                r$rho_pooled_weighted - r$rho_pooled_pooled))
  }
}
