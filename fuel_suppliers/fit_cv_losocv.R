###############################################################################
# fuel_suppliers/fit_cv_losocv.R
#
# PURPOSE
#   Leave-one-sector-out cross-validation (LOSOCV). Each NACE 2-digit sector
#   is held out in turn; the model is trained on the remaining sectors and
#   predicts the held-out sector. With sector RE (s(nace2d_f, bs="re")), the
#   RE for the unseen sector defaults to the population mean.
#
#   Loads training_sample.RData (produced by build_proxy.R).
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#     Contains: training_sample, foldid, K_FOLDS, syt
#
# OUTPUT
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance_losocv.csv
#   {OUTPUT_DIR}/within_sy_rho_losocv_*.csv   (per-model rho detail)
#   {OUTPUT_DIR}/loso_preds_*.csv             (firm-level predictions)
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

# syt is saved by build_proxy.R; recreate if absent
# (e.g. when using an older training_sample.RData from before the refactor)
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n\n", 100 * mean(panel$emit)))


# ── Model specifications ────────────────────────────────────────────────────
re_base      <- "year_f + s(nace2d_f, bs = 're')"
re_year_only <- "year_f"

make_hurdle_spec <- function(name, rhs, re) {
  list(
    name        = name,
    ext_formula = as.formula(paste("emit ~", rhs, "+", re)),
    int_formula = as.formula(paste("y ~", rhs, "+", re))
  )
}

# LOSOCV hurdle specs: {proxy_pooled, proxy_weighted} x {base RE, year-only}
# All with indicator (I(proxy > 0)) since k-fold showed it consistently helps.
losocv_specs <- list(
  # proxy_pooled + indicator
  make_hurdle_spec("losocv_hurdle_proxy_pooled_ind_base", "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_base),
  make_hurdle_spec("losocv_hurdle_proxy_pooled_ind_year", "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_year_only),
  # proxy_weighted + indicator
  make_hurdle_spec("losocv_hurdle_proxy_weighted_ind_base", "log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted)", re_base),
  make_hurdle_spec("losocv_hurdle_proxy_weighted_ind_year", "log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted)", re_year_only)
)

# Threshold grid for hurdle
THRESHOLDS <- seq(0.10, 0.50, by = 0.05)


# ── Assign sector folds ──────────────────────────────────────────────────────
firm_sector <- panel %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

sector_levels <- sort(unique(firm_sector$primary_nace2d))
n_sector_folds <- length(sector_levels)

panel <- panel %>%
  left_join(firm_sector %>% select(vat, primary_nace2d), by = "vat")

cat("Assigned", n_distinct(panel$vat), "firms to", n_sector_folds, "sector folds\n\n")

# Preserve full factor levels for mgcv
all_nace2d_levels <- levels(panel$nace2d_f)
all_nace5d_levels <- levels(panel$nace5d_f)
all_year_levels   <- levels(panel$year_f)


# ── LOSOCV loop ──────────────────────────────────────────────────────────────
cat("═══ LOSOCV: Leave-One-Sector-Out CV ═══\n")

results <- list()
losocv_rho_details <- list()
losocv_firm_preds  <- list()

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

for (losocv_sp in losocv_specs) {
  losocv_nm <- losocv_sp$name
  cat(sprintf("\n── Running LOSOCV: %s ──\n", losocv_nm))

  # Pre-allocate prediction columns
  panel$losocv_phat  <- NA_real_
  panel$losocv_muhat <- NA_real_

  cat("Running LOSOCV (", n_sector_folds, " sector folds)...\n", sep = "")
  t0_losocv <- Sys.time()

  for (s_idx in seq_along(sector_levels)) {
    sec <- sector_levels[s_idx]
    t0_fold <- Sys.time()

    train_idx <- which(panel$primary_nace2d != sec)
    test_idx  <- which(panel$primary_nace2d == sec)

    train <- panel[train_idx, ]
    test  <- panel[test_idx, ]

    # Ensure factor levels match the full panel (critical for mgcv RE)
    train$nace2d_f <- factor(train$nace2d, levels = all_nace2d_levels)
    test$nace2d_f  <- factor(test$nace2d,  levels = all_nace2d_levels)
    train$nace5d_f <- factor(train$nace5d, levels = all_nace5d_levels)
    test$nace5d_f  <- factor(test$nace5d,  levels = all_nace5d_levels)
    train$year_f   <- factor(train$year,   levels = all_year_levels)
    test$year_f    <- factor(test$year,    levels = all_year_levels)

    # Step 1: Extensive margin (logit on full training data)
    fit_ext <- tryCatch(
      gam(losocv_sp$ext_formula, data = train,
          family = binomial(link = "logit"), method = "REML"),
      error = function(e) NULL
    )
    if (!is.null(fit_ext)) {
      phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
      phat <- pmin(pmax(phat, 0), 1)
      panel$losocv_phat[test_idx] <- phat
    }

    # Step 2: Intensive margin (Poisson on emitters only)
    train_emit <- train[train$emit == 1, ]
    if (nrow(train_emit) > 0) {
      fit_int <- tryCatch(
        gam(losocv_sp$int_formula, data = train_emit,
            family = poisson(link = "log"), method = "REML"),
        error = function(e) NULL
      )
      if (!is.null(fit_int)) {
        muhat <- pmax(as.numeric(predict(fit_int, newdata = test, type = "response")), 0)
        panel$losocv_muhat[test_idx] <- muhat
      }
    }

    elapsed_fold <- round(difftime(Sys.time(), t0_fold, units = "secs"), 1)
    cat(sprintf("  Sector %s (%2d/%d): %4d test obs (%.1fs)\n",
                sec, s_idx, n_sector_folds, length(test_idx), elapsed_fold))
  }

  elapsed_losocv <- round(difftime(Sys.time(), t0_losocv, units = "mins"), 1)
  cat(sprintf("LOSOCV %s done (%.1f min)\n\n", losocv_nm, elapsed_losocv))

  # Combine predictions — search over thresholds
  ok_losocv <- !is.na(panel$losocv_phat) & !is.na(panel$losocv_muhat) & !is.na(panel$y)

  if (sum(ok_losocv) > 0) {
    cat("Searching over LOSOCV thresholds:", paste(THRESHOLDS, collapse = ", "), "\n")

    best_losocv     <- list(rmse = Inf)
    best_losocv_thr <- NA_real_
    best_m_losocv   <- NULL

    for (thr in THRESHOLDS) {
      yhat_thr <- pmax(
        as.numeric(panel$losocv_phat[ok_losocv] > thr) *
          panel$losocv_muhat[ok_losocv],
        0
      )

      # Joint calibration + cap
      yhat_cap <- calibrate_with_cap(
        yhat_thr, panel$emit[ok_losocv], panel$y[ok_losocv],
        panel$nace2d[ok_losocv], panel$year[ok_losocv], syt
      )

      m <- calc_metrics(
        panel$y[ok_losocv], yhat_cap,
        nace2d = panel$nace2d[ok_losocv], year = panel$year[ok_losocv]
      )

      if (!is.na(m$rmse) && m$rmse < best_losocv$rmse) {
        best_losocv     <- m
        best_losocv_thr <- thr
        best_m_losocv   <- m
      }
    }

    this_thr <- best_losocv_thr
    this_m   <- best_m_losocv

    cat(sprintf("%s best threshold: %.2f\n", losocv_nm, this_thr))
    cat("  Metrics (calibrated + clipped):\n")
    cat(sprintf("  nRMSE = %.3f, MAPD = %.3f, Spearman = %.3f\n",
                this_m$nrmse_sd, this_m$mapd_emitters, this_m$spearman))
    cat(sprintf("  FPR = %.3f, TPR = %.3f\n",
                this_m$fpr_nonemitters, this_m$tpr_emitters))

    # Append to results
    results[[losocv_nm]] <- make_result_row(losocv_nm, "calibrated_clipped",
                                             this_thr, this_m)

    # Store rho detail and firm-level predictions
    losocv_rho_details[[losocv_nm]] <- this_m$within_sy_rho_detail

    yhat_best <- pmax(
      as.numeric(panel$losocv_phat[ok_losocv] > this_thr) *
        panel$losocv_muhat[ok_losocv], 0
    )
    yhat_best_cap <- calibrate_with_cap(
      yhat_best, panel$emit[ok_losocv], panel$y[ok_losocv],
      panel$nace2d[ok_losocv], panel$year[ok_losocv], syt
    )
    losocv_firm_preds[[losocv_nm]] <- data.frame(
      vat = panel$vat[ok_losocv], nace2d = panel$nace2d[ok_losocv],
      year = panel$year[ok_losocv], y = panel$y[ok_losocv],
      yhat_clip = yhat_best_cap, stringsAsFactors = FALSE
    )

  } else {
    cat(sprintf("WARNING: No valid LOSOCV predictions for %s. Skipping.\n", losocv_nm))
  }

  # Clean up prediction columns before next variant
  panel$losocv_phat  <- NULL
  panel$losocv_muhat <- NULL
}

# Clean up temporary column
panel$primary_nace2d <- NULL


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cv_performance <- bind_rows(results)
out_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance_losocv.csv")
write.csv(cv_performance, out_path, row.names = FALSE)

# Save LOSO rho detail CSVs
for (nm in names(losocv_rho_details)) {
  rho_df <- losocv_rho_details[[nm]]
  if (!is.null(rho_df) && nrow(rho_df) > 0) {
    write.csv(rho_df,
              file.path(OUTPUT_DIR, paste0("within_sy_rho_", nm, ".csv")),
              row.names = FALSE)
  }
}

# Save LOSO firm-level predictions
for (nm in names(losocv_firm_preds)) {
  if (!is.null(losocv_firm_preds[[nm]])) {
    write.csv(losocv_firm_preds[[nm]],
              file.path(OUTPUT_DIR, paste0("loso_preds_", nm, ".csv")),
              row.names = FALSE)
  }
}

cat("\n══════════════════════════════════════════════\n")
cat("LOSOCV performance saved to:", out_path, "\n")
cat("══════════════════════════════════════════════\n")

cat("\n═══ LOSOCV performance summary ═══\n")
print(cv_performance %>% select(model, variant, threshold, nRMSE, spearman,
                                 within_sy_rho_med, within_sy_rho_min, within_sy_rho_max),
      row.names = FALSE)


# ── LOSOCV rho comparison: base RE vs year-only ──────────────────────────────
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

  cat(sprintf("\n  Cells: %d | Nested wins: %d (%.0f%%) | Base wins: %d (%.0f%%) | Ties: %d\n\n",
              nrow(merged),
              sum(merged$rho_nested > merged$rho_base),
              100 * mean(merged$rho_nested > merged$rho_base),
              sum(merged$rho_nested < merged$rho_base),
              100 * mean(merged$rho_nested < merged$rho_base),
              sum(merged$rho_nested == merged$rho_base)))
}

# LOSO: base RE vs year-only RE (proxy_pooled)
if (!is.null(losocv_rho_details[["losocv_hurdle_proxy_pooled_ind_base"]]) &&
    !is.null(losocv_rho_details[["losocv_hurdle_proxy_pooled_ind_year"]])) {
  compare_rho_variants(
    losocv_rho_details[["losocv_hurdle_proxy_pooled_ind_base"]],
    losocv_rho_details[["losocv_hurdle_proxy_pooled_ind_year"]],
    "LOSO proxy_pooled_ind: base RE (=nested) vs year-only (=base)"
  )
}

# LOSO: base RE vs year-only RE (proxy_weighted)
if (!is.null(losocv_rho_details[["losocv_hurdle_proxy_weighted_ind_base"]]) &&
    !is.null(losocv_rho_details[["losocv_hurdle_proxy_weighted_ind_year"]])) {
  compare_rho_variants(
    losocv_rho_details[["losocv_hurdle_proxy_weighted_ind_base"]],
    losocv_rho_details[["losocv_hurdle_proxy_weighted_ind_year"]],
    "LOSO proxy_weighted_ind: base RE (=nested) vs year-only (=base)"
  )
}

# LOSO: pooled within-sector rho for all LOSO specs
pooled_within_sector_rho <- function(preds, label) {
  preds <- preds %>%
    group_by(nace2d, year) %>%
    mutate(
      y_dm    = y - mean(y),
      yhat_dm = yhat_clip - mean(yhat_clip)
    ) %>%
    ungroup()

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

cat("\n\n═══ LOSO pooled within-sector rho ═══\n")
for (nm in names(losocv_firm_preds)) {
  pooled_within_sector_rho(losocv_firm_preds[[nm]],
    paste0("LOSO: ", nm, " (cal_clip)"))
}
