###############################################################################
# fuel_suppliers/build_proxy_and_cv.R
#
# PURPOSE
#   Build fuel-consumption proxies from elastic-net-identified suppliers
#   and evaluate them via group k-fold CV (K=10, grouped by firm).
#
#   Six model variants are evaluated:
#     PPML (single-equation Poisson):
#       (a) benchmark         — revenue + year FE + sector RE
#       (b) proxy_pooled      — + proxy from pooled-identified suppliers
#       (c) proxy_within_buyer — + proxy from within-buyer-identified suppliers
#
#     Hurdle (logit step 1 + Poisson step 2, with threshold search):
#       (d) hurdle_benchmark
#       (e) hurdle_proxy_pooled
#       (f) hurdle_proxy_within_buyer
#
#   All predictions are calibrated to sector-year emission totals
#   (deployment-style proportional rescaling).
#
# INPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   {PROC_DATA}/b2b_selected_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance.csv
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


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net inputs...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData"))

cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)


# ── Extract identified supplier sets ─────────────────────────────────────────
# Union of suppliers with positive coef in any model at lambda.min
suppliers_pooled <- supplier_summary_pooled %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

suppliers_fe <- supplier_summary_fe %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

cat("Identified suppliers (pooled):       ", length(suppliers_pooled), "\n")
cat("Identified suppliers (within-buyer): ", length(suppliers_fe), "\n")


# ── Build proxies ────────────────────────────────────────────────────────────
cat("\nBuilding fuel-consumption proxies...\n")

# Filter B2B to LHS buyers, years >= 2005
b2b_lhs <- b2b %>%
  filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)

# Helper: build proxy from a supplier set
build_proxy <- function(b2b_df, supplier_set, proxy_name) {
  b2b_df %>%
    filter(vat_i_ano %in% supplier_set) %>%
    group_by(vat_j_ano, year) %>%
    summarise(proxy = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
    rename(vat = vat_j_ano) %>%
    rename_with(~ proxy_name, .cols = "proxy")
}

proxy_pooled <- build_proxy(b2b_lhs, suppliers_pooled, "proxy_pooled")
proxy_fe     <- build_proxy(b2b_lhs, suppliers_fe,     "proxy_fe")
rm(b2b_lhs)

# Merge proxies into LHS panel
panel <- lhs %>%
  left_join(proxy_pooled, by = c("vat", "year")) %>%
  left_join(proxy_fe,     by = c("vat", "year")) %>%
  mutate(
    proxy_pooled = coalesce(proxy_pooled, 0),
    proxy_fe     = coalesce(proxy_fe, 0),
    year_f       = factor(year),
    nace2d_f     = factor(nace2d),
    emit         = as.integer(y > 0)
  )

cat("Panel rows:", nrow(panel), "\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("Proxy coverage (pooled > 0):", sum(panel$proxy_pooled > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_pooled > 0)))
cat("Proxy coverage (within-buyer > 0):", sum(panel$proxy_fe > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_fe > 0)))


# ── Sector-year emission totals (for calibration) ───────────────────────────
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")

cat("Sector-year cells:", nrow(syt), "\n")
cat("Cells with E_total > 0:", sum(syt$E_total > 0), "\n\n")


# ── Calibration function ────────────────────────────────────────────────────
# Post-hoc calibration: all obs have out-of-fold predictions, so we calibrate
# globally by (nace2d, year) cell.
#   yhat_cal = E_total * (yhat / denom)  where  denom = sum(yhat) in cell
#   Fallback: if denom == 0 and E_total > 0 -> equal split E_total / n_full
#   If E_total == 0 -> yhat_cal = 0
calibrate_predictions <- function(yhat, nace2d, year, syt) {
  df <- data.frame(
    yhat   = yhat,
    nace2d = nace2d,
    year   = year,
    idx    = seq_along(yhat),
    stringsAsFactors = FALSE
  )
  # Cell-level denominator
  cell_denom <- df %>%
    group_by(nace2d, year) %>%
    summarise(denom = sum(yhat, na.rm = TRUE), .groups = "drop")

  df <- df %>%
    left_join(cell_denom, by = c("nace2d", "year")) %>%
    left_join(syt, by = c("nace2d", "year"))

  yhat_cal <- rep(NA_real_, nrow(df))

  # E_total == 0 -> calibrated prediction is 0
  idx0 <- !is.na(df$E_total) & df$E_total == 0
  yhat_cal[idx0] <- 0

  # E_total > 0 and denom > 0 -> proportional rescaling
  idx_pos <- !is.na(df$E_total) & df$E_total > 0 & df$denom > 0
  yhat_cal[idx_pos] <- df$E_total[idx_pos] * (df$yhat[idx_pos] / df$denom[idx_pos])

  # Fallback: E_total > 0 but denom == 0 -> equal split
  idx_fb <- !is.na(df$E_total) & df$E_total > 0 & (df$denom == 0 | is.na(df$denom))
  yhat_cal[idx_fb] <- df$E_total[idx_fb] / df$n_full[idx_fb]

  yhat_cal[order(df$idx)]
}


# ── Model specifications ────────────────────────────────────────────────────

# PPML specs
ppml_specs <- list(
  list(
    name    = "benchmark",
    formula = y ~ log_revenue + year_f + s(nace2d_f, bs = "re")
  ),
  list(
    name    = "proxy_pooled",
    formula = y ~ log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled) +
                  year_f + s(nace2d_f, bs = "re")
  ),
  list(
    name    = "proxy_within_buyer",
    formula = y ~ log_revenue + I(proxy_fe > 0) + asinh(proxy_fe) +
                  year_f + s(nace2d_f, bs = "re")
  )
)

# Hurdle specs: each has an extensive (logit) and intensive (Poisson) formula
hurdle_specs <- list(
  list(
    name        = "hurdle_benchmark",
    ext_formula = emit ~ log_revenue + year_f + s(nace2d_f, bs = "re"),
    int_formula = y ~ log_revenue + year_f + s(nace2d_f, bs = "re")
  ),
  list(
    name        = "hurdle_proxy_pooled",
    ext_formula = emit ~ log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled) +
                         year_f + s(nace2d_f, bs = "re"),
    int_formula = y ~ log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled) +
                       year_f + s(nace2d_f, bs = "re")
  ),
  list(
    name        = "hurdle_proxy_within_buyer",
    ext_formula = emit ~ log_revenue + I(proxy_fe > 0) + asinh(proxy_fe) +
                         year_f + s(nace2d_f, bs = "re"),
    int_formula = y ~ log_revenue + I(proxy_fe > 0) + asinh(proxy_fe) +
                       year_f + s(nace2d_f, bs = "re")
  )
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

# ── PPML metrics (raw + calibrated) ─────────────────────────────────────────
for (sp in ppml_specs) {
  nm <- sp$name
  yhat_raw <- panel[[paste0("yhat_ppml_", nm)]]

  ok <- !is.na(yhat_raw) & !is.na(panel$y)
  if (sum(ok) == 0) next

  # Raw
  m_raw <- calc_metrics(panel$y[ok], yhat_raw[ok], nace2d = panel$nace2d[ok])

  # Calibrated
  yhat_cal <- rep(NA_real_, nrow(panel))
  yhat_cal[ok] <- calibrate_predictions(
    yhat_raw[ok], panel$nace2d[ok], panel$year[ok], syt
  )
  m_cal <- calc_metrics(panel$y[ok], yhat_cal[ok], nace2d = panel$nace2d[ok])

  results[[paste0(nm, "_raw")]] <- data.frame(
    model = nm, variant = "raw", threshold = NA_real_,
    n = m_raw$n, nRMSE = m_raw$nrmse_sd,
    rmse = m_raw$rmse, mae = m_raw$mae,
    mapd_emitters = m_raw$mapd_emitters, spearman = m_raw$spearman,
    fpr_nonemitters = m_raw$fpr_nonemitters, tpr_emitters = m_raw$tpr_emitters,
    emitter_mass_captured = m_raw$emitter_mass_captured,
    nonemit_p50_rank_19 = m_raw$nonemit_p50_rank_19,
    nonemit_p90_rank_19 = m_raw$nonemit_p90_rank_19,
    nonemit_p99_rank_19 = m_raw$nonemit_p99_rank_19,
    nonemit_p50_rank_24 = m_raw$nonemit_p50_rank_24,
    nonemit_p90_rank_24 = m_raw$nonemit_p90_rank_24,
    nonemit_p99_rank_24 = m_raw$nonemit_p99_rank_24,
    stringsAsFactors = FALSE
  )
  results[[paste0(nm, "_cal")]] <- data.frame(
    model = nm, variant = "calibrated", threshold = NA_real_,
    n = m_cal$n, nRMSE = m_cal$nrmse_sd,
    rmse = m_cal$rmse, mae = m_cal$mae,
    mapd_emitters = m_cal$mapd_emitters, spearman = m_cal$spearman,
    fpr_nonemitters = m_cal$fpr_nonemitters, tpr_emitters = m_cal$tpr_emitters,
    emitter_mass_captured = m_cal$emitter_mass_captured,
    nonemit_p50_rank_19 = m_cal$nonemit_p50_rank_19,
    nonemit_p90_rank_19 = m_cal$nonemit_p90_rank_19,
    nonemit_p99_rank_19 = m_cal$nonemit_p99_rank_19,
    nonemit_p50_rank_24 = m_cal$nonemit_p50_rank_24,
    nonemit_p90_rank_24 = m_cal$nonemit_p90_rank_24,
    nonemit_p99_rank_24 = m_cal$nonemit_p99_rank_24,
    stringsAsFactors = FALSE
  )
}

# ── Hurdle metrics (threshold search, raw + calibrated) ─────────────────────
cat("Searching over hurdle thresholds:", paste(THRESHOLDS, collapse = ", "), "\n")

for (sp in hurdle_specs) {
  nm <- sp$name
  phat  <- panel[[paste0("phat_", nm)]]
  muhat <- panel[[paste0("muhat_", nm)]]

  ok <- !is.na(phat) & !is.na(muhat) & !is.na(panel$y)
  if (sum(ok) == 0) next

  best_raw <- list(rmse = Inf)
  best_cal <- list(rmse = Inf)
  best_thr_raw <- NA_real_
  best_thr_cal <- NA_real_
  best_m_raw <- NULL
  best_m_cal <- NULL

  for (thr in THRESHOLDS) {
    # Hard threshold combination
    yhat_hard <- pmax(as.numeric(phat[ok] > thr) * muhat[ok], 0)

    # Raw metrics
    m_raw <- calc_metrics(panel$y[ok], yhat_hard, nace2d = panel$nace2d[ok])

    # Calibrated
    yhat_cal <- calibrate_predictions(
      yhat_hard, panel$nace2d[ok], panel$year[ok], syt
    )
    m_cal <- calc_metrics(panel$y[ok], yhat_cal, nace2d = panel$nace2d[ok])

    if (!is.na(m_raw$rmse) && m_raw$rmse < best_raw$rmse) {
      best_raw <- m_raw
      best_thr_raw <- thr
      best_m_raw <- m_raw
    }
    if (!is.na(m_cal$rmse) && m_cal$rmse < best_cal$rmse) {
      best_cal <- m_cal
      best_thr_cal <- thr
      best_m_cal <- m_cal
    }
  }

  cat(sprintf("  %s: best threshold (raw) = %.2f, best threshold (cal) = %.2f\n",
              nm, best_thr_raw, best_thr_cal))

  if (!is.null(best_m_raw)) {
    results[[paste0(nm, "_raw")]] <- data.frame(
      model = nm, variant = "raw", threshold = best_thr_raw,
      n = best_m_raw$n, nRMSE = best_m_raw$nrmse_sd,
      rmse = best_m_raw$rmse, mae = best_m_raw$mae,
      mapd_emitters = best_m_raw$mapd_emitters, spearman = best_m_raw$spearman,
      fpr_nonemitters = best_m_raw$fpr_nonemitters,
      tpr_emitters = best_m_raw$tpr_emitters,
      emitter_mass_captured = best_m_raw$emitter_mass_captured,
      nonemit_p50_rank_19 = best_m_raw$nonemit_p50_rank_19,
      nonemit_p90_rank_19 = best_m_raw$nonemit_p90_rank_19,
      nonemit_p99_rank_19 = best_m_raw$nonemit_p99_rank_19,
      nonemit_p50_rank_24 = best_m_raw$nonemit_p50_rank_24,
      nonemit_p90_rank_24 = best_m_raw$nonemit_p90_rank_24,
      nonemit_p99_rank_24 = best_m_raw$nonemit_p99_rank_24,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(best_m_cal)) {
    results[[paste0(nm, "_cal")]] <- data.frame(
      model = nm, variant = "calibrated", threshold = best_thr_cal,
      n = best_m_cal$n, nRMSE = best_m_cal$nrmse_sd,
      rmse = best_m_cal$rmse, mae = best_m_cal$mae,
      mapd_emitters = best_m_cal$mapd_emitters, spearman = best_m_cal$spearman,
      fpr_nonemitters = best_m_cal$fpr_nonemitters,
      tpr_emitters = best_m_cal$tpr_emitters,
      emitter_mass_captured = best_m_cal$emitter_mass_captured,
      nonemit_p50_rank_19 = best_m_cal$nonemit_p50_rank_19,
      nonemit_p90_rank_19 = best_m_cal$nonemit_p90_rank_19,
      nonemit_p99_rank_19 = best_m_cal$nonemit_p99_rank_19,
      nonemit_p50_rank_24 = best_m_cal$nonemit_p50_rank_24,
      nonemit_p90_rank_24 = best_m_cal$nonemit_p90_rank_24,
      nonemit_p99_rank_24 = best_m_cal$nonemit_p99_rank_24,
      stringsAsFactors = FALSE
    )
  }
}


# ── Combine and display ─────────────────────────────────────────────────────
cv_performance <- bind_rows(results)

cat("\n═══ Group k-fold CV performance ═══\n")
print(cv_performance, row.names = FALSE)


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance.csv")
write.csv(cv_performance, out_path, row.names = FALSE)

cat("\n══════════════════════════════════════════════\n")
cat("CV performance saved to:", out_path, "\n")
cat("══════════════════════════════════════════════\n")
