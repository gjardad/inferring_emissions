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


# ── Deployment-style clipping ─────────────────────────────────────────────
# After proportional calibration, cap non-emitter predictions at the maximum
# observed emission among emitters within each (nace2d, year) cell.
# Iteratively redistribute clipped excess to uncapped non-emitters so that
# the sector-year total is preserved.
#
# Inputs:
#   yhat_cal : calibrated predictions (same length as emit, y, nace2d, year)
#   emit     : binary (1 = emitter / ETS, 0 = non-emitter)
#   y        : observed emissions (used only to compute the cap)
#   nace2d, year : cell identifiers
clip_to_ets_max <- function(yhat_cal, emit, y, nace2d, year, max_iter = 50) {
  result <- yhat_cal

  cells <- unique(data.frame(nace2d = nace2d, year = year,
                             stringsAsFactors = FALSE))

  for (r in seq_len(nrow(cells))) {
    sec <- cells$nace2d[r]
    yr  <- cells$year[r]

    in_cell    <- (nace2d == sec & year == yr)
    is_emitter <- (in_cell & emit == 1)
    is_nonemit <- (in_cell & emit == 0)

    if (!any(is_emitter) || !any(is_nonemit)) next

    cap <- max(y[is_emitter], na.rm = TRUE)
    if (!is.finite(cap) || cap <= 0) next

    idx_non <- which(is_nonemit)

    for (iter in seq_len(max_iter)) {
      over <- idx_non[result[idx_non] > cap]
      if (length(over) == 0) break

      excess <- sum(result[over] - cap)
      result[over] <- cap

      uncapped <- idx_non[result[idx_non] > 0 & result[idx_non] < cap]
      if (length(uncapped) == 0) break

      denom <- sum(result[uncapped])
      if (denom == 0) break
      result[uncapped] <- result[uncapped] + excess * (result[uncapped] / denom)
    }
  }

  result
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
  m_raw <- calc_metrics(panel$y[ok], yhat_raw[ok], nace2d = panel$nace2d[ok], year = panel$year[ok])

  # Calibrated
  yhat_cal <- rep(NA_real_, nrow(panel))
  yhat_cal[ok] <- calibrate_predictions(
    yhat_raw[ok], panel$nace2d[ok], panel$year[ok], syt
  )
  m_cal <- calc_metrics(panel$y[ok], yhat_cal[ok], nace2d = panel$nace2d[ok], year = panel$year[ok])

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
    avg_nonemit_p50_rank = m_raw$avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = m_raw$avg_nonemit_p99_rank,
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
    avg_nonemit_p50_rank = m_cal$avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = m_cal$avg_nonemit_p99_rank,
    stringsAsFactors = FALSE
  )

  # Calibrated + clipped
  yhat_clip <- clip_to_ets_max(
    yhat_cal[ok], panel$emit[ok], panel$y[ok],
    panel$nace2d[ok], panel$year[ok]
  )
  m_clip <- calc_metrics(panel$y[ok], yhat_clip, nace2d = panel$nace2d[ok], year = panel$year[ok])

  results[[paste0(nm, "_clip")]] <- data.frame(
    model = nm, variant = "calibrated_clipped", threshold = NA_real_,
    n = m_clip$n, nRMSE = m_clip$nrmse_sd,
    rmse = m_clip$rmse, mae = m_clip$mae,
    mapd_emitters = m_clip$mapd_emitters, spearman = m_clip$spearman,
    fpr_nonemitters = m_clip$fpr_nonemitters, tpr_emitters = m_clip$tpr_emitters,
    emitter_mass_captured = m_clip$emitter_mass_captured,
    nonemit_p50_rank_19 = m_clip$nonemit_p50_rank_19,
    nonemit_p90_rank_19 = m_clip$nonemit_p90_rank_19,
    nonemit_p99_rank_19 = m_clip$nonemit_p99_rank_19,
    nonemit_p50_rank_24 = m_clip$nonemit_p50_rank_24,
    nonemit_p90_rank_24 = m_clip$nonemit_p90_rank_24,
    nonemit_p99_rank_24 = m_clip$nonemit_p99_rank_24,
    avg_nonemit_p50_rank = m_clip$avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = m_clip$avg_nonemit_p99_rank,
    stringsAsFactors = FALSE
  )
}

# ── Hurdle metrics (threshold search, raw + calibrated + clipped) ────────────
cat("Searching over hurdle thresholds:", paste(THRESHOLDS, collapse = ", "), "\n")

for (sp in hurdle_specs) {
  nm <- sp$name
  phat  <- panel[[paste0("phat_", nm)]]
  muhat <- panel[[paste0("muhat_", nm)]]

  ok <- !is.na(phat) & !is.na(muhat) & !is.na(panel$y)
  if (sum(ok) == 0) next

  best_raw  <- list(rmse = Inf)
  best_cal  <- list(rmse = Inf)
  best_clip <- list(rmse = Inf)
  best_thr_raw  <- NA_real_
  best_thr_cal  <- NA_real_
  best_thr_clip <- NA_real_
  best_m_raw  <- NULL
  best_m_cal  <- NULL
  best_m_clip <- NULL

  for (thr in THRESHOLDS) {
    # Hard threshold combination
    yhat_hard <- pmax(as.numeric(phat[ok] > thr) * muhat[ok], 0)

    # Raw metrics
    m_raw <- calc_metrics(panel$y[ok], yhat_hard, nace2d = panel$nace2d[ok], year = panel$year[ok])

    # Calibrated
    yhat_cal <- calibrate_predictions(
      yhat_hard, panel$nace2d[ok], panel$year[ok], syt
    )
    m_cal <- calc_metrics(panel$y[ok], yhat_cal, nace2d = panel$nace2d[ok], year = panel$year[ok])

    # Calibrated + clipped
    yhat_clip <- clip_to_ets_max(
      yhat_cal, panel$emit[ok], panel$y[ok],
      panel$nace2d[ok], panel$year[ok]
    )
    m_clip <- calc_metrics(panel$y[ok], yhat_clip, nace2d = panel$nace2d[ok], year = panel$year[ok])

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
    if (!is.na(m_clip$rmse) && m_clip$rmse < best_clip$rmse) {
      best_clip <- m_clip
      best_thr_clip <- thr
      best_m_clip <- m_clip
    }
  }

  cat(sprintf("  %s: best thr (raw)=%.2f, (cal)=%.2f, (clip)=%.2f\n",
              nm, best_thr_raw, best_thr_cal, best_thr_clip))

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
      avg_nonemit_p50_rank = best_m_raw$avg_nonemit_p50_rank,
      avg_nonemit_p99_rank = best_m_raw$avg_nonemit_p99_rank,
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
      avg_nonemit_p50_rank = best_m_cal$avg_nonemit_p50_rank,
      avg_nonemit_p99_rank = best_m_cal$avg_nonemit_p99_rank,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(best_m_clip)) {
    results[[paste0(nm, "_clip")]] <- data.frame(
      model = nm, variant = "calibrated_clipped", threshold = best_thr_clip,
      n = best_m_clip$n, nRMSE = best_m_clip$nrmse_sd,
      rmse = best_m_clip$rmse, mae = best_m_clip$mae,
      mapd_emitters = best_m_clip$mapd_emitters, spearman = best_m_clip$spearman,
      fpr_nonemitters = best_m_clip$fpr_nonemitters,
      tpr_emitters = best_m_clip$tpr_emitters,
      emitter_mass_captured = best_m_clip$emitter_mass_captured,
      nonemit_p50_rank_19 = best_m_clip$nonemit_p50_rank_19,
      nonemit_p90_rank_19 = best_m_clip$nonemit_p90_rank_19,
      nonemit_p99_rank_19 = best_m_clip$nonemit_p99_rank_19,
      nonemit_p50_rank_24 = best_m_clip$nonemit_p50_rank_24,
      nonemit_p90_rank_24 = best_m_clip$nonemit_p90_rank_24,
      nonemit_p99_rank_24 = best_m_clip$nonemit_p99_rank_24,
      avg_nonemit_p50_rank = best_m_clip$avg_nonemit_p50_rank,
      avg_nonemit_p99_rank = best_m_clip$avg_nonemit_p99_rank,
      stringsAsFactors = FALSE
    )
  }
}


# ── Combine k-fold results ─────────────────────────────────────────────────
cv_performance <- bind_rows(results)

cat("\n═══ Group k-fold CV performance ═══\n")
print(cv_performance, row.names = FALSE)


# ══════════════════════════════════════════════════════════════════════════════
# LOSOCV: Leave-One-Sector-Out CV for the hurdle + pooled proxy
#
# Evaluates how the model performs for firms in sectors NOT in the training
# sample. Each NACE 2-digit sector is held out in turn; the model is trained
# on the remaining sectors and predicts the held-out sector. With sector RE
# (s(nace2d_f, bs="re")), the RE for the unseen sector defaults to the
# population mean (correct behavior).
# ══════════════════════════════════════════════════════════════════════════════

cat("\n\n═══ LOSOCV: Leave-One-Sector-Out CV ═══\n")

# (a) Best threshold from k-fold CV (hurdle_proxy_pooled, calibrated_clipped)
best_kfold <- cv_performance[cv_performance$model == "hurdle_proxy_pooled" &
                             cv_performance$variant == "calibrated_clipped", ]

if (nrow(best_kfold) == 0) {
  cat("WARNING: hurdle_proxy_pooled calibrated_clipped not found in k-fold results.\n")
  cat("         Falling back to calibrated variant.\n")
  best_kfold <- cv_performance[cv_performance$model == "hurdle_proxy_pooled" &
                               cv_performance$variant == "calibrated", ]
}

losocv_threshold <- if (nrow(best_kfold) > 0 && !is.na(best_kfold$threshold[1])) {
  best_kfold$threshold[1]
} else {
  0.30  # sensible default
}
cat("Using threshold from k-fold CV:", losocv_threshold, "\n")

# (b) Assign sector folds (one fold per NACE 2-digit sector)
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
all_year_levels   <- levels(panel$year_f)

# (c) LOSOCV hurdle formulas (same as hurdle_proxy_pooled)
losocv_ext_formula <- emit ~ log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled) +
                             year_f + s(nace2d_f, bs = "re")
losocv_int_formula <- y ~ log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled) +
                          year_f + s(nace2d_f, bs = "re")

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
  train$year_f   <- factor(train$year,   levels = all_year_levels)
  test$year_f    <- factor(test$year,    levels = all_year_levels)

  # Step 1: Extensive margin (logit on full training data)
  fit_ext <- tryCatch(
    gam(losocv_ext_formula, data = train,
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
      gam(losocv_int_formula, data = train_emit,
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
cat(sprintf("LOSOCV done (%.1f min)\n\n", elapsed_losocv))

# (d) Combine predictions, calibrate, clip
ok_losocv <- !is.na(panel$losocv_phat) & !is.na(panel$losocv_muhat) & !is.na(panel$y)

if (sum(ok_losocv) > 0) {
  yhat_losocv_raw <- pmax(
    as.numeric(panel$losocv_phat[ok_losocv] > losocv_threshold) *
      panel$losocv_muhat[ok_losocv],
    0
  )

  # Calibrate
  yhat_losocv_cal <- calibrate_predictions(
    yhat_losocv_raw, panel$nace2d[ok_losocv], panel$year[ok_losocv], syt
  )

  # Clip
  yhat_losocv_clip <- clip_to_ets_max(
    yhat_losocv_cal, panel$emit[ok_losocv], panel$y[ok_losocv],
    panel$nace2d[ok_losocv], panel$year[ok_losocv]
  )

  # (e) Compute metrics
  m_losocv <- calc_metrics(
    panel$y[ok_losocv], yhat_losocv_clip,
    nace2d = panel$nace2d[ok_losocv], year = panel$year[ok_losocv]
  )

  cat("LOSOCV metrics (calibrated + clipped):\n")
  cat(sprintf("  nRMSE = %.3f, MAPD = %.3f, Spearman = %.3f\n",
              m_losocv$nrmse_sd, m_losocv$mapd_emitters, m_losocv$spearman))
  cat(sprintf("  FPR = %.3f, TPR = %.3f\n",
              m_losocv$fpr_nonemitters, m_losocv$tpr_emitters))
  cat(sprintf("  avg_nonemit_p50_rank = %.3f, avg_nonemit_p99_rank = %.3f\n",
              m_losocv$avg_nonemit_p50_rank, m_losocv$avg_nonemit_p99_rank))

  # (f) Append to results
  losocv_row <- data.frame(
    model = "losocv_hurdle_proxy_pooled",
    variant = "calibrated_clipped",
    threshold = losocv_threshold,
    n = m_losocv$n, nRMSE = m_losocv$nrmse_sd,
    rmse = m_losocv$rmse, mae = m_losocv$mae,
    mapd_emitters = m_losocv$mapd_emitters, spearman = m_losocv$spearman,
    fpr_nonemitters = m_losocv$fpr_nonemitters,
    tpr_emitters = m_losocv$tpr_emitters,
    emitter_mass_captured = m_losocv$emitter_mass_captured,
    nonemit_p50_rank_19 = m_losocv$nonemit_p50_rank_19,
    nonemit_p90_rank_19 = m_losocv$nonemit_p90_rank_19,
    nonemit_p99_rank_19 = m_losocv$nonemit_p99_rank_19,
    nonemit_p50_rank_24 = m_losocv$nonemit_p50_rank_24,
    nonemit_p90_rank_24 = m_losocv$nonemit_p90_rank_24,
    nonemit_p99_rank_24 = m_losocv$nonemit_p99_rank_24,
    avg_nonemit_p50_rank = m_losocv$avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = m_losocv$avg_nonemit_p99_rank,
    stringsAsFactors = FALSE
  )

  cv_performance <- bind_rows(cv_performance, losocv_row)

} else {
  cat("WARNING: No valid LOSOCV predictions. Skipping.\n")
}

# Clean up temporary column
panel$primary_nace2d <- NULL
panel$losocv_phat    <- NULL
panel$losocv_muhat   <- NULL


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance.csv")
write.csv(cv_performance, out_path, row.names = FALSE)

cat("\n══════════════════════════════════════════════\n")
cat("CV performance saved to:", out_path, "\n")
cat("══════════════════════════════════════════════\n")
