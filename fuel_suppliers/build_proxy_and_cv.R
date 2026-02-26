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

# Merge nace5d from loocv_training_sample (lhs only has nace2d)
load(file.path(PROC_DATA, "loocv_training_sample.RData"))
nace5d_lookup <- loocv_training_sample %>%
  filter(year >= 2005) %>%
  select(vat, year, nace5d) %>%
  mutate(nace4d = substr(nace5d, 1, 4))
rm(loocv_training_sample)

# Merge proxies into LHS panel
panel <- lhs %>%
  left_join(proxy_pooled, by = c("vat", "year")) %>%
  left_join(proxy_fe,     by = c("vat", "year")) %>%
  left_join(nace5d_lookup, by = c("vat", "year")) %>%
  mutate(
    proxy_pooled = coalesce(proxy_pooled, 0),
    proxy_fe     = coalesce(proxy_fe, 0),
    year_f       = factor(year),
    nace2d_f     = factor(nace2d),
    nace4d_f     = factor(nace4d),
    nace5d_f     = factor(nace5d),
    emit         = as.integer(y > 0)
  )
rm(nace5d_lookup)

cat("Panel rows:", nrow(panel), "\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("Proxy coverage (pooled > 0):", sum(panel$proxy_pooled > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_pooled > 0)))
cat("Proxy coverage (within-buyer > 0):", sum(panel$proxy_fe > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_fe > 0)))

# ── Save analysis-ready panel (for local experimentation) ────────────────────
# Contains everything needed to run model variants without B2B or elastic net
# inputs. Small enough to transfer to local desktop via Dropbox.
analysis_panel <- panel %>%
  select(vat, year, y, log_revenue, nace2d, nace5d, euets, emit,
         proxy_pooled, proxy_fe)
save(analysis_panel, file = file.path(PROC_DATA, "fuel_suppliers_analysis_panel.RData"))
cat("Saved analysis panel:", nrow(analysis_panel), "rows to",
    file.path(PROC_DATA, "fuel_suppliers_analysis_panel.RData"), "\n\n")
rm(analysis_panel)

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


# ── Joint calibration with cap ─────────────────────────────────────────────
# Jointly calibrate raw predictions to known sector-year totals while capping
# non-emitter predictions at min(y[ETS]) within each cell. Uses iterative
# proportional fitting with box constraints (water-filling algorithm):
#   1. Proportionally allocate E_total among all firms
#   2. Cap non-emitters that exceed min(y[ETS])
#   3. Redistribute excess to ALL remaining uncapped firms proportionally
#   4. Iterate until no violations
#
# Among uncapped firms, proportionality is preserved exactly. This is the
# standard approach for combining benchmarking with inequality constraints
# in small area estimation (Rao & Molina 2015, Ch. 6; Chen et al. 2022).
#
# Inputs:
#   yhat   : raw predictions (NOT pre-calibrated)
#   emit   : binary (1 = emitter / ETS, 0 = non-emitter)
#   y      : observed emissions (used only to compute the cap)
#   nace2d, year : cell identifiers
#   syt    : sector-year totals (data.frame with nace2d, year, E_total, n_full)
calibrate_with_cap <- function(yhat, emit, y, nace2d, year, syt) {

  df <- data.frame(
    yhat   = yhat,
    emit   = emit,
    y      = y,
    nace2d = nace2d,
    year   = year,
    idx    = seq_along(yhat),
    stringsAsFactors = FALSE
  )
  df <- merge(df, syt, by = c("nace2d", "year"), all.x = TRUE)
  df <- df[order(df$idx), ]

  result <- rep(NA_real_, nrow(df))

  cells <- unique(df[, c("nace2d", "year")])

  for (r in seq_len(nrow(cells))) {
    sec <- cells$nace2d[r]
    yr  <- cells$year[r]
    in_cell <- which(df$nace2d == sec & df$year == yr)

    E_total <- df$E_total[in_cell[1]]
    n_full  <- df$n_full[in_cell[1]]

    # ── E_total missing or zero → all predictions = 0 ──
    if (is.na(E_total) || E_total == 0) {
      result[in_cell] <- 0
      next
    }

    raw    <- df$yhat[in_cell]
    is_emi <- df$emit[in_cell] == 1
    is_non <- df$emit[in_cell] == 0

    # ── Compute cap: min observed emitter emission in cell ──
    # If no emitters or no non-emitters, just do proportional calibration
    has_cap <- any(is_emi) && any(is_non)
    if (has_cap) {
      cap <- min(df$y[in_cell[is_emi]], na.rm = TRUE) * (1 - 1e-10)
      if (!is.finite(cap) || cap <= 0) has_cap <- FALSE
    }

    # ── Iterative proportional fitting with cap ──
    # Firms with yhat = 0 (e.g. zeroed by hurdle threshold) stay at 0
    # and do not participate in the allocation.
    x       <- rep(0, length(in_cell))
    active  <- which(raw > 0)           # only firms with positive predictions
    if (length(active) == 0) active <- seq_along(in_cell)  # fallback: all firms
    fixed   <- integer(0)
    E_rem   <- E_total

    for (iter in seq_len(length(in_cell) + 1)) {
      # Proportional allocation among active firms
      r_active <- raw[active]
      denom    <- sum(r_active, na.rm = TRUE)

      if (denom > 0) {
        x[active] <- E_rem * r_active / denom
      } else {
        x[active] <- E_rem / length(active)
      }

      if (!has_cap) break

      # Find non-emitter violations
      violations <- active[is_non[active] & x[active] > cap]
      if (length(violations) == 0) break

      # Fix violators at cap
      x[violations] <- cap
      E_rem  <- E_rem - length(violations) * cap
      fixed  <- c(fixed, violations)
      active <- setdiff(active, violations)

      if (length(active) == 0) break

      # Safety: if capped mass exceeds total, scale down capped firms
      if (E_rem < 0) {
        x[active] <- 0
        x[fixed]  <- E_total / length(fixed)
        break
      }
    }

    # Non-negativity
    x <- pmax(x, 0)

    result[in_cell] <- x
  }

  result
}


# ── Model specifications ────────────────────────────────────────────────────
# All models are run with two RE variants:
#   "nested": s(nace2d_f, bs = "re") + s(nace5d_f, bs = "re")
#   "base":   s(nace2d_f, bs = "re") only
# Base variants get a "_base" name suffix. Both go into the output CSV.
# A comparison of within-sector-year rho is printed at the end.

re_nested <- "year_f + s(nace2d_f, bs = 're') + s(nace5d_f, bs = 're')"
re_base   <- "year_f + s(nace2d_f, bs = 're')"

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
  make_ppml_spec("proxy_within_buyer_base", "log_revenue + asinh(proxy_fe)", re_base)
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
  make_hurdle_spec("hurdle_proxy_within_buyer_base", "log_revenue + asinh(proxy_fe)", re_base)
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
    within_sy_rho_med = m_raw$within_sy_rho_med,
    within_sy_rho_min = m_raw$within_sy_rho_min,
    within_sy_rho_max = m_raw$within_sy_rho_max,
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
    within_sy_rho_med = m_cal$within_sy_rho_med,
    within_sy_rho_min = m_cal$within_sy_rho_min,
    within_sy_rho_max = m_cal$within_sy_rho_max,
    stringsAsFactors = FALSE
  )

  # Joint calibration + cap
  yhat_cap <- calibrate_with_cap(
    yhat_raw[ok], panel$emit[ok], panel$y[ok],
    panel$nace2d[ok], panel$year[ok], syt
  )
  m_cap <- calc_metrics(panel$y[ok], yhat_cap, nace2d = panel$nace2d[ok], year = panel$year[ok])

  results[[paste0(nm, "_clip")]] <- data.frame(
    model = nm, variant = "calibrated_clipped", threshold = NA_real_,
    n = m_cap$n, nRMSE = m_cap$nrmse_sd,
    rmse = m_cap$rmse, mae = m_cap$mae,
    mapd_emitters = m_cap$mapd_emitters, spearman = m_cap$spearman,
    fpr_nonemitters = m_cap$fpr_nonemitters, tpr_emitters = m_cap$tpr_emitters,
    emitter_mass_captured = m_cap$emitter_mass_captured,
    nonemit_p50_rank_19 = m_cap$nonemit_p50_rank_19,
    nonemit_p90_rank_19 = m_cap$nonemit_p90_rank_19,
    nonemit_p99_rank_19 = m_cap$nonemit_p99_rank_19,
    nonemit_p50_rank_24 = m_cap$nonemit_p50_rank_24,
    nonemit_p90_rank_24 = m_cap$nonemit_p90_rank_24,
    nonemit_p99_rank_24 = m_cap$nonemit_p99_rank_24,
    avg_nonemit_p50_rank = m_cap$avg_nonemit_p50_rank,
    avg_nonemit_p99_rank = m_cap$avg_nonemit_p99_rank,
    within_sy_rho_med = m_cap$within_sy_rho_med,
    within_sy_rho_min = m_cap$within_sy_rho_min,
    within_sy_rho_max = m_cap$within_sy_rho_max,
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
      within_sy_rho_med = best_m_raw$within_sy_rho_med,
      within_sy_rho_min = best_m_raw$within_sy_rho_min,
      within_sy_rho_max = best_m_raw$within_sy_rho_max,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(best_m_cal)) {
    results[[paste0(nm, "_cal")]] <- data.frame(
      model = nm, variant = "calibrated", threshold = best_thr_raw,
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
      within_sy_rho_med = best_m_cal$within_sy_rho_med,
      within_sy_rho_min = best_m_cal$within_sy_rho_min,
      within_sy_rho_max = best_m_cal$within_sy_rho_max,
      stringsAsFactors = FALSE
    )
  }
  if (!is.null(best_m_clip)) {
    results[[paste0(nm, "_clip")]] <- data.frame(
      model = nm, variant = "calibrated_clipped", threshold = best_thr_raw,
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
      within_sy_rho_med = best_m_clip$within_sy_rho_med,
      within_sy_rho_min = best_m_clip$within_sy_rho_min,
      within_sy_rho_max = best_m_clip$within_sy_rho_max,
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
all_nace5d_levels <- levels(panel$nace5d_f)
all_year_levels   <- levels(panel$year_f)

# (c) LOSOCV hurdle formulas — nested + base variants
losocv_specs <- list(
  make_hurdle_spec("losocv_hurdle_proxy_pooled",          "log_revenue + asinh(proxy_pooled)", re_nested),
  make_hurdle_spec("losocv_hurdle_proxy_pooled_base",     "log_revenue + asinh(proxy_pooled)", re_base),
  make_hurdle_spec("losocv_hurdle_proxy_pooled_ind",      "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_nested),
  make_hurdle_spec("losocv_hurdle_proxy_pooled_ind_base", "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_base)
)

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

  # (d) Combine predictions — search over LOSO-specific thresholds
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
    losocv_row <- data.frame(
      model = losocv_nm,
      variant = "calibrated_clipped",
      threshold = this_thr,
      n = this_m$n, nRMSE = this_m$nrmse_sd,
      rmse = this_m$rmse, mae = this_m$mae,
      mapd_emitters = this_m$mapd_emitters, spearman = this_m$spearman,
      fpr_nonemitters = this_m$fpr_nonemitters,
      tpr_emitters = this_m$tpr_emitters,
      emitter_mass_captured = this_m$emitter_mass_captured,
      nonemit_p50_rank_19 = this_m$nonemit_p50_rank_19,
      nonemit_p90_rank_19 = this_m$nonemit_p90_rank_19,
      nonemit_p99_rank_19 = this_m$nonemit_p99_rank_19,
      nonemit_p50_rank_24 = this_m$nonemit_p50_rank_24,
      nonemit_p90_rank_24 = this_m$nonemit_p90_rank_24,
      nonemit_p99_rank_24 = this_m$nonemit_p99_rank_24,
      avg_nonemit_p50_rank = this_m$avg_nonemit_p50_rank,
      avg_nonemit_p99_rank = this_m$avg_nonemit_p99_rank,
      within_sy_rho_med = this_m$within_sy_rho_med,
      within_sy_rho_min = this_m$within_sy_rho_min,
      within_sy_rho_max = this_m$within_sy_rho_max,
      stringsAsFactors = FALSE
    )
    cv_performance <- bind_rows(cv_performance, losocv_row)

    # Capture within-sector-year rho detail
    if (losocv_nm == "losocv_hurdle_proxy_pooled") {
      losocv_hurdle_sy_rho <- this_m$within_sy_rho_detail
    } else if (losocv_nm == "losocv_hurdle_proxy_pooled_ind") {
      losocv_hurdle_ind_sy_rho <- this_m$within_sy_rho_detail
    } else if (losocv_nm == "losocv_hurdle_proxy_pooled_ind_base") {
      losocv_hurdle_ind_base_sy_rho <- this_m$within_sy_rho_detail
    }

    # Save firm-level LOSO predictions for pooled within-sector rho diagnostic
    if (losocv_nm %in% c("losocv_hurdle_proxy_pooled_ind",
                          "losocv_hurdle_proxy_pooled_ind_base")) {
      yhat_best <- pmax(
        as.numeric(panel$losocv_phat[ok_losocv] > this_thr) *
          panel$losocv_muhat[ok_losocv], 0
      )
      yhat_best_cap <- calibrate_with_cap(
        yhat_best, panel$emit[ok_losocv], panel$y[ok_losocv],
        panel$nace2d[ok_losocv], panel$year[ok_losocv], syt
      )
      loso_preds <- data.frame(
        vat = panel$vat[ok_losocv], nace2d = panel$nace2d[ok_losocv],
        year = panel$year[ok_losocv], y = panel$y[ok_losocv],
        yhat_clip = yhat_best_cap, stringsAsFactors = FALSE
      )
      if (losocv_nm == "losocv_hurdle_proxy_pooled_ind") {
        loso_preds_nested <- loso_preds
      } else {
        loso_preds_base <- loso_preds
      }
    }

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

out_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance.csv")
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
  benchmark_sy_rho_raw              = "within_sy_rho_benchmark_raw.csv",
  proxy_pooled_sy_rho_raw           = "within_sy_rho_proxy_pooled_raw.csv",
  hurdle_proxy_pooled_sy_rho_raw    = "within_sy_rho_hurdle_proxy_pooled_raw.csv",
  hurdle_proxy_pooled_ind_sy_rho_raw  = "within_sy_rho_hurdle_proxy_pooled_ind_raw.csv",
  hurdle_proxy_pooled_ind_sy_rho_clip = "within_sy_rho_hurdle_proxy_pooled_ind_cal_clip.csv",
  losocv_hurdle_sy_rho              = "within_sy_rho_losocv_hurdle_proxy_pooled.csv",
  losocv_hurdle_ind_sy_rho          = "within_sy_rho_losocv_hurdle_proxy_pooled_ind.csv",
  hurdle_ind_base_sy_rho_clip       = "within_sy_rho_hurdle_proxy_pooled_ind_base_cal_clip.csv",
  losocv_hurdle_ind_base_sy_rho     = "within_sy_rho_losocv_hurdle_proxy_pooled_ind_base.csv"
)
for (v in names(sy_rho_saves)) {
  if (exists(v) && nrow(get(v)) > 0) {
    write.csv(get(v),
              file.path(OUTPUT_DIR, sy_rho_saves[[v]]),
              row.names = FALSE)
  }
}

cat("\n══════════════════════════════════════════════\n")
cat("CV performance saved to:", out_path, "\n")
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

if (exists("losocv_hurdle_ind_sy_rho") &&
    exists("losocv_hurdle_ind_base_sy_rho")) {
  compare_rho_variants(
    losocv_hurdle_ind_sy_rho,
    losocv_hurdle_ind_base_sy_rho,
    "LOSO: hurdle_proxy_pooled_ind (calibrated_clipped)"
  )
}

# ── Save firm-level predictions for diagnostics ─────────────────────────────
for (obj_name in c("firm_preds_nested", "firm_preds_base",
                    "loso_preds_nested", "loso_preds_base")) {
  if (exists(obj_name)) {
    write.csv(get(obj_name),
              file.path(OUTPUT_DIR, paste0(obj_name, ".csv")),
              row.names = FALSE)
  }
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

if (exists("loso_preds_nested") && exists("loso_preds_base")) {
  lrho_nest <- pooled_within_sector_rho(loso_preds_nested,
    "LOSO NESTED: hurdle_proxy_pooled_ind (cal_clip)")
  lrho_base <- pooled_within_sector_rho(loso_preds_base,
    "LOSO BASE: hurdle_proxy_pooled_ind (cal_clip)")

  lcomp <- merge(lrho_base, lrho_nest, by = "nace2d", suffixes = c("_base", "_nested"))
  cat("\n── LOSO: pooled rho comparison ──\n")
  cat(sprintf("%-6s  %8s  %10s  %10s  %10s\n",
              "NACE", "FirmYrs", "Base", "Nested", "Delta"))
  cat(strrep("-", 50), "\n")
  for (i in seq_len(nrow(lcomp))) {
    r <- lcomp[i, ]
    cat(sprintf("%-6s  %8d  %10.3f  %10.3f  %+10.3f\n",
                r$nace2d, r$n_firmyears_base,
                r$rho_pooled_base, r$rho_pooled_nested,
                r$rho_pooled_nested - r$rho_pooled_base))
  }
}
