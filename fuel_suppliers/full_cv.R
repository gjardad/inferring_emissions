###############################################################################
# fuel_suppliers/full_cv.R
#
# PURPOSE
#   Run the full CV pipeline locally from training_sample.RData (which has all
#   proxy columns pre-built). Produces fuel_suppliers_cv_performance.csv for
#   table generation. This avoids needing B2B / elastic net inputs.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
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
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

panel <- panel %>%
  mutate(
    year_f   = factor(year),
    nace2d_f = factor(nace2d),
    nace5d_f = factor(nace5d),
    emit     = as.integer(y > 0),
    log_revenue = ifelse(is.na(log_revenue), 0, log_revenue)
  )

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))


# ── Assign folds ─────────────────────────────────────────────────────────────
K_FOLDS <- 10L
set.seed(42)
unique_firms <- unique(panel$vat)
firm_folds <- sample(rep(1:K_FOLDS, length.out = length(unique_firms)))
names(firm_folds) <- unique_firms
foldid <- unname(firm_folds[panel$vat])


# ── Sector-year totals ──────────────────────────────────────────────────────
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# ── Calibration functions ────────────────────────────────────────────────────
calibrate_predictions <- function(yhat, nace2d, year, syt) {
  df <- data.frame(yhat = yhat, nace2d = nace2d, year = year,
                   idx = seq_along(yhat), stringsAsFactors = FALSE)
  cell_denom <- df %>%
    group_by(nace2d, year) %>%
    summarise(denom = sum(yhat, na.rm = TRUE), .groups = "drop")
  df <- df %>%
    left_join(cell_denom, by = c("nace2d", "year")) %>%
    left_join(syt, by = c("nace2d", "year"))
  yhat_cal <- rep(NA_real_, nrow(df))
  idx0 <- !is.na(df$E_total) & df$E_total == 0
  yhat_cal[idx0] <- 0
  idx_pos <- !is.na(df$E_total) & df$E_total > 0 & df$denom > 0
  yhat_cal[idx_pos] <- df$E_total[idx_pos] * (df$yhat[idx_pos] / df$denom[idx_pos])
  idx_fb <- !is.na(df$E_total) & df$E_total > 0 & (df$denom == 0 | is.na(df$denom))
  yhat_cal[idx_fb] <- df$E_total[idx_fb] / df$n_full[idx_fb]
  yhat_cal[order(df$idx)]
}

calibrate_with_cap <- function(yhat, emit, y, nace2d, year, syt) {
  df <- data.frame(yhat = yhat, emit = emit, y = y,
                   nace2d = nace2d, year = year,
                   idx = seq_along(yhat), stringsAsFactors = FALSE)
  df <- merge(df, syt, by = c("nace2d", "year"), all.x = TRUE)
  df <- df[order(df$idx), ]
  result <- rep(NA_real_, nrow(df))
  cells <- unique(df[, c("nace2d", "year")])
  for (r in seq_len(nrow(cells))) {
    sec <- cells$nace2d[r]; yr <- cells$year[r]
    in_cell <- which(df$nace2d == sec & df$year == yr)
    E_total <- df$E_total[in_cell[1]]; n_full <- df$n_full[in_cell[1]]
    if (is.na(E_total) || E_total == 0) { result[in_cell] <- 0; next }
    raw <- df$yhat[in_cell]; is_emi <- df$emit[in_cell] == 1; is_non <- df$emit[in_cell] == 0
    has_cap <- any(is_emi) && any(is_non)
    if (has_cap) { cap <- min(df$y[in_cell[is_emi]], na.rm = TRUE) * (1 - 1e-10); if (!is.finite(cap) || cap <= 0) has_cap <- FALSE }
    x <- rep(0, length(in_cell)); active <- which(raw > 0)
    if (length(active) == 0) active <- seq_along(in_cell)
    fixed <- integer(0); E_rem <- E_total
    for (iter in seq_len(length(in_cell) + 1)) {
      r_active <- raw[active]; denom <- sum(r_active, na.rm = TRUE)
      if (denom > 0) { x[active] <- E_rem * r_active / denom } else { x[active] <- E_rem / length(active) }
      if (!has_cap) break
      violations <- active[is_non[active] & x[active] > cap]
      if (length(violations) == 0) break
      x[violations] <- cap; E_rem <- E_rem - length(violations) * cap
      fixed <- c(fixed, violations); active <- setdiff(active, violations)
      if (length(active) == 0) break
      if (E_rem < 0) { x[active] <- 0; x[fixed] <- E_total / length(fixed); break }
    }
    x <- pmax(x, 0); result[in_cell] <- x
  }
  result
}


# ── Model specifications ────────────────────────────────────────────────────
re_nested    <- "year_f + s(nace2d_f, bs = 're') + s(nace5d_f, bs = 're')"
re_base      <- "year_f + s(nace2d_f, bs = 're')"
re_year_only <- "year_f"

make_ppml_spec <- function(name, rhs, re) {
  list(name = name, formula = as.formula(paste("y ~", rhs, "+", re)))
}
make_hurdle_spec <- function(name, rhs, re) {
  list(name = name,
       ext_formula = as.formula(paste("emit ~", rhs, "+", re)),
       int_formula = as.formula(paste("y ~", rhs, "+", re)))
}

# Only _base variants (we know base >= nested from Phase 2A)
ppml_specs <- list(
  make_ppml_spec("benchmark_base",       "log_revenue", re_base),
  make_ppml_spec("proxy_pooled_base",    "log_revenue + asinh(proxy_pooled)", re_base),
  make_ppml_spec("proxy_weighted_base",  "log_revenue + asinh(proxy_weighted)", re_base)
)

hurdle_specs <- list(
  make_hurdle_spec("hurdle_proxy_pooled_base",       "log_revenue + asinh(proxy_pooled)", re_base),
  make_hurdle_spec("hurdle_proxy_pooled_ind_base",   "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_base),
  make_hurdle_spec("hurdle_proxy_weighted_base",     "log_revenue + asinh(proxy_weighted)", re_base),
  make_hurdle_spec("hurdle_proxy_weighted_ind_base", "log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted)", re_base)
)

THRESHOLDS <- seq(0.10, 0.50, by = 0.05)


# ── Pre-allocate ────────────────────────────────────────────────────────────
for (sp in ppml_specs) panel[[paste0("yhat_ppml_", sp$name)]] <- NA_real_
for (sp in hurdle_specs) {
  panel[[paste0("phat_", sp$name)]]  <- NA_real_
  panel[[paste0("muhat_", sp$name)]] <- NA_real_
}


# ── Group k-fold CV ─────────────────────────────────────────────────────────
cat("\nRunning group k-fold CV (K =", K_FOLDS, ")...\n\n")
t0_total <- Sys.time()

for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)

  # PPML
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

  # Hurdle
  train_emit <- train[train$emit == 1, ]
  for (sp in hurdle_specs) {
    fit_ext <- tryCatch(
      gam(sp$ext_formula, data = train, family = binomial(link = "logit"), method = "REML"),
      error = function(e) NULL
    )
    if (!is.null(fit_ext)) {
      phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
      phat <- pmin(pmax(phat, 0), 1)
      panel[[paste0("phat_", sp$name)]][test_idx] <- phat
    }
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

elapsed_total <- round(difftime(Sys.time(), t0_total, units = "mins"), 1)
cat(sprintf("\nK-fold CV complete (%.1f min)\n\n", elapsed_total))


# ── Compute metrics ──────────────────────────────────────────────────────────
cat("Computing performance metrics...\n")
results <- list()

make_result_row <- function(nm, variant, thr, m) {
  data.frame(
    model = nm, variant = variant, threshold = thr,
    n = m$n, nRMSE = m$nrmse_sd, rmse = m$rmse, mae = m$mae,
    mapd_emitters = m$mapd_emitters, spearman = m$spearman,
    fpr_nonemitters = m$fpr_nonemitters, tpr_emitters = m$tpr_emitters,
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

# PPML metrics
for (sp in ppml_specs) {
  nm <- sp$name
  yhat_raw <- panel[[paste0("yhat_ppml_", nm)]]
  ok <- !is.na(yhat_raw) & !is.na(panel$y)
  if (sum(ok) == 0) next

  m_raw <- calc_metrics(panel$y[ok], yhat_raw[ok], nace2d = panel$nace2d[ok], year = panel$year[ok])
  results[[paste0(nm, "_raw")]] <- make_result_row(nm, "raw", NA_real_, m_raw)

  yhat_cap <- calibrate_with_cap(yhat_raw[ok], panel$emit[ok], panel$y[ok],
                                  panel$nace2d[ok], panel$year[ok], syt)
  m_cap <- calc_metrics(panel$y[ok], yhat_cap, nace2d = panel$nace2d[ok], year = panel$year[ok])
  results[[paste0(nm, "_clip")]] <- make_result_row(nm, "calibrated_clipped", NA_real_, m_cap)
}

# Hurdle metrics
cat("Searching over hurdle thresholds:", paste(THRESHOLDS, collapse = ", "), "\n")
for (sp in hurdle_specs) {
  nm <- sp$name
  phat  <- panel[[paste0("phat_", nm)]]
  muhat <- panel[[paste0("muhat_", nm)]]
  ok <- !is.na(phat) & !is.na(muhat) & !is.na(panel$y)
  if (sum(ok) == 0) next

  best_raw <- list(rmse = Inf); best_thr_raw <- NA_real_; best_m_raw <- NULL
  for (thr in THRESHOLDS) {
    yhat_hard <- pmax(as.numeric(phat[ok] > thr) * muhat[ok], 0)
    m_raw <- calc_metrics(panel$y[ok], yhat_hard, nace2d = panel$nace2d[ok], year = panel$year[ok])
    if (!is.na(m_raw$rmse) && m_raw$rmse < best_raw$rmse) {
      best_raw <- m_raw; best_thr_raw <- thr; best_m_raw <- m_raw
    }
  }

  yhat_hard <- pmax(as.numeric(phat[ok] > best_thr_raw) * muhat[ok], 0)
  yhat_cap <- calibrate_with_cap(yhat_hard, panel$emit[ok], panel$y[ok],
                                  panel$nace2d[ok], panel$year[ok], syt)
  best_m_clip <- calc_metrics(panel$y[ok], yhat_cap, nace2d = panel$nace2d[ok], year = panel$year[ok])

  cat(sprintf("  %s: best thr=%.2f  nRMSE_raw=%.3f  nRMSE_clip=%.3f\n",
              nm, best_thr_raw, best_m_raw$nrmse_sd, best_m_clip$nrmse_sd))

  if (!is.null(best_m_raw))
    results[[paste0(nm, "_raw")]] <- make_result_row(nm, "raw", best_thr_raw, best_m_raw)
  if (!is.null(best_m_clip))
    results[[paste0(nm, "_clip")]] <- make_result_row(nm, "calibrated_clipped", best_thr_raw, best_m_clip)
}

cv_performance <- bind_rows(results)


# ══════════════════════════════════════════════════════════════════════════════
# LOSOCV
# ══════════════════════════════════════════════════════════════════════════════
cat("\n\n═══ LOSOCV ═══\n")

firm_sector <- panel %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

sector_levels <- sort(unique(firm_sector$primary_nace2d))
n_sector_folds <- length(sector_levels)

panel <- panel %>%
  left_join(firm_sector %>% select(vat, primary_nace2d), by = "vat")

cat("Assigned", n_distinct(panel$vat), "firms to", n_sector_folds, "sector folds\n\n")

all_nace2d_levels <- levels(panel$nace2d_f)
all_nace5d_levels <- levels(panel$nace5d_f)
all_year_levels   <- levels(panel$year_f)

losocv_specs <- list(
  make_hurdle_spec("losocv_hurdle_proxy_pooled_ind_base",   "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_base),
  make_hurdle_spec("losocv_hurdle_proxy_pooled_ind_year",   "log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled)", re_year_only),
  make_hurdle_spec("losocv_hurdle_proxy_weighted_ind_base", "log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted)", re_base),
  make_hurdle_spec("losocv_hurdle_proxy_weighted_ind_year", "log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted)", re_year_only)
)

for (losocv_sp in losocv_specs) {
  losocv_nm <- losocv_sp$name
  cat(sprintf("\n── Running LOSOCV: %s ──\n", losocv_nm))

  panel$losocv_phat  <- NA_real_
  panel$losocv_muhat <- NA_real_

  t0_losocv <- Sys.time()

  for (s_idx in seq_along(sector_levels)) {
    sec <- sector_levels[s_idx]
    t0_fold <- Sys.time()

    train_idx <- which(panel$primary_nace2d != sec)
    test_idx  <- which(panel$primary_nace2d == sec)

    train <- panel[train_idx, ]
    test  <- panel[test_idx, ]

    train$nace2d_f <- factor(train$nace2d, levels = all_nace2d_levels)
    test$nace2d_f  <- factor(test$nace2d,  levels = all_nace2d_levels)
    train$nace5d_f <- factor(train$nace5d, levels = all_nace5d_levels)
    test$nace5d_f  <- factor(test$nace5d,  levels = all_nace5d_levels)
    train$year_f   <- factor(train$year,   levels = all_year_levels)
    test$year_f    <- factor(test$year,    levels = all_year_levels)

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
  cat(sprintf("LOSOCV %s done (%.1f min)\n", losocv_nm, elapsed_losocv))

  ok_losocv <- !is.na(panel$losocv_phat) & !is.na(panel$losocv_muhat) & !is.na(panel$y)

  if (sum(ok_losocv) > 0) {
    best_losocv <- list(rmse = Inf); best_losocv_thr <- NA_real_; best_m_losocv <- NULL

    for (thr in THRESHOLDS) {
      yhat_thr <- pmax(as.numeric(panel$losocv_phat[ok_losocv] > thr) *
                         panel$losocv_muhat[ok_losocv], 0)
      yhat_cap <- calibrate_with_cap(yhat_thr, panel$emit[ok_losocv], panel$y[ok_losocv],
                                      panel$nace2d[ok_losocv], panel$year[ok_losocv], syt)
      m <- calc_metrics(panel$y[ok_losocv], yhat_cap,
                        nace2d = panel$nace2d[ok_losocv], year = panel$year[ok_losocv])
      if (!is.na(m$rmse) && m$rmse < best_losocv$rmse) {
        best_losocv <- m; best_losocv_thr <- thr; best_m_losocv <- m
      }
    }

    cat(sprintf("  Best thr=%.2f  nRMSE=%.3f  rho_med=%.3f [%.3f, %.3f]\n",
                best_losocv_thr, best_m_losocv$nrmse_sd,
                best_m_losocv$within_sy_rho_med,
                best_m_losocv$within_sy_rho_min, best_m_losocv$within_sy_rho_max))

    cv_performance <- bind_rows(cv_performance,
      make_result_row(losocv_nm, "calibrated_clipped", best_losocv_thr, best_m_losocv))
  }

  panel$losocv_phat  <- NULL
  panel$losocv_muhat <- NULL
}

panel$primary_nace2d <- NULL


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance.csv")
write.csv(cv_performance, out_path, row.names = FALSE)

cat("\n═══ Group k-fold CV performance ═══\n")
print(cv_performance %>% select(model, variant, threshold, nRMSE, spearman,
                                 within_sy_rho_med, within_sy_rho_min, within_sy_rho_max),
      row.names = FALSE)

cat("\n\nCV results saved to:", out_path, "\n")
cat("Now run table_cv_performance.R to generate LaTeX tables.\n")
