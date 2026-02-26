###############################################################################
# fuel_suppliers/local_phase1_experiments.R
#
# PURPOSE
#   Phase 1 local experimentation: test additional features, functional forms,
#   and interactions against the current hurdle model baseline.
#
#   All specs use leave-firms-out 10-fold CV with hurdle architecture
#   (logit extensive + Poisson intensive + threshold search), calibrated
#   with cap. Primary comparison metric: within-sector-year Spearman rho.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   Console: comparison table + per-sector rho detail
#   {OUTPUT_DIR}/phase1_spec_comparison.csv
#   {OUTPUT_DIR}/phase1_rho_detail_{spec}.csv (for baseline + best)
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


# ── Prepare panel ────────────────────────────────────────────────────────────
# Rename annual accounts variables if they have original codes
if ("turnover_VAT" %in% names(panel) && !"revenue" %in% names(panel)) {
  panel <- panel %>% rename(revenue = turnover_VAT)
}
if ("v_0022_27" %in% names(panel) && !"capital" %in% names(panel)) {
  panel <- panel %>% rename(capital = v_0022_27)
}
if ("v_0001003" %in% names(panel) && !"fte" %in% names(panel)) {
  panel <- panel %>% rename(fte = v_0001003)
}
if ("v_0001023" %in% names(panel) && !"wage_bill" %in% names(panel)) {
  panel <- panel %>% rename(wage_bill = v_0001023)
}
if ("v_0009800" %in% names(panel) && !"value_added" %in% names(panel)) {
  panel <- panel %>% rename(value_added = v_0009800)
}

# Derived variables
panel <- panel %>%
  mutate(
    year_f       = factor(year),
    nace2d_f     = factor(nace2d),
    nace5d_f     = factor(nace5d),
    asinh_capital = asinh(coalesce(capital, 0)),
    asinh_fte     = asinh(coalesce(fte, 0)),
    capital_intensity = asinh(coalesce(capital, 0) / pmax(exp(log_revenue), 1))
  )

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("Has capital:", sum(!is.na(panel$capital) & panel$capital > 0),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(panel$capital) & panel$capital > 0)))
cat("Has FTE:", sum(!is.na(panel$fte) & panel$fte > 0),
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(panel$fte) & panel$fte > 0)))


# ── Assign folds ─────────────────────────────────────────────────────────────
K_FOLDS <- 10L
set.seed(42)
unique_firms <- unique(panel$vat)
firm_folds <- sample(rep(1:K_FOLDS, length.out = length(unique_firms)))
names(firm_folds) <- unique_firms
foldid <- unname(firm_folds[panel$vat])

cat("Fold sizes:", paste(table(foldid), collapse = ", "), "\n\n")


# ── Sector-year totals ──────────────────────────────────────────────────────
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# ── Calibration with cap (copied from build_proxy_and_cv.R) ─────────────────
calibrate_with_cap <- function(yhat, emit, y, nace2d, year, syt) {
  df <- data.frame(
    yhat = yhat, emit = emit, y = y,
    nace2d = nace2d, year = year,
    idx = seq_along(yhat), stringsAsFactors = FALSE
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

    if (is.na(E_total) || E_total == 0) {
      result[in_cell] <- 0
      next
    }

    raw    <- df$yhat[in_cell]
    is_emi <- df$emit[in_cell] == 1
    is_non <- df$emit[in_cell] == 0

    has_cap <- any(is_emi) && any(is_non)
    if (has_cap) {
      cap <- min(df$y[in_cell[is_emi]], na.rm = TRUE) * (1 - 1e-10)
      if (!is.finite(cap) || cap <= 0) has_cap <- FALSE
    }

    x       <- rep(0, length(in_cell))
    active  <- which(raw > 0)
    if (length(active) == 0) active <- seq_along(in_cell)
    fixed   <- integer(0)
    E_rem   <- E_total

    for (iter in seq_len(length(in_cell) + 1)) {
      r_active <- raw[active]
      denom    <- sum(r_active, na.rm = TRUE)

      if (denom > 0) {
        x[active] <- E_rem * r_active / denom
      } else {
        x[active] <- E_rem / length(active)
      }

      if (!has_cap) break

      violations <- active[is_non[active] & x[active] > cap]
      if (length(violations) == 0) break

      x[violations] <- cap
      E_rem  <- E_rem - length(violations) * cap
      fixed  <- c(fixed, violations)
      active <- setdiff(active, violations)

      if (length(active) == 0) break

      if (E_rem < 0) {
        x[active] <- 0
        x[fixed]  <- E_total / length(fixed)
        break
      }
    }

    x <- pmax(x, 0)
    result[in_cell] <- x
  }

  result
}


# ── Model specifications ────────────────────────────────────────────────────
# Each spec has: name, ext_formula (logit), int_formula (Poisson)
# All use base RE: s(nace2d_f, bs = "re")

re <- "s(nace2d_f, bs = 're')"
proxy_terms <- "I(proxy_pooled > 0) + asinh(proxy_pooled)"

make_spec <- function(name, ext_rhs, int_rhs) {
  list(
    name        = name,
    ext_formula = as.formula(paste("emit ~", ext_rhs, "+ year_f +", re)),
    int_formula = as.formula(paste("y ~",    int_rhs, "+ year_f +", re))
  )
}

# For specs where both margins use the same RHS (most cases)
make_spec_same <- function(name, rhs) {
  make_spec(name, rhs, rhs)
}

specs <- list(
  # Baseline
  make_spec_same("B0_baseline",
    paste("log_revenue +", proxy_terms)),

  # Group A: additional features
  make_spec_same("A1_capital",
    paste("log_revenue +", proxy_terms, "+ asinh_capital")),

  make_spec_same("A2_cap_intensity",
    paste("log_revenue +", proxy_terms, "+ capital_intensity")),

  make_spec_same("A3_fte",
    paste("log_revenue +", proxy_terms, "+ asinh_fte")),

  make_spec_same("A4_capital_fte",
    paste("log_revenue +", proxy_terms, "+ asinh_capital + asinh_fte")),

  # Group B: non-linear functional form
  make_spec_same("B1_smooth_revenue",
    paste("s(log_revenue, k = 5) +", proxy_terms)),

  # B2: smooth proxy only in intensive margin
  make_spec("B2_smooth_proxy",
    paste("log_revenue +", proxy_terms),
    "log_revenue + I(proxy_pooled > 0) + s(asinh(proxy_pooled), k = 5)"),

  make_spec("B3_smooth_both",
    paste("s(log_revenue, k = 5) +", proxy_terms),
    "s(log_revenue, k = 5) + I(proxy_pooled > 0) + s(asinh(proxy_pooled), k = 5)"),

  # Group C: sector-proxy interaction (random slope)
  make_spec_same("C1_sector_proxy_slope",
    paste("log_revenue +", proxy_terms,
          "+ s(nace2d_f, by = asinh(proxy_pooled), bs = 're')"))
)

THRESHOLDS <- seq(0.10, 0.50, by = 0.05)

cat("Specs to run:", length(specs), "\n")
cat("Folds:", K_FOLDS, "\n")
cat("Thresholds:", paste(THRESHOLDS, collapse = ", "), "\n\n")


# ── Pre-allocate prediction storage ──────────────────────────────────────────
for (sp in specs) {
  panel[[paste0("phat_", sp$name)]]  <- NA_real_
  panel[[paste0("muhat_", sp$name)]] <- NA_real_
}


# ── Run CV ───────────────────────────────────────────────────────────────────
cat("Running leave-firms-out CV...\n\n")
t0_total <- Sys.time()

for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)

  train_emit <- train[train$emit == 1, ]

  for (sp in specs) {
    # Extensive margin (logit on all training data)
    fit_ext <- tryCatch(
      gam(sp$ext_formula, data = train,
          family = binomial(link = "logit"), method = "REML"),
      error = function(e) { message("  [", sp$name, " ext] ", e$message); NULL }
    )
    if (!is.null(fit_ext)) {
      phat <- as.numeric(predict(fit_ext, newdata = test, type = "response"))
      phat <- pmin(pmax(phat, 0), 1)
      panel[[paste0("phat_", sp$name)]][test_idx] <- phat
    }

    # Intensive margin (Poisson on emitters only)
    if (nrow(train_emit) > 0) {
      fit_int <- tryCatch(
        gam(sp$int_formula, data = train_emit,
            family = poisson(link = "log"), method = "REML"),
        error = function(e) { message("  [", sp$name, " int] ", e$message); NULL }
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
cat(sprintf("\nCV complete (%.1f min)\n\n", elapsed_total))


# ── Compute metrics (threshold search + calibration) ─────────────────────────
cat("Computing metrics...\n\n")

results <- list()
rho_details <- list()

for (sp in specs) {
  nm <- sp$name
  phat  <- panel[[paste0("phat_", nm)]]
  muhat <- panel[[paste0("muhat_", nm)]]

  ok <- !is.na(phat) & !is.na(muhat) & !is.na(panel$y)
  if (sum(ok) == 0) {
    cat(sprintf("  %s: SKIPPED (no valid predictions)\n", nm))
    next
  }

  # Search over thresholds using calibrated+clipped predictions
  best <- list(rmse = Inf)
  best_thr <- NA_real_
  best_m <- NULL

  for (thr in THRESHOLDS) {
    yhat_raw <- pmax(as.numeric(phat[ok] > thr) * muhat[ok], 0)

    yhat_cap <- calibrate_with_cap(
      yhat_raw, panel$emit[ok], panel$y[ok],
      panel$nace2d[ok], panel$year[ok], syt
    )

    m <- calc_metrics(panel$y[ok], yhat_cap,
                      nace2d = panel$nace2d[ok], year = panel$year[ok])

    if (!is.na(m$rmse) && m$rmse < best$rmse) {
      best     <- m
      best_thr <- thr
      best_m   <- m
    }
  }

  if (is.null(best_m)) {
    cat(sprintf("  %s: SKIPPED (no valid threshold)\n", nm))
    next
  }

  cat(sprintf("  %s: thr=%.2f  nRMSE=%.3f  MAPD=%.3f  rho_med=%.3f [%.3f, %.3f]\n",
              nm, best_thr, best_m$nrmse_sd, best_m$mapd_emitters,
              best_m$within_sy_rho_med, best_m$within_sy_rho_min,
              best_m$within_sy_rho_max))

  results[[nm]] <- data.frame(
    spec = nm, threshold = best_thr,
    n = best_m$n, nRMSE = best_m$nrmse_sd,
    rmse = best_m$rmse, mae = best_m$mae,
    mapd_emitters = best_m$mapd_emitters, spearman = best_m$spearman,
    fpr_nonemitters = best_m$fpr_nonemitters,
    tpr_emitters = best_m$tpr_emitters,
    emitter_mass_captured = best_m$emitter_mass_captured,
    within_sy_rho_med = best_m$within_sy_rho_med,
    within_sy_rho_min = best_m$within_sy_rho_min,
    within_sy_rho_max = best_m$within_sy_rho_max,
    stringsAsFactors = FALSE
  )

  rho_details[[nm]] <- best_m$within_sy_rho_detail
}


# ── Print comparison table ───────────────────────────────────────────────────
comparison <- bind_rows(results)

cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("Phase 1 specification comparison (leave-firms-out, cal+clip)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Key columns
print(comparison %>%
  select(spec, threshold, nRMSE, mapd_emitters, spearman,
         fpr_nonemitters, tpr_emitters,
         within_sy_rho_med, within_sy_rho_min),
  row.names = FALSE)

# Save
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
write.csv(comparison,
          file.path(OUTPUT_DIR, "phase1_spec_comparison.csv"),
          row.names = FALSE)


# ── Per-sector rho comparison: baseline vs each spec ─────────────────────────
if ("B0_baseline" %in% names(rho_details)) {
  baseline_rho <- rho_details[["B0_baseline"]]

  cat("\n\n══════════════════════════════════════════════════════════════\n")
  cat("Per-sector rho: each spec vs baseline\n")
  cat("══════════════════════════════════════════════════════════════\n")

  for (nm in setdiff(names(rho_details), "B0_baseline")) {
    spec_rho <- rho_details[[nm]]

    merged <- merge(baseline_rho, spec_rho,
                    by = c("nace2d", "year"), suffixes = c("_base", "_spec"))

    sector_summary <- merged %>%
      group_by(nace2d) %>%
      summarise(
        n_years    = n(),
        n_firms    = median(n_firms_base),
        rho_base   = median(rho_base),
        rho_spec   = median(rho_spec),
        delta      = median(rho_spec) - median(rho_base),
        .groups    = "drop"
      ) %>%
      arrange(nace2d)

    # Only print if there's a meaningful difference
    overall_delta <- median(merged$rho_spec) - median(merged$rho_base)

    cat(sprintf("\n── %s (overall delta: %+.3f) ──\n", nm, overall_delta))
    cat(sprintf("%-6s  %5s  %5s  %8s  %8s  %8s\n",
                "NACE", "Yrs", "Firms", "Base", "Spec", "Delta"))
    cat(strrep("-", 50), "\n")
    for (i in seq_len(nrow(sector_summary))) {
      r <- sector_summary[i, ]
      cat(sprintf("%-6s  %5d  %5.0f  %8.3f  %8.3f  %+8.3f\n",
                  r$nace2d, r$n_years, r$n_firms,
                  r$rho_base, r$rho_spec, r$delta))
    }

    n_wins  <- sum(merged$rho_spec > merged$rho_base)
    n_loses <- sum(merged$rho_spec < merged$rho_base)
    n_ties  <- sum(merged$rho_spec == merged$rho_base)
    cat(sprintf("\n  Cells: %d | Wins: %d (%.0f%%) | Losses: %d (%.0f%%) | Ties: %d\n",
                nrow(merged), n_wins, 100 * n_wins / nrow(merged),
                n_loses, 100 * n_loses / nrow(merged), n_ties))

    # Save rho detail
    write.csv(spec_rho,
              file.path(OUTPUT_DIR, paste0("phase1_rho_detail_", nm, ".csv")),
              row.names = FALSE)
  }

  write.csv(baseline_rho,
            file.path(OUTPUT_DIR, "phase1_rho_detail_B0_baseline.csv"),
            row.names = FALSE)
}


cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("Phase 1 complete. Results saved to:", OUTPUT_DIR, "\n")
cat("══════════════════════════════════════════════════════════════\n")
