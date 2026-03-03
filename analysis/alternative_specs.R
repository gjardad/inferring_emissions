###############################################################################
# analysis/alternative_specs.R
#
# PURPOSE
#   Comprehensive comparison of all model specifications.
#   Tests 9 feature groups x 2 proxy variants x 2 architectures:
#
#   Feature groups:
#     B0: baseline (log_revenue + proxy indicator + asinh proxy)
#     A1: + capital
#     A2: + capital intensity
#     A3: + FTE
#     A4: + capital + FTE
#     B1: smooth revenue (s(log_revenue))
#     B2: smooth proxy in intensive margin only (hurdle only)
#     B3: smooth both in intensive margin only (hurdle only)
#     C1: sector-specific proxy slope (random slope on proxy by sector)
#
#   Proxy variants: proxy_pooled, proxy_weighted
#
#   Architectures:
#     hurdle: logit ext margin + Poisson int margin + threshold + calibrate_with_cap
#     hybrid: logit ext margin + raw proxy ranking + threshold + calibrate_with_cap
#
#   B2 and B3 hybrid variants are omitted (identical to B0 and B1 hybrid
#   respectively, since the hybrid architecture has no intensive margin).
#
#   Total: 18 hurdle + 14 hybrid = 32 specs.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/alternative_specs_comparison.csv
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


# ── Prepare panel ────────────────────────────────────────────────────────────
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

panel <- panel %>%
  mutate(
    year_f       = factor(year),
    nace2d_f     = factor(nace2d),
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


# ── Build spec grid ──────────────────────────────────────────────────────────
re <- "s(nace2d_f, bs = 're')"

build_specs <- function() {
  specs <- list()

  for (pname in c("pooled", "weighted")) {
    pvar      <- paste0("proxy_", pname)
    pi        <- paste0("I(", pvar, " > 0)")
    pa        <- paste0("asinh(", pvar, ")")
    pt        <- paste(pi, "+", pa)
    pt_smooth <- paste(pi, "+ s(", pa, ", k = 5)")
    pslope    <- paste0("s(nace2d_f, by = ", pa, ", bs = 're')")

    # Each entry: list(id, rev_ext, proxy_ext, rev_int, proxy_int, skip_hybrid)
    feats <- list(
      list("B0", "log_revenue", pt,
                  "log_revenue", pt, FALSE),
      list("A1", "log_revenue", paste(pt, "+ asinh_capital"),
                  "log_revenue", paste(pt, "+ asinh_capital"), FALSE),
      list("A2", "log_revenue", paste(pt, "+ capital_intensity"),
                  "log_revenue", paste(pt, "+ capital_intensity"), FALSE),
      list("A3", "log_revenue", paste(pt, "+ asinh_fte"),
                  "log_revenue", paste(pt, "+ asinh_fte"), FALSE),
      list("A4", "log_revenue", paste(pt, "+ asinh_capital + asinh_fte"),
                  "log_revenue", paste(pt, "+ asinh_capital + asinh_fte"), FALSE),
      list("B1", "s(log_revenue, k = 5)", pt,
                  "s(log_revenue, k = 5)", pt, FALSE),
      list("B2", "log_revenue", pt,
                  "log_revenue", pt_smooth, TRUE),
      list("B3", "s(log_revenue, k = 5)", pt,
                  "s(log_revenue, k = 5)", pt_smooth, TRUE),
      list("C1", "log_revenue", paste(pt, "+", pslope),
                  "log_revenue", paste(pt, "+", pslope), FALSE)
    )

    for (f in feats) {
      fid <- f[[1]]
      ext_rhs <- paste(f[[2]], "+", f[[3]], "+ year_f +", re)
      int_rhs <- paste(f[[4]], "+", f[[5]], "+ year_f +", re)

      # Hurdle spec
      specs[[length(specs) + 1]] <- list(
        name = paste0(fid, "_", pname, "_hurdle"),
        feature = fid, proxy = pname, arch = "hurdle",
        proxy_col = pvar,
        ext_formula = as.formula(paste("emit ~", ext_rhs)),
        int_formula = as.formula(paste("y ~", int_rhs))
      )

      # Hybrid spec (skip B2, B3: identical to B0, B1 hybrid)
      if (!f[[6]]) {
        specs[[length(specs) + 1]] <- list(
          name = paste0(fid, "_", pname, "_hybrid"),
          feature = fid, proxy = pname, arch = "hybrid",
          proxy_col = pvar,
          ext_formula = as.formula(paste("emit ~", ext_rhs)),
          int_formula = NULL
        )
      }
    }
  }

  specs
}

specs <- build_specs()

THRESHOLDS <- seq(0.01, 0.60, by = 0.01)

n_hurdle <- sum(sapply(specs, function(s) s$arch == "hurdle"))
n_hybrid <- sum(sapply(specs, function(s) s$arch == "hybrid"))
cat(sprintf("Spec grid: %d specs (%d hurdle + %d hybrid)\n",
            length(specs), n_hurdle, n_hybrid))
cat("Folds:", K_FOLDS, "\n")
cat("Thresholds:", length(THRESHOLDS), "values from",
    min(THRESHOLDS), "to", max(THRESHOLDS), "\n\n")


# ── Pre-allocate prediction storage ──────────────────────────────────────────
for (sp in specs) {
  panel[[paste0("phat_", sp$name)]] <- NA_real_
  if (sp$arch == "hurdle") {
    panel[[paste0("muhat_", sp$name)]] <- NA_real_
  }
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
      panel[[paste0("phat_", sp$name)]][test_idx] <- pmin(pmax(phat, 0), 1)
    }

    # Intensive margin (Poisson on emitters only; hurdle architecture only)
    if (sp$arch == "hurdle" && nrow(train_emit) > 0) {
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
cat("Computing metrics (threshold search + calibration)...\n\n")

results <- list()

for (sp in specs) {
  nm <- sp$name
  phat <- panel[[paste0("phat_", nm)]]

  if (sp$arch == "hurdle") {
    muhat <- panel[[paste0("muhat_", nm)]]
    ok <- !is.na(phat) & !is.na(muhat) & !is.na(panel$y)
  } else {
    proxy <- panel[[sp$proxy_col]]
    ok <- !is.na(phat) & !is.na(proxy) & !is.na(panel$y)
  }

  if (sum(ok) == 0) {
    cat(sprintf("  %s: SKIPPED (no valid predictions)\n", nm))
    next
  }

  # Search over thresholds using calibrated+clipped RMSE
  best_rmse <- Inf
  best_thr <- NA_real_
  best_m <- NULL

  for (thr in THRESHOLDS) {
    if (sp$arch == "hurdle") {
      yhat_raw <- pmax(as.numeric(phat[ok] > thr) * muhat[ok], 0)
    } else {
      yhat_raw <- pmax(as.numeric(phat[ok] > thr) * proxy[ok], 0)
    }

    yhat_cap <- calibrate_with_cap(
      yhat_raw, panel$emit[ok], panel$y[ok],
      panel$nace2d[ok], panel$year[ok], syt
    )

    m <- calc_metrics(panel$y[ok], yhat_cap,
                      nace2d = panel$nace2d[ok], year = panel$year[ok])

    if (!is.na(m$rmse) && m$rmse < best_rmse) {
      best_rmse <- m$rmse
      best_thr <- thr
      best_m <- m
    }
  }

  if (is.null(best_m)) {
    cat(sprintf("  %s: SKIPPED (no valid threshold)\n", nm))
    next
  }

  cat(sprintf("  %s: thr=%.2f nRMSE=%.3f rho_s=%.3f\n",
              nm, best_thr, best_m$nrmse_sd, best_m$rho_pooled))

  results[[nm]] <- data.frame(
    spec = nm, feature = sp$feature, proxy = sp$proxy,
    architecture = sp$arch, threshold = best_thr,
    n = best_m$n,
    nRMSE = best_m$nrmse_sd,
    rmse = best_m$rmse,
    mae = best_m$mae,
    median_apd = best_m$median_apd,
    spearman = best_m$spearman,
    rho_pooled = best_m$rho_pooled,
    rho_pooled_min = best_m$rho_pooled_min,
    rho_pooled_max = best_m$rho_pooled_max,
    fpr_nonemitters = best_m$fpr_nonemitters,
    tpr_emitters = best_m$tpr_emitters,
    emitter_mass_captured = best_m$emitter_mass_captured,
    stringsAsFactors = FALSE
  )
}


# ── Print comparison tables ──────────────────────────────────────────────────
comparison <- bind_rows(results)

cat("\n\n")
cat("==============================================================\n")
cat("Alternative specifications comparison (leave-firms-out, cal+clip)\n")
cat("==============================================================\n")

# By architecture
cat("\n-- By architecture (median across specs) --\n")
print(comparison %>%
  group_by(architecture) %>%
  summarise(
    n_specs   = n(),
    nRMSE_med = round(median(nRMSE, na.rm = TRUE), 3),
    rho_s_med = round(median(rho_pooled, na.rm = TRUE), 3),
    FPR_med   = round(median(fpr_nonemitters, na.rm = TRUE), 3),
    TPR_med   = round(median(tpr_emitters, na.rm = TRUE), 3),
    .groups = "drop"
  ), row.names = FALSE)

# By proxy
cat("\n-- By proxy (median across specs) --\n")
print(comparison %>%
  group_by(proxy) %>%
  summarise(
    n_specs   = n(),
    nRMSE_med = round(median(nRMSE, na.rm = TRUE), 3),
    rho_s_med = round(median(rho_pooled, na.rm = TRUE), 3),
    .groups = "drop"
  ), row.names = FALSE)

# By feature (best spec within each feature group)
cat("\n-- By feature (best rho_s across architecture/proxy) --\n")
print(comparison %>%
  group_by(feature) %>%
  summarise(
    best_nRMSE = round(min(nRMSE, na.rm = TRUE), 3),
    best_rho_s = round(max(rho_pooled, na.rm = TRUE), 3),
    best_spec  = spec[which.max(rho_pooled)],
    .groups = "drop"
  ) %>%
  arrange(desc(best_rho_s)),
  row.names = FALSE)

# Full table sorted by rho_s
cat("\n-- Full table (sorted by rho_s descending) --\n")
print(comparison %>%
  arrange(desc(rho_pooled)) %>%
  select(spec, threshold, nRMSE, spearman, rho_pooled,
         rho_pooled_min, rho_pooled_max,
         fpr_nonemitters, tpr_emitters),
  row.names = FALSE)


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_fn <- file.path(OUTPUT_DIR, "alternative_specs_comparison.csv")
write.csv(comparison, out_fn, row.names = FALSE)
cat(sprintf("\nSaved: %s\n", out_fn))

cat("\n==============================================================\n")
cat("Done.\n")
cat("==============================================================\n")
