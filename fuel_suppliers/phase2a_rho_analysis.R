###############################################################################
# fuel_suppliers/phase2a_rho_analysis.R
#
# PURPOSE
#   Compare within-sector ranking (pooled Spearman rho on demeaned predictions)
#   for proxy_pooled vs proxy_weighted. Runs hurdle CV with indicator + base RE,
#   calibrates with cap, then computes per-sector pooled rho.
#
# INPUT
#   {PROC_DATA}/training_sample.RData (must include proxy_weighted column)
#
# OUTPUT
#   Console: per-sector rho comparison table
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
    emit     = as.integer(y > 0)
  )

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("proxy_weighted > 0:", sum(panel$proxy_weighted > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_weighted > 0)))


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


# ── Calibration with cap ─────────────────────────────────────────────────────
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


# ── Model specs (only the two we care about) ─────────────────────────────────
re_base <- "year_f + s(nace2d_f, bs = 're')"

specs <- list(
  list(
    name = "pooled_ind_base",
    ext_formula = as.formula(paste("emit ~ log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled) +", re_base)),
    int_formula = as.formula(paste("y ~ log_revenue + I(proxy_pooled > 0) + asinh(proxy_pooled) +", re_base))
  ),
  list(
    name = "weighted_ind_base",
    ext_formula = as.formula(paste("emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +", re_base)),
    int_formula = as.formula(paste("y ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +", re_base))
  )
)

THRESHOLDS <- seq(0.10, 0.50, by = 0.05)


# ── Pre-allocate ──────────────────────────────────────────────────────────────
for (sp in specs) {
  panel[[paste0("phat_", sp$name)]]  <- NA_real_
  panel[[paste0("muhat_", sp$name)]] <- NA_real_
}


# ── Run CV ────────────────────────────────────────────────────────────────────
cat("\nRunning leave-firms-out CV (2 specs x 10 folds)...\n\n")
t0_total <- Sys.time()

for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train <- panel[foldid != k, ]
  test  <- panel[foldid == k, ]
  test_idx <- which(foldid == k)
  train_emit <- train[train$emit == 1, ]

  for (sp in specs) {
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


# ── Threshold search + calibration ────────────────────────────────────────────
cat("Threshold search + calibration...\n\n")

firm_preds <- list()
rho_details <- list()

for (sp in specs) {
  nm <- sp$name
  phat  <- panel[[paste0("phat_", nm)]]
  muhat <- panel[[paste0("muhat_", nm)]]

  ok <- !is.na(phat) & !is.na(muhat) & !is.na(panel$y)

  # Step 1: search threshold by RAW RMSE (calibration comes after)
  best_raw <- list(rmse = Inf)
  best_thr <- NA_real_

  for (thr in THRESHOLDS) {
    yhat_raw <- pmax(as.numeric(phat[ok] > thr) * muhat[ok], 0)
    m_raw <- calc_metrics(panel$y[ok], yhat_raw,
                          nace2d = panel$nace2d[ok], year = panel$year[ok])
    if (!is.na(m_raw$rmse) && m_raw$rmse < best_raw$rmse) {
      best_raw <- m_raw
      best_thr <- thr
    }
  }

  # Step 2: calibrate at chosen threshold, compute final metrics
  yhat_raw <- pmax(as.numeric(phat[ok] > best_thr) * muhat[ok], 0)
  yhat_cap <- calibrate_with_cap(
    yhat_raw, panel$emit[ok], panel$y[ok],
    panel$nace2d[ok], panel$year[ok], syt
  )
  best <- calc_metrics(panel$y[ok], yhat_cap,
                       nace2d = panel$nace2d[ok], year = panel$year[ok])

  cat(sprintf("  %s: thr=%.2f  nRMSE=%.3f  Spearman=%.3f  rho_med=%.3f [%.3f, %.3f]\n",
              nm, best_thr, best$nrmse_sd, best$spearman,
              best$within_sy_rho_med, best$within_sy_rho_min, best$within_sy_rho_max))

  # Store per-cell rho detail from calibrated predictions
  rho_details[[nm]] <- best$within_sy_rho_detail

  # Store calibrated predictions for pooled rho analysis
  firm_preds[[nm]] <- data.frame(
    vat = panel$vat[ok], nace2d = panel$nace2d[ok], year = panel$year[ok],
    y = panel$y[ok], yhat_clip = yhat_cap, stringsAsFactors = FALSE
  )
}


# ── Pooled within-sector rho (demeaned by sector-year, pooled across years) ──
pooled_within_sector_rho <- function(preds) {
  preds %>%
    group_by(nace2d, year) %>%
    mutate(
      y_dm    = y - mean(y),
      yhat_dm = yhat_clip - mean(yhat_clip)
    ) %>%
    ungroup() %>%
    group_by(nace2d) %>%
    summarise(
      n_firmyears = n(),
      n_firms     = n_distinct(vat),
      n_years     = n_distinct(year),
      rho_pooled  = suppressWarnings(
        cor(y_dm, yhat_dm, method = "spearman", use = "complete.obs")
      ),
      .groups = "drop"
    ) %>%
    arrange(nace2d)
}


cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("Pooled within-sector rho: proxy_weighted vs proxy_pooled\n")
cat("(demeaned by sector-year, Spearman rho pooled across years)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

rho_pooled  <- pooled_within_sector_rho(firm_preds[["pooled_ind_base"]])
rho_weighted <- pooled_within_sector_rho(firm_preds[["weighted_ind_base"]])

comp <- merge(rho_pooled, rho_weighted, by = "nace2d", suffixes = c("_pooled", "_weighted"))

cat(sprintf("%-6s  %5s  %5s  %10s  %10s  %10s\n",
            "NACE", "Firms", "Years", "Pooled", "Weighted", "Delta"))
cat(strrep("-", 55), "\n")
for (i in seq_len(nrow(comp))) {
  r <- comp[i, ]
  cat(sprintf("%-6s  %5d  %5d  %10.3f  %10.3f  %+10.3f\n",
              r$nace2d, r$n_firms_pooled, r$n_years_pooled,
              r$rho_pooled_pooled, r$rho_pooled_weighted,
              r$rho_pooled_weighted - r$rho_pooled_pooled))
}

overall_pooled  <- suppressWarnings(cor(
  firm_preds[["pooled_ind_base"]] %>%
    group_by(nace2d, year) %>% mutate(y_dm = y - mean(y), yhat_dm = yhat_clip - mean(yhat_clip)) %>%
    ungroup() %>% pull(y_dm),
  firm_preds[["pooled_ind_base"]] %>%
    group_by(nace2d, year) %>% mutate(y_dm = y - mean(y), yhat_dm = yhat_clip - mean(yhat_clip)) %>%
    ungroup() %>% pull(yhat_dm),
  method = "spearman", use = "complete.obs"
))
overall_weighted <- suppressWarnings(cor(
  firm_preds[["weighted_ind_base"]] %>%
    group_by(nace2d, year) %>% mutate(y_dm = y - mean(y), yhat_dm = yhat_clip - mean(yhat_clip)) %>%
    ungroup() %>% pull(y_dm),
  firm_preds[["weighted_ind_base"]] %>%
    group_by(nace2d, year) %>% mutate(y_dm = y - mean(y), yhat_dm = yhat_clip - mean(yhat_clip)) %>%
    ungroup() %>% pull(yhat_dm),
  method = "spearman", use = "complete.obs"
))

cat(strrep("-", 55), "\n")
cat(sprintf("%-6s  %5s  %5s  %10.3f  %10.3f  %+10.3f\n",
            "ALL", "", "", overall_pooled, overall_weighted,
            overall_weighted - overall_pooled))

# Summary stats
n_wins  <- sum(comp$rho_pooled_weighted > comp$rho_pooled_pooled, na.rm = TRUE)
n_loses <- sum(comp$rho_pooled_weighted < comp$rho_pooled_pooled, na.rm = TRUE)
n_ties  <- sum(comp$rho_pooled_weighted == comp$rho_pooled_pooled, na.rm = TRUE)
cat(sprintf("\nSectors: %d | Weighted wins: %d | Pooled wins: %d | Ties: %d\n",
            nrow(comp), n_wins, n_loses, n_ties))

# Save pooled rho comparison (needed by rho_star_test.R)
pooled_rho_fn <- file.path(OUTPUT_DIR, "rho_pooled_sector_comparison.csv")
write.csv(comp, pooled_rho_fn, row.names = FALSE)
cat(sprintf("  Saved: %s\n", pooled_rho_fn))

cat("\n══════════════════════════════════════════════════════════════\n")


# ── Per-(sector, year) cell rho comparison ──────────────────────────────────
cat("Per-(sector, year) cell rho comparison\n")
cat("(from calc_metrics: Spearman rho within each sector-year cell, >=5 firms)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

rd_pooled   <- rho_details[["pooled_ind_base"]]
rd_weighted <- rho_details[["weighted_ind_base"]]

# Merge on (nace2d, year) — both should have same cells since same panel
rho_comp <- merge(rd_pooled, rd_weighted, by = c("nace2d", "year"),
                  suffixes = c("_pooled", "_weighted"))
rho_comp$delta <- rho_comp$rho_weighted - rho_comp$rho_pooled

# ── Aggregate by sector: median rho across years ──
sec_summary <- rho_comp %>%
  group_by(nace2d) %>%
  summarise(
    n_cells      = n(),
    n_firms_med  = round(median(n_firms_pooled)),
    rho_pooled   = median(rho_pooled, na.rm = TRUE),
    rho_weighted = median(rho_weighted, na.rm = TRUE),
    delta_med    = median(delta, na.rm = TRUE),
    pooled_min   = min(rho_pooled, na.rm = TRUE),
    weighted_min = min(rho_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(rho_pooled)

cat(sprintf("%-6s  %5s  %5s  %10s  %10s  %10s  %10s  %10s\n",
            "NACE", "Cells", "Firms", "Pooled", "Weighted", "Delta",
            "Pool_min", "Wt_min"))
cat(strrep("-", 78), "\n")
for (i in seq_len(nrow(sec_summary))) {
  r <- sec_summary[i, ]
  cat(sprintf("%-6s  %5d  %5d  %10.3f  %10.3f  %+10.3f  %10.3f  %10.3f\n",
              r$nace2d, r$n_cells, r$n_firms_med,
              r$rho_pooled, r$rho_weighted, r$delta_med,
              r$pooled_min, r$weighted_min))
}

# ── Identify sectors with negative rho ──
cat("\n\n── Sectors with negative median rho ──\n\n")
neg_pooled   <- sec_summary$nace2d[sec_summary$rho_pooled < 0]
neg_weighted <- sec_summary$nace2d[sec_summary$rho_weighted < 0]

cat("Negative rho (pooled proxy):  ", if (length(neg_pooled) > 0) paste(neg_pooled, collapse = ", ") else "none", "\n")
cat("Negative rho (weighted proxy):", if (length(neg_weighted) > 0) paste(neg_weighted, collapse = ", ") else "none", "\n")

both_neg   <- intersect(neg_pooled, neg_weighted)
only_pooled <- setdiff(neg_pooled, neg_weighted)
only_weighted <- setdiff(neg_weighted, neg_pooled)
cat("\nBad in BOTH:        ", if (length(both_neg) > 0) paste(both_neg, collapse = ", ") else "none", "\n")
cat("Bad ONLY in pooled: ", if (length(only_pooled) > 0) paste(only_pooled, collapse = ", ") else "none", "\n")
cat("Bad ONLY in weighted:", if (length(only_weighted) > 0) paste(only_weighted, collapse = ", ") else "none", "\n")

# ── Correlation between sector-level pooled and weighted rho ──
sec_rho_corr <- suppressWarnings(
  cor(sec_summary$rho_pooled, sec_summary$rho_weighted,
      method = "spearman", use = "complete.obs")
)
cat(sprintf("\nCross-sector Spearman correlation (pooled vs weighted median rho): %.3f\n", sec_rho_corr))

n_wins  <- sum(sec_summary$delta_med > 0, na.rm = TRUE)
n_loses <- sum(sec_summary$delta_med < 0, na.rm = TRUE)
n_ties  <- sum(sec_summary$delta_med == 0, na.rm = TRUE)
cat(sprintf("Sectors: %d | Weighted wins: %d | Pooled wins: %d | Ties: %d\n",
            nrow(sec_summary), n_wins, n_loses, n_ties))


# ── Save rho detail CSVs ──
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
for (nm in names(rho_details)) {
  fn <- file.path(OUTPUT_DIR, paste0("within_sy_rho_hurdle_", nm, "_cal_clip.csv"))
  write.csv(rho_details[[nm]], fn, row.names = FALSE)
  cat(sprintf("  Saved: %s\n", fn))
}

# Save the merged comparison
comp_fn <- file.path(OUTPUT_DIR, "rho_detail_pooled_vs_weighted.csv")
write.csv(rho_comp, comp_fn, row.names = FALSE)
cat(sprintf("  Saved: %s\n", comp_fn))

sec_fn <- file.path(OUTPUT_DIR, "rho_sector_summary_pooled_vs_weighted.csv")
write.csv(sec_summary, sec_fn, row.names = FALSE)
cat(sprintf("  Saved: %s\n", sec_fn))

# ── Decompose bad-sector rho: cross-firm vs within-firm ─────────────────────
# For sectors negative in both proxies, check whether the negative correlation
# comes from ranking firms wrong within a year, or ranking years wrong within
# a firm.
cat("\n══════════════════════════════════════════════════════════════\n")
cat("Rho decomposition for consistently negative sectors\n")
cat("══════════════════════════════════════════════════════════════\n\n")

bad_sectors <- c("08", "30", "33", "49", "71")

for (proxy_nm in c("pooled_ind_base", "weighted_ind_base")) {
  fp <- firm_preds[[proxy_nm]]

  cat(sprintf("── %s ──\n\n", proxy_nm))
  cat(sprintf("%-6s  %5s  %5s  %10s  %10s  %10s  %10s  %10s\n",
              "NACE", "Firms", "Years", "Pooled",
              "XFirm_med", "XFirm_frac+",
              "XYear_med", "XYear_frac+"))
  cat(strrep("-", 75), "\n")

  for (sec in bad_sectors) {
    d <- fp[fp$nace2d == sec, ]
    if (nrow(d) == 0) next

    # Demean by sector-year
    d <- d %>%
      group_by(year) %>%
      mutate(y_dm = y - mean(y), yhat_dm = yhat_clip - mean(yhat_clip)) %>%
      ungroup()

    n_firms <- n_distinct(d$vat)
    n_years <- n_distinct(d$year)

    # Pooled rho (should match the main table)
    rho_pooled <- suppressWarnings(
      cor(d$y_dm, d$yhat_dm, method = "spearman", use = "complete.obs")
    )

    # 1. Cross-firm within year: for each year, Spearman rho across firms
    xfirm <- d %>%
      group_by(year) %>%
      summarise(
        rho = suppressWarnings(cor(y_dm, yhat_dm, method = "spearman", use = "complete.obs")),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(rho))

    xfirm_med  <- if (nrow(xfirm) > 0) median(xfirm$rho) else NA
    xfirm_frac <- if (nrow(xfirm) > 0) mean(xfirm$rho > 0) else NA

    # 2. Within-firm across years: for each firm, Spearman rho across years
    xyear <- d %>%
      group_by(vat) %>%
      summarise(
        rho = suppressWarnings(cor(y_dm, yhat_dm, method = "spearman", use = "complete.obs")),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(rho))

    xyear_med  <- if (nrow(xyear) > 0) median(xyear$rho) else NA
    xyear_frac <- if (nrow(xyear) > 0) mean(xyear$rho > 0) else NA

    cat(sprintf("%-6s  %5d  %5d  %10.3f  %10.3f  %10.1f%%  %10.3f  %10.1f%%\n",
                sec, n_firms, n_years,
                ifelse(is.na(rho_pooled), NA, rho_pooled),
                ifelse(is.na(xfirm_med), NA, xfirm_med),
                ifelse(is.na(xfirm_frac), NA, 100 * xfirm_frac),
                ifelse(is.na(xyear_med), NA, xyear_med),
                ifelse(is.na(xyear_frac), NA, 100 * xyear_frac)))

    # Print per-firm detail for small sectors
    if (n_firms <= 6 && nrow(xyear) > 0) {
      for (j in seq_len(nrow(xyear))) {
        cat(sprintf("         firm %d: across-year rho = %+.3f (%d years)\n",
                    j, xyear$rho[j], xyear$n[j]))
      }
    }
    cat("\n")
  }
}

cat("══════════════════════════════════════════════════════════════\n")
cat("Done.\n")
