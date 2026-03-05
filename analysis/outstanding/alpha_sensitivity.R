###############################################################################
# analysis/nested_cv/alpha_sensitivity.R
#
# PURPOSE
#   Robustness check: is the fuel-supply proxy sensitive to the elastic net
#   mixing parameter alpha?
#
#   For each alpha in {0.1, 0.25, 0.5, 0.75, 0.9, 1.0}, re-runs the nested
#   CV pipeline (identical to build_fold_specific_proxy.R) and evaluates
#   downstream prediction performance using the same Row 3/4/5 specifications
#   from models_with_fold_specific_proxy.R.
#
# CROSS-FITTING
#   Each alpha produces a separate fold-specific proxy via the K=5 sector-fold
#   nested CV. Alpha is NOT selected from the data — it is swept for
#   robustness. Each proxy is then evaluated using the same calibration and
#   hurdle pipeline.
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/nested_cv/alpha_sensitivity.csv       (metrics by alpha)
#   {OUTPUT_DIR}/nested_cv/alpha_sensitivity_diag.csv  (EN diagnostics by fold × alpha)
#
# RUNS ON: RMD (requires full B2B data)
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
  REPO_DIR <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(Matrix)
library(glmnet)

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))


# ── Parameters ───────────────────────────────────────────────────────────────
ALPHAS         <- c(0.1, 0.25, 0.5, 0.75, 0.9, 1.0)
K_OUTER        <- 5L
K_INNER        <- 10L
MIN_LHS_BUYERS <- 5L
SEED           <- 42L


# =============================================================================
# STEP 1: Load data and prepare LHS panel
# (identical to build_fold_specific_proxy.R steps 1–7)
# =============================================================================
cat("Loading B2B selected sample...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading LOOCV training sample...\n")
load(file.path(PROC_DATA, "loocv_training_sample.RData"))

lhs <- loocv_training_sample %>%
  filter(year >= 2005) %>%
  mutate(
    y = emissions,
    log_revenue = log(pmax(revenue, 1e-12))
  ) %>%
  select(vat, year, y, log_revenue, nace2d, euets) %>%
  arrange(vat, year)
rm(loocv_training_sample)

lhs$emit <- as.integer(lhs$y > 0)

cat("LHS panel:", nrow(lhs), "firm-years,",
    length(unique(lhs$vat)), "unique firms\n\n")


# ── Primary sector assignment ────────────────────────────────────────────────
firm_sector <- lhs %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")
lhs <- lhs %>% left_join(firm_sector, by = "vat")


# ── Sector fold assignment (identical to build_fold_specific_proxy.R) ────────
sector_emitter_fy <- lhs %>%
  filter(emit == 1) %>%
  count(primary_nace2d, name = "n_emitter_fy") %>%
  arrange(desc(n_emitter_fy))

all_sectors <- sort(unique(lhs$primary_nace2d))
sector_emitter_fy <- data.frame(nace2d = all_sectors, stringsAsFactors = FALSE) %>%
  left_join(sector_emitter_fy, by = c("nace2d" = "primary_nace2d")) %>%
  mutate(n_emitter_fy = coalesce(n_emitter_fy, 0L)) %>%
  arrange(desc(n_emitter_fy))

n_sec <- nrow(sector_emitter_fy)
fold_assignment <- integer(n_sec)
for (i in seq_len(n_sec)) {
  cycle_pos <- ((i - 1) %% (2 * K_OUTER))
  if (cycle_pos < K_OUTER) {
    fold_assignment[i] <- cycle_pos + 1L
  } else {
    fold_assignment[i] <- 2L * K_OUTER - cycle_pos
  }
}

sector_fold_map <- data.frame(
  nace2d = sector_emitter_fy$nace2d,
  fold_k = fold_assignment,
  n_emitter_fy = sector_emitter_fy$n_emitter_fy,
  stringsAsFactors = FALSE
)

# Hard constraint: NACE 19 and 24 in different folds
fold_19 <- sector_fold_map$fold_k[sector_fold_map$nace2d == "19"]
fold_24 <- sector_fold_map$fold_k[sector_fold_map$nace2d == "24"]
if (length(fold_19) > 0 && length(fold_24) > 0 && fold_19 == fold_24) {
  fy_24 <- sector_fold_map$n_emitter_fy[sector_fold_map$nace2d == "24"]
  candidates <- sector_fold_map %>%
    filter(fold_k != fold_19, nace2d != "19") %>%
    mutate(fy_diff = abs(n_emitter_fy - fy_24)) %>%
    arrange(fy_diff)
  swap_nace <- candidates$nace2d[1]
  swap_fold <- candidates$fold_k[1]
  sector_fold_map$fold_k[sector_fold_map$nace2d == "24"]      <- swap_fold
  sector_fold_map$fold_k[sector_fold_map$nace2d == swap_nace] <- fold_19
  cat("Swapped NACE 24 with", swap_nace, "to enforce separation from NACE 19\n")
}

lhs <- lhs %>%
  left_join(sector_fold_map %>% select(nace2d, fold_k),
            by = c("primary_nace2d" = "nace2d"))

# ── Sector-year totals ──────────────────────────────────────────────────────
syt <- lhs %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")

# ── B2B for LHS buyers ──────────────────────────────────────────────────────
b2b_lhs <- b2b %>% filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)

# ── Inner fold assignment ────────────────────────────────────────────────────
set.seed(SEED)
all_unique_firms <- unique(lhs$vat)
inner_fold_map <- sample(rep(1:K_INNER, length.out = length(all_unique_firms)))
names(inner_fold_map) <- all_unique_firms

# ── Helpers ──────────────────────────────────────────────────────────────────
extract_suppliers <- function(fit, n_ctrl, eligible_sellers_k, s = "lambda.min") {
  co <- coef(fit, s = s)
  supplier_idx <- (n_ctrl + 2):length(co)
  vals <- co[supplier_idx]
  data.frame(
    vat_i_ano = eligible_sellers_k,
    coef      = as.numeric(vals),
    stringsAsFactors = FALSE
  ) %>%
    filter(coef != 0) %>%
    arrange(desc(abs(coef)))
}

learn_percentile_threshold <- function(proxy_vals, emit_vals) {
  pctile_ranks <- ecdf(proxy_vals)(proxy_vals)
  cutoffs <- seq(0.01, 0.99, by = 0.01)
  best_youden <- -Inf
  best_p <- NA_real_
  n_emit <- sum(emit_vals == 1)
  n_nonemit <- sum(emit_vals == 0)
  for (p in cutoffs) {
    pred_emit <- as.integer(pctile_ranks >= p)
    tpr <- if (n_emit > 0) sum(pred_emit == 1 & emit_vals == 1) / n_emit else NA_real_
    fpr <- if (n_nonemit > 0) sum(pred_emit == 1 & emit_vals == 0) / n_nonemit else NA_real_
    youden <- tpr - fpr
    if (!is.na(youden) && youden > best_youden) {
      best_youden <- youden
      best_p <- p
    }
  }
  best_p
}

print_metrics <- function(label, m) {
  cat(sprintf("  %s: nRMSE=%.3f  Med_APD=%.3f  rho_g=%.3f  rho_s=%.3f  FPR=%.3f  TPR=%.3f\n",
              label, m$nrmse_sd, m$median_apd, m$rho_pooled_global,
              m$rho_pooled, m$fpr_nonemitters, m$tpr_emitters))
}


# =============================================================================
# MAIN LOOP: sweep over alpha values
# =============================================================================
all_results <- data.frame()
all_diag    <- data.frame()

for (alpha in ALPHAS) {
  cat("\n\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  ALPHA = %.2f\n", alpha))
  cat("═══════════════════════════════════════════════════════════════\n\n")

  t0_alpha <- Sys.time()

  # Storage for this alpha's fold-specific proxies
  proxy_pieces <- list()
  diag_rows    <- list()

  for (k in seq_len(K_OUTER)) {
    cat(sprintf("  Fold %d / %d ...", k, K_OUTER))
    t0_fold <- Sys.time()

    held_out_sectors <- sector_fold_map$nace2d[sector_fold_map$fold_k == k]
    train_lhs <- lhs[lhs$fold_k != k, ]
    test_lhs  <- lhs[lhs$fold_k == k, ]

    # ── Eligible sellers ──
    b2b_train_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% train_lhs$vat, ]
    seller_counts_k <- b2b_train_k %>%
      distinct(vat_i_ano, vat_j_ano) %>%
      count(vat_i_ano, name = "n_lhs_buyers")
    eligible_sellers_k <- seller_counts_k %>%
      filter(n_lhs_buyers >= MIN_LHS_BUYERS) %>%
      pull(vat_i_ano) %>%
      sort()
    seller_map_k <- data.frame(
      vat_i_ano = eligible_sellers_k,
      col_idx   = seq_along(eligible_sellers_k),
      stringsAsFactors = FALSE
    )

    # ── Design matrix ──
    train_lhs$row_idx <- seq_len(nrow(train_lhs))

    b2b_agg_k <- b2b_train_k %>%
      filter(vat_i_ano %in% eligible_sellers_k) %>%
      group_by(vat_i_ano, vat_j_ano, year) %>%
      summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
      filter(sales != 0) %>%
      inner_join(train_lhs %>% select(vat, year, row_idx),
                 by = c("vat_j_ano" = "vat", "year" = "year")) %>%
      inner_join(seller_map_k, by = "vat_i_ano")

    n_rows_k <- nrow(train_lhs)
    n_cols_k <- length(eligible_sellers_k)

    X_asinh_k <- sparseMatrix(
      i = b2b_agg_k$row_idx, j = b2b_agg_k$col_idx,
      x = asinh(b2b_agg_k$sales), dims = c(n_rows_k, n_cols_k)
    )
    colnames(X_asinh_k) <- eligible_sellers_k

    rm(b2b_agg_k, b2b_train_k)

    # Controls
    year_dummies_k <- model.matrix(~ factor(year), data = train_lhs)[, -1, drop = FALSE]
    train_sectors <- sort(unique(train_lhs$nace2d))
    sector_dummies_k <- model.matrix(~ factor(nace2d, levels = train_sectors),
                                     data = train_lhs)[, -1, drop = FALSE]
    X_controls_k <- cbind(log_revenue = train_lhs$log_revenue,
                           year_dummies_k, sector_dummies_k)
    n_controls_k <- ncol(X_controls_k)

    X_full_k <- cbind(Matrix(X_controls_k, sparse = TRUE), X_asinh_k)
    pf_k <- c(rep(0, n_controls_k), rep(1, n_cols_k))

    # Drop zero-variance columns
    col_means <- colMeans(X_full_k)
    col_vars <- colMeans(X_full_k^2) - col_means^2
    zero_var <- which(col_vars == 0)
    if (length(zero_var) > 0) {
      X_full_k <- X_full_k[, -zero_var]
      pf_k <- pf_k[-zero_var]
      n_controls_k <- sum(pf_k == 0)
      eligible_sellers_k_enet <- colnames(X_full_k)[which(pf_k == 1)]
    } else {
      eligible_sellers_k_enet <- eligible_sellers_k
    }

    rm(X_asinh_k, X_controls_k, year_dummies_k, sector_dummies_k)

    # ── cv.glmnet ──
    inner_foldid_k <- unname(inner_fold_map[train_lhs$vat])

    fit_k <- cv.glmnet(
      x = X_full_k, y = train_lhs$y,
      family = "gaussian", alpha = alpha,
      penalty.factor = pf_k, foldid = inner_foldid_k,
      standardize = TRUE
    )

    lambda_min_k <- fit_k$lambda.min
    cv_rmse_k <- sqrt(min(fit_k$cvm))

    rm(X_full_k)

    # ── Extract suppliers ──
    coef_pos_k <- extract_suppliers(fit_k, n_controls_k,
                                     eligible_sellers_k_enet, "lambda.min") %>%
      filter(coef > 0)
    n_selected_k <- nrow(coef_pos_k)

    rm(fit_k)

    # ── Build proxy for held-out firms ──
    heldout_vats_k <- unique(test_lhs$vat)

    if (n_selected_k > 0) {
      b2b_heldout_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]
      proxy_k <- b2b_heldout_k %>%
        inner_join(coef_pos_k, by = "vat_i_ano") %>%
        group_by(vat_j_ano, year) %>%
        summarise(fold_specific_proxy = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                  .groups = "drop") %>%
        rename(vat = vat_j_ano)
      rm(b2b_heldout_k)
    } else {
      proxy_k <- data.frame(vat = character(0), year = integer(0),
                             fold_specific_proxy = numeric(0),
                             stringsAsFactors = FALSE)
    }

    test_proxy_k <- test_lhs %>%
      select(vat, year) %>%
      left_join(proxy_k, by = c("vat", "year")) %>%
      mutate(fold_specific_proxy = coalesce(fold_specific_proxy, 0))

    proxy_pieces[[k]] <- test_proxy_k

    fold_time <- round(difftime(Sys.time(), t0_fold, units = "mins"), 1)
    cat(sprintf(" %d suppliers, lambda=%.4f, cv_rmse=%.0f, %.1f min\n",
                n_selected_k, lambda_min_k, cv_rmse_k, fold_time))

    diag_rows[[k]] <- data.frame(
      alpha = alpha, fold_k = k,
      n_train = nrow(train_lhs), n_test = nrow(test_lhs),
      n_eligible_sellers = length(eligible_sellers_k),
      n_selected_pos = n_selected_k,
      lambda_min = lambda_min_k, cv_rmse = cv_rmse_k,
      runtime_min = as.numeric(fold_time),
      stringsAsFactors = FALSE
    )

    rm(train_lhs, test_lhs, coef_pos_k, proxy_k, test_proxy_k,
       eligible_sellers_k, eligible_sellers_k_enet,
       seller_map_k, seller_counts_k, pf_k, inner_foldid_k, heldout_vats_k)
    gc()
  }

  # ── Assemble proxy for this alpha ──
  all_proxies <- bind_rows(proxy_pieces)
  panel <- lhs %>%
    left_join(all_proxies, by = c("vat", "year")) %>%
    mutate(fold_specific_proxy = coalesce(fold_specific_proxy, 0))

  # ── Evaluate Rows 3, 4, 5 ──

  # Row 3: proxy-proportional, no hurdle
  yhat_r3 <- calibrate_predictions(panel$fold_specific_proxy,
                                    panel$nace2d, panel$year, syt)
  m3 <- calc_metrics(panel$y, yhat_r3, nace2d = panel$nace2d, year = panel$year)

  # Row 4: proxy + hurdle (cross-sector from 19/24)
  sec19_idx <- which(panel$nace2d == "19")
  sec24_idx <- which(panel$nace2d == "24")
  tau_24 <- learn_percentile_threshold(panel$fold_specific_proxy[sec24_idx], panel$emit[sec24_idx])
  tau_19 <- learn_percentile_threshold(panel$fold_specific_proxy[sec19_idx], panel$emit[sec19_idx])

  panel$thresholded_proxy <- panel$fold_specific_proxy
  if (length(sec19_idx) > 0) {
    pctile_19 <- ecdf(panel$fold_specific_proxy[sec19_idx])(panel$fold_specific_proxy[sec19_idx])
    panel$thresholded_proxy[sec19_idx] <- ifelse(pctile_19 >= tau_24, panel$fold_specific_proxy[sec19_idx], 0)
  }
  if (length(sec24_idx) > 0) {
    pctile_24 <- ecdf(panel$fold_specific_proxy[sec24_idx])(panel$fold_specific_proxy[sec24_idx])
    panel$thresholded_proxy[sec24_idx] <- ifelse(pctile_24 >= tau_19, panel$fold_specific_proxy[sec24_idx], 0)
  }

  yhat_r4 <- calibrate_predictions(panel$thresholded_proxy,
                                    panel$nace2d, panel$year, syt)
  m4 <- calc_metrics(panel$y, yhat_r4, nace2d = panel$nace2d, year = panel$year)

  # Row 5: hurdle + cap
  yhat_r5 <- calibrate_with_cap(panel$thresholded_proxy, panel$emit, panel$y,
                                 panel$nace2d, panel$year, syt)
  m5 <- calc_metrics(panel$y, yhat_r5, nace2d = panel$nace2d, year = panel$year)

  cat(sprintf("\n  Alpha = %.2f results:\n", alpha))
  print_metrics("Row 3", m3)
  print_metrics("Row 4", m4)
  print_metrics("Row 5", m5)

  alpha_time <- round(difftime(Sys.time(), t0_alpha, units = "mins"), 1)
  cat(sprintf("  Total time for alpha=%.2f: %.1f min\n", alpha, alpha_time))

  # Store results
  for (row_info in list(
    list(row = "row3_no_hurdle", m = m3),
    list(row = "row4_hurdle", m = m4),
    list(row = "row5_hurdle_cap", m = m5)
  )) {
    all_results <- rbind(all_results, data.frame(
      alpha = alpha,
      row = row_info$row,
      nrmse_sd = row_info$m$nrmse_sd,
      median_apd = row_info$m$median_apd,
      rho_pooled_global = row_info$m$rho_pooled_global,
      rho_pooled = row_info$m$rho_pooled,
      fpr = row_info$m$fpr_nonemitters,
      tpr = row_info$m$tpr_emitters,
      stringsAsFactors = FALSE
    ))
  }

  all_diag <- rbind(all_diag, bind_rows(diag_rows))

  rm(panel, all_proxies, proxy_pieces, diag_rows)
  gc()
}


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ALPHA SENSITIVITY SUMMARY\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

for (r in unique(all_results$row)) {
  cat(sprintf("── %s ──\n", r))
  sub <- all_results[all_results$row == r, ]
  print(sub[, c("alpha", "nrmse_sd", "median_apd", "rho_pooled_global",
                "rho_pooled", "fpr", "tpr")], row.names = FALSE)
  best_rho <- sub[which.max(sub$rho_pooled), ]
  best_nrmse <- sub[which.min(sub$nrmse_sd), ]
  cat(sprintf("  Best rho_s: alpha=%.2f (%.3f) | Best nRMSE: alpha=%.2f (%.3f)\n\n",
              best_rho$alpha, best_rho$rho_pooled, best_nrmse$alpha, best_nrmse$nrmse_sd))
}

cat("\n── EN diagnostics (suppliers selected per fold) ──\n")
diag_summary <- all_diag %>%
  group_by(alpha) %>%
  summarise(
    avg_n_selected = mean(n_selected_pos),
    min_n_selected = min(n_selected_pos),
    max_n_selected = max(n_selected_pos),
    avg_cv_rmse = mean(cv_rmse),
    total_runtime_min = sum(runtime_min),
    .groups = "drop"
  )
print(diag_summary, row.names = FALSE)


# ── Save ─────────────────────────────────────────────────────────────────────
out_dir <- file.path(OUTPUT_DIR, "nested_cv")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
write.csv(all_results, file.path(out_dir, "alpha_sensitivity.csv"), row.names = FALSE)
write.csv(all_diag, file.path(out_dir, "alpha_sensitivity_diag.csv"), row.names = FALSE)
cat("\nSaved to:", out_dir, "\n")
