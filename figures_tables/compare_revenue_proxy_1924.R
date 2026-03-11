###############################################################################
# figures_tables/compare_revenue_proxy_1924.R
#
# PURPOSE
#   Compare revenue-based vs proxy-based emission predictions for sectors
#   19 (petroleum refining) and 24 (basic metals). These are the only
#   training-sample sectors with genuine extensive-margin variation:
#   they contain both EU ETS emitters and non-ETS firms with zero emissions.
#
#   Uses rank-and-calibrate: within each sector-year, rank firms by signal
#   (revenue or proxy), allocate the sector-year emission total proportionally
#   to signal among firms with signal > 0.
#
# INPUTS
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUTS
#   Console output (diagnostic)
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

# ── Load data ────────────────────────────────────────────────────────────────
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

df <- training_sample %>%
  rename(fold_specific_proxy = fold_specific_proxy_asinh) %>%
  filter(primary_nace2d %in% c("19", "24"))
syt_1924 <- syt %>% filter(nace2d %in% c("19", "24"))
rm(training_sample)

cat("Sectors 19/24:", nrow(df), "firm-years,", n_distinct(df$vat), "firms\n")
cat("  Emitters:", sum(df$emit), "firm-years\n")
cat("  Non-emitters:", sum(!df$emit), "firm-years\n\n")


# ── Rank-and-calibrate ───────────────────────────────────────────────────────
predict_rc <- function(data, signal_col, total_df) {
  data %>%
    group_by(primary_nace2d, year) %>%
    mutate(
      signal = .data[[signal_col]],
      pred_emit = as.integer(signal > 0),
      total_signal = sum(signal[pred_emit == 1]),
      signal_share = ifelse(pred_emit == 1 & total_signal > 0,
                            signal / total_signal, 0),
      signal_share = ifelse(is.nan(signal_share), 0, signal_share)
    ) %>%
    left_join(total_df %>% select(nace2d, year, E_total),
              by = c("primary_nace2d" = "nace2d", "year")) %>%
    mutate(y_hat = signal_share * E_total) %>%
    ungroup()
}

df$revenue <- exp(df$log_revenue)

pred_rev   <- predict_rc(df, "revenue", syt_1924)
pred_proxy <- predict_rc(df, "fold_specific_proxy", syt_1924)


# ── Metrics ──────────────────────────────────────────────────────────────────
nrmse_sd <- function(y, y_hat) sqrt(mean((y - y_hat)^2)) / sd(y)
nrmse_mean <- function(y, y_hat) sqrt(mean((y - y_hat)^2)) / mean(y)

eval_metrics <- function(pred_df, label) {
  cat(sprintf("\n=== %s ===\n", label))

  # Extensive margin
  tp <- sum(pred_df$pred_emit == 1 & pred_df$emit == 1)
  fp <- sum(pred_df$pred_emit == 1 & pred_df$emit == 0)
  fn <- sum(pred_df$pred_emit == 0 & pred_df$emit == 1)
  tn <- sum(pred_df$pred_emit == 0 & pred_df$emit == 0)
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  cat(sprintf("  TPR: %.3f  FPR: %.3f  (TP=%d FP=%d FN=%d TN=%d)\n",
              tpr, fpr, tp, fp, fn, tn))

  # Overall nRMSE
  cat(sprintf("  nRMSE (all, /sd): %.3f\n", nrmse_sd(pred_df$y, pred_df$y_hat)))
  cat(sprintf("  nRMSE (all, /mean): %.3f\n", nrmse_mean(pred_df$y, pred_df$y_hat)))

  # Emitters only
  emitters <- pred_df %>% filter(emit == 1)
  if (nrow(emitters) > 5) {
    rho <- cor(emitters$y, emitters$y_hat, method = "spearman")
    cat(sprintf("  Emitters: nRMSE(/sd)=%.3f, nRMSE(/mean)=%.3f, Spearman=%.3f (N=%d)\n",
                nrmse_sd(emitters$y, emitters$y_hat),
                nrmse_mean(emitters$y, emitters$y_hat),
                rho, nrow(emitters)))
  }

  # By sector
  for (s in c("19", "24")) {
    sub <- pred_df %>% filter(primary_nace2d == s)
    tp_s <- sum(sub$pred_emit == 1 & sub$emit == 1)
    fp_s <- sum(sub$pred_emit == 1 & sub$emit == 0)
    fn_s <- sum(sub$pred_emit == 0 & sub$emit == 1)
    tn_s <- sum(sub$pred_emit == 0 & sub$emit == 0)
    tpr_s <- tp_s / max(tp_s + fn_s, 1)
    fpr_s <- fp_s / max(fp_s + tn_s, 1)

    emit_s <- sub %>% filter(emit == 1)
    rho_s <- if (nrow(emit_s) > 5) cor(emit_s$y, emit_s$y_hat, method = "spearman") else NA

    cat(sprintf("  Sector %s: nRMSE(/sd)=%.3f, TPR=%.3f, FPR=%.3f, Spearman=%.3f (emit=%d, non-emit=%d)\n",
                s, nrmse_sd(sub$y, sub$y_hat),
                tpr_s, fpr_s, ifelse(is.na(rho_s), 0, rho_s),
                tp_s + fn_s, fp_s + tn_s))
  }
}

eval_metrics(pred_rev, "REVENUE-BASED")
eval_metrics(pred_proxy, "PROXY-BASED")


# ── Distribution overlap ─────────────────────────────────────────────────────
cat("\n\n=== SIGNAL DISTRIBUTION BY EMITTER STATUS ===\n")
for (s in c("19", "24")) {
  sub <- df %>% filter(primary_nace2d == s)
  cat(sprintf("\nSector %s (emitters=%d, non-emitters=%d):\n",
              s, sum(sub$emit == 1), sum(sub$emit == 0)))

  med_rev_emit <- median(sub$revenue[sub$emit == 1])
  share_nonemit_above <- mean(sub$revenue[sub$emit == 0] >= med_rev_emit)
  cat(sprintf("  Revenue: median emitter = %.0f, share non-emitters above = %.3f\n",
              med_rev_emit, share_nonemit_above))

  med_proxy_emit <- median(sub$fold_specific_proxy[sub$emit == 1])
  proxy_nonemit <- sub$fold_specific_proxy[sub$emit == 0]
  share_nonemit_above_proxy <- mean(proxy_nonemit >= med_proxy_emit)
  share_nonemit_zero_proxy <- mean(proxy_nonemit == 0)
  cat(sprintf("  Proxy:   median emitter = %.1f, share non-emitters above = %.3f, share at 0 = %.3f\n",
              med_proxy_emit, share_nonemit_above_proxy, share_nonemit_zero_proxy))
}

cat("\nDone.\n")
