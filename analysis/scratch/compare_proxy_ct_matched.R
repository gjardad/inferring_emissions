###############################################################################
# analysis/scratch/compare_proxy_ct_matched.R
#
# Compare 6 approaches on the CT-matched subsample:
#   1-5: proxy variants (firm-level, restricted to CT-matched firms)
#   6:   Climate TRACE direct estimates (installation-level)
#
# Metrics: nRMSE, Spearman rho, Median APD
#
# RUNS ON: local 1
###############################################################################

REPO_DIR <- "c:/Users/jota_/Documents/inferring_emissions"
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

cat("Loading fold-specific (sector-fold) proxy...\n")
load(file.path(PROC_DATA, "fold_specific_proxy.RData"))

cat("Loading firm-fold CV proxy...\n")
load(file.path(PROC_DATA, "firmfoldcv_proxy.RData"))

cat("Loading CT results...\n")
load(file.path(PROC_DATA, "enet_climate_trace_results.RData"))


# ── Build panel ──────────────────────────────────────────────────────────────
panel <- training_sample %>%
  left_join(fs_proxy_panel %>% select(vat, year, fold_specific_proxy),
            by = c("vat", "year")) %>%
  left_join(firmfoldcv_proxy_panel %>% select(vat, year, firmfoldcv_proxy),
            by = c("vat", "year")) %>%
  mutate(
    proxy_tabachova     = coalesce(proxy_tabachova, 0),
    fold_specific_proxy = coalesce(fold_specific_proxy, 0),
    firmfoldcv_proxy    = coalesce(firmfoldcv_proxy, 0),
    emit = as.integer(y > 0)
  )
rm(training_sample, fs_proxy_panel, firmfoldcv_proxy_panel)

# ── Restrict to exact CT firm-years ──────────────────────────────────────────
# Use the same (vat, year) pairs that CT covers for an apples-to-apples comparison
ct_fy <- ct_train_panel %>%
  filter(is.finite(ct_emissions)) %>%
  select(vat, year, ct_emissions)

panel_ct <- panel %>%
  inner_join(ct_fy, by = c("vat", "year"))

cat("\nFull panel:", nrow(panel), "firm-years\n")
cat("CT-matched firm-years:", nrow(panel_ct), "\n")
cat("CT-matched firms:", n_distinct(panel_ct$vat), "\n")
cat("  All emitters?", all(panel_ct$emit == 1), "\n\n")


# ── National calibration (on full panel, then subset) ────────────────────────
calibrate_national <- function(yhat, year, panel_y) {
  df <- data.frame(yhat = yhat, year = year, idx = seq_along(yhat))
  nat <- aggregate(panel_y, by = list(year = year), FUN = sum, na.rm = TRUE)
  names(nat)[2] <- "E_national"
  nat$denom <- aggregate(yhat, by = list(year = year), FUN = sum, na.rm = TRUE)[[2]]
  df <- merge(df, nat, by = "year", all.x = TRUE)
  df <- df[order(df$idx), ]
  yhat_cal <- rep(NA_real_, nrow(df))
  idx_pos <- !is.na(df$E_national) & df$E_national > 0 & df$denom > 0
  yhat_cal[idx_pos] <- df$E_national[idx_pos] * (df$yhat[idx_pos] / df$denom[idx_pos])
  idx0 <- !is.na(df$E_national) & df$E_national == 0
  yhat_cal[idx0] <- 0
  yhat_cal
}

# Calibrate on full panel, then subset to CT firm-years
panel$row_id <- seq_len(nrow(panel))
panel_ct$row_id <- match(
  paste(panel_ct$vat, panel_ct$year),
  paste(panel$vat, panel$year)
)

yhat_1_full <- calibrate_national(panel$proxy_tabachova, panel$year, panel$y)
yhat_3_full <- calibrate_national(panel$firmfoldcv_proxy, panel$year, panel$y)
yhat_5_full <- calibrate_national(panel$fold_specific_proxy, panel$year, panel$y)

yhat_1 <- yhat_1_full[panel_ct$row_id]
yhat_2 <- panel_ct$firmfoldcv_proxy
yhat_3 <- yhat_3_full[panel_ct$row_id]
yhat_4 <- panel_ct$fold_specific_proxy
yhat_5 <- yhat_5_full[panel_ct$row_id]


# ── Compute metrics for rows 1-5 (firm-level, CT-matched firms) ─────────────
labels <- c(
  "Tabachova + nat calib",
  "Firm-fold EN (raw)",
  "Firm-fold EN + nat calib",
  "Sector-fold EN (raw)",
  "Sector-fold EN + nat calib"
)

yhats <- list(yhat_1, yhat_2, yhat_3, yhat_4, yhat_5)

results <- lapply(seq_along(yhats), function(i) {
  m <- calc_metrics(panel_ct$y, yhats[[i]], nace2d = panel_ct$nace2d, year = panel_ct$year)

  data.frame(
    model      = labels[i],
    n          = m$n,
    nrmse_sd   = m$nrmse_sd,
    spearman   = m$spearman,
    median_apd = m$median_apd,
    stringsAsFactors = FALSE
  )
})


# ── Row 6: Climate TRACE direct estimates ────────────────────────────────────
# Same firm-years as rows 1-5, using CT emissions as yhat
y_ct    <- panel_ct$y
yhat_ct <- panel_ct$ct_emissions

m_ct <- calc_metrics(y_ct, yhat_ct, nace2d = panel_ct$nace2d, year = panel_ct$year)

results[[6]] <- data.frame(
  model      = "Climate TRACE (direct)",
  n          = m_ct$n,
  nrmse_sd   = m_ct$nrmse_sd,
  spearman   = m_ct$spearman,
  median_apd = m_ct$median_apd,
  stringsAsFactors = FALSE
)


# ── Print ────────────────────────────────────────────────────────────────────
results_df <- bind_rows(results)

cat("\n══════════════ CT-MATCHED SAMPLE COMPARISON ══════════════\n\n")
print(results_df, row.names = FALSE)
cat("\n══════════════════════════════════════════════════════════\n")
