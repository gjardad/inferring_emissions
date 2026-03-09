###############################################################################
# analysis/scratch/compare_proxy_variants.R
#
# PURPOSE
#   Compare 5 ways of computing yhat from different proxy variants:
#     1. proxy_tabachova + proportional calibration
#     2. firmfoldcv_proxy (raw)
#     3. firmfoldcv_proxy + proportional calibration
#     4. fold_specific_proxy (sector-fold, raw)
#     5. fold_specific_proxy (sector-fold) + proportional calibration
#
#   Metrics: nRMSE, Spearman rho, within-sector rho (pooled, year-demeaned),
#            FPR, TPR
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
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

# Recreate syt if not loaded
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}

cat("Panel:", nrow(panel), "rows,", n_distinct(panel$vat), "firms\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n\n", 100 * mean(panel$emit)))


# ── National-level calibration helper ────────────────────────────────────────
# Proportional allocation using only national totals within each year:
#   yhat_cal = E_national_year * (yhat / sum(yhat in year))
calibrate_national <- function(yhat, year, panel_y) {
  df <- data.frame(yhat = yhat, year = year, idx = seq_along(yhat))
  nat <- aggregate(panel_y, by = list(year = year), FUN = sum, na.rm = TRUE)
  names(nat)[2] <- "E_national"
  nat$denom <- aggregate(yhat, by = list(year = year), FUN = sum, na.rm = TRUE)[[2]]

  df <- merge(df, nat, by = "year", all.x = TRUE)
  df <- df[order(df$idx), ]

  yhat_cal <- rep(NA_real_, nrow(df))

  # E_national > 0 and denom > 0: proportional
  idx_pos <- !is.na(df$E_national) & df$E_national > 0 & df$denom > 0
  yhat_cal[idx_pos] <- df$E_national[idx_pos] * (df$yhat[idx_pos] / df$denom[idx_pos])

  # E_national == 0: zero
  idx0 <- !is.na(df$E_national) & df$E_national == 0
  yhat_cal[idx0] <- 0

  # Fallback: E_national > 0 but denom == 0
  idx_fb <- !is.na(df$E_national) & df$E_national > 0 & (df$denom == 0 | is.na(df$denom))
  if (any(idx_fb)) {
    n_yr <- table(df$year[idx_fb])
    for (yr in names(n_yr)) {
      ii <- which(idx_fb & df$year == yr)
      yhat_cal[ii] <- df$E_national[ii[1]] / length(ii)
    }
  }

  yhat_cal
}


# ── Compute yhat for each variant ────────────────────────────────────────────

# 1. Tabachova + national proportional calibration
yhat_1 <- calibrate_national(panel$proxy_tabachova, panel$year, panel$y)

# 2. Firm-fold EN proxy (raw)
yhat_2 <- panel$firmfoldcv_proxy

# 3. Firm-fold EN proxy + national proportional calibration
yhat_3 <- calibrate_national(panel$firmfoldcv_proxy, panel$year, panel$y)

# 4. Sector-fold EN proxy (raw)
yhat_4 <- panel$fold_specific_proxy

# 5. Sector-fold EN proxy + national proportional calibration
yhat_5 <- calibrate_national(panel$fold_specific_proxy, panel$year, panel$y)


# ── Compute metrics ─────────────────────────────────────────────────────────
labels <- c(
  "Tabachova + calib",
  "Firm-fold EN (raw)",
  "Firm-fold EN + calib",
  "Sector-fold EN (raw)",
  "Sector-fold EN + calib"
)

yhats <- list(yhat_1, yhat_2, yhat_3, yhat_4, yhat_5)

# Helper: compute per-sector pooled rho (demeaned by sector-year) and return
# the full distribution, not just median/min/max
compute_sector_rhos <- function(y, yhat, nace2d, year) {
  all_secs <- sort(unique(nace2d))
  all_yrs  <- sort(unique(year))
  y_dm    <- numeric(length(y))
  yhat_dm <- numeric(length(yhat))

  for (sec in all_secs) {
    for (yr in all_yrs) {
      idx <- which(nace2d == sec & year == yr)
      if (length(idx) == 0) next
      y_dm[idx]    <- y[idx] - mean(y[idx])
      yhat_dm[idx] <- yhat[idx] - mean(yhat[idx])
    }
  }

  sector_rhos <- c()
  sector_names <- c()
  for (sec in all_secs) {
    idx <- which(nace2d == sec)
    if (length(idx) < 5) next
    rho_sec <- suppressWarnings(
      cor(y_dm[idx], yhat_dm[idx], method = "spearman", use = "complete.obs")
    )
    if (is.finite(rho_sec)) {
      sector_rhos <- c(sector_rhos, rho_sec)
      sector_names <- c(sector_names, sec)
    }
  }
  data.frame(nace2d = sector_names, rho = sector_rhos, stringsAsFactors = FALSE)
}

results <- lapply(seq_along(yhats), function(i) {
  m <- calc_metrics(panel$y, yhats[[i]], nace2d = panel$nace2d, year = panel$year)
  sr <- compute_sector_rhos(panel$y, yhats[[i]], panel$nace2d, panel$year)
  q <- quantile(sr$rho, probs = c(0.25, 0.75), na.rm = TRUE)
  data.frame(
    model              = labels[i],
    rho_pooled_global  = m$rho_pooled_global,
    rho_pooled_median  = m$rho_pooled,
    rho_pooled_q25     = unname(q[1]),
    rho_pooled_q75     = unname(q[2]),
    rho_pooled_min     = m$rho_pooled_min,
    rho_pooled_max     = m$rho_pooled_max,
    n_sectors          = nrow(sr),
    stringsAsFactors   = FALSE
  )
})

results_df <- bind_rows(results)

cat("\n══════════════ WITHIN-SECTOR RHO DISTRIBUTION ══════════════\n\n")
print(results_df, row.names = FALSE)
cat("\n═══════════════════════════════════════════════════════════\n")

# ── Per-sector rho comparison (raw proxies only, no calibration) ─────────────
sr_tab  <- compute_sector_rhos(panel$y, yhat_1, panel$nace2d, panel$year)
sr_ff   <- compute_sector_rhos(panel$y, yhat_2, panel$nace2d, panel$year)
sr_sf   <- compute_sector_rhos(panel$y, yhat_4, panel$nace2d, panel$year)

sector_compare <- sr_tab %>%
  rename(rho_tabachova = rho) %>%
  full_join(sr_ff %>% rename(rho_firmfold = rho), by = "nace2d") %>%
  full_join(sr_sf %>% rename(rho_sectorfold = rho), by = "nace2d") %>%
  arrange(nace2d)

# Add sector firm counts
sector_n <- panel %>%
  group_by(nace2d) %>%
  summarise(n_fy = n(), n_emitters = sum(emit), .groups = "drop")
sector_compare <- sector_compare %>%
  left_join(sector_n, by = "nace2d")

cat("\n══════════════ PER-SECTOR RHO COMPARISON ══════════════\n\n")
print(sector_compare, row.names = FALSE, digits = 3)

cat("\n── Sectors with rho < 0.3 ──\n")
low_tab <- sector_compare$nace2d[!is.na(sector_compare$rho_tabachova) & sector_compare$rho_tabachova < 0.3]
low_ff  <- sector_compare$nace2d[!is.na(sector_compare$rho_firmfold) & sector_compare$rho_firmfold < 0.3]
low_sf  <- sector_compare$nace2d[!is.na(sector_compare$rho_sectorfold) & sector_compare$rho_sectorfold < 0.3]

cat("  Tabachova:    ", paste(low_tab, collapse = ", "), "\n")
cat("  Firm-fold:    ", paste(low_ff, collapse = ", "), "\n")
cat("  Sector-fold:  ", paste(low_sf, collapse = ", "), "\n")
cat("  All three:    ", paste(Reduce(intersect, list(low_tab, low_ff, low_sf)), collapse = ", "), "\n")
cat("  Any of three: ", paste(Reduce(union, list(low_tab, low_ff, low_sf)), collapse = ", "), "\n")
