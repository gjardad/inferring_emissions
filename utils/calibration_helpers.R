###############################################################################
# utils/calibration_helpers.R
#
# Shared helpers for repeated-CV evaluation scripts:
#   - proxy_to_levels(): back-transform asinh proxy to levels
#   - assign_folds(): reconstruct fold assignments deterministically
#   - calibrate_sector(): fold-aware sector-level calibration
#
# Sourced by: table_main_results.R, sector_heterogeneity.R
###############################################################################

# ── Back-transform asinh proxy to levels ──────────────────────────────────
proxy_to_levels <- function(proxy) pmax(sinh(proxy), 0)

# ── Reconstruct fold assignments ─────────────────────────────────────────
# Must match the logic used in build_repeated_cv_proxy_asinh.R on RMD.
assign_folds <- function(panel, cv_type, K, seed) {
  set.seed(seed)
  if (cv_type == "sector") {
    sectors <- sort(unique(panel$primary_nace2d))
    sector_folds <- sample(rep(1:K, length.out = length(sectors)))
    sfm <- data.frame(primary_nace2d = sectors, fold_k = sector_folds,
                       stringsAsFactors = FALSE)
    fold_k <- sfm$fold_k[match(panel$primary_nace2d, sfm$primary_nace2d)]
  } else {
    firms <- unique(panel[, c("vat", "primary_nace2d")])
    firms <- firms[order(firms$vat), ]
    firm_folds <- integer(nrow(firms))
    for (sec in unique(firms$primary_nace2d)) {
      idx <- which(firms$primary_nace2d == sec)
      firm_folds[idx] <- sample(rep(1:K, length.out = length(idx)))
    }
    firms$fold_k <- firm_folds
    fold_k <- firms$fold_k[match(panel$vat, firms$vat)]
  }
  fold_k
}

# ── Fold-aware sector-level calibration ──────────────────────────────────
# For each fold k and each (sector, year) cell among held-out firms:
#   E_target = E_total_sy - E_train_sy_k
#   Distribute E_target proportionally to proxy among held-out firms.
calibrate_sector <- function(df, proxy_raw, fold_k) {
  sy_key <- paste(df$nace2d, df$year)
  E_sy <- tapply(df$y, sy_key, sum, na.rm = TRUE)

  folds <- sort(unique(fold_k))
  result <- rep(NA_real_, nrow(df))

  for (k in folds) {
    held_out <- which(fold_k == k)
    train_k  <- which(fold_k != k)

    sy_train <- paste(df$nace2d[train_k], df$year[train_k])
    E_train_sy <- tapply(df$y[train_k], sy_train, sum, na.rm = TRUE)

    ho_sy <- paste(df$nace2d[held_out], df$year[held_out])

    for (sy in unique(ho_sy)) {
      idx_in_ho <- which(ho_sy == sy)
      idx <- held_out[idx_in_ho]

      E_total <- E_sy[sy]
      E_train <- ifelse(is.na(E_train_sy[sy]), 0, E_train_sy[sy])
      E_target <- E_total - E_train

      if (is.na(E_target) || E_target <= 0) {
        result[idx] <- 0
        next
      }

      raw <- proxy_raw[idx]
      denom <- sum(raw, na.rm = TRUE)
      if (denom > 0) {
        result[idx] <- E_target * (raw / denom)
      } else {
        result[idx] <- E_target / length(idx)
      }
    }
  }
  result
}
