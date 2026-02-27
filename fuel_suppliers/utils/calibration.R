###############################################################################
# fuel_suppliers/utils/calibration.R
#
# Shared calibration functions for the fuel-supplier CV pipeline.
# Sourced by fit_cv_lofocv.R and fit_cv_losocv.R.
###############################################################################

# ── Simple proportional calibration ─────────────────────────────────────────
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
