###############################################################################
# analysis/within_firm_emission_dynamics.R
#
# PURPOSE
#   Document the within-firm vs between-firm variance structure of emissions
#   in the training sample. This provides context for interpreting the
#   within-firm regression results: if most emission variance is between-firm,
#   even a perfect fuel proxy would show large attenuation under firm FE.
#
# INPUTS
#   - {PROC_DATA}/training_sample.RData
#
# OUTPUTS (to console + OUTPUT_DIR)
#   - emission_variance_decomposition.csv
#
# STATISTICS REPORTED
#   1. Variance decomposition: between-firm vs within-firm share of
#      var(asinh(emissions)) and var(log(emissions)) [emitters only]
#   2. Within-firm autocorrelation of emissions (AR(1) coefficient)
#   3. Share of firms with constant emitter/non-emitter status
#   4. Distribution of within-firm emission changes (IQR, median abs change)
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


# ── Load training sample ─────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

panel <- training_sample %>%
  mutate(
    asinh_y = asinh(y),
    log_y   = ifelse(y > 0, log(y), NA),
    emitter = as.integer(y > 0)
  ) %>%
  arrange(vat, year)

n_obs   <- nrow(panel)
n_firms <- n_distinct(panel$vat)
n_years <- n_distinct(panel$year)

cat(sprintf("\nTraining sample: %d obs, %d firms, %d years (%d-%d)\n",
            n_obs, n_firms, n_years, min(panel$year), max(panel$year)))
cat(sprintf("Emitter obs: %d (%.1f%%)\n",
            sum(panel$emitter), 100 * mean(panel$emitter)))


# ══════════════════════════════════════════════════════════════════════════════
#   1. VARIANCE DECOMPOSITION
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 60), "\n")
cat("  1. VARIANCE DECOMPOSITION\n")
cat(strrep("=", 60), "\n")

decompose_variance <- function(df, var_name, label) {
  x <- df[[var_name]]
  x <- x[!is.na(x)]

  # Compute firm means
  firm_means <- df %>%
    filter(!is.na(.data[[var_name]])) %>%
    group_by(vat) %>%
    summarise(fm = mean(.data[[var_name]], na.rm = TRUE), .groups = "drop")

  df_tmp <- df %>%
    filter(!is.na(.data[[var_name]])) %>%
    left_join(firm_means, by = "vat") %>%
    mutate(within = .data[[var_name]] - fm)

  total_var   <- var(df_tmp[[var_name]])
  between_var <- var(df_tmp$fm)
  within_var  <- var(df_tmp$within)

  cat(sprintf("\n  %s (N = %d):\n", label, nrow(df_tmp)))
  cat(sprintf("    Total variance:    %12.4f\n", total_var))
  cat(sprintf("    Between-firm:      %12.4f  (%.1f%%)\n",
              between_var, 100 * between_var / total_var))
  cat(sprintf("    Within-firm:       %12.4f  (%.1f%%)\n",
              within_var, 100 * within_var / total_var))

  data.frame(
    variable       = label,
    total_var      = total_var,
    between_var    = between_var,
    within_var     = within_var,
    between_share  = between_var / total_var,
    within_share   = within_var / total_var,
    n_obs          = nrow(df_tmp),
    n_firms        = n_distinct(df_tmp$vat)
  )
}

vd1 <- decompose_variance(panel, "asinh_y", "asinh(emissions) — all obs")
vd2 <- decompose_variance(panel, "y", "emissions (levels) — all obs")

# Emitters only
panel_emitters <- panel %>% filter(emitter == 1)
vd3 <- decompose_variance(panel_emitters, "asinh_y", "asinh(emissions) — emitters only")
vd4 <- decompose_variance(panel_emitters, "log_y", "log(emissions) — emitters only")

var_decomp <- bind_rows(vd1, vd2, vd3, vd4)


# ══════════════════════════════════════════════════════════════════════════════
#   2. WITHIN-FIRM AUTOCORRELATION
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 60), "\n")
cat("  2. WITHIN-FIRM AUTOCORRELATION\n")
cat(strrep("=", 60), "\n")

# AR(1) coefficient from firm-demeaned emissions
panel_ar <- panel %>%
  group_by(vat) %>%
  mutate(
    asinh_y_demean = asinh_y - mean(asinh_y),
    lag_asinh_y_demean = lag(asinh_y_demean)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_asinh_y_demean))

ar1_fit <- lm(asinh_y_demean ~ lag_asinh_y_demean - 1, data = panel_ar)
ar1_coef <- coef(ar1_fit)[1]

# Also raw correlation
ar1_corr <- cor(panel_ar$asinh_y_demean, panel_ar$lag_asinh_y_demean)

cat(sprintf("\n  AR(1) coefficient (demeaned asinh(emissions)):  %.4f\n", ar1_coef))
cat(sprintf("  Correlation (demeaned asinh(y_t), asinh(y_{t-1})):  %.4f\n", ar1_corr))

# Among emitters only (in levels and logs)
panel_ar_emit <- panel_emitters %>%
  group_by(vat) %>%
  mutate(
    log_y_demean = log_y - mean(log_y, na.rm = TRUE),
    lag_log_y_demean = lag(log_y_demean)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_log_y_demean))

ar1_emit <- cor(panel_ar_emit$log_y_demean, panel_ar_emit$lag_log_y_demean, use = "complete.obs")
cat(sprintf("  Correlation (demeaned log(y_t), log(y_{t-1})), emitters only:  %.4f\n", ar1_emit))


# ══════════════════════════════════════════════════════════════════════════════
#   3. EMITTER STATUS PERSISTENCE
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 60), "\n")
cat("  3. EMITTER STATUS PERSISTENCE\n")
cat(strrep("=", 60), "\n")

firm_status <- panel %>%
  group_by(vat) %>%
  summarise(
    n_years       = n(),
    n_emitter     = sum(emitter),
    n_non_emitter = sum(1 - emitter),
    always_emitter     = all(emitter == 1),
    always_non_emitter = all(emitter == 0),
    ever_switches      = !always_emitter & !always_non_emitter,
    .groups = "drop"
  )

n_always_emit <- sum(firm_status$always_emitter)
n_always_zero <- sum(firm_status$always_non_emitter)
n_switchers   <- sum(firm_status$ever_switches)

cat(sprintf("\n  Firms always emitting:      %d / %d (%.1f%%)\n",
            n_always_emit, n_firms, 100 * n_always_emit / n_firms))
cat(sprintf("  Firms always zero:          %d / %d (%.1f%%)\n",
            n_always_zero, n_firms, 100 * n_always_zero / n_firms))
cat(sprintf("  Firms switching status:     %d / %d (%.1f%%)\n",
            n_switchers, n_firms, 100 * n_switchers / n_firms))

# Among multi-year firms only
multi_year <- firm_status %>% filter(n_years >= 2)
cat(sprintf("\n  Among firms with >= 2 years (%d firms):\n", nrow(multi_year)))
cat(sprintf("    Always emitting:    %d (%.1f%%)\n",
            sum(multi_year$always_emitter), 100 * mean(multi_year$always_emitter)))
cat(sprintf("    Always zero:        %d (%.1f%%)\n",
            sum(multi_year$always_non_emitter), 100 * mean(multi_year$always_non_emitter)))
cat(sprintf("    Switches:           %d (%.1f%%)\n",
            sum(multi_year$ever_switches), 100 * mean(multi_year$ever_switches)))


# ══════════════════════════════════════════════════════════════════════════════
#   4. DISTRIBUTION OF WITHIN-FIRM EMISSION CHANGES
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 60), "\n")
cat("  4. DISTRIBUTION OF YEAR-TO-YEAR EMISSION CHANGES\n")
cat(strrep("=", 60), "\n")

changes <- panel %>%
  group_by(vat) %>%
  mutate(
    d_y       = y - lag(y),
    d_asinh_y = asinh_y - lag(asinh_y),
    d_log_y   = log_y - lag(log_y),
    pct_change = ifelse(lag(y) > 0, (y - lag(y)) / lag(y), NA)
  ) %>%
  ungroup() %>%
  filter(!is.na(d_y))

cat(sprintf("\n  Year-to-year changes (N = %d transitions):\n", nrow(changes)))

cat("\n  asinh(emissions) changes:\n")
cat(sprintf("    Mean abs change:   %.4f\n", mean(abs(changes$d_asinh_y))))
cat(sprintf("    Median abs change: %.4f\n", median(abs(changes$d_asinh_y))))
cat(sprintf("    IQR:               [%.4f, %.4f]\n",
            quantile(changes$d_asinh_y, 0.25), quantile(changes$d_asinh_y, 0.75)))
cat(sprintf("    SD:                %.4f\n", sd(changes$d_asinh_y)))

# Compare to cross-sectional SD
cat(sprintf("\n  For context — cross-sectional SD of asinh(emissions): %.4f\n",
            sd(panel$asinh_y)))
cat(sprintf("  Ratio (within SD / cross-sectional SD): %.3f\n",
            sd(changes$d_asinh_y) / sd(panel$asinh_y)))

# Among emitters only: percentage changes
changes_emit <- changes %>% filter(!is.na(pct_change))
cat(sprintf("\n  Percentage changes among emitters (N = %d):\n", nrow(changes_emit)))
cat(sprintf("    Median abs pct change: %.1f%%\n", 100 * median(abs(changes_emit$pct_change))))
cat(sprintf("    IQR of pct change:     [%.1f%%, %.1f%%]\n",
            100 * quantile(changes_emit$pct_change, 0.25),
            100 * quantile(changes_emit$pct_change, 0.75)))


# ══════════════════════════════════════════════════════════════════════════════
#   SAVE
# ══════════════════════════════════════════════════════════════════════════════
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(var_decomp,
          file.path(OUTPUT_DIR, "emission_variance_decomposition.csv"),
          row.names = FALSE)
cat("\n\nSaved variance decomposition to:",
    file.path(OUTPUT_DIR, "emission_variance_decomposition.csv"), "\n")
