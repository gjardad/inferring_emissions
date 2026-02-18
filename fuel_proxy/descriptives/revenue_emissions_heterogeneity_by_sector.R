###############################################################################
# fuel_proxy/descriptives/revenue_emissions_heterogeneity_by_sector.R
#
# PURPOSE
#   Assess whether the revenue-emissions link differs across sectors, to
#   motivate a heterogeneous-slope PPML specification (dirty vs clean).
#
#   Three analyses:
#     1) Per-sector statistics: N firms, share emitters, emissions mass,
#        Spearman correlation, and revenue-emissions elasticity (from
#        within-sector log-log OLS among emitters).
#     2) Pooled regression with dirty*revenue interaction to test the
#        heterogeneity conjecture at the aggregate level.
#     3) Figures: elasticity dot plot with CIs; scatter of share emitters
#        vs elasticity labelled by NACE.
#
# INPUTS
#   - PROC_DATA/loocv_training_sample.RData
#
# OUTPUTS (to OUTPUT_DIR)
#   - sector_revenue_emissions_heterogeneity.csv
#   - sector_elasticity_dotplot.pdf
#   - sector_elasticity_vs_emitter_share.pdf
#   - (printed) pooled regression summary
###############################################################################

# ====================
# Define paths -------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(data.table)
library(ggplot2)
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel", repos = "https://cloud.r-project.org")
library(ggrepel)

# ==================
# Load data --------
# ==================

load(file.path(PROC_DATA, "loocv_training_sample.RData"))
DT <- as.data.table(loocv_training_sample)

DT[, nace2d := as.character(substr(nace5d, 1, 2))]
DT[, emit := as.integer(emissions > 0)]
DT[, log_rev := log(pmax(revenue, 1e-12))]
DT[, log_emi := ifelse(emissions > 0, log(emissions), NA_real_)]

cat(sprintf("Training sample: %d obs, %d firms, %d sectors\n",
            nrow(DT), uniqueN(DT$vat), uniqueN(DT$nace2d)))

# =====================================================================
# 1) PER-SECTOR STATISTICS
# =====================================================================

sector_stats <- DT[, {

  n_firms     <- uniqueN(vat)
  n_firmyears <- .N
  share_emit  <- mean(emit)
  total_emi   <- sum(emissions, na.rm = TRUE)

  # Among emitters: Spearman correlation and log-log elasticity
  em <- .SD[emit == 1 & is.finite(log_rev) & is.finite(log_emi)]
  n_emit <- nrow(em)

  if (n_emit >= 5) {
    spearman <- cor(em$revenue, em$emissions, method = "spearman", use = "complete.obs")
    fit <- tryCatch(lm(log_emi ~ log_rev, data = em), error = function(e) NULL)
    if (!is.null(fit)) {
      s <- summary(fit)
      beta     <- coef(fit)[["log_rev"]]
      beta_se  <- s$coefficients["log_rev", "Std. Error"]
      beta_p   <- s$coefficients["log_rev", "Pr(>|t|)"]
    } else {
      beta <- beta_se <- beta_p <- NA_real_
    }
  } else {
    spearman <- beta <- beta_se <- beta_p <- NA_real_
  }

  list(
    n_firms     = n_firms,
    n_firmyears = n_firmyears,
    n_emitters  = n_emit,
    share_emit  = round(share_emit, 4),
    total_emi   = total_emi,
    spearman    = round(spearman, 4),
    elasticity  = round(beta, 4),
    elast_se    = round(beta_se, 4),
    elast_p     = round(beta_p, 6)
  )
}, by = nace2d]

# Add share of total emissions
sector_stats[, share_total_emi := round(total_emi / sum(total_emi), 4)]

setorder(sector_stats, -elasticity)

cat("\n=== SECTOR-LEVEL REVENUE-EMISSIONS STATISTICS ===\n")
print(sector_stats, nrows = 100)

write.csv(sector_stats,
          file.path(OUTPUT_DIR, "sector_revenue_emissions_heterogeneity.csv"),
          row.names = FALSE)
cat("\nSaved table to", file.path(OUTPUT_DIR, "sector_revenue_emissions_heterogeneity.csv"), "\n")


# =====================================================================
# 2) POOLED REGRESSION WITH DIRTY INTERACTION
# =====================================================================

# Preliminary dirty list from CRF-to-NACE mapping
dirty_naces_preliminary <- c("19", "24", "20", "23", "17", "35")

DT[, dirty := as.integer(nace2d %in% dirty_naces_preliminary)]

reg_dt <- DT[emit == 1 & is.finite(log_rev) & is.finite(log_emi) &
               is.finite(revenue) & revenue > 0]

cat(sprintf("\nPooled regression sample: %d emitter obs, %d firms, %d sectors\n",
            nrow(reg_dt), uniqueN(reg_dt$vat), uniqueN(reg_dt$nace2d)))
cat(sprintf("Dirty firms: %d obs (%d firms) | Clean firms: %d obs (%d firms)\n",
            sum(reg_dt$dirty == 1), uniqueN(reg_dt[dirty == 1]$vat),
            sum(reg_dt$dirty == 0), uniqueN(reg_dt[dirty == 0]$vat)))

# log(emissions) ~ log(revenue) + log(revenue):dirty + year FE + sector FE
model_interaction <- lm(
  log_emi ~ log_rev + log_rev:dirty + factor(year) + factor(nace2d),
  data = reg_dt
)

s_int <- summary(model_interaction)

cat("\n=== POOLED REGRESSION: log(emissions) ~ log(revenue) + log(revenue):dirty + FEs ===\n")
cat(sprintf("N = %d, R^2 = %.4f, Adj R^2 = %.4f\n",
            nobs(model_interaction), s_int$r.squared, s_int$adj.r.squared))

# Print key coefficients
ct <- coef(s_int)
key_vars <- c("log_rev", "log_rev:dirty")
for (v in key_vars) {
  if (v %in% rownames(ct)) {
    est <- ct[v, "Estimate"]
    se  <- ct[v, "Std. Error"]
    pv  <- ct[v, "Pr(>|t|)"]
    stars <- if (pv < 0.001) "***" else if (pv < 0.01) "**" else if (pv < 0.05) "*" else if (pv < 0.1) "." else ""
    cat(sprintf("  %-20s  %8.4f  (se = %.4f)  p = %.4e  %s\n", v, est, se, pv, stars))
  }
}

beta_clean <- ct["log_rev", "Estimate"]
beta_dirty_interaction <- ct["log_rev:dirty", "Estimate"]
cat(sprintf("\n  => Clean elasticity:  %.4f\n", beta_clean))
cat(sprintf("  => Dirty elasticity:  %.4f  (= %.4f + %.4f)\n",
            beta_clean + beta_dirty_interaction, beta_clean, beta_dirty_interaction))


# =====================================================================
# 3) FIGURES
# =====================================================================

# --- Dot plot: sector-level elasticities with CIs ---

plot_dt <- sector_stats[!is.na(elasticity) & !is.na(elast_se)]
plot_dt[, ci_lo := elasticity - 1.96 * elast_se]
plot_dt[, ci_hi := elasticity + 1.96 * elast_se]
plot_dt[, dirty_label := ifelse(nace2d %in% dirty_naces_preliminary, "Dirty (preliminary)", "Clean")]

# Order by elasticity for the plot
plot_dt[, nace2d_f := factor(nace2d, levels = plot_dt[order(elasticity)]$nace2d)]

p_dotplot <- ggplot(plot_dt, aes(x = elasticity, y = nace2d_f, color = dirty_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.3, linewidth = 0.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Dirty (preliminary)" = "firebrick", "Clean" = "steelblue")) +
  labs(
    x = "Revenue-emissions elasticity (log-log OLS among emitters)",
    y = "NACE 2-digit sector",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13)
  )

ggsave(file.path(OUTPUT_DIR, "sector_elasticity_dotplot.pdf"),
       p_dotplot, width = 9, height = 7, dpi = 300)
cat("\nSaved dotplot to", file.path(OUTPUT_DIR, "sector_elasticity_dotplot.pdf"), "\n")


# --- Scatter: share emitters vs elasticity, labelled by NACE ---

p_scatter <- ggplot(plot_dt, aes(x = share_emit, y = elasticity, label = nace2d)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(color = dirty_label), size = 3) +
  geom_text_repel(size = 3.5, max.overlaps = 20) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Dirty (preliminary)" = "firebrick", "Clean" = "steelblue")) +
  labs(
    x = "Share of firm-years with emissions > 0",
    y = "Revenue-emissions elasticity",
    color = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

ggsave(file.path(OUTPUT_DIR, "sector_elasticity_vs_emitter_share.pdf"),
       p_scatter, width = 8, height = 6, dpi = 300)
cat("\nSaved scatter to", file.path(OUTPUT_DIR, "sector_elasticity_vs_emitter_share.pdf"), "\n")

cat("\nDone.\n")
