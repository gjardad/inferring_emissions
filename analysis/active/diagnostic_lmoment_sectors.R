###############################################################################
# analysis/active/diagnostic_lmoment_sectors.R
#
# PURPOSE
#   Diagnose whether within-sector emission distributions share a common shape
#   across NACE 2-digit sectors. If they do, a single parametric family can
#   serve as the reference distribution for quantile mapping.
#
# APPROACH
#   1. Load training_sample.RData.
#   2. For emitters (y > 0), compute year-demeaned log emissions:
#        tilde_j = log(y_j) - mu_t
#      where mu_t = mean of log(y) across all emitters in year t.
#   3. Within each sector s, compute deviations from sector mean:
#        d_j = tilde_j - mu_s
#      where mu_s = mean of tilde_j within sector s (pooled across years).
#   4. For each sector with >= 10 emitter firm-years, compute L-moment ratios:
#        L-CV (t2 = L2/L1 of absolute deviations),
#        L-skewness (t3 = L3/L2), L-kurtosis (t4 = L4/L2).
#   5. Plot L-moment ratio diagram (t3 vs t4) with theoretical curves for
#      common distributions. If sectors cluster, a common family is justified.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/lmoment_ratio_diagram.pdf
#   {OUTPUT_DIR}/lmoment_sector_table.csv
#   Console: summary statistics and diagnostic interpretation
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
library(lmom)

cat("================================================================\n")
cat("  L-MOMENT DIAGNOSTIC: Within-sector emission shape\n")
cat("================================================================\n\n")

# =============================================================================
# STEP 1: Load data and prepare emitter panel
# =============================================================================
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

emitters <- training_sample %>%
  filter(emissions > 0, year >= 2005) %>%
  mutate(log_y = log(emissions)) %>%
  select(vat, year, nace2d, log_y)

cat(sprintf("  %d emitter firm-years, %d unique sectors\n",
            nrow(emitters), length(unique(emitters$nace2d))))

# =============================================================================
# STEP 2: Year-demean log emissions
# =============================================================================
year_means <- emitters %>%
  group_by(year) %>%
  summarise(mu_t = mean(log_y), .groups = "drop")

emitters <- emitters %>%
  left_join(year_means, by = "year") %>%
  mutate(tilde = log_y - mu_t)

cat(sprintf("  Year effects range: [%.3f, %.3f]\n",
            min(year_means$mu_t), max(year_means$mu_t)))

# =============================================================================
# STEP 3: Within-sector deviations
# =============================================================================
sector_means <- emitters %>%
  group_by(nace2d) %>%
  summarise(mu_s = mean(tilde), n = n(), .groups = "drop")

emitters <- emitters %>%
  left_join(sector_means %>% select(nace2d, mu_s), by = "nace2d") %>%
  mutate(d = tilde - mu_s)

cat(sprintf("  Sectors with >= 10 emitter firm-years: %d of %d\n",
            sum(sector_means$n >= 10), nrow(sector_means)))

# =============================================================================
# STEP 4: L-moment ratios by sector
# =============================================================================
MIN_N <- 10  # minimum emitter firm-years for reliable L-moments

lmom_by_sector <- emitters %>%
  group_by(nace2d) %>%
  filter(n() >= MIN_N) %>%
  summarise(
    n_obs = n(),
    n_firms = n_distinct(vat),
    mean_d = mean(d),
    sd_d   = sd(d),
    {
      lm <- samlmu(d, nmom = 4)
      data.frame(L1 = lm[1], L2 = lm[2], t3 = lm[3], t4 = lm[4])
    },
    .groups = "drop"
  )

cat("\n── L-moment ratios by sector ──────────────────────────────────\n")
print(lmom_by_sector %>%
        select(nace2d, n_obs, n_firms, sd_d, t3, t4) %>%
        arrange(desc(n_obs)) %>%
        as.data.frame(),
      digits = 3, row.names = FALSE)

# =============================================================================
# STEP 5: Pooled reference distribution L-moments
# =============================================================================
pooled_lmom <- samlmu(emitters$d, nmom = 4)
cat(sprintf("\n── Pooled reference distribution (all sectors, n=%d) ──────────\n",
            nrow(emitters)))
cat(sprintf("  L1 (location) = %.4f\n", pooled_lmom[1]))
cat(sprintf("  L2 (scale)    = %.4f\n", pooled_lmom[2]))
cat(sprintf("  t3 (L-skew)   = %.4f\n", pooled_lmom[3]))
cat(sprintf("  t4 (L-kurt)   = %.4f\n", pooled_lmom[4]))

# =============================================================================
# STEP 6: L-moment ratio diagram
# =============================================================================

# Theoretical L-kurtosis as function of L-skewness for common distributions
# Normal: t3 = 0, t4 = 0.1226 (a point, not a curve)
# Logistic: t3 = 0, t4 = 1/6
# Generalized Logistic (GLO): t4 = (1 + 5*t3^2) / 6
# Generalized Extreme Value (GEV): parametric curve via shape parameter
# Generalized Pareto (GPA): t4 = (1 + 5*t3*(t3+1)) / (5+t3) ... approximation
# Log-normal: parametric curve via sigma

# GLO curve (exact)
t3_grid <- seq(-0.5, 0.5, length.out = 200)
t4_glo <- (1 + 5 * t3_grid^2) / 6

# GEV curve (numerical via lmom package)
t4_gev <- sapply(t3_grid, function(t3) {
  tryCatch({
    # GEV: L-skewness = 2(1 - 3^(-k))/(1 - 2^(-k)) - 3
    # Invert numerically for k, then compute t4
    obj <- function(k) {
      if (abs(k) < 1e-6) return(t3 - (6*log(2) - 6*log(3)) / (2*log(2) - 2))
      lsk <- -3 + 2*(1 - 3^(-k)) / (1 - 2^(-k))
      lsk - t3
    }
    k <- tryCatch(uniroot(obj, c(-2, 5))$root, error = function(e) NA)
    if (is.na(k)) return(NA)
    if (abs(k) < 1e-6) {
      # Gumbel limit
      return(0.1699)
    }
    gk <- gamma(1 + k)
    g2k <- gamma(1 + 2*k)
    g3k <- gamma(1 + 3*k)
    g4k <- gamma(1 + 4*k)
    lam2 <- gk * (1 - 2^(-k)) * gamma(1+k)  # simplified
    # Use lmom package directly
    p <- c(0, 1, k)  # xi, alpha, k
    lm_theo <- lmrgev(p, nmom = 4)
    lm_theo[4]
  }, error = function(e) NA)
})

# GPA curve (numerical via lmom package)
t4_gpa <- sapply(t3_grid, function(t3) {
  tryCatch({
    # GPA t3 = (1-3k)/(1+k)(1+2k)... invert numerically
    p <- c(0, 1, 0)  # xi, alpha, k placeholder
    obj <- function(k) {
      p[3] <- k
      lm_theo <- tryCatch(lmrgpa(p, nmom = 4), error = function(e) rep(NA, 4))
      if (any(is.na(lm_theo))) return(Inf)
      lm_theo[3] - t3
    }
    k <- tryCatch(uniroot(obj, c(-1, 5))$root, error = function(e) NA)
    if (is.na(k)) return(NA)
    p[3] <- k
    lm_theo <- lmrgpa(p, nmom = 4)
    lm_theo[4]
  }, error = function(e) NA)
})

# Normal: single point
t3_norm <- 0
t4_norm <- 0.1226

# Lognormal curve (parametric via sigma)
sigma_grid <- seq(0.01, 3, length.out = 200)
lnorm_curve <- t(sapply(sigma_grid, function(sig) {
  tryCatch({
    p <- c(0, sig)  # mu, sigma (of underlying normal)
    lm_theo <- lmrln3(c(0, 0, sig), nmom = 4)
    c(t3 = lm_theo[3], t4 = lm_theo[4])
  }, error = function(e) c(t3 = NA, t4 = NA))
}))

# ── Plot ──────────────────────────────────────────────────────────
pdf(file.path(OUTPUT_DIR, "lmoment_ratio_diagram.pdf"), width = 8, height = 6)
par(mar = c(4.5, 4.5, 2, 1))

plot(NULL, xlim = c(-0.3, 0.5), ylim = c(0, 0.35),
     xlab = expression("L-skewness " * (tau[3])),
     ylab = expression("L-kurtosis " * (tau[4])),
     main = "L-moment ratio diagram: within-sector emission deviations")

# Theoretical curves
lines(t3_grid, t4_glo, col = "grey60", lty = 2, lwd = 1.5)
lines(t3_grid[!is.na(t4_gev)], t4_gev[!is.na(t4_gev)],
      col = "grey60", lty = 3, lwd = 1.5)
lines(t3_grid[!is.na(t4_gpa)], t4_gpa[!is.na(t4_gpa)],
      col = "grey60", lty = 4, lwd = 1.5)
lines(lnorm_curve[!is.na(lnorm_curve[,1]), 1],
      lnorm_curve[!is.na(lnorm_curve[,1]), 2],
      col = "grey60", lty = 5, lwd = 1.5)
points(t3_norm, t4_norm, pch = 3, col = "grey40", cex = 1.5)

# Sector points — size proportional to log(n)
cex_pts <- 0.5 + 1.5 * (log(lmom_by_sector$n_obs) - log(MIN_N)) /
  (max(log(lmom_by_sector$n_obs)) - log(MIN_N))
points(lmom_by_sector$t3, lmom_by_sector$t4,
       pch = 16, col = "steelblue", cex = cex_pts)
text(lmom_by_sector$t3, lmom_by_sector$t4,
     labels = lmom_by_sector$nace2d,
     pos = 3, cex = 0.6, col = "steelblue4")

# Pooled point
points(pooled_lmom[3], pooled_lmom[4],
       pch = 18, col = "red", cex = 2)
text(pooled_lmom[3], pooled_lmom[4],
     labels = "POOLED", pos = 1, cex = 0.7, col = "red4")

legend("topleft", cex = 0.7,
       legend = c("GLO", "GEV", "GPA", "Log-normal", "Normal",
                  "Sector", "Pooled"),
       lty = c(2, 3, 4, 5, NA, NA, NA),
       pch = c(NA, NA, NA, NA, 3, 16, 18),
       col = c(rep("grey60", 4), "grey40", "steelblue", "red"),
       lwd = c(1.5, 1.5, 1.5, 1.5, NA, NA, NA))

dev.off()
cat(sprintf("\nPlot saved: %s\n", file.path(OUTPUT_DIR, "lmoment_ratio_diagram.pdf")))

# =============================================================================
# STEP 7: Save sector-level table
# =============================================================================
write.csv(lmom_by_sector,
          file.path(OUTPUT_DIR, "lmoment_sector_table.csv"),
          row.names = FALSE)
cat(sprintf("Table saved: %s\n", file.path(OUTPUT_DIR, "lmoment_sector_table.csv")))

# =============================================================================
# STEP 8: Diagnostic summary
# =============================================================================
cat("\n── DIAGNOSTIC SUMMARY ─────────────────────────────────────────\n")
cat(sprintf("  Spread of sector t3 (L-skew):  [%.3f, %.3f], SD = %.3f\n",
            min(lmom_by_sector$t3), max(lmom_by_sector$t3), sd(lmom_by_sector$t3)))
cat(sprintf("  Spread of sector t4 (L-kurt):  [%.3f, %.3f], SD = %.3f\n",
            min(lmom_by_sector$t4), max(lmom_by_sector$t4), sd(lmom_by_sector$t4)))
cat(sprintf("  Pooled:  t3 = %.3f, t4 = %.3f\n", pooled_lmom[3], pooled_lmom[4]))

# Check if sectors cluster tightly
t3_iqr <- IQR(lmom_by_sector$t3)
t4_iqr <- IQR(lmom_by_sector$t4)
cat(sprintf("  IQR of t3: %.3f    IQR of t4: %.3f\n", t3_iqr, t4_iqr))

if (t3_iqr < 0.15 && t4_iqr < 0.10) {
  cat("  => Sectors cluster reasonably well. Common distributional family is plausible.\n")
} else {
  cat("  => Sectors show substantial dispersion. Common shape assumption may be strained.\n")
}

cat("\nDone.\n")
