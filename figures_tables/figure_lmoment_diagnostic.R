###############################################################################
# figures_tables/figure_lmoment_diagnostic.R
#
# PURPOSE
#   Publication-quality L-moment ratio diagram showing within-sector emission
#   shape heterogeneity. Demonstrates that no single parametric distribution
#   family fits all sectors, motivating the use of a pooled reference
#   distribution as a practical compromise.
#
#   Also generates a summary table of L-moment ratios by sector.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/figure_lmoment_ratio_diagram.pdf
#   {OUTPUT_DIR}/table_lmoment_sectors.tex
#
# RUNS ON: local 1
###############################################################################

REPO_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
  error = function(e) normalizePath(getwd(), winslash = "/")
)
while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(lmom)

cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

# =============================================================================
# Compute within-sector deviations (year-demeaned, sector-demeaned)
# =============================================================================
emitters <- training_sample %>%
  filter(emissions > 0, year >= 2005) %>%
  mutate(
    log_y  = log(emissions),
    nace2d = as.character(nace2d)
  ) %>%
  select(vat, year, nace2d, log_y)

# Combine 17/18
emitters$nace2d[emitters$nace2d %in% c("17", "18")] <- "17/18"

# Year-demean
year_means <- emitters %>% group_by(year) %>% summarise(mu_t = mean(log_y), .groups = "drop")
emitters <- emitters %>% left_join(year_means, by = "year") %>% mutate(tilde = log_y - mu_t)

# Sector-demean
sector_means <- emitters %>% group_by(nace2d) %>% summarise(mu_s = mean(tilde), n = n(), .groups = "drop")
emitters <- emitters %>% left_join(sector_means %>% select(nace2d, mu_s), by = "nace2d") %>%
  mutate(d = tilde - mu_s)

# =============================================================================
# L-moment ratios by sector
# =============================================================================
MIN_N <- 15  # minimum for reliable L-moment estimates

lmom_df <- emitters %>%
  group_by(nace2d) %>%
  filter(n() >= MIN_N) %>%
  summarise(
    n_obs   = n(),
    n_firms = n_distinct(vat),
    sd_d    = sd(d),
    {
      lm <- samlmu(d, nmom = 4)
      data.frame(L1 = lm[1], L2 = lm[2], t3 = lm[3], t4 = lm[4])
    },
    .groups = "drop"
  )

# Pooled
pooled_lmom <- samlmu(emitters$d, nmom = 4)

# NACE sector labels for the plot
nace_labels <- c(
  "08" = "Mining", "10" = "Food", "11" = "Beverages", "13" = "Textiles",
  "16" = "Wood", "17/18" = "Paper", "19" = "Petroleum", "20" = "Chemicals",
  "21" = "Pharma", "22" = "Plastics", "23" = "Non-met. minerals",
  "24" = "Basic metals", "25" = "Metal prod.", "27" = "Electrical",
  "28" = "Machinery", "29" = "Motor veh.", "30" = "Transport eq.",
  "31" = "Furniture", "33" = "Repair", "35" = "Electricity",
  "38" = "Waste", "42" = "Civil eng.", "46" = "Wholesale",
  "49" = "Land transport", "52" = "Warehousing", "71" = "Architecture"
)
lmom_df$label <- ifelse(lmom_df$nace2d %in% names(nace_labels),
                         nace_labels[lmom_df$nace2d],
                         lmom_df$nace2d)

cat(sprintf("  %d sectors with >= %d emitter firm-years\n", nrow(lmom_df), MIN_N))

# =============================================================================
# Theoretical distribution curves
# =============================================================================
t3_grid <- seq(-0.5, 0.6, length.out = 300)

# GLO: t4 = (1 + 5*t3^2) / 6
t4_glo <- (1 + 5 * t3_grid^2) / 6

# GEV: numerical via lmom
t4_gev <- sapply(t3_grid, function(t3) {
  tryCatch({
    obj <- function(k) {
      if (abs(k) < 1e-6) return(t3 - (6*log(2) - 6*log(3)) / (2*log(2) - 2))
      -3 + 2*(1 - 3^(-k)) / (1 - 2^(-k)) - t3
    }
    k <- tryCatch(uniroot(obj, c(-2, 5))$root, error = function(e) NA)
    if (is.na(k)) return(NA)
    p <- c(0, 1, k)
    lmrgev(p, nmom = 4)[4]
  }, error = function(e) NA)
})

# GPA: numerical via lmom
t4_gpa <- sapply(t3_grid, function(t3) {
  tryCatch({
    obj <- function(k) {
      p <- c(0, 1, k)
      lm_theo <- tryCatch(lmrgpa(p, nmom = 4), error = function(e) rep(NA, 4))
      if (any(is.na(lm_theo))) return(Inf)
      lm_theo[3] - t3
    }
    k <- tryCatch(uniroot(obj, c(-1, 5))$root, error = function(e) NA)
    if (is.na(k)) return(NA)
    lmrgpa(c(0, 1, k), nmom = 4)[4]
  }, error = function(e) NA)
})

# Log-normal curve
sigma_grid <- seq(0.01, 3, length.out = 300)
lnorm_curve <- t(sapply(sigma_grid, function(sig) {
  tryCatch({
    lm_theo <- lmrln3(c(0, 0, sig), nmom = 4)
    c(t3 = lm_theo[3], t4 = lm_theo[4])
  }, error = function(e) c(t3 = NA, t4 = NA))
}))

# =============================================================================
# Figure: L-moment ratio diagram
# =============================================================================
pdf(file.path(OUTPUT_DIR, "figure_lmoment_ratio_diagram.pdf"), width = 9, height = 7)
par(mar = c(4.5, 4.5, 1.5, 1), family = "serif")

# Determine axis limits to include all sectors
xlim <- c(min(lmom_df$t3, -0.3) - 0.05, max(lmom_df$t3, 0.5) + 0.05)
ylim <- c(min(lmom_df$t4, -0.05) - 0.03, max(lmom_df$t4, 0.35) + 0.03)

plot(NULL, xlim = xlim, ylim = ylim,
     xlab = expression("L-skewness " * (tau[3])),
     ylab = expression("L-kurtosis " * (tau[4])),
     main = "")

# Reference lines
abline(h = 0, col = "grey85", lty = 1)
abline(v = 0, col = "grey85", lty = 1)

# Theoretical curves
mask_gev <- !is.na(t4_gev) & t4_gev >= ylim[1] & t4_gev <= ylim[2]
mask_gpa <- !is.na(t4_gpa) & t4_gpa >= ylim[1] & t4_gpa <= ylim[2]
mask_glo <- t4_glo >= ylim[1] & t4_glo <= ylim[2]
mask_ln  <- !is.na(lnorm_curve[,1]) & lnorm_curve[,2] >= ylim[1] & lnorm_curve[,2] <= ylim[2]

lines(t3_grid[mask_glo], t4_glo[mask_glo], col = "grey55", lty = 2, lwd = 1.2)
lines(t3_grid[mask_gev], t4_gev[mask_gev], col = "grey55", lty = 3, lwd = 1.2)
lines(t3_grid[mask_gpa], t4_gpa[mask_gpa], col = "grey55", lty = 4, lwd = 1.2)
lines(lnorm_curve[mask_ln, 1], lnorm_curve[mask_ln, 2], col = "grey55", lty = 5, lwd = 1.2)

# Normal point
points(0, 0.1226, pch = 3, col = "grey40", cex = 1.5, lwd = 1.5)

# Sector points: size proportional to log(n_obs)
cex_pts <- 0.8 + 1.5 * (log(lmom_df$n_obs) - log(MIN_N)) /
  max(log(lmom_df$n_obs) - log(MIN_N), 1)

# Color: highlight the three zero-emitter sectors
cols <- rep("steelblue3", nrow(lmom_df))
cols[lmom_df$nace2d %in% c("17/18", "19", "24")] <- "firebrick3"

points(lmom_df$t3, lmom_df$t4, pch = 16, col = cols, cex = cex_pts)

# Labels: use short labels, offset to avoid overlap
# Only label sectors with n >= 30 or the three highlighted sectors
label_mask <- lmom_df$n_obs >= 30 | lmom_df$nace2d %in% c("17/18", "19", "24")
text(lmom_df$t3[label_mask], lmom_df$t4[label_mask],
     labels = lmom_df$label[label_mask],
     pos = 3, cex = 0.55, col = ifelse(cols[label_mask] == "firebrick3", "firebrick4", "steelblue4"),
     offset = 0.4)

# Pooled point
points(pooled_lmom[3], pooled_lmom[4], pch = 18, col = "red3", cex = 2.2)
text(pooled_lmom[3], pooled_lmom[4], labels = "Pooled",
     pos = 1, cex = 0.65, col = "red4", offset = 0.5)

legend("topleft", cex = 0.65, bg = "white",
       legend = c("Gen. Logistic (GLO)", "Gen. Extreme Value (GEV)",
                  "Gen. Pareto (GPA)", "Log-normal", "Normal",
                  "Sector (training)", "Sector (zero-emitter)", "Pooled"),
       lty = c(2, 3, 4, 5, NA, NA, NA, NA),
       pch = c(NA, NA, NA, NA, 3, 16, 16, 18),
       col = c(rep("grey55", 4), "grey40", "steelblue3", "firebrick3", "red3"),
       lwd = c(1.2, 1.2, 1.2, 1.2, 1.5, NA, NA, NA),
       pt.cex = c(NA, NA, NA, NA, 1.2, 1.2, 1.2, 1.8))

dev.off()
cat("Figure saved:", file.path(OUTPUT_DIR, "figure_lmoment_ratio_diagram.pdf"), "\n")

# =============================================================================
# Table: L-moment ratios by sector
# =============================================================================

# Sort by n_obs descending
lmom_df <- lmom_df %>% arrange(desc(n_obs))

# LaTeX table
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

tex <- c(
  "\\begin{tabular}{l l r r r r r}",
  "\\toprule",
  "NACE & Sector & $n$ & $\\sigma_d$ & $\\tau_3$ & $\\tau_4$ \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(lmom_df))) {
  row <- lmom_df[i, ]
  # Escape ampersands in labels
  lab <- gsub("&", "\\\\&", row$label)
  tex <- c(tex, sprintf("%s & %s & %d & %s & %s & %s \\\\",
                        row$nace2d, lab, row$n_obs,
                        fmt(row$sd_d), fmt(row$t3), fmt(row$t4)))
}

tex <- c(tex,
  "\\midrule",
  sprintf("\\textit{Pooled} & & %d & %s & %s & %s \\\\",
          nrow(emitters), fmt(sd(emitters$d)), fmt(pooled_lmom[3]), fmt(pooled_lmom[4])),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_path <- file.path(OUTPUT_DIR, "table_lmoment_sectors.tex")
writeLines(tex, tex_path)
cat("Table saved:", tex_path, "\n")

cat("Done.\n")
