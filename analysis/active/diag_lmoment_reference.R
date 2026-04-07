# diag_lmoment_reference.R
#
# Goodness-of-fit diagnostic for the reference distribution used in
# threshold_p_levels_eval_all.R. The aim is to decide whether GPA, GLO, GEV,
# GNO, or PE3 is the natural choice on PURE GOODNESS-OF-FIT grounds — without
# looking at any mixed-sector results.
#
# Reference distribution (matches threshold_p_levels_eval_all.R):
#   y > 0 emitter firm-years from CRF groups OTHER than paper / refining /
#   metals, with log y demeaned by year and then by primary_crf_group.
#
# Diagnostics:
#
#   1. L-moment ratios (l1, l2, t3, t4). Three-parameter L-moment fits match
#      l1, l2, t3 by construction, so the discrepancy lives at t4.
#
#   2. For each candidate distribution: fit by L-moments, then compute the
#      theoretical t4 of the fitted distribution and compare to sample t4.
#
#   3. Bootstrap SE of sample t4 (B = 1000), used to express the model gap
#      in standard errors. This is the basis of Hosking's Z statistic for
#      goodness of fit.
#
#   4. Hosking Z: Z_DIST = (t4_DIST - t4_sample) / se_boot(t4). Distributions
#      with |Z| <= 1.64 are acceptable at the 90% confidence level.
#
#   5. L-moment ratio diagram (t3, t4) with the sample point and the
#      theoretical curves of GPA, GLO, GEV, GNO, PE3 (parameterized by t3).
#
#   6. Q-Q plot of the reference data against each fitted distribution.

suppressPackageStartupMessages({
  library(dplyr)
  library(lmom)
  library(ggplot2)
})

source("paths.R")

in_path <- file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData")
out_dir <- file.path("analysis", "active", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

load(in_path)

panel <- repeated_cv_proxy_panel

# --- Build reference distribution -------------------------------------------
ref_df <- panel %>%
  filter(y > 0,
         !primary_crf_group %in% c("paper", "refining", "metals"),
         !is.na(primary_crf_group)) %>%
  mutate(log_y = log(y)) %>%
  group_by(year)              %>% mutate(tilde = log_y - mean(log_y)) %>% ungroup() %>%
  group_by(primary_crf_group) %>% mutate(d     = tilde - mean(tilde)) %>% ungroup()

ref <- sort(ref_df$d)
n_ref <- length(ref)
cat(sprintf("Reference: %d emitter firm-years from %d CRF groups\n",
            n_ref, length(unique(ref_df$primary_crf_group))))

# --- Sample L-moments -------------------------------------------------------
lmoms_sample <- samlmu(ref, nmom = 5)
cat("\nSample L-moments:\n")
print(lmoms_sample)

# --- Fit each candidate distribution ----------------------------------------
fits <- list(
  gpa = pelgpa(lmoms_sample[1:3]),
  glo = pelglo(lmoms_sample[1:3]),
  gev = pelgev(lmoms_sample[1:3]),
  gno = pelgno(lmoms_sample[1:3]),
  pe3 = pelpe3(lmoms_sample[1:3])
)

lmrs <- list(
  gpa = lmrgpa(fits$gpa, nmom = 5),
  glo = lmrglo(fits$glo, nmom = 5),
  gev = lmrgev(fits$gev, nmom = 5),
  gno = lmrgno(fits$gno, nmom = 5),
  pe3 = lmrpe3(fits$pe3, nmom = 5)
)

# --- Bootstrap SE of sample t4 (and t3) -------------------------------------
set.seed(20260407)
B <- 1000
boot_t3 <- numeric(B)
boot_t4 <- numeric(B)
for (b in seq_len(B)) {
  s <- sample(ref, n_ref, replace = TRUE)
  lm_b <- samlmu(s, nmom = 4)
  boot_t3[b] <- lm_b["t_3"]
  boot_t4[b] <- lm_b["t_4"]
}
se_t3 <- sd(boot_t3)
se_t4 <- sd(boot_t4)
cat(sprintf("\nBootstrap (B=%d) SE of sample t3: %.5f\n", B, se_t3))
cat(sprintf("Bootstrap (B=%d) SE of sample t4: %.5f\n", B, se_t4))

# --- Hosking Z statistic ----------------------------------------------------
gof <- data.frame(
  dist        = names(fits),
  t4_model    = sapply(lmrs, function(x) x["tau_4"]),
  t4_sample   = unname(lmoms_sample["t_4"]),
  delta_t4    = sapply(lmrs, function(x) x["tau_4"] - lmoms_sample["t_4"]),
  Z_hosking   = sapply(lmrs, function(x) (x["tau_4"] - lmoms_sample["t_4"]) / se_t4),
  abs_Z       = sapply(lmrs, function(x) abs((x["tau_4"] - lmoms_sample["t_4"]) / se_t4)),
  acceptable_90 = sapply(lmrs,
                         function(x) abs((x["tau_4"] - lmoms_sample["t_4"]) / se_t4) <= 1.64),
  row.names = NULL
)
gof <- gof[order(gof$abs_Z), ]
cat("\nHosking goodness-of-fit (smaller |Z| is better):\n")
print(gof, row.names = FALSE)

# --- L-moment ratio diagram -------------------------------------------------
# For each distribution, trace the (t3, t4) curve as t3 varies over a grid.
# Three-parameter location-scale-shape distributions have a one-to-one map
# from t3 to t4 (since location and scale don't affect L-moment ratios).
make_curve <- function(pel_fun, lmr_fun, t3_grid) {
  l1 <- 0; l2 <- 1
  out <- data.frame(t3 = t3_grid, t4 = NA_real_)
  for (i in seq_along(t3_grid)) {
    pars <- tryCatch(pel_fun(c(l1, l2, t3_grid[i])), error = function(e) NULL)
    if (is.null(pars)) next
    lm <- tryCatch(lmr_fun(pars, nmom = 4), error = function(e) NULL)
    if (is.null(lm)) next
    out$t4[i] <- lm["t_4"]
  }
  out
}

t3_grid <- seq(-0.5, 0.95, by = 0.005)
curves <- bind_rows(
  cbind(dist = "GPA", make_curve(pelgpa, lmrgpa, t3_grid)),
  cbind(dist = "GLO", make_curve(pelglo, lmrglo, t3_grid)),
  cbind(dist = "GEV", make_curve(pelgev, lmrgev, t3_grid)),
  cbind(dist = "GNO", make_curve(pelgno, lmrgno, t3_grid)),
  cbind(dist = "PE3", make_curve(pelpe3, lmrpe3, t3_grid))
)
curves <- curves %>% filter(!is.na(t4))

# Bootstrap cloud for the sample (t3, t4) point
boot_pts <- data.frame(t3 = boot_t3, t4 = boot_t4)
samp_pt  <- data.frame(t3 = unname(lmoms_sample["t_3"]),
                        t4 = unname(lmoms_sample["t_4"]))

p_lmr <- ggplot() +
  geom_line(data = curves, aes(t3, t4, color = dist)) +
  geom_point(data = boot_pts, aes(t3, t4),
             color = "grey60", alpha = 0.3, size = 0.6) +
  geom_point(data = samp_pt, aes(t3, t4),
             color = "black", size = 3, shape = 18) +
  coord_cartesian(xlim = c(-0.2, 0.4), ylim = c(0.0, 0.35)) +
  labs(x = "L-skewness  t3", y = "L-kurtosis  t4",
       color = "distribution",
       title = "L-moment ratio diagram (reference distribution)",
       subtitle = "Black diamond: sample (t3, t4). Grey: 1000 bootstrap resamples.") +
  theme_minimal()

ggsave(file.path(out_dir, "diag_lmr_diagram.png"),
       p_lmr, width = 8, height = 6, dpi = 150)

# --- Q-Q plots --------------------------------------------------------------
q_emp <- (seq_len(n_ref) - 0.5) / n_ref
qq_df <- bind_rows(
  data.frame(dist = "GPA", emp = ref, theo = quagpa(q_emp, fits$gpa)),
  data.frame(dist = "GLO", emp = ref, theo = quaglo(q_emp, fits$glo)),
  data.frame(dist = "GEV", emp = ref, theo = quagev(q_emp, fits$gev)),
  data.frame(dist = "GNO", emp = ref, theo = quagno(q_emp, fits$gno)),
  data.frame(dist = "PE3", emp = ref, theo = quape3(q_emp, fits$pe3))
)

p_qq <- ggplot(qq_df, aes(theo, emp)) +
  geom_point(size = 0.4, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(~ dist, scales = "free") +
  labs(x = "theoretical quantile", y = "empirical quantile",
       title = "Q-Q plots: reference distribution vs fitted distributions") +
  theme_minimal()

ggsave(file.path(out_dir, "diag_lmr_qq.png"),
       p_qq, width = 9, height = 6, dpi = 150)

cat("\nSaved:\n",
    "  ", file.path(out_dir, "diag_lmr_diagram.png"), "\n",
    "  ", file.path(out_dir, "diag_lmr_qq.png"), "\n", sep = "")

saveRDS(list(lmoms_sample = lmoms_sample, fits = fits, lmrs = lmrs,
             gof = gof, se_t3 = se_t3, se_t4 = se_t4),
        file.path(out_dir, "diag_lmr_results.rds"))
