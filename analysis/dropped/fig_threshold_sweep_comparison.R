###############################################################################
# analysis/model_selection/losocv/fig_threshold_sweep_comparison.R
#
# PURPOSE
#   Compare threshold sweeps for row 4 (hurdle × EN) vs row 5 (hurdle × proxy
#   allocation). Row 4's RMSE is flat — the threshold has no effect because
#   all EN predictions are positive. Row 5 shows a clear optimum — the proxy
#   ranking gives the hurdle real discriminatory power.
#
# INPUT
#   {OUTPUT_DIR}/model_selection/losocv/row4_threshold_sweep.csv
#   {OUTPUT_DIR}/model_selection/losocv/row5_threshold_sweep.csv
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/fig_threshold_sweep_comparison.pdf
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
  REPO_DIR <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(ggplot2)
library(tidyr)

OUT_DIR <- file.path(OUTPUT_DIR, "model_selection", "losocv")


# ── Load threshold sweeps ────────────────────────────────────────────────────
row4 <- read.csv(file.path(OUT_DIR, "row4_threshold_sweep.csv")) %>%
  mutate(model = "Row 4: Hurdle × EN")

row5 <- read.csv(file.path(OUT_DIR, "row5_threshold_sweep.csv")) %>%
  mutate(model = "Row 5: Hurdle × Proxy allocation")

sweep <- bind_rows(row4, row5)

cat(sprintf("Row 4 nRMSE range: [%.6f, %.6f] (spread: %.2e)\n",
            min(row4$nrmse_sd), max(row4$nrmse_sd),
            max(row4$nrmse_sd) - min(row4$nrmse_sd)))
cat(sprintf("Row 5 nRMSE range: [%.6f, %.6f] (spread: %.2e)\n",
            min(row5$nrmse_sd), max(row5$nrmse_sd),
            max(row5$nrmse_sd) - min(row5$nrmse_sd)))


# ── Panel A: nRMSE vs threshold ─────────────────────────────────────────────
p1 <- ggplot(sweep, aes(x = threshold, y = nrmse_sd, colour = model)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c(
    "Row 4: Hurdle × EN" = "#B2182B",
    "Row 5: Hurdle × Proxy allocation" = "#2166AC"
  )) +
  labs(
    x = "Classification threshold (τ)",
    y = "nRMSE",
    colour = NULL,
    title = "Threshold sweep: nRMSE",
    subtitle = "Row 4 is flat — the EN always predicts positive, so τ is irrelevant"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


# ── Panel B: FPR and TPR vs threshold ────────────────────────────────────────
rates <- sweep %>%
  select(threshold, model, fpr, tpr) %>%
  pivot_longer(cols = c(fpr, tpr), names_to = "rate", values_to = "value") %>%
  mutate(rate = ifelse(rate == "fpr", "FPR", "TPR"))

p2 <- ggplot(rates, aes(x = threshold, y = value, colour = model, linetype = rate)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c(
    "Row 4: Hurdle × EN" = "#B2182B",
    "Row 5: Hurdle × Proxy allocation" = "#2166AC"
  )) +
  scale_linetype_manual(values = c("FPR" = "dashed", "TPR" = "solid")) +
  labs(
    x = "Classification threshold (τ)",
    y = "Rate",
    colour = NULL,
    linetype = NULL,
    title = "Threshold sweep: FPR and TPR",
    subtitle = "Row 4 FPR stays near 1 across all thresholds; Row 5 achieves meaningful FPR reduction"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


# ── Save ─────────────────────────────────────────────────────────────────────
out1 <- file.path(OUT_DIR, "fig_threshold_sweep_nrmse.pdf")
ggsave(out1, p1, width = 8, height = 5)
cat(sprintf("Saved: %s\n", out1))

out2 <- file.path(OUT_DIR, "fig_threshold_sweep_rates.pdf")
ggsave(out2, p2, width = 8, height = 5)
cat(sprintf("Saved: %s\n", out2))
