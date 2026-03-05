###############################################################################
# analysis/model_selection/losocv/fig_en_prediction_density.R
#
# PURPOSE
#   Density plot of row 3 (EN + proxy) predictions, split by emitter status.
#   Shows that the EN produces strictly positive predictions for virtually
#   all observations, making it impossible to discriminate emitters from
#   non-emitters — which is why the hurdle (row 4) adds nothing.
#
# INPUT
#   {OUTPUT_DIR}/model_selection/losocv/row3_preds.csv
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/fig_en_prediction_density.pdf
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

OUT_DIR <- file.path(OUTPUT_DIR, "model_selection", "losocv")


# ── Load predictions ─────────────────────────────────────────────────────────
preds <- read.csv(file.path(OUT_DIR, "row3_preds.csv"))
preds <- preds %>%
  mutate(
    emitter = ifelse(y > 0, "Emitter", "Non-emitter"),
    asinh_yhat = asinh(yhat)
  )

cat(sprintf("Predictions: %d obs (%d emitters, %d non-emitters)\n",
            nrow(preds), sum(preds$y > 0), sum(preds$y == 0)))
cat(sprintf("yhat > 0: %d / %d (%.1f%%)\n",
            sum(preds$yhat > 0), nrow(preds), 100 * mean(preds$yhat > 0)))
cat(sprintf("yhat range: [%.1f, %.1f]\n", min(preds$yhat), max(preds$yhat)))


# ── Plot: density of asinh(yhat) by emitter status ──────────────────────────
p <- ggplot(preds, aes(x = asinh_yhat, fill = emitter)) +
  geom_density(alpha = 0.5, colour = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = c("Emitter" = "#2166AC", "Non-emitter" = "#B2182B")) +
  labs(
    x = "asinh(predicted emissions from EN + proxy)",
    y = "Density",
    fill = NULL,
    title = "EN predictions overlap for emitters and non-emitters",
    subtitle = "Nearly all predictions are strictly positive — no threshold can separate the two groups"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

out_path <- file.path(OUT_DIR, "fig_en_prediction_density.pdf")
ggsave(out_path, p, width = 8, height = 5)
cat(sprintf("Saved: %s\n", out_path))


# ── Supplementary: zoom on the lower tail ────────────────────────────────────
p2 <- p +
  coord_cartesian(xlim = c(-1, quantile(preds$asinh_yhat, 0.25))) +
  labs(subtitle = "Zoomed into lower tail — still no separation")

out_path2 <- file.path(OUT_DIR, "fig_en_prediction_density_zoom.pdf")
ggsave(out_path2, p2, width = 8, height = 5)
cat(sprintf("Saved: %s\n", out_path2))
