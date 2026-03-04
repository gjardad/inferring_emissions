###############################################################################
# analysis/model_selection/losocv/fig_proxy_vs_emissions_by_sector.R
#
# PURPOSE
#   Scatter of proxy_weighted vs emissions (y), faceted by NACE 2-digit sector.
#   Shows that the proxy-to-emissions relationship varies dramatically across
#   sectors — a single regression coefficient cannot capture this, which is
#   why row 3 (EN + proxy) shows only modest improvement over row 2.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/fig_proxy_vs_emissions_by_sector.pdf
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


# ── Load data ────────────────────────────────────────────────────────────────
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)


# ── Prepare plot data ────────────────────────────────────────────────────────
# Focus on emitters (y > 0) so the slope is meaningful
# Also keep non-emitters with proxy > 0 to show false positives
plot_df <- panel %>%
  filter(y > 0 | proxy_weighted > 0) %>%
  mutate(
    asinh_proxy = asinh(proxy_weighted),
    asinh_y     = asinh(y),
    emitter     = ifelse(y > 0, "Emitter", "Non-emitter")
  )

# Count firms per sector for labelling
sec_counts <- plot_df %>%
  group_by(nace2d) %>%
  summarise(n = n(), n_emit = sum(y > 0), .groups = "drop") %>%
  mutate(label = sprintf("%s (n=%d, emit=%d)", nace2d, n, n_emit))

plot_df <- plot_df %>%
  left_join(sec_counts %>% select(nace2d, label), by = "nace2d")

# Keep only sectors with at least 5 emitters for readable facets
keep_sectors <- sec_counts %>% filter(n_emit >= 5) %>% pull(nace2d)
plot_df <- plot_df %>% filter(nace2d %in% keep_sectors)


# ── Plot ─────────────────────────────────────────────────────────────────────
p <- ggplot(plot_df, aes(x = asinh_proxy, y = asinh_y)) +
  geom_point(aes(colour = emitter), alpha = 0.3, size = 0.8) +
  geom_smooth(
    data = plot_df %>% filter(y > 0, proxy_weighted > 0),
    method = "lm", se = FALSE, colour = "black", linewidth = 0.5
  ) +
  facet_wrap(~ label, scales = "free") +
  scale_colour_manual(values = c("Emitter" = "#2166AC", "Non-emitter" = "#B2182B")) +
  labs(
    x = "asinh(proxy_weighted)",
    y = "asinh(emissions)",
    colour = NULL,
    title = "Proxy vs emissions by sector",
    subtitle = "Slope varies across sectors — a single coefficient cannot capture this"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 7),
    plot.title = element_text(face = "bold")
  )

out_path <- file.path(OUT_DIR, "fig_proxy_vs_emissions_by_sector.pdf")
ggsave(out_path, p, width = 14, height = 10)
cat(sprintf("Saved: %s\n", out_path))
