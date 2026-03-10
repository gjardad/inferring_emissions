###############################################################################
# figures_tables/fig_proxy_density_by_emitter.R
#
# PURPOSE
#   Kernel density of the EN and Tabachova proxies, separately for EU ETS
#   emitters and non-ETS firms, within sectors 19 and 24.
#   2x2 panel: rows = proxy type (EN, Tabachova), columns = sector (19, 24).
#   Each panel shows EU ETS vs Non-ETS densities.
#
# INPUTS
#   {PROC_DATA}/training_sample.RData        (contains proxy_tabachova_asinh)
#   {PROC_DATA}/fold_specific_proxy_asinh.RData
#
# OUTPUTS
#   {OUTPUT_DIR}/proxy_density_by_emitter.pdf
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
library(tidyr)
library(ggplot2)

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

cat("Loading fold-specific proxy (asinh LHS)...\n")
load(file.path(PROC_DATA, "fold_specific_proxy_asinh.RData"))

# Merge
panel <- training_sample %>%
  left_join(fs_proxy_panel_asinh %>% select(vat, year, fold_specific_proxy_asinh),
            by = c("vat", "year")) %>%
  mutate(fold_specific_proxy_asinh = coalesce(fold_specific_proxy_asinh, 0),
         proxy_tabachova_asinh     = coalesce(proxy_tabachova_asinh, 0))
rm(training_sample, fs_proxy_panel_asinh)

# ── Restrict to sectors 19 and 24, reshape to long ─────────────────────────
df <- panel %>%
  filter(nace2d %in% c("19", "24") | nace2d %in% c(19, 24)) %>%
  mutate(
    nace2d = as.character(nace2d),
    group  = ifelse(euets == 1, "EU ETS", "Non-ETS")
  ) %>%
  select(vat, year, nace2d, group,
         `EN-selected` = fold_specific_proxy_asinh,
         `Fuel-related` = proxy_tabachova_asinh) %>%
  pivot_longer(cols = c(`EN-selected`, `Fuel-related`),
               names_to = "proxy_type", values_to = "proxy_value") %>%
  mutate(
    sector_short = ifelse(nace2d == "19", "Oil Refining", "Basic Metals"),
    panel_label  = paste0("(", c("a","b","c","d")[
      match(paste(proxy_type, sector_short),
            c("EN-selected Oil Refining", "EN-selected Basic Metals",
              "Fuel-related Oil Refining", "Fuel-related Basic Metals"))
    ], ")")
  )

cat("Sector 19:", sum(df$nace2d == "19") / 2, "obs;",
    "Sector 24:", sum(df$nace2d == "24") / 2, "obs\n")

# ── 2x2 faceted plot ─────────────────────────────────────────────────────────
p <- ggplot(df, aes(x = proxy_value, linetype = group, color = group)) +
  geom_density(aes(y = after_stat(scaled)),
               linewidth = 0.9, adjust = 1.2,
               key_glyph = "path") +
  scale_linetype_manual(values = c("EU ETS" = "solid", "Non-ETS" = "dashed")) +
  scale_color_manual(values = c("EU ETS" = "black", "Non-ETS" = "grey50")) +
  guides(linetype = guide_legend(keywidth = 2.5), color = guide_legend(keywidth = 2.5)) +
  facet_wrap(~ panel_label, scales = "free", ncol = 2) +
  labs(
    x        = "Fuel-supply proxy",
    y        = NULL,
    linetype = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 14),
    panel.grid.minor  = element_blank(),
    axis.title.x      = element_text(size = 15, margin = margin(t = 15)),
    strip.text        = element_text(size = 14, face = "bold"),
    plot.margin       = margin(10, 15, 10, 10)
  )

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

ggsave(file.path(OUTPUT_DIR, "proxy_density_by_emitter.pdf"),
       p, width = 9, height = 6, dpi = 300)
cat("Saved:", file.path(OUTPUT_DIR, "proxy_density_by_emitter.pdf"), "\n")
