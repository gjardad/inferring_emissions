###############################################################################
# figures_tables/fig_proxy_density_by_emitter.R
#
# PURPOSE
#   Kernel density of the EN-selected and Tabachova proxies, separately for
#   EU ETS emitters and non-ETS firms, within sectors 19, 24, and 17/18.
#   3x2 panel: rows = sector (19, 24, 17/18), columns = proxy type (EN, Tabachova).
#   Each subplot shows EU ETS vs Non-ETS densities scaled to peak = 1.
#
# INPUTS
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
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
cat("Loading firm-year panel with proxies...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

panel <- training_sample %>%
  mutate(fold_specific_proxy_all_asinh = pmax(coalesce(fold_specific_proxy_all_asinh, 0), 0),
         proxy_tabachova               = asinh(pmax(coalesce(proxy_tabachova, 0), 0)),
         nace2d = as.character(nace2d))
rm(training_sample)

# ── Restrict to sectors 19, 24, 17/18 and reshape to long ───────────────────
df <- panel %>%
  filter(nace2d %in% c("17", "18", "19", "24")) %>%
  mutate(
    sector = ifelse(nace2d %in% c("17", "18"), "17/18", nace2d),
    group  = ifelse(euets == 1, "EU ETS", "Non-ETS")
  ) %>%
  select(vat, year, sector, group,
         EN  = fold_specific_proxy_all_asinh,
         Tab = proxy_tabachova) %>%
  pivot_longer(cols = c(EN, Tab),
               names_to = "proxy_type", values_to = "proxy_value") %>%
  mutate(
    # Order: rows = 19, 24, 17/18; cols = EN, Tabachova
    sector     = factor(sector, levels = c("19", "24", "17/18")),
    proxy_type = factor(proxy_type, levels = c("EN", "Tab")),
    panel_label = factor(paste0("(", letters[
      (as.integer(sector) - 1) * 2 + as.integer(proxy_type)
    ], ")"), levels = paste0("(", letters[1:6], ")"))
  )

cat("Obs per sector (pre-pivot):\n")
print(df %>% group_by(sector) %>% summarise(n = n() / 2))

# ── Compute densities bounded at zero ──────────────────────────────────────
# Use density(from = 0) so the kernel doesn't bleed below zero,
# then scale each curve so its peak equals 1.
dens_df <- df %>%
  group_by(panel_label, group) %>%
  do({
    vals <- .$proxy_value
    d <- density(vals, adjust = 1.2, from = 0, n = 512)
    data.frame(x = d$x, y = d$y)
  }) %>%
  ungroup() %>%
  group_by(panel_label, group) %>%
  mutate(y_scaled = y / max(y)) %>%
  ungroup()

# ── 3x2 faceted plot ────────────────────────────────────────────────────────
p <- ggplot(dens_df, aes(x = x, y = y_scaled, linetype = group, color = group)) +
  geom_line(linewidth = 0.9, key_glyph = "path") +
  scale_linetype_manual(values = c("EU ETS" = "solid", "Non-ETS" = "dashed")) +
  scale_color_manual(values = c("EU ETS" = "black", "Non-ETS" = "grey50")) +
  guides(linetype = guide_legend(keywidth = 2.5), color = guide_legend(keywidth = 2.5)) +
  facet_wrap(~ panel_label, scales = "free_x", ncol = 2) +
  labs(
    x        = "Fuel-supply proxy",
    y        = "Density",
    linetype = NULL,
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 14),
    panel.grid.minor  = element_blank(),
    axis.title.x      = element_text(size = 15, margin = margin(t = 15)),
    axis.title.y      = element_text(size = 15, margin = margin(r = 10)),
    strip.text        = element_text(size = 14, face = "bold"),
    plot.margin       = margin(10, 15, 10, 10)
  )

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

ggsave(file.path(OUTPUT_DIR, "proxy_density_by_emitter.pdf"),
       p, width = 9, height = 9, dpi = 300)
cat("Saved:", file.path(OUTPUT_DIR, "proxy_density_by_emitter.pdf"), "\n")
