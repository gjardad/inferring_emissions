###############################################################################
# figures_tables/fig_proxy_density_slide.R
#
# PURPOSE
#   CONDENSED versions of fig_proxy_density_by_emitter.R for the defense slides.
#   Panels are labeled descriptively ("B2B proxy" / "NACE proxy") rather than
#   (a)/(b), since the slides have no figure note.
#
#   Produces TWO figures:
#     1. proxy_density_slide.pdf     -> main slide: sector 19 only  (1 x 2)
#     2. proxy_density_appendix.pdf  -> appendix:  sectors 24, 17/18 (2 x 2)
#
#   Each panel plots EU ETS vs non-ETS densities of the proxy (scaled to
#   peak = 1), bounded at zero.
#
# INPUTS
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUTS  (written to {REPO_DIR}/output_rmd, created if missing; then copy
#           both PDFs into the defense-slides repo under figures/)
#   {REPO_DIR}/output_rmd/proxy_density_slide.pdf
#   {REPO_DIR}/output_rmd/proxy_density_appendix.pdf
#
# HOW TO RUN
#   Rscript figures_tables/fig_proxy_density_slide.R
#   (or open in RStudio and Source the whole file -- do not run line-by-line,
#    so REPO_DIR resolves from the script path.)
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

# Write to a dedicated folder inside the repo (created if missing); copy the
# two PDFs into the defense-slides repo (figures/) afterwards.
SLIDES_FIG_DIR <- file.path(REPO_DIR, "output_rmd")

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

# ── Reshape sectors 17/18, 19, 24 to long ───────────────────────────────────
df_all <- panel %>%
  filter(nace2d %in% c("17", "18", "19", "24")) %>%
  mutate(
    sector = ifelse(nace2d %in% c("17", "18"), "17/18", nace2d),
    group  = ifelse(euets == 1, "EU ETS", "Non-ETS")
  ) %>%
  select(vat, year, sector, group,
         `B2B proxy`  = fold_specific_proxy_all_asinh,
         `NACE proxy` = proxy_tabachova) %>%
  pivot_longer(cols = c(`B2B proxy`, `NACE proxy`),
               names_to = "proxy_type", values_to = "proxy_value") %>%
  mutate(proxy_type = factor(proxy_type, levels = c("B2B proxy", "NACE proxy")))

# ── Density helper: bounded at zero, scaled to peak = 1 ─────────────────────
compute_dens <- function(d) {
  d %>%
    group_by(sector, proxy_type, group) %>%
    do({
      dd <- density(.$proxy_value, adjust = 1.2, from = 0, n = 512)
      data.frame(x = dd$x, y = dd$y)
    }) %>%
    ungroup() %>%
    group_by(sector, proxy_type, group) %>%
    mutate(y_scaled = y / max(y)) %>%
    ungroup()
}

base_layers <- function() {
  list(
    geom_line(aes(linetype = group, color = group), linewidth = 0.9, key_glyph = "path"),
    scale_linetype_manual(values = c("EU ETS" = "solid", "Non-ETS" = "dashed")),
    scale_color_manual(values = c("EU ETS" = "black", "Non-ETS" = "grey50")),
    guides(linetype = guide_legend(keywidth = 2.5), color = guide_legend(keywidth = 2.5)),
    labs(x = "Fuel-supply proxy", y = "Density", linetype = NULL, color = NULL),
    theme_minimal(base_size = 13),
    theme(
      legend.position   = "bottom",
      legend.text       = element_text(size = 14),
      panel.grid.minor  = element_blank(),
      axis.title.x      = element_text(size = 14, margin = margin(t = 10)),
      axis.title.y      = element_text(size = 14, margin = margin(r = 8)),
      strip.text        = element_text(size = 14, face = "bold"),
      plot.margin       = margin(8, 12, 6, 8)
    )
  )
}

if (!dir.exists(SLIDES_FIG_DIR)) dir.create(SLIDES_FIG_DIR, recursive = TRUE)

# ── (1) Main slide: sector 19, one row of two panels ────────────────────────
dens19 <- compute_dens(filter(df_all, sector == "19"))
p19 <- ggplot(dens19, aes(x = x, y = y_scaled)) +
  base_layers() +
  facet_wrap(~ proxy_type, scales = "free_x", ncol = 2)
ggsave(file.path(SLIDES_FIG_DIR, "proxy_density_slide.pdf"),
       p19, width = 9, height = 3.4, dpi = 300)
cat("Saved:", file.path(SLIDES_FIG_DIR, "proxy_density_slide.pdf"), "\n")

# ── (2) Appendix: sectors 24 and 17/18, 2 x 2 grid ──────────────────────────
sector_labs <- c("24" = "Iron & steel (24)", "17/18" = "Paper & print (17/18)")
densApp <- compute_dens(filter(df_all, sector %in% c("24", "17/18"))) %>%
  mutate(sector = factor(sector, levels = c("24", "17/18")))
pApp <- ggplot(densApp, aes(x = x, y = y_scaled)) +
  base_layers() +
  facet_grid(sector ~ proxy_type, scales = "free_x",
             labeller = labeller(sector = sector_labs))
ggsave(file.path(SLIDES_FIG_DIR, "proxy_density_appendix.pdf"),
       pApp, width = 9, height = 6, dpi = 300)
cat("Saved:", file.path(SLIDES_FIG_DIR, "proxy_density_appendix.pdf"), "\n")
