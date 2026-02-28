###############################################################################
# fuel_suppliers/descriptives/plot_rho_vs_nfirms.R
#
# Scatter plot: within-sector pooled Spearman rho vs number of firms.
# Weighted proxy only. Red shaded region highlights N < 20, rho < 0.3.
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

library(ggplot2)

# ── Load data ────────────────────────────────────────────────────────────────
comp <- read.csv(file.path(OUTPUT_DIR, "rho_pooled_sector_comparison.csv"),
                 stringsAsFactors = FALSE)

df <- data.frame(
  nace2d  = comp$nace2d,
  n_firms = comp$n_firms_weighted,
  rho     = comp$rho_pooled_weighted,
  stringsAsFactors = FALSE
)
df <- df[!is.na(df$rho), ]

cat(sprintf("Plotting %d sectors (weighted proxy)\n", nrow(df)))

# ── Plot ─────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(x = n_firms, y = rho)) +
  # Red shaded region: N < 20, rho < 0.3
  annotate("rect",
           xmin = 1, xmax = 20, ymin = -1.1, ymax = 0.3,
           fill = "#D94040", alpha = 0.12) +
  # Points
  geom_point(color = "gray45", size = 2.5) +
  # Axis labels
  labs(x = "Number of firms",
       y = "Rank correlation") +
  # Log scale for x-axis (range is 2-385)
  scale_x_log10(
    breaks = c(2, 5, 10, 20, 50, 100, 200, 400),
    labels = c("2", "5", "10", "20", "50", "100", "200", "400")
  ) +
  scale_y_continuous(breaks = seq(-1, 1, 0.2)) +
  # Clean theme
  theme_minimal(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    axis.line        = element_line(color = "black", linewidth = 0.3),
    axis.ticks       = element_line(color = "black", linewidth = 0.3),
    axis.title.x     = element_text(margin = margin(t = 10)),
    axis.title.y     = element_text(margin = margin(r = 10))
  )

# ── Save ─────────────────────────────────────────────────────────────────────
out_pdf <- file.path(OUTPUT_DIR, "rho_vs_nfirms_weighted.pdf")
out_png <- file.path(OUTPUT_DIR, "rho_vs_nfirms_weighted.png")

ggsave(out_pdf, p, width = 6, height = 4.5)
ggsave(out_png, p, width = 6, height = 4.5, dpi = 300)

cat(sprintf("Saved: %s\n", out_pdf))
cat(sprintf("Saved: %s\n", out_png))
