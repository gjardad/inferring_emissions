###############################################################################
# figures_tables/fig_binscatter_levels_vs_ranks.R
#
# PURPOSE
#   Two-panel binscatter showing the proxy is a better ranking signal than
#   a level predictor.
#
#   Panel A: proxy vs asinh(emissions) pooled across sectors.
#            Shows positive but dispersed relationship — same proxy value
#            maps to very different emission levels across sectors.
#
#   Panel B: within-sector-year proxy rank vs within-sector-year emissions
#            rank. Shows a much tighter relationship closer to the 45-degree
#            line.
#
#   Uses the cross-validated (fold-specific) proxy for honesty.
#   Restricted to EU ETS firms with positive emissions and positive proxy.
#   Within-sector-year ranks computed only for sector-years with >= 5 firms.
#
# INPUTS
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/fold_specific_proxy_asinh.RData
#
# OUTPUTS
#   {OUTPUT_DIR}/binscatter_levels.pdf
#   {OUTPUT_DIR}/binscatter_ranks.pdf
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
  mutate(fold_specific_proxy_asinh = coalesce(fold_specific_proxy_asinh, 0))
rm(training_sample, fs_proxy_panel_asinh)

# Restrict to EU ETS firms with positive emissions and positive proxy
df <- panel %>%
  filter(euets == 1, y > 0, fold_specific_proxy_asinh > 0) %>%
  mutate(
    asinh_y     = asinh(y),
    asinh_proxy = fold_specific_proxy_asinh  # already in asinh scale
  )

cat(sprintf("Binscatter sample: %d obs, %d firms, %d sectors\n",
            nrow(df), n_distinct(df$vat), n_distinct(df$nace2d)))

# ── Panel A: Levels binscatter ───────────────────────────────────────────────
# Bin proxy into 20 equal-sized bins, compute mean proxy and mean emissions
n_bins <- 20

df_a <- df %>%
  mutate(bin = ntile(asinh_proxy, n_bins)) %>%
  group_by(bin) %>%
  summarise(
    mean_proxy = mean(asinh_proxy),
    mean_y     = mean(asinh_y),
    n          = n(),
    .groups    = "drop"
  )

p_a <- ggplot(df_a, aes(x = mean_proxy, y = mean_y)) +
  geom_point(color = "gray30", size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 0.8) +
  labs(
    x = "Proxy (binned mean)",
    y = "asinh(emissions) (binned mean)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  )

# ── Panel B: Within-sector-year ranks binscatter ─────────────────────────────
# Compute ranks within sector-year, restrict to sector-years with >= 5 firms
min_firms <- 5

df_b <- df %>%
  group_by(nace2d, year) %>%
  mutate(n_sy = n()) %>%
  filter(n_sy >= min_firms) %>%
  mutate(
    rank_proxy = rank(asinh_proxy) / n_sy,
    rank_y     = rank(y) / n_sy
  ) %>%
  ungroup()

cat(sprintf("Rank sample (sector-years with >= %d firms): %d obs, %d sector-years\n",
            min_firms, nrow(df_b), n_distinct(paste(df_b$nace2d, df_b$year))))

# Bin proxy rank into 20 equal-sized bins
df_b_binned <- df_b %>%
  mutate(bin = ntile(rank_proxy, n_bins)) %>%
  group_by(bin) %>%
  summarise(
    mean_rank_proxy = mean(rank_proxy),
    mean_rank_y     = mean(rank_y),
    n               = n(),
    .groups         = "drop"
  )

p_b <- ggplot(df_b_binned, aes(x = mean_rank_proxy, y = mean_rank_y)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(color = "gray30", size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 0.8) +
  labs(
    x = "Within-sector proxy rank (binned mean)",
    y = "Within-sector emissions rank (binned mean)"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  )

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

ggsave(file.path(OUTPUT_DIR, "binscatter_levels.pdf"),
       p_a, width = 5, height = 4.5)
cat("Saved Panel A:", file.path(OUTPUT_DIR, "binscatter_levels.pdf"), "\n")

ggsave(file.path(OUTPUT_DIR, "binscatter_ranks.pdf"),
       p_b, width = 5, height = 4.5)
cat("Saved Panel B:", file.path(OUTPUT_DIR, "binscatter_ranks.pdf"), "\n")

# ── Print diagnostics ────────────────────────────────────────────────────────
# Pooled R² (levels)
r2_levels <- summary(lm(asinh_y ~ asinh_proxy, data = df))$r.squared
# Pooled R² (within-sector ranks)
r2_ranks  <- summary(lm(rank_y ~ rank_proxy, data = df_b))$r.squared
# Spearman rho (within-sector, pooled)
rho_ranks <- cor(df_b$rank_proxy, df_b$rank_y, method = "spearman")

cat(sprintf("\nPooled R² (asinh levels): %.3f\n", r2_levels))
cat(sprintf("Pooled R² (within-sector ranks): %.3f\n", r2_ranks))
cat(sprintf("Spearman rho (within-sector ranks): %.3f\n", rho_ranks))
