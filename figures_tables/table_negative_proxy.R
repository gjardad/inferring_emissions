###############################################################################
# figures_tables/table_negative_proxy.R
#
# PURPOSE
#   Justify flooring negative proxy values to zero by showing that negative
#   proxy values are almost entirely driven by zero-emission sector firms.
#
#   Three-row table: proxy sign group (negative, zero, positive) with
#   columns for N, % in zero-emission sectors, % emitters, and median
#   emissions conditional on being an emitter.
#
# INPUTS
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUTS
#   {OUTPUT_DIR}/table_negative_proxy.tex
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(data.table)

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading firm-year panel with proxies...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

dt <- as.data.table(training_sample)
rm(training_sample)

# Zero-emission sectors: NACE 17, 18, 19, 24
zero_sectors <- c("17", "18", "19", "24")

# ── Proxy sign groups ───────────────────────────────────────────────────────
dt[, proxy_group := fifelse(
  fold_specific_proxy_all_asinh < 0, "Negative",
  fifelse(fold_specific_proxy_all_asinh == 0, "Zero", "Positive")
)]

# Order factor for display
dt[, proxy_group := factor(proxy_group, levels = c("Negative", "Zero", "Positive"))]

# ── Compute summary ─────────────────────────────────────────────────────────
tab <- dt[, .(
  n              = .N,
  pct_zero_sect  = round(100 * mean(primary_nace2d %in% zero_sectors), 1),
  pct_emitters   = round(100 * mean(emit == 1), 1),
  median_y_emit  = {
    ys <- y[emit == 1]
    if (length(ys) == 0) NA_real_ else round(median(ys, na.rm = TRUE), 0)
  }
), by = proxy_group][order(proxy_group)]

cat("\n=== Summary by proxy sign ===\n")
print(tab)

# ── Build LaTeX table ────────────────────────────────────────────────────────
fmt_n <- function(x) format(x, big.mark = ",")
fmt_pct <- function(x) sprintf("%.1f", x)
fmt_med <- function(x) ifelse(is.na(x), "---", fmt_n(x))

tex <- c(
  "\\begin{tabular}{l cccc}",
  "\\toprule",
  "Proxy sign & $N$ & \\% zero-emission sectors & \\% emitters & Median $y \\mid$ emit \\\\",
  "\\midrule",
  sprintf("Negative & %s & %s & %s & %s \\\\",
          fmt_n(tab[proxy_group == "Negative", n]),
          fmt_pct(tab[proxy_group == "Negative", pct_zero_sect]),
          fmt_pct(tab[proxy_group == "Negative", pct_emitters]),
          fmt_med(tab[proxy_group == "Negative", median_y_emit])),
  sprintf("Zero & %s & %s & %s & %s \\\\",
          fmt_n(tab[proxy_group == "Zero", n]),
          fmt_pct(tab[proxy_group == "Zero", pct_zero_sect]),
          fmt_pct(tab[proxy_group == "Zero", pct_emitters]),
          fmt_med(tab[proxy_group == "Zero", median_y_emit])),
  sprintf("Positive & %s & %s & %s & %s \\\\",
          fmt_n(tab[proxy_group == "Positive", n]),
          fmt_pct(tab[proxy_group == "Positive", pct_zero_sect]),
          fmt_pct(tab[proxy_group == "Positive", pct_emitters]),
          fmt_med(tab[proxy_group == "Positive", median_y_emit])),
  "\\bottomrule",
  "\\end{tabular}"
)

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "table_negative_proxy.tex")
writeLines(tex, out_path)
cat("\nSaved table to:", out_path, "\n")
