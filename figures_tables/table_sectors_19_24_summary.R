###############################################################################
# figures_tables/table_sectors_19_24_summary.R
#
# PURPOSE
#   Descriptive statistics for firms in NACE 19 (Oil Refining) and NACE 24
#   (Basic Metals) split by EU ETS status, showing that non-ETS firms are a
#   meaningful share of these sectors.
#
#   Columns: Firms, Firm-years, Mean revenue (M EUR), Share of sector revenue.
#
# INPUTS
#   {PROC_DATA}/training_sample.RData
#
# OUTPUTS
#   {OUTPUT_DIR}/sectors_19_24_summary.tex
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

library(dplyr)

# ── Load data ────────────────────────────────────────────────────────────────
load(file.path(PROC_DATA, "training_sample.RData"))

panel <- training_sample %>%
  filter(nace2d %in% c(19, 24)) %>%
  select(vat, year, nace2d, euets, turnover_VAT)
rm(training_sample)

# ── Compute summary stats ────────────────────────────────────────────────────
stats <- panel %>%
  mutate(group = ifelse(euets == 1, "ETS", "Non-ETS")) %>%
  group_by(nace2d, group) %>%
  summarise(
    firms    = n_distinct(vat),
    obs      = n(),
    mean_rev = mean(turnover_VAT, na.rm = TRUE),
    total_rev = sum(turnover_VAT, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(nace2d) %>%
  mutate(share_rev = total_rev / sum(total_rev) * 100) %>%
  ungroup()

# ── Formatting helpers ───────────────────────────────────────────────────────
fmt_int <- function(x) format(x, big.mark = ",")
fmt_rev <- function(x) formatC(x / 1e6, format = "f", digits = 0, big.mark = ",")
fmt_pct <- function(x) sprintf("%.1f", x)

get_val <- function(nace, grp) {
  stats %>% filter(nace2d == nace, group == grp)
}

s19_ets    <- get_val(19, "ETS")
s19_nonets <- get_val(19, "Non-ETS")
s24_ets    <- get_val(24, "ETS")
s24_nonets <- get_val(24, "Non-ETS")

# ── Build LaTeX table ────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{l cccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Oil Refining (NACE 19)} & \\multicolumn{2}{c}{Basic Metals (NACE 24)} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & ETS & Non-ETS & ETS & Non-ETS \\\\",
  "\\midrule",
  sprintf("\\# Firms & %s & %s & %s & %s \\\\",
          fmt_int(s19_ets$firms), fmt_int(s19_nonets$firms),
          fmt_int(s24_ets$firms), fmt_int(s24_nonets$firms)),
  sprintf("\\# Firm-years & %s & %s & %s & %s \\\\",
          fmt_int(s19_ets$obs), fmt_int(s19_nonets$obs),
          fmt_int(s24_ets$obs), fmt_int(s24_nonets$obs)),
  sprintf("Mean revenue (M\\euro) & %s & %s & %s & %s \\\\",
          fmt_rev(s19_ets$mean_rev), fmt_rev(s19_nonets$mean_rev),
          fmt_rev(s24_ets$mean_rev), fmt_rev(s24_nonets$mean_rev)),
  sprintf("Revenue share (\\%%) & %s & %s & %s & %s \\\\",
          fmt_pct(s19_ets$share_rev), fmt_pct(s19_nonets$share_rev),
          fmt_pct(s24_ets$share_rev), fmt_pct(s24_nonets$share_rev)),
  "\\bottomrule",
  "\\end{tabular}"
)

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "sectors_19_24_summary.tex")
writeLines(tex, out_path)
cat("Saved to:", out_path, "\n")

# ── Print to console ────────────────────────────────────────────────────────
cat("\n"); print(stats %>% select(nace2d, group, firms, obs, mean_rev, share_rev))
