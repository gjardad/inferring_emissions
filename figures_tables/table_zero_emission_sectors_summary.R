###############################################################################
# figures_tables/table_zero_emission_sectors_summary.R
#
# PURPOSE
#   Descriptive statistics for firms in zero-emission sectors (NACE 17, 18, 19,
#   24) split by EU ETS status, showing that non-ETS firms are a meaningful
#   share of these sectors.
#
#   Long-format table: each sector is a pair of rows (EU ETS / Non-ETS).
#   Columns: Firms, Firm-years, Mean revenue (M EUR), Share of sector revenue.
#
# INPUTS
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUTS
#   {OUTPUT_DIR}/zero_emission_sectors_summary.tex
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
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

panel <- training_sample %>%
  filter(nace2d %in% c(17, 18, 19, 24)) %>%
  select(vat, year, nace2d, euets, turnover_VAT)
rm(training_sample, syt)

# ── Assign sector groups (merge NACE 17+18) ──────────────────────────────────
panel <- panel %>%
  mutate(sector_group = case_when(
    nace2d %in% c(17, 18) ~ "ppp",
    nace2d == "19"         ~ "refining",
    nace2d == "24"         ~ "steel"
  ))

# ── Compute summary stats ────────────────────────────────────────────────────
stats <- panel %>%
  mutate(group = ifelse(euets == 1, "EU ETS", "Non-ETS")) %>%
  group_by(sector_group, group) %>%
  summarise(
    firms    = n_distinct(vat),
    obs      = n(),
    mean_rev = mean(turnover_VAT, na.rm = TRUE),
    total_rev = sum(turnover_VAT, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(sector_group) %>%
  mutate(share_rev = total_rev / sum(total_rev) * 100) %>%
  ungroup()

# ── Formatting helpers ───────────────────────────────────────────────────────
fmt_int <- function(x) format(x, big.mark = ",")
fmt_rev <- function(x) formatC(x / 1e6, format = "f", digits = 0, big.mark = ",")
fmt_pct <- function(x) sprintf("%.1f", x)

get_val <- function(sg, grp) {
  row <- stats %>% filter(sector_group == sg, group == grp)
  if (nrow(row) == 0) return(list(firms = 0, obs = 0, mean_rev = NA, share_rev = 0))
  row
}

# ── Sector definitions ──────────────────────────────────────────────────────
sectors <- list(
  list(key = "ppp",      label = "Paper, pulp \\& printing"),
  list(key = "refining", label = "Petroleum refining"),
  list(key = "steel",    label = "Iron \\& steel")
)

# ── Build LaTeX table ────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{ll ccc}",
  "\\toprule",
  "Sector & & N Firms & N Firm-years & Revenue share (\\%) \\\\",
  "\\midrule"
)

for (i in seq_along(sectors)) {
  s <- sectors[[i]]
  ets    <- get_val(s$key, "EU ETS")
  nonets <- get_val(s$key, "Non-ETS")

  has_ets <- ets$firms > 0

  if (has_ets) {
    tex <- c(tex,
      sprintf("\\multirow{2}{*}{%s} & EU ETS & %s & %s & %s \\\\",
              s$label,
              fmt_int(ets$firms), fmt_int(ets$obs),
              fmt_pct(ets$share_rev)),
      sprintf(" & Non-ETS & %s & %s & %s \\\\",
              fmt_int(nonets$firms), fmt_int(nonets$obs),
              fmt_pct(nonets$share_rev))
    )
  } else {
    tex <- c(tex,
      sprintf("%s & Non-ETS & %s & %s & %s \\\\",
              s$label,
              fmt_int(nonets$firms), fmt_int(nonets$obs),
              fmt_pct(nonets$share_rev))
    )
  }

  if (i < length(sectors)) {
    tex <- c(tex, "\\midrule")
  }
}

tex <- c(tex,
  "\\bottomrule",
  "\\end{tabular}"
)

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "zero_emission_sectors_summary.tex")
writeLines(tex, out_path)
cat("Saved to:", out_path, "\n")

# ── Print to console ────────────────────────────────────────────────────────
cat("\n"); print(stats %>% select(sector_group, group, firms, obs, mean_rev, share_rev))
