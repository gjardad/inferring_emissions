###############################################################################
# figures_tables/table_sample_coverage.R
#
# PURPOSE
#   Generate Table: Coverage of Selected Sample.
#   Shows firm counts, value added, wage bill, and emissions coverage
#   for selected years (2002, 2012, 2022).
#
# INPUTS
#   - df_national_accounts_with_5digits.RData  (full AA universe, MUST be full data)
#   - annual_accounts_more_selected_sample.RData  (De Loecker/Dhyne sample)
#   - firm_year_belgian_euets.RData  (EUETS firms matched to sample)
#   - emissions_by_crf_year.RData  (sector-year emissions from NIR, optional)
#   - crf_to_nace_map.RData  (CRF-to-NACE crosswalk)
#
# OUTPUTS
#   - sample_coverage.tex  (LaTeX table)
#   - sample_coverage.csv  (CSV for reference)
#
# NOTE
#   This script must be run on RMD with full data. On local 1, the annual
#   accounts files are downsampled and will produce incorrect aggregates.
###############################################################################

# ── Setup ────────────────────────────────────────────────────────────────────

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
library(tidyr)

DISPLAY_YEARS <- c(2002, 2012, 2022)

# ── Variable definitions ─────────────────────────────────────────────────────
# From annual_accounts_key_variables.R:
#   value_added = v_0009800
#   wage_bill   = v_0001023
#   revenue     = turnover_VAT
#   fte         = v_0001003
#   capital     = v_0022_27

# ── 1. Load data ─────────────────────────────────────────────────────────────

# Full annual accounts universe (before selection)
load(file.path(PROC_DATA, "df_national_accounts_with_5digits.RData"))

# De Loecker/Dhyne selected sample
load(file.path(PROC_DATA, "annual_accounts_more_selected_sample.RData"))

# EUETS firms matched to sample
load(file.path(PROC_DATA, "firm_year_belgian_euets.RData"))

# ── 2. Compute sample-level aggregates ───────────────────────────────────────

# 2a. Full universe aggregates (denominator for "% of agg.")
agg_full <- df_national_accounts %>%
  filter(year %in% DISPLAY_YEARS) %>%
  group_by(year) %>%
  summarise(
    n_full         = n_distinct(vat_ano),
    va_full        = sum(v_0009800, na.rm = TRUE),
    wage_bill_full = sum(v_0001023, na.rm = TRUE),
    .groups = "drop"
  )

# 2b. Selected sample aggregates (numerator)
agg_selected <- df_annual_accounts_more_selected_sample %>%
  filter(year %in% DISPLAY_YEARS) %>%
  group_by(year) %>%
  summarise(
    n_selected         = n_distinct(vat_ano),
    va_selected        = sum(v_0009800, na.rm = TRUE),
    wage_bill_selected = sum(v_0001023, na.rm = TRUE),
    .groups = "drop"
  )

# ── 3. EUETS firms in selected sample ───────────────────────────────────────

# Join on both firm and year so that an EUETS firm only counts in years
# where it also meets the De Loecker/Dhyne sample-selection criteria
selected_firm_years <- df_annual_accounts_more_selected_sample %>%
  select(vat_ano, year) %>%
  distinct()

euets_in_sample <- firm_year_belgian_euets %>%
  inner_join(selected_firm_years,
             by = c("vat" = "vat_ano", "year" = "year")) %>%
  filter(year %in% DISPLAY_YEARS)

agg_euets <- euets_in_sample %>%
  group_by(year) %>%
  summarise(
    n_euets          = n_distinct(vat),
    emissions_sample = sum(emissions, na.rm = TRUE),
    .groups = "drop"
  )

# ── 4. National emission aggregates ─────────────────────────────────────────

# Try to load sector-year emissions from NIR (built by
# preprocess/build_emissions_by_sector_year_from_nir.R)
nir_file <- file.path(PROC_DATA, "emissions_by_crf_year.RData")
has_nir <- file.exists(nir_file)

if (has_nir) {
  load(nir_file)
  load(file.path(PROC_DATA, "crf_to_nace_map.RData"))

  # Total national emissions by year (sum across all CRF categories)
  national_emissions <- emissions_by_crf_year %>%
    filter(year %in% DISPLAY_YEARS) %>%
    group_by(year) %>%
    summarise(emissions_national = sum(co2_emissions, na.rm = TRUE),
              .groups = "drop")

  # Emissions in NACE sectors where EUETS firms are present (per year)
  euets_nace2d_by_year <- firm_year_belgian_euets %>%
    mutate(nace2d = substr(nace5d, 1, 2)) %>%
    select(year, nace2d) %>%
    distinct()

  # Map CRF to NACE 2-digit
  crf_nace2d <- crf_to_nace_map %>%
    mutate(nace2d = substr(nace_code, 2, 3)) %>%
    select(crf, nace2d) %>%
    distinct()

  euets_sector_emissions <- emissions_by_crf_year %>%
    left_join(crf_nace2d, by = "crf") %>%
    inner_join(euets_nace2d_by_year, by = c("year", "nace2d")) %>%
    filter(year %in% DISPLAY_YEARS) %>%
    group_by(year) %>%
    summarise(emissions_euets_sectors = sum(co2_emissions, na.rm = TRUE),
              .groups = "drop")
} else {
  message(
    "emissions_by_crf_year.RData not found. ",
    "Run preprocess/build_emissions_by_sector_year_from_nir.R first, ",
    "or emissions columns will show NA."
  )
  national_emissions <- data.frame(
    year = DISPLAY_YEARS,
    emissions_national = NA_real_
  )
  euets_sector_emissions <- data.frame(
    year = DISPLAY_YEARS,
    emissions_euets_sectors = NA_real_
  )
}

# ── 5. Assemble table ───────────────────────────────────────────────────────

tbl <- agg_selected %>%
  left_join(agg_full, by = "year") %>%
  left_join(agg_euets, by = "year") %>%
  left_join(national_emissions, by = "year") %>%
  left_join(euets_sector_emissions, by = "year") %>%
  mutate(
    # Value added in billion euros
    va_bn       = va_selected / 1e9,         # v_0009800 is in EUR
    va_pct      = va_selected / va_full * 100,

    # Wage bill in billion euros
    wb_bn       = wage_bill_selected / 1e9,  # v_0001023 is in EUR
    wb_pct      = wage_bill_selected / wage_bill_full * 100,

    # Emissions in million tonnes CO2eq
    em_mt       = emissions_sample / 1e6,
    em_pct      = ifelse(!is.na(emissions_national) & emissions_national > 0,
                         emissions_sample / emissions_national * 100, NA),
    em_sector_pct = ifelse(!is.na(emissions_euets_sectors) & emissions_euets_sectors > 0,
                           emissions_sample / emissions_euets_sectors * 100, NA),

    # Handle 2002 (no EUETS)
    n_euets     = ifelse(year < 2005, NA_integer_, n_euets),
    em_mt       = ifelse(year < 2005, NA_real_, em_mt),
    em_pct      = ifelse(year < 2005, NA_real_, em_pct),
    em_sector_pct = ifelse(year < 2005, NA_real_, em_sector_pct)
  )

# ── 6. Save CSV ─────────────────────────────────────────────────────────────

out_csv <- tbl %>%
  select(year, n_selected, n_euets, va_bn, va_pct,
         wb_bn, wb_pct, em_mt, em_pct, em_sector_pct)

write.csv(out_csv, file.path(OUTPUT_DIR, "sample_coverage.csv"), row.names = FALSE)
cat("Saved:", file.path(OUTPUT_DIR, "sample_coverage.csv"), "\n")

# ── 7. Generate LaTeX table ─────────────────────────────────────────────────

fmt_int   <- function(x) ifelse(is.na(x), "$-$", formatC(x, format = "d", big.mark = ","))
fmt_bn    <- function(x) ifelse(is.na(x), "$-$", formatC(round(x), format = "d", big.mark = ","))
fmt_pct   <- function(x) ifelse(is.na(x), "$-$", paste0(round(x), "\\%"))
fmt_mt    <- function(x) ifelse(is.na(x), "$-$", formatC(round(x), format = "d", big.mark = ","))

rows <- lapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]
  paste0(
    r$year, " & ",
    fmt_int(r$n_selected), " & ",
    fmt_int(r$n_euets), " & ",
    fmt_bn(r$va_bn), " & ",
    fmt_pct(r$va_pct), " & ",
    fmt_bn(r$wb_bn), " & ",
    fmt_pct(r$wb_pct), " & ",
    fmt_mt(round(r$em_mt)), " & ",
    fmt_pct(r$em_pct), " & ",
    fmt_pct(r$em_sector_pct),
    " \\\\"
  )
})

# Add midrule between each row
rows_with_rules <- paste(
  sapply(seq_along(rows), function(i) {
    if (i < length(rows)) paste0(rows[[i]], " \\midrule") else rows[[i]]
  }),
  collapse = "\n"
)

tex <- paste0(
  "\\begin{table}[!htp]\n",
  "\\centering\n",
  "\\caption{Coverage of Selected Sample}\n",
  "\\label{table: sample coverage}\n",
  "\\scalebox{0.85}{\n",
  "\\begin{tabular}{lccccccccc}\n",
  "\\toprule\n",
  " & & & \\multicolumn{2}{c}{GDP (Value added)} & \\multicolumn{2}{c}{Wage bill} & \\multicolumn{3}{c}{Emissions} \\\\\n",
  " \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-10}\n",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) \\\\\n",
  "Year & Count & \\# EU\\,ETS firms &  & \\% of agg. &  & \\% of agg. & & \\% of agg. & \\% of EU\\,ETS sectors\\\\\n",
  "\\midrule\\midrule\n",
  rows_with_rules, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "}\n",
  "\\begin{minipage}{1.05\\textwidth}\n",
  "    {\\footnotesize \\vspace{0.25cm} \\noindent \\textit{Notes:} ",
  "Column~(1) reports the number of firms in the selected sample. ",
  "Column~(2) reports the number of EU\\,ETS-regulated firms in the sample. ",
  "Columns~(3) and~(5) report aggregate value added and wage bill of sample firms, ",
  "respectively, in billion euro (current prices). ",
  "Columns~(4) and~(6) report these as a share of the corresponding totals ",
  "across all firms filing annual accounts. ",
  "Column~(7) reports total verified emissions of EU\\,ETS firms in the sample, ",
  "in million tonnes of CO$_2$-equivalent GHG. ",
  "Column~(8) reports this as a share of total Belgian GHG emissions from the National Inventory. ",
  "Column~(9) reports it as a share of total emissions in NACE~2-digit sectors ",
  "where EU\\,ETS firms are present. ",
  "There are no emission figures for 2002 because the EU\\,ETS was introduced in 2005.}\n",
  "\\end{minipage}\n",
  "\\end{table}\n"
)

writeLines(tex, file.path(OUTPUT_DIR, "sample_coverage.tex"))
cat("Saved:", file.path(OUTPUT_DIR, "sample_coverage.tex"), "\n")

# ── 8. Print summary to console ─────────────────────────────────────────────

cat("\n=== Sample Coverage Summary ===\n\n")
for (i in seq_len(nrow(out_csv))) {
  r <- out_csv[i, ]
  cat(sprintf(
    "Year %d: %s firms (%s EUETS) | VA: %.0f bn (%.0f%%) | WB: %.0f bn (%.0f%%) | Em: %.0f Mt (%.0f%%, %.0f%% of EUETS sectors)\n",
    r$year,
    fmt_int(r$n_selected),
    ifelse(is.na(r$n_euets), "-", fmt_int(r$n_euets)),
    ifelse(is.na(r$va_bn), NA, r$va_bn),
    ifelse(is.na(r$va_pct), NA, r$va_pct),
    ifelse(is.na(r$wb_bn), NA, r$wb_bn),
    ifelse(is.na(r$wb_pct), NA, r$wb_pct),
    ifelse(is.na(r$em_mt), NA, r$em_mt),
    ifelse(is.na(r$em_pct), NA, r$em_pct),
    ifelse(is.na(r$em_sector_pct), NA, r$em_sector_pct)
  ))
}
