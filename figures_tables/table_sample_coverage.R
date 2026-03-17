###############################################################################
# figures_tables/table_sample_coverage.R
#
# PURPOSE
#   Generate Table: Coverage of Selected Sample.
#   Shows firm counts, value added, wage bill, and emissions coverage
#   for selected years (2005, 2010, 2015, 2020).
#
# INPUTS
#   - df_national_accounts_with_5digits.RData  (full AA universe, MUST be full data)
#   - annual_accounts_more_selected_sample.RData  (De Loecker/Dhyne sample)
#   - firm_year_belgian_euets.RData  (EUETS firms matched to sample, with emissions_foreign flag)
#   - belcrt_denominators.tsv  (NIR CO2 totals extracted from BEL-CRT 2025 files)
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

DISPLAY_YEARS <- c(2005, 2010, 2015, 2020)

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

# EUETS firms matched to sample (includes emissions_foreign flag)
load(file.path(PROC_DATA, "firm_year_belgian_euets.RData"))

# All Belgian EU ETS installations (for total Belgian ETS emissions denominator)
load(file.path(PROC_DATA, "installation_year_in_belgium.RData"))

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

# Emissions numerator: Belgian-only emissions (subtract foreign installations)
# If emissions_foreign column is not yet available, fall back to total emissions
has_foreign_flag <- "emissions_foreign" %in% names(euets_in_sample)

agg_euets <- euets_in_sample %>%
  group_by(year) %>%
  summarise(
    n_euets              = n_distinct(vat),
    emissions_total      = sum(emissions, na.rm = TRUE),
    emissions_belgian    = if (has_foreign_flag) {
      sum(emissions - emissions_foreign, na.rm = TRUE)
    } else {
      sum(emissions, na.rm = TRUE)
    },
    .groups = "drop"
  )

if (!has_foreign_flag) {
  message("WARNING: emissions_foreign column not found in firm_year_belgian_euets. ",
          "Emissions column includes foreign installations. ",
          "Re-run preprocess/build_firm_year_euets.R to add the flag.")
}

# ── 4. National emission denominators from BEL-CRT ──────────────────────────
# Denominator = 1.A.1. + 1.A.2. + 1.A.4. - 1.A.4.b. + 2.
# = stationary combustion (excl. residential & transport) + industrial processes
# Source: BEL-CRT-2025 files, CO2 column (kt)

nir_denom_file <- file.path(PROC_DATA, "belcrt_denominators.tsv")
if (file.exists(nir_denom_file)) {
  nir_raw <- read.delim(nir_denom_file, stringsAsFactors = FALSE)
  # Parse comma-formatted numbers
  nir_raw$co2_kt <- as.numeric(gsub(",", "", nir_raw$co2_kt))

  nir_wide <- nir_raw %>%
    mutate(crf_clean = gsub("\\.", "_", crf)) %>%
    select(year, crf_clean, co2_kt) %>%
    pivot_wider(names_from = crf_clean, values_from = co2_kt)

  national_stationary <- nir_wide %>%
    transmute(
      year = year,
      # 1.A.1 + 1.A.2 + 1.A.4 - 1.A.4.b + 2 (all in kt CO2)
      nir_denominator_kt = `1_A_1_` + `1_A_2_` + `1_A_4_` - `1_A_4_b_` + `2_`
    )
} else {
  message("belcrt_denominators.tsv not found. ",
          "Run scripts/extract_belcrt_denominators.ps1 first, ",
          "or emissions share column will show NA.")
  national_stationary <- data.frame(
    year = DISPLAY_YEARS,
    nir_denominator_kt = NA_real_
  )
}

# ── 4b. Total Belgian EU ETS installation emissions (denominator for col 9) ─
agg_be_installations <- installation_year_in_belgium %>%
  filter(year %in% DISPLAY_YEARS, !is.na(verified)) %>%
  group_by(year) %>%
  summarise(
    emissions_all_be_inst = sum(verified, na.rm = TRUE),
    .groups = "drop"
  )

# ── 5. Assemble table ───────────────────────────────────────────────────────

tbl <- agg_selected %>%
  left_join(agg_full, by = "year") %>%
  left_join(agg_euets, by = "year") %>%
  left_join(national_stationary, by = "year") %>%
  left_join(agg_be_installations, by = "year") %>%
  mutate(
    # Value added in billion euros
    va_bn       = va_selected / 1e9,
    va_pct      = va_selected / va_full * 100,

    # Wage bill in billion euros
    wb_bn       = wage_bill_selected / 1e9,
    wb_pct      = wage_bill_selected / wage_bill_full * 100,

    # Emissions: Belgian-only, in kt (firm_year_belgian_euets uses tonnes)
    em_sample_kt = emissions_belgian / 1e3,
    em_pct       = ifelse(!is.na(nir_denominator_kt) & nir_denominator_kt > 0,
                          em_sample_kt / nir_denominator_kt * 100, NA),

    # Column 9: sample emissions as % of total Belgian EU ETS installation emissions
    em_euets_pct = ifelse(emissions_all_be_inst > 0,
                          emissions_belgian / emissions_all_be_inst * 100, NA)
  )

# ── 6. Save CSV ─────────────────────────────────────────────────────────────

out_csv <- tbl %>%
  select(year, n_selected, n_euets, va_bn, va_pct,
         wb_bn, wb_pct, em_sample_kt, nir_denominator_kt, em_pct, em_euets_pct)

write.csv(out_csv, file.path(OUTPUT_DIR, "sample_coverage.csv"), row.names = FALSE)
cat("Saved:", file.path(OUTPUT_DIR, "sample_coverage.csv"), "\n")

# ── 7. Generate LaTeX table ─────────────────────────────────────────────────

fmt_int   <- function(x) ifelse(is.na(x), "$-$", formatC(x, format = "d", big.mark = ","))
fmt_bn    <- function(x) ifelse(is.na(x), "$-$", formatC(round(x), format = "d", big.mark = ","))
fmt_pct   <- function(x) ifelse(is.na(x), "$-$", sprintf("%.1f\\%%", x))
fmt_kt    <- function(x) ifelse(is.na(x), "$-$", formatC(round(x), format = "d", big.mark = ","))

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
    fmt_kt(r$em_sample_kt), " & ",
    fmt_pct(r$em_pct), " & ",
    fmt_pct(r$em_euets_pct),
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
  "\\begin{tabular}{lccccccccc}\n",
  "\\toprule\n",
  " & & & \\multicolumn{2}{c}{Value added} & \\multicolumn{2}{c}{Wage bill} & \\multicolumn{3}{c}{Emissions} \\\\\n",
  " \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-10}\n",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) \\\\\n",
  "Year & Firms & EU\\,ETS firms &  bn\\,\\euro{} & \\% of agg. &  bn\\,\\euro{} & \\% of agg. & kt\\,CO$_2$ & \\% of stat.\\,+\\,ind. & \\% of EU\\,ETS \\\\\n",
  "\\midrule\\midrule\n",
  rows_with_rules, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

writeLines(tex, file.path(OUTPUT_DIR, "sample_coverage.tex"))
cat("Saved:", file.path(OUTPUT_DIR, "sample_coverage.tex"), "\n")

# ── 8. Print summary to console ─────────────────────────────────────────────

cat("\n=== Sample Coverage Summary ===\n\n")
for (i in seq_len(nrow(out_csv))) {
  r <- out_csv[i, ]
  cat(sprintf(
    "Year %d: %s firms (%s EUETS) | VA: %.0f bn (%.1f%%) | WB: %.0f bn (%.1f%%) | Em: %s kt (%.1f%% stat+ind, %.1f%% EUETS)\n",
    r$year,
    fmt_int(r$n_selected),
    ifelse(is.na(r$n_euets), "-", fmt_int(r$n_euets)),
    ifelse(is.na(r$va_bn), NA, r$va_bn),
    ifelse(is.na(r$va_pct), NA, r$va_pct),
    ifelse(is.na(r$wb_bn), NA, r$wb_bn),
    ifelse(is.na(r$wb_pct), NA, r$wb_pct),
    ifelse(is.na(r$em_sample_kt), "-", fmt_kt(r$em_sample_kt)),
    ifelse(is.na(r$em_pct), NA, r$em_pct),
    ifelse(is.na(r$em_euets_pct), NA, r$em_euets_pct)
  ))
}
