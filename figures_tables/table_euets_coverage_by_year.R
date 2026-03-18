###############################################################################
# figures_tables/table_euets_coverage_by_year.R
#
# PURPOSE
#   Generate table: % of total stationary emissions covered by EU ETS, by year.
#
# INPUTS
#   - compliance.csv  (EUTL verified emissions by installation-year)
#   - belcrt_denominators_all_years.tsv  (NIR CO2 totals for 2005-2023)
#   - annex_xii_2025.tsv  (Annex XII 2025, covering 2023)
#
# OUTPUTS
#   - euets_coverage_by_year.tex  (LaTeX table)
#   - euets_coverage_by_year.csv  (CSV for reference)
#
# PREREQUISITES
#   Run scripts/extract_belcrt_denominators_all_years.ps1 first to generate
#   belcrt_denominators_all_years.tsv from BEL-CRT xlsx files.
#
# NOTE
#   The EUTL verified emissions are total CO2eq (includes small N2O from
#   nitric acid production), while the BEL-CRT denominator is CO2-only.
#   The difference is negligible (~0.4% of total).
#   Can be run on local 1 (no downsampled data dependency).
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

DISPLAY_YEARS <- c(2005, 2010, 2015, 2020, 2022, 2023)

# ── 1. EU ETS verified emissions (numerator) ────────────────────────────────
# Sum verified emissions from all Belgian installations (ID prefix "BE_")

compliance <- read.csv(
  file.path(RAW_DATA, "EUTL", "Oct_2024_version", "compliance.csv"),
  stringsAsFactors = FALSE
)

# Exclude BE_esd (Effort Sharing Decision national aggregate, not an installation)
euets_by_year <- compliance %>%
  filter(grepl("^BE_\\d", installation_id),
         year %in% DISPLAY_YEARS,
         !is.na(verified)) %>%
  group_by(year) %>%
  summarise(
    euets_verified_t = sum(verified, na.rm = TRUE),
    n_installations  = n(),
    .groups = "drop"
  ) %>%
  mutate(euets_verified_kt = euets_verified_t / 1e3)

# ── 2. National stationary emissions denominator (from BEL-CRT) ─────────────
# Denominator = 1.A.1. + 1.A.2. + 1.A.4. - 1.A.4.b. + 2.
# = stationary combustion (excl. residential & transport) + industrial processes

nir_denom_file <- file.path(PROC_DATA, "belcrt_denominators_all_years.tsv")
if (!file.exists(nir_denom_file)) {
  stop("belcrt_denominators_all_years.tsv not found. ",
       "Run scripts/extract_belcrt_denominators_all_years.ps1 first.")
}

nir_raw <- read.delim(nir_denom_file, stringsAsFactors = FALSE)
nir_raw$co2_kt <- as.numeric(gsub(",", "", nir_raw$co2_kt))

nir_wide <- nir_raw %>%
  mutate(crf_clean = gsub("\\.", "_", crf)) %>%
  select(year, crf_clean, co2_kt) %>%
  pivot_wider(names_from = crf_clean, values_from = co2_kt)

national_stationary <- nir_wide %>%
  transmute(
    year = year,
    nir_denominator_kt = `1_A_1_` + `1_A_2_` + `1_A_4_` - `1_A_4_b_` + `2_`
  )

# ── 3. Annex XII comparison values ──────────────────────────────────────────
# For apples-to-apples comparison, we use the Annex XII "total CO2 from
# stationary ETS installations" as numerator (includes combustion + industrial
# processes CO2), paired with our same BEL-CRT denominator.
# Source: row "CO2 emissions (...stationary installations...)" in each file.
#   Annex XII 2024 (covering 2022): 39,540.3 kt CO2
#   Annex XII 2025 (covering 2023): 35,253.8 kt CO2
# Remaining difference vs. our compliance.csv numerator is:
#   (a) CO2 vs CO2eq (~150 kt from N2O in nitric acid)
#   (b) data vintage (Oct 2024 EUTL download vs. inventory submission)

annex_xii <- data.frame(
  year               = c(2022,     2023),
  annex_xii_euets_kt = c(39540.3,  35253.8),   # total CO2 from stationary ETS
  stringsAsFactors = FALSE
)

# ── 4. Assemble table ───────────────────────────────────────────────────────

tbl <- euets_by_year %>%
  left_join(national_stationary, by = "year") %>%
  left_join(annex_xii, by = "year") %>%
  mutate(
    euets_pct     = euets_verified_kt / nir_denominator_kt * 100,
    annex_xii_pct = ifelse(!is.na(annex_xii_euets_kt),
                           annex_xii_euets_kt / nir_denominator_kt * 100, NA)
  )

# ── 5. Save CSV ─────────────────────────────────────────────────────────────

out_csv <- tbl %>%
  select(year, n_installations, euets_verified_kt, nir_denominator_kt,
         euets_pct, annex_xii_euets_kt, annex_xii_pct)

write.csv(out_csv, file.path(OUTPUT_DIR, "euets_coverage_by_year.csv"),
          row.names = FALSE)
cat("Saved:", file.path(OUTPUT_DIR, "euets_coverage_by_year.csv"), "\n")

# ── 6. Generate LaTeX table ─────────────────────────────────────────────────

fmt_int <- function(x) ifelse(is.na(x), "$-$",
                               formatC(x, format = "d", big.mark = ","))
fmt_kt  <- function(x) ifelse(is.na(x), "$-$",
                               formatC(round(x), format = "d", big.mark = ","))
fmt_pct <- function(x) ifelse(is.na(x), "",
                               sprintf("%.1f\\%%", x))

rows <- lapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]
  annex_col <- if (!is.na(r$annex_xii_pct)) {
    fmt_pct(r$annex_xii_pct)
  } else {
    ""
  }
  paste0(
    r$year, " & ",
    fmt_int(r$n_installations), " & ",
    fmt_kt(r$euets_verified_kt), " & ",
    fmt_kt(r$nir_denominator_kt), " & ",
    fmt_pct(r$euets_pct), " & ",
    annex_col,
    " \\\\"
  )
})

rows_str <- paste(
  sapply(seq_along(rows), function(i) {
    if (i < length(rows)) paste0(rows[[i]], " \\midrule") else rows[[i]]
  }),
  collapse = "\n"
)

tex <- paste0(
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) & (4) & (5) \\\\\n",
  "Year & Installations & EU\\,ETS (kt\\,CO$_2$eq) & Stationary (kt\\,CO$_2$) & \\% covered & Annex\\,XII \\% \\\\\n",
  "\\midrule\\midrule\n",
  rows_str, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

writeLines(tex, file.path(OUTPUT_DIR, "euets_coverage_by_year.tex"))
cat("Saved:", file.path(OUTPUT_DIR, "euets_coverage_by_year.tex"), "\n")

# ── 7. Print summary ────────────────────────────────────────────────────────

cat("\n=== EU ETS Coverage of Stationary Emissions ===\n\n")
for (i in seq_len(nrow(tbl))) {
  r <- tbl[i, ]
  annex_str <- if (!is.na(r$annex_xii_pct)) {
    sprintf(" | Annex XII: %.1f%%", r$annex_xii_pct)
  } else {
    ""
  }
  cat(sprintf(
    "Year %d: %s installations | EU ETS: %s kt | Stationary: %s kt | Coverage: %.1f%%%s\n",
    r$year,
    fmt_int(r$n_installations),
    fmt_kt(r$euets_verified_kt),
    fmt_kt(r$nir_denominator_kt),
    r$euets_pct,
    annex_str
  ))
}
