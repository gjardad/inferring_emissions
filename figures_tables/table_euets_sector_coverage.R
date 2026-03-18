###############################################################################
# figures_tables/table_euets_sector_coverage.R
#
# PURPOSE
#   Generate table: % of stationary emissions covered by EU ETS, by sector
#   and year. Four rows: Stationary (all), Paper/pulp/printing (NACE 17/18),
#   Petroleum Refining (NACE 19), Iron & Steel (NACE 24.1X/24.2X/24.3X/24.51/24.52).
#
# INPUTS
#   - compliance.csv  (EUTL verified emissions by installation-year)
#   - installation.csv  (EUTL installation NACE codes)
#   - belcrt_denominators_all_years.tsv  (aggregate stationary denominator)
#   - belcrt_sector_denominators.tsv  (sector-specific NIR CO2 values)
#
# OUTPUTS
#   - euets_sector_coverage.tex  (LaTeX table)
#   - euets_sector_coverage.csv  (CSV for reference)
#
# PREREQUISITES
#   Run scripts/extract_belcrt_denominators_all_years.ps1 and
#   scripts/extract_belcrt_sector_denominators.ps1 first.
#
# Can be run on local 1.
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

# ── 1. Load EUTL data ───────────────────────────────────────────────────────

compliance <- read.csv(
  file.path(RAW_DATA, "EUTL", "Oct_2024_version", "compliance.csv"),
  stringsAsFactors = FALSE
)

installation <- read.csv(
  file.path(RAW_DATA, "EUTL", "Oct_2024_version", "installation.csv"),
  stringsAsFactors = FALSE
)

# Belgian installations with NACE codes
be_inst <- installation %>%
  filter(grepl("^BE_\\d", id)) %>%
  select(id, nace_id) %>%
  mutate(
    nace2d = as.integer(sub("\\..*", "", nace_id)),
    nace4d = nace_id
  )

# ── 2. EU ETS verified emissions by sector ───────────────────────────────────

be_compliance <- compliance %>%
  filter(grepl("^BE_\\d", installation_id),
         year %in% DISPLAY_YEARS,
         !is.na(verified)) %>%
  left_join(be_inst, by = c("installation_id" = "id"))

# Sector definitions
# Iron & steel: NACE 24.1X, 24.2X, 24.3X, 24.51, 24.52
is_iron_steel <- function(nace4d) {
  grepl("^24\\.[123]", nace4d) |
    nace4d %in% c("24.51", "24.52")
}

euets_stationary <- be_compliance %>%
  group_by(year) %>%
  summarise(euets_kt = sum(verified, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  mutate(sector = "Stationary emissions")

euets_paper <- be_compliance %>%
  filter(nace2d %in% c(17, 18)) %>%
  group_by(year) %>%
  summarise(euets_kt = sum(verified, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  mutate(sector = "Paper, pulp & printing")

euets_petrol <- be_compliance %>%
  filter(nace2d == 19) %>%
  group_by(year) %>%
  summarise(euets_kt = sum(verified, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  mutate(sector = "Petroleum refining")

euets_iron <- be_compliance %>%
  filter(is_iron_steel(nace4d)) %>%
  group_by(year) %>%
  summarise(euets_kt = sum(verified, na.rm = TRUE) / 1e3, .groups = "drop") %>%
  mutate(sector = "Iron & steel")

euets_sectors <- bind_rows(euets_stationary, euets_paper, euets_petrol, euets_iron)

# ── 3. NIR denominators ─────────────────────────────────────────────────────

# 3a. Aggregate stationary denominator (1.A.1 + 1.A.2 + 1.A.4 - 1.A.4.b + 2)
nir_agg_file <- file.path(PROC_DATA, "belcrt_denominators_all_years.tsv")
if (!file.exists(nir_agg_file)) {
  stop("belcrt_denominators_all_years.tsv not found. ",
       "Run scripts/extract_belcrt_denominators_all_years.ps1 first.")
}

nir_agg_raw <- read.delim(nir_agg_file, stringsAsFactors = FALSE)
nir_agg_raw$co2_kt <- as.numeric(gsub(",", "", nir_agg_raw$co2_kt))

nir_agg_wide <- nir_agg_raw %>%
  mutate(crf_clean = gsub("\\.", "_", crf)) %>%
  select(year, crf_clean, co2_kt) %>%
  pivot_wider(names_from = crf_clean, values_from = co2_kt)

nir_stationary <- nir_agg_wide %>%
  filter(year %in% DISPLAY_YEARS) %>%
  transmute(
    year = year,
    sector = "Stationary emissions",
    nir_kt = `1_A_1_` + `1_A_2_` + `1_A_4_` - `1_A_4_b_` + `2_`
  )

# 3b. Sector-specific denominators
nir_sec_file <- file.path(PROC_DATA, "belcrt_sector_denominators.tsv")
if (!file.exists(nir_sec_file)) {
  stop("belcrt_sector_denominators.tsv not found. ",
       "Run scripts/extract_belcrt_sector_denominators.ps1 first.")
}

nir_sec_raw <- read.delim(nir_sec_file, stringsAsFactors = FALSE)
nir_sec_raw$co2_kt <- as.numeric(gsub(",", "", nir_sec_raw$co2_kt))

nir_sec_wide <- nir_sec_raw %>%
  mutate(crf_clean = gsub("\\.", "_", crf)) %>%
  select(year, crf_clean, co2_kt) %>%
  pivot_wider(names_from = crf_clean, values_from = co2_kt)

# Paper, pulp & printing: 1.A.2.d. + 2.H.1.
nir_paper <- nir_sec_wide %>%
  transmute(
    year = year,
    sector = "Paper, pulp & printing",
    nir_kt = `1_A_2_d_` + `2_H_1_`
  )

# Petroleum refining: 1.A.1.b.
nir_petrol <- nir_sec_wide %>%
  transmute(
    year = year,
    sector = "Petroleum refining",
    nir_kt = `1_A_1_b_`
  )

# Iron & steel: 1.A.2.a. + 2.C.1.
nir_iron <- nir_sec_wide %>%
  transmute(
    year = year,
    sector = "Iron & steel",
    nir_kt = `1_A_2_a_` + `2_C_1_`
  )

nir_sectors <- bind_rows(nir_stationary, nir_paper, nir_petrol, nir_iron)

# ── 4. Annex XII comparison (2022 and 2023) ──────────────────────────────────
# Sector-level EU ETS CO2 from Annex XII, using our same CRF denominators.
# Sources: Annex XII 2024 (covering 2022), Annex XII 2025 (covering 2023).

annex_xii_sectors <- data.frame(
  year   = c(2022, 2022, 2022, 2022,
             2023, 2023, 2023, 2023),
  sector = rep(c("Stationary emissions", "Paper, pulp & printing",
                 "Petroleum refining", "Iron & steel"), 2),
  # Annex XII EU ETS CO2 numerators (kt):
  # Stationary: total CO2 from stationary ETS (row "CO2 emissions...")
  # Paper: 1.A.2.d ETS + 2.H ETS
  # Petroleum: 1.A.1.b ETS
  # Iron & steel: 1.A.2.a ETS + 2.C.1 ETS
  annex_xii_euets_kt = c(
    # 2022
    39540.3,             # stationary total
    545.2 + 22.6,        # paper: 1.A.2.d + 2.H
    4726.9,              # petroleum: 1.A.1.b
    947.9 + 3416.9,      # iron & steel: 1.A.2.a + 2.C.1
    # 2023
    35253.8,             # stationary total
    507.8 + 13.3,        # paper: 1.A.2.d + 2.H
    4525.2,              # petroleum: 1.A.1.b
    960.8 + 3248.6       # iron & steel: 1.A.2.a + 2.C.1
  ),
  stringsAsFactors = FALSE
)

# ── 5. Assemble table ───────────────────────────────────────────────────────

tbl <- euets_sectors %>%
  left_join(nir_sectors, by = c("year", "sector")) %>%
  left_join(annex_xii_sectors, by = c("year", "sector")) %>%
  mutate(
    pct_covered     = euets_kt / nir_kt * 100,
    annex_xii_pct   = ifelse(!is.na(annex_xii_euets_kt),
                             annex_xii_euets_kt / nir_kt * 100, NA)
  )

# ── 6. Save CSV ─────────────────────────────────────────────────────────────

write.csv(tbl, file.path(OUTPUT_DIR, "euets_sector_coverage.csv"),
          row.names = FALSE)
cat("Saved:", file.path(OUTPUT_DIR, "euets_sector_coverage.csv"), "\n")

# ── 7. Generate LaTeX table ─────────────────────────────────────────────────

# Pivot to wide format: sector x year
tbl_wide <- tbl %>%
  select(sector, year, pct_covered) %>%
  pivot_wider(names_from = year, values_from = pct_covered)

# Also pivot Annex XII for 2022/2023
annex_wide <- tbl %>%
  filter(!is.na(annex_xii_pct)) %>%
  select(sector, year, annex_xii_pct) %>%
  pivot_wider(names_from = year, values_from = annex_xii_pct,
              names_prefix = "axii_")

tbl_wide <- tbl_wide %>%
  left_join(annex_wide, by = "sector")

# Order rows
sector_order <- c("Stationary emissions", "Paper, pulp & printing",
                  "Petroleum refining", "Iron & steel")
tbl_wide <- tbl_wide %>%
  mutate(sector = factor(sector, levels = sector_order)) %>%
  arrange(sector)

fmt_pct <- function(x) ifelse(is.na(x), "$-$", sprintf("%.1f\\%%", x))

# Format: for 2022/2023, show "our% [Annex XII%]"
fmt_with_annex <- function(our, axii) {
  if (is.na(axii)) return(fmt_pct(our))
  sprintf("%.1f\\%% {\\scriptsize [%.1f\\%%]}", our, axii)
}

rows <- lapply(seq_len(nrow(tbl_wide)), function(i) {
  r <- tbl_wide[i, ]
  cells <- sapply(DISPLAY_YEARS, function(y) {
    yc <- as.character(y)
    axii_col <- paste0("axii_", y)
    if (axii_col %in% names(r) && !is.na(r[[axii_col]])) {
      fmt_with_annex(r[[yc]], r[[axii_col]])
    } else {
      fmt_pct(r[[yc]])
    }
  })
  paste0(r$sector, " & ", paste(cells, collapse = " & "), " \\\\")
})

rows_str <- paste(
  sapply(seq_along(rows), function(i) {
    if (i < length(rows)) paste0(rows[[i]], " \\midrule") else rows[[i]]
  }),
  collapse = "\n"
)

year_cols <- paste(DISPLAY_YEARS, collapse = " & ")
n_cols <- length(DISPLAY_YEARS)

tex <- paste0(
  "\\begin{tabular}{l", paste(rep("c", n_cols), collapse = ""), "}\n",
  "\\toprule\n",
  " & ", year_cols, " \\\\\n",
  "\\midrule\\midrule\n",
  rows_str, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)

writeLines(tex, file.path(OUTPUT_DIR, "euets_sector_coverage.tex"))
cat("Saved:", file.path(OUTPUT_DIR, "euets_sector_coverage.tex"), "\n")

# ── 8. Print summary ────────────────────────────────────────────────────────

cat("\n=== EU ETS Sector Coverage ===\n\n")
for (i in seq_len(nrow(tbl))) {
  r <- tbl[i, ]
  axii_str <- if (!is.na(r$annex_xii_pct)) {
    sprintf(" | Annex XII: %.1f%%", r$annex_xii_pct)
  } else ""
  cat(sprintf("%-25s %d: EU ETS %.0f kt / NIR %.0f kt = %.1f%%%s\n",
              r$sector, r$year, r$euets_kt, r$nir_kt, r$pct_covered, axii_str))
}
