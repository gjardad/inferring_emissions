###############################################################################
# figures_tables/table_petroleum_dependency.R
#
# PURPOSE
#   Extract CO2 emissions by fuel type for each CRF 1.A.2 subcategory
#   from BEL-CRT sheets, and compute the share of emissions from petroleum
#   products (liquid fuels) vs. well-measured fuels (gaseous, solid).
#
# MOTIVATION
#   The Belgian NIR uses different data sources by fuel type:
#   - Natural gas: metered by grid operators (Fluvius, CWaPE, SIBELGA) — reliable
#   - Solid fuels: tracked via IMJV, REGINE surveys, ETS reports — reliable
#   - Petroleum products: extrapolated from known firms' electricity ratios — uncertain
#   This table shows which CRF 1.A.2 sectors are most dependent on the
#   uncertain petroleum component.
#
# OUTPUT
#   Table with columns: CRF category, total CO2 (kt), and shares by fuel type
###############################################################################

# ====================
# Setup
# ====================

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

library(tidyverse)
library(readxl)

# ====================
# Parameters
# ====================

years <- 2013:2022  # Post-ETS integration period

nir_folder <- file.path(RAW_DATA, "NIR")

# CRT file naming pattern (2025 submission)
file_for_year <- function(y) {
  # Try 2025 submission first, fall back to 2024
  f25 <- file.path(nir_folder,
    sprintf("BEL-CRT-2025-V1.0-%d-20250411-104305_awaiting submission.xlsx", y))
  f24 <- file.path(nir_folder,
    sprintf("BEL-CRT-2024-V1.0-%d-20241217-192810_awaiting submission.xlsx", y))
  if (file.exists(f25)) return(f25)
  if (file.exists(f24)) return(f24)
  stop("No CRT file found for year ", y)
}

# CRF 1.A.2 subcategories to extract
crf_categories <- c(
  "1.A.2"           = "1.A.2 Manufacturing industries and construction",
  "1.A.2.a"         = "1.A.2.a.  Iron and steel",
  "1.A.2.b"         = "1.A.2.b.  Non-ferrous metals",
  "1.A.2.c"         = "1.A.2.c.  Chemicals",
  "1.A.2.d"         = "1.A.2.d.  Pulp, paper and print",
  "1.A.2.e"         = "1.A.2.e.  Food processing, beverages and tobacco",
  "1.A.2.f"         = "1.A.2.f.  Non-metallic minerals",
  "1.A.2.g"         = "1.A.2.g. Other"
)

# Fuel type labels as they appear in the CRT (rows below each category)
fuel_labels <- c(
  "liquid"  = "Liquid fuels",
  "solid"   = "Solid fuels",
  "gaseous" = "Gaseous fuels (6)",
  "other"   = "Other fossil fuels (7)",
  "biomass" = "Biomass (3)"
)

# ====================
# Extract function
# ====================

extract_1a2_by_fuel <- function(year) {
  f <- file_for_year(year)

  # Sheet 2 contains CRF 1.A.2 (manufacturing)
  df <- read_excel(f, sheet = "Table1.A(a)s2", col_types = "text", n_max = 120)

  # Column 1 = category/fuel label, column 7 = CO2 emissions (kt)
  labels <- df[[1]]
  co2_raw <- df[[7]]

  results <- list()

  for (i in seq_along(crf_categories)) {
    crf_code <- names(crf_categories)[i]
    crf_label <- crf_categories[i]

    # Find the row matching this category
    row_idx <- which(str_detect(labels, fixed(crf_label)))
    if (length(row_idx) == 0) next

    # The fuel breakdown is in the rows immediately following
    fuel_rows <- (row_idx + 1):(row_idx + length(fuel_labels))

    fuel_co2 <- sapply(fuel_labels, function(fl) {
      fr <- which(labels[fuel_rows] == fl)
      if (length(fr) == 0) return(NA_real_)
      val <- co2_raw[fuel_rows[fr[1]]]
      if (is.na(val) || val %in% c("NO", "NE", "IE", "NA")) return(0)
      as.numeric(val)
    })

    total_co2 <- co2_raw[row_idx]
    total_co2 <- if (!is.na(total_co2) && !total_co2 %in% c("NO", "NE", "IE"))
                   as.numeric(total_co2) else NA_real_

    results[[i]] <- tibble(
      year = year,
      crf = crf_code,
      crf_label = crf_label,
      total_co2_kt = total_co2,
      liquid_co2_kt = fuel_co2["liquid"],
      solid_co2_kt = fuel_co2["solid"],
      gaseous_co2_kt = fuel_co2["gaseous"],
      other_fossil_co2_kt = fuel_co2["other"],
      biomass_co2_kt = fuel_co2["biomass"]
    )
  }

  bind_rows(results)
}

# ====================
# Extract for all years
# ====================

cat("Extracting CRT data for years", min(years), "to", max(years), "...\n")
fuel_by_sector <- map_dfr(years, function(y) {
  cat("  Year", y, "\n")
  tryCatch(extract_1a2_by_fuel(y), error = function(e) {
    warning("Failed for year ", y, ": ", e$message)
    tibble()
  })
})

# ====================
# Compute shares
# ====================

# Fossil-only total (excluding biomass)
fuel_by_sector <- fuel_by_sector %>%
  mutate(
    fossil_co2_kt = liquid_co2_kt + solid_co2_kt + gaseous_co2_kt + other_fossil_co2_kt,
    pct_liquid  = 100 * liquid_co2_kt / fossil_co2_kt,
    pct_solid   = 100 * solid_co2_kt / fossil_co2_kt,
    pct_gaseous = 100 * gaseous_co2_kt / fossil_co2_kt,
    pct_other   = 100 * other_fossil_co2_kt / fossil_co2_kt
  )

# ====================
# Summary table: average shares across years
# ====================

summary_table <- fuel_by_sector %>%
  filter(crf != "1.A.2") %>%  # exclude the total row
  group_by(crf, crf_label) %>%
  summarise(
    avg_fossil_co2_kt = mean(fossil_co2_kt, na.rm = TRUE),
    avg_pct_liquid  = mean(pct_liquid, na.rm = TRUE),
    avg_pct_solid   = mean(pct_solid, na.rm = TRUE),
    avg_pct_gaseous = mean(pct_gaseous, na.rm = TRUE),
    avg_pct_other   = mean(pct_other, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(crf)

# Also compute the total 1.A.2 row
total_row <- fuel_by_sector %>%
  filter(crf == "1.A.2") %>%
  summarise(
    crf = "1.A.2",
    crf_label = "Total Manufacturing (1.A.2)",
    avg_fossil_co2_kt = mean(fossil_co2_kt, na.rm = TRUE),
    avg_pct_liquid  = mean(pct_liquid, na.rm = TRUE),
    avg_pct_solid   = mean(pct_solid, na.rm = TRUE),
    avg_pct_gaseous = mean(pct_gaseous, na.rm = TRUE),
    avg_pct_other   = mean(pct_other, na.rm = TRUE)
  )

summary_table <- bind_rows(summary_table, total_row)

# ====================
# Print results
# ====================

cat("\n========================================================\n")
cat("CO2 Emissions by Fuel Type: CRF 1.A.2 Subcategories\n")
cat("Average over", min(years), "-", max(years), "\n")
cat("========================================================\n\n")

cat(sprintf("%-45s %8s %8s %8s %8s %8s\n",
            "CRF Category", "CO2(kt)", "Liquid%", "Solid%", "Gas%", "Other%"))
cat(paste(rep("-", 90), collapse = ""), "\n")

for (i in seq_len(nrow(summary_table))) {
  r <- summary_table[i, ]
  # Clean up label
  lbl <- gsub("^1\\.A\\.2[a-g.]*\\s*", "", r$crf_label)
  lbl <- gsub("\\(\\d+\\)", "", lbl)
  lbl <- trimws(lbl)
  if (r$crf == "1.A.2") lbl <- "TOTAL MANUFACTURING"

  cat(sprintf("%-45s %8.0f %7.1f%% %7.1f%% %7.1f%% %7.1f%%\n",
              paste0(r$crf, "  ", lbl),
              r$avg_fossil_co2_kt,
              r$avg_pct_liquid,
              r$avg_pct_solid,
              r$avg_pct_gaseous,
              r$avg_pct_other))
}

cat("\n")
cat("Interpretation:\n")
cat("  'Liquid%' = share of fossil CO2 from petroleum products (extrapolated in NIR)\n")
cat("  'Gas%'    = share from natural gas (metered by grid operators — reliable)\n")
cat("  'Solid%'  = share from solid fuels (firm-level surveys/ETS — reliable)\n")
cat("  'Other%'  = share from other fossil fuels\n")

# ====================
# Save
# ====================

save(fuel_by_sector, summary_table,
     file = file.path(OUTPUT_DIR, "petroleum_dependency_by_sector.RData"))
cat("\nSaved to", file.path(OUTPUT_DIR, "petroleum_dependency_by_sector.RData"), "\n")

# Also save year-by-year detail for time trends
write.csv(fuel_by_sector,
          file = file.path(OUTPUT_DIR, "petroleum_dependency_by_sector_year.csv"),
          row.names = FALSE)
cat("Saved CSV to", file.path(OUTPUT_DIR, "petroleum_dependency_by_sector_year.csv"), "\n")

# ====================
# Reliability classification
# ====================

cat("\n========================================================\n")
cat("Reliability Classification of NIR Sectoral Aggregates\n")
cat("========================================================\n\n")

summary_table %>%
  filter(crf != "1.A.2") %>%
  mutate(
    reliability = case_when(
      avg_pct_liquid < 10 ~ "HIGH: <10% petroleum-dependent",
      avg_pct_liquid < 25 ~ "MODERATE: 10-25% petroleum-dependent",
      TRUE ~ "LOW: >25% petroleum-dependent"
    )
  ) %>%
  select(crf, avg_pct_liquid, reliability) %>%
  arrange(desc(avg_pct_liquid)) %>%
  {
    for (i in seq_len(nrow(.))) {
      r <- .[i, ]
      cat(sprintf("  %-12s  Liquid share: %5.1f%%  → %s\n",
                  r$crf, r$avg_pct_liquid, r$reliability))
    }
  }
