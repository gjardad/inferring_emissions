###############################################################################
# fuel_proxy/descriptives/execute_descriptives.R
#
# PURPOSE
#   Run all descriptive scripts in sequence. Each script sources paths.R
#   independently, so this runner simply executes them one by one.
#
# NOTES
#   - selected_proxy_pair_diagnosis.R will skip with a warning until the
#     proxy placeholder filenames are replaced with actual proxy names.
#
# OUTPUTS
#   See individual scripts for their outputs. Summary:
#
#   total_supply_vs_imports_fossil_fuels.R
#     -> total_supply_vs_imports.png
#
#   quality_of_implied_prices_from_customs.R
#     -> tier_breakdown.tex
#     -> pseudo_ground_truth_test.tex
#     -> price_dispersion_by_cn4.tex
#
#   nace_sector_support_tables.R
#     -> sector_coverage.tex
#     -> histogram_firms_per_nace5d.pdf
#     -> histogram_firms_per_nace2d.pdf
#
#   nace_sectors_totally_covered_by_euets.R
#     -> (no file output; builds nace_coverage data frame)
#
#   table_euets_coverage_emissions_by_sector_from_nir.R
#     -> euets_coverage_by_sector.tex
#
#   share_c19_c24_regulated_by_euets.R
#     -> ets_share_c19_c24.tex
#
#   benchmark_fuel_proxy_diagnosis.R
#     -> benchmark_proxy_regression.txt
#     -> benchmark_proxy_summary_stats.tex
#     -> benchmark_proxy_density_C19.pdf
#     -> benchmark_proxy_density_C24.pdf
#
#   zero_fuel_share_diagnostics.R
#     -> (console only)
#
#   selected_proxy_pair_diagnosis.R
#     -> selected_proxy_{step}_regression.txt
#     -> selected_proxy_{step}_summary_stats.tex
#     -> selected_proxy_{step}_density_C19.pdf
#     -> selected_proxy_{step}_density_C24.pdf
###############################################################################

# ====================
# Define paths -------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

DESC_DIR <- file.path(REPO_DIR, "fuel_proxy", "descriptives")


# =====================================================================
# Helper: run a script in a clean environment
# =====================================================================

run_script <- function(script_name) {
  script_path <- file.path(DESC_DIR, script_name)
  cat("\n", strrep("=", 70), "\n")
  cat("  RUNNING:", script_name, "\n")
  cat(strrep("=", 70), "\n\n")
  tryCatch(
    source(script_path, local = new.env(parent = globalenv())),
    error = function(e) {
      cat("\n*** ERROR in", script_name, "***\n")
      cat(conditionMessage(e), "\n\n")
    }
  )
}


# =====================================================================
# 1) Supply vs imports: Belgium relies on fuel imports
# =====================================================================

run_script("total_supply_vs_imports_fossil_fuels.R")


# =====================================================================
# 2) Data quality: customs price imputation
# =====================================================================

run_script("quality_of_implied_prices_from_customs.R")


# =====================================================================
# 3) Sector coverage and support tables
# =====================================================================

run_script("nace_sector_support_tables.R")


# =====================================================================
# 4) EU ETS coverage by sector (NIR-based)
# =====================================================================

run_script("nace_sectors_totally_covered_by_euets.R")
run_script("table_euets_coverage_emissions_by_sector_from_nir.R")


# =====================================================================
# 5) C19 / C24 ETS vs non-ETS
# =====================================================================

run_script("share_c19_c24_regulated_by_euets.R")


# =====================================================================
# 6) Fuel proxy diagnostics
# =====================================================================

run_script("benchmark_fuel_proxy_diagnosis.R")
run_script("zero_fuel_share_diagnostics.R")
run_script("selected_proxy_pair_diagnosis.R")


# =====================================================================
cat("\n", strrep("=", 70), "\n")
cat("  ALL DESCRIPTIVES FINISHED\n")
cat(strrep("=", 70), "\n")
cat("Outputs in:", OUTPUT_DIR, "\n\n")
