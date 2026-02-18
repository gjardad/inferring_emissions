###############################################################################
# fuel_proxy/execute_preprocessing.R
#
# PURPOSE
#   Run all preprocessing scripts in dependency order, then build proxy
#   auxiliary tables and the 96 fuel-consumption proxy variants.
#
#   This script assumes raw data files are already available on disk
#   (Annual_Accounts, B2B, import_export on DATA_DIR) and that Eurostat
#   downloads (NCV, fuel use, supply/imports energy balance) have already
#   been fetched on a machine with internet access and copied to DATA_DIR.
#
# USAGE
#   source("fuel_proxy/execute_preprocessing.R")
#
# NOTES
#   - Each script is sourced in an isolated environment to avoid cross-
#     contamination of objects between phases.
#   - Errors in individual scripts are trapped and reported; the pipeline
#     continues to the next script.
#   - Downsampling scripts (build_downsampled_raw_data.R,
#     refilter_b2b_downsampled.R) are excluded — those are for local
#     development only.
#   - Eurostat download scripts are excluded — RMD has no internet.
#     Run them on a local machine first:
#       preprocess/download_ncv_from_energy_balance.R
#       preprocess/download_fuel_use_from_energy_balance.R
#       preprocess/download_supply_imports_from_energy_balance.R
#
# OUTPUTS
#   See individual scripts.  Key final outputs:
#     PROC_DATA/loocv_training_sample.RData
#     CACHE_DIR/aux_*.rds   (auxiliary tables for proxy builders)
#     CACHE_DIR/proxy_*.rds (96 fuel-consumption proxy variants)
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

PREPROC_DIR <- file.path(REPO_DIR, "preprocess")
PROXY_DIR   <- file.path(REPO_DIR, "fuel_proxy", "proxies")


# =====================================================================
# Helper: run a script in a clean environment
# =====================================================================

run_script <- function(script_path, label = NULL) {
  if (is.null(label)) label <- basename(script_path)
  cat("\n", strrep("=", 70), "\n")
  cat("  RUNNING:", label, "\n")
  cat(strrep("=", 70), "\n\n")
  t0 <- Sys.time()
  tryCatch(
    source(script_path, local = new.env(parent = globalenv())),
    error = function(e) {
      cat("\n*** ERROR in", label, "***\n")
      cat(conditionMessage(e), "\n\n")
    }
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("  [%s finished in %.1f s]\n", label, elapsed))
}

pp <- function(fname) file.path(PREPROC_DIR, fname)


# =====================================================================
# Phase 1: Raw data loading  (.dta -> .RData conversion)
#   Skipped when SKIP_DTA_CONVERSION is TRUE (e.g. files already exist
#   on RMD from a previous run).
# =====================================================================

if (!isTRUE(SKIP_DTA_CONVERSION)) {
  run_script(pp("build_annual_accounts.R"))
  run_script(pp("load_b2b_in_rdata.R"))
  run_script(pp("build_df_trade.R"))
} else {
  cat("\n  [Phase 1 skipped: SKIP_DTA_CONVERSION = TRUE]\n")
}


# =====================================================================
# Phase 2: Annual accounts sample
# =====================================================================

run_script(pp("annual_accounts_sample_selection.R"))
run_script(pp("annual_accounts_key_variables.R"))


# =====================================================================
# Phase 3: EU ETS
# =====================================================================

run_script(pp("build_installation_year.R"))
run_script(pp("build_firm_year_euets.R"))


# =====================================================================
# Phase 4: Firm-level variables
# =====================================================================

run_script(pp("build_firm_year_domestic_input_costs.R"))
run_script(pp("build_firm_year_total_imports.R"))


# =====================================================================
# Phase 5: Fuel classification mappings
# =====================================================================

run_script(pp("build_cn8_fossil_fuel_list.R"))
run_script(pp("build_siec_to_ipcc_mapping.R"))
run_script(pp("build_hs_cn_to_ipcc_mapping.R"))
run_script(pp("build_hs_to_siec_map.R"))
run_script(pp("build_crf_to_nace_map.R"))
run_script(pp("build_emission_factors_ipcc.R"))


# =====================================================================
# Phase 6: Energy balance processing
# =====================================================================

run_script(pp("build_ncv_by_fuel_year.R"))
run_script(pp("build_fuels_used_for_energy_generation.R"))
run_script(pp("build_fuels_used_for_energy_generation_by_sector.R"))


# =====================================================================
# Phase 7: Fuel importers & B2B
# =====================================================================

run_script(pp("fuel_imported_by_firm_year.R"))
run_script(pp("build_b2b_selected_sample.R"))


# =====================================================================
# Phase 8: Physical quantities
# =====================================================================

run_script(pp("build_firm_cncode_year_physical_qty.R"))


# =====================================================================
# Phase 9: NCV & emission-factor calculations
# =====================================================================

run_script(pp("build_ipcc_ncv_year.R"))
run_script(pp("build_firm_cncode_year_ef_weighted_qty.R"))


# =====================================================================
# Phase 10: Fuel supplier classification
# =====================================================================

run_script(pp("fuel_supplier_likelihood_by_nace.R"))


# =====================================================================
# Phase 11: Fuel amounts & cost shares
# =====================================================================

run_script(pp("amount_consumed_from_fuel_importers.R"))
run_script(pp("build_fuel_input_cost_share.R"))


# =====================================================================
# Phase 12: Training sample & emissions panel
# =====================================================================

run_script(pp("build_loocv_training_sample.R"))
run_script(pp("build_firm_year_emissions.R"))


# =====================================================================
# Phase 13: NIR sector-year emissions
# =====================================================================

run_script(pp("build_emissions_by_sector_year_from_nir.R"))


# =====================================================================
# Phase 14: Proxy-specific preprocessing
# =====================================================================

run_script(file.path(UTILS_DIR, "build_auxiliary_tables.R"),
           label = "build_auxiliary_tables.R (fuel_proxy/utils)")
run_script(file.path(PROXY_DIR, "build_proxies.R"),
           label = "build_proxies.R (fuel_proxy/proxies)")


# =====================================================================
cat("\n", strrep("=", 70), "\n")
cat("  ALL PREPROCESSING FINISHED\n")
cat(strrep("=", 70), "\n")
cat("Processed data in:", PROC_DATA, "\n")
cat("Proxy cache in:   ", CACHE_DIR, "\n\n")
