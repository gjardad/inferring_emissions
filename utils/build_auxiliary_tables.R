###############################################################################
# build_auxiliary_tables.R
#
# PURPOSE:
#   Loads all raw/intermediate datasets needed for proxy construction and
#   produces cached "aux tables" saved to /output/loocv/cache.
#
# OUTPUT:
#   Writes:
#     - aux_aa_nace.rds
#     - aux_likelihood_tbl.rds
#     - aux_cn8_strict.rds
#     - aux_firm_year_fuel_totals.rds
#     - aux_firm_year_cn8.rds
#     - aux_firm_year_siec.rds
#     - aux_siec_all_used.rds
#     - aux_buyer_sector_siec.rds
#     - aux_b2b_minimal.rds
###############################################################################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  DATA_DIR <- "C:/Users/jota_/Documents/NBB_projects/data"
  REPO_DIR <- "C:/Users/jota_/Documents/NBB_projects/inferring_emissions"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

UTILS_DIR <- file.path(REPO_DIR, "utils")
LOOCV_DIR <- file.path(REPO_DIR, "loocv")

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}



tic("01_build_aux_tables")

source(file.path(LOOCV_ROOT, "00_config", "load_packages.R"))

log_step("Loading processed datasets...")
load(paste0(PROC_DATA,"/b2b_selected_sample.RData"))
load(paste0(PROC_DATA,"/annual_accounts_selected_sample_key_variables.RData"))
load(paste0(PROC_DATA,"/firm_cncode_year_ef_weighted_qty.RData"))
load(paste0(PROC_DATA,"/fuels_used_for_energy_across_sectors.RData"))
load(paste0(PROC_DATA,"/fuels_used_for_energy_by_sector.RData"))
load(paste0(PROC_DATA,"/likelihood_of_being_fuel_supplier_by_nace.RData"))
load(paste0(PROC_DATA,"/siec_to_ipcc.RData"))
load(paste0(PROC_DATA,"/cn8digit_codes_for_fossil_fuels.RData"))

# ------------------------------------------------------------------
# Strict CN8 list (old / narrow fuel definition)
# ------------------------------------------------------------------

stopifnot(exists("cn8digit_codes_for_fossil_fuels"))

cn8_strict <- cn8digit_codes_for_fossil_fuels %>%
  dplyr::transmute(cn8 = as.character(cn_code)) %>%
  dplyr::filter(!is.na(cn8), nchar(cn8) == 8) %>%
  dplyr::distinct()

saveRDS(cn8_strict, file.path(CACHE_DIR, "aux_cn8_strict.rds"))
log_step("Saved aux_cn8_strict.rds")

# ------------------------------------------------------------------
# Annual accounts NACE helper
# ------------------------------------------------------------------

aa <- df_annual_accounts_selected_sample_key_variables
aa_id <- intersect(names(aa), c("vat_ano","vat"))
stopifnot(length(aa_id) > 0)
aa_id <- aa_id[[1]]

aa_nace <- aa %>%
  transmute(
    firm_id = .data[[aa_id]],
    nace5d = as.character(nace5d),
    nace2d = substr(str_replace_all(nace5d, "\\D", ""), 1, 2),
    nace4d = substr(str_replace_all(nace5d, "\\D", ""), 1, 4)
  ) %>%
  distinct()

saveRDS(aa_nace, file.path(CACHE_DIR, "aux_aa_nace.rds"))
log_step("Saved aux_aa_nace.rds")

# ------------------------------------------------------------------
# Supplier likelihood table
# ------------------------------------------------------------------

likelihood_tbl <- likelihood_of_being_fuel_supplier_by_nace %>%
  transmute(
    nace4d = as.character(nace4d),
    likelihood = as.character(likelihood)
  ) %>%
  distinct()

saveRDS(likelihood_tbl, file.path(CACHE_DIR, "aux_likelihood_tbl.rds"))
log_step("Saved aux_likelihood_tbl.rds")

# ------------------------------------------------------------------
# Customs data with SIEC mapping (broad CH27 definition)
# ------------------------------------------------------------------

customs_siec <- ef_weighted_fuel_qty %>%
  transmute(
    firm_id = vat_ano,
    year = year,
    imports_value = imports_value,
    co2_kg = co2_kg,
    is_euets = is_euets,
    ipcc_fuel = as.character(ipcc_fuel)
  ) %>%
  left_join(
    siec_to_ipcc %>%
      transmute(
        ipcc_fuel = as.character(ipcc_fuel),
        siec_code = as.character(siec_code)
      ),
    by = "ipcc_fuel"
  )

# ------------------------------------------------------------------
# Supplier–year fuel totals (broad CH27)
# ------------------------------------------------------------------

firm_year_fuel_totals <- customs_siec %>%
  group_by(firm_id, year) %>%
  summarise(
    imports_value_total = sum(imports_value, na.rm = TRUE),
    co2_kg_total = sum(co2_kg, na.rm = TRUE),
    is_euets = max(is_euets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    intensity_kg_per_eur =
      if_else(imports_value_total > 0,
              co2_kg_total / imports_value_total,
              NA_real_)
  )

saveRDS(firm_year_fuel_totals,
        file.path(CACHE_DIR, "aux_firm_year_fuel_totals.rds"))
log_step("Saved aux_firm_year_fuel_totals.rds")

# ------------------------------------------------------------------
# Supplier–year CN8 table (for strict CN8 definition)
# ------------------------------------------------------------------

aux_firm_year_cn8 <- ef_weighted_fuel_qty %>%
  transmute(
    supplier_id = vat_ano,
    year = year,
    cn8 = as.character(cncode),
    imports_value = as.numeric(imports_value),
    co2_kg = as.numeric(co2_kg),
    is_euets = as.integer(is_euets)
  ) %>%
  filter(!is.na(supplier_id), !is.na(year), !is.na(cn8))

saveRDS(aux_firm_year_cn8,
        file.path(CACHE_DIR, "aux_firm_year_cn8.rds"))
log_step("Saved aux_firm_year_cn8.rds")

# ------------------------------------------------------------------
# Supplier–year SIEC presence
# ------------------------------------------------------------------

firm_year_siec <- customs_siec %>%
  filter(!is.na(siec_code)) %>%
  distinct(firm_id, year, siec_code)

saveRDS(firm_year_siec, file.path(CACHE_DIR, "aux_firm_year_siec.rds"))
log_step("Saved aux_firm_year_siec.rds")

# ------------------------------------------------------------------
# Fuels used anywhere / by sector
# ------------------------------------------------------------------

siec_all_used <- siec_used_for_energy %>%
  transmute(siec_code = as.character(siec_code)) %>%
  distinct()

saveRDS(siec_all_used, file.path(CACHE_DIR, "aux_siec_all_used.rds"))
log_step("Saved aux_siec_all_used.rds")

buyer_sector_siec <- siec_used_for_energy_by_sector %>%
  transmute(
    nace2d = as.character(nace2d),
    siec_code = as.character(siec_code)
  ) %>%
  distinct()

saveRDS(buyer_sector_siec,
        file.path(CACHE_DIR, "aux_buyer_sector_siec.rds"))
log_step("Saved aux_buyer_sector_siec.rds")

# ------------------------------------------------------------------
# B2B transactions (minimal)
# ------------------------------------------------------------------

b2b <- df_b2b_selected_sample %>%
  transmute(
    supplier_id = vat_i_ano,
    buyer_id = vat_j_ano,
    year = year,
    sales_ij = corr_sales_ij
  )

saveRDS(b2b, file.path(CACHE_DIR, "aux_b2b_minimal.rds"))
log_step("Saved aux_b2b_minimal.rds")

toc()
