###############################################################################
# fuel_proxy_builders.R
#
# PURPOSE:
#   Builds a fuel consumption proxy for any combination of proxy modifications,
#   using cached auxiliary tables in /output/loocv/cache.
#
# OUTPUT:
#   Returns a tibble: buyer_id, year, fuel_proxy
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))

load_aux <- function() {
  list(
    aa_nace = readRDS(file.path(CACHE_DIR, "aux_aa_nace.rds")),
    likelihood_tbl = readRDS(file.path(CACHE_DIR, "aux_likelihood_tbl.rds")),
    cn8_strict = readRDS(file.path(CACHE_DIR, "aux_cn8_strict.rds")),
    firm_year_fuel_totals = readRDS(file.path(CACHE_DIR, "aux_firm_year_fuel_totals.rds")),
    firm_year_cn8 = readRDS(file.path(CACHE_DIR, "aux_firm_year_cn8.rds")),
    firm_year_siec = readRDS(file.path(CACHE_DIR, "aux_firm_year_siec.rds")),
    siec_all_used = readRDS(file.path(CACHE_DIR, "aux_siec_all_used.rds")),
    buyer_sector_siec = readRDS(file.path(CACHE_DIR, "aux_buyer_sector_siec.rds")),
    b2b = readRDS(file.path(CACHE_DIR, "aux_b2b_minimal.rds"))
  )
}

build_fuel_proxy <- function(mods, aux) {
  
  mods <- modifyList(list(
    fuel_def = "broad_ch27",        # "broad_ch27" | "strict_cn8"
    use_siec_all = FALSE,
    supplier_non_euets = FALSE,
    buyer_sector_siec = FALSE,
    supplier_nace_filter = "none",  # "none" | "high" | "high_medium"
    emissions_weighted = FALSE
  ), mods)
  
  # ---------------------------------------------------------------------------
  # 0) Supplier universe (fuel importers) depends on fuel_def
  # ---------------------------------------------------------------------------
  
  if (identical(mods$fuel_def, "broad_ch27")) {
    
    suppliers <- aux$firm_year_fuel_totals %>%
      dplyr::transmute(
        supplier_id = firm_id,
        year,
        imports_value_total,
        co2_kg_total,
        intensity_kg_per_eur,
        is_euets
      )
    
    supplier_year_siec_tbl <- aux$firm_year_siec  %>%
      dplyr::transmute(supplier_id = firm_id, year, siec_code) %>%
      dplyr::distinct()
    
  } else if (identical(mods$fuel_def, "strict_cn8")) {
    
    # firm-year totals restricted to strict CN8 list
    strict_totals <- aux$firm_year_cn8 %>%
      dplyr::inner_join(aux$cn8_strict, by = "cn8") %>%
      dplyr::group_by(supplier_id, year) %>%
      dplyr::summarise(
        imports_value_total = sum(imports_value, na.rm = TRUE),
        co2_kg_total = sum(co2_kg, na.rm = TRUE),
        is_euets = dplyr::if_else(all(is.na(is_euets)), NA_integer_, max(is_euets, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        intensity_kg_per_eur = dplyr::if_else(imports_value_total > 0,
                                              co2_kg_total / imports_value_total,
                                              NA_real_)
      )
    
    suppliers <- strict_totals
    
    # keep the same SIEC presence table (it’s fine; you’re only changing who counts as fuel importer)
    supplier_year_siec_tbl <- aux$firm_year_siec  %>%
      dplyr::transmute(supplier_id = firm_id, year, siec_code) %>%
      dplyr::distinct()
    
  } else {
    stop("Unknown fuel_def: ", mods$fuel_def)
  }
  
  # ---------------------------------------------------------------------------
  # 1) Mod: restrict suppliers to those importing fuels used in energy (all sectors)
  # ---------------------------------------------------------------------------
  if (isTRUE(mods$use_siec_all)) {
    
    suppliers_siec_ok <- supplier_year_siec_tbl %>%
      dplyr::inner_join(aux$siec_all_used, by = "siec_code") %>%
      dplyr::distinct(supplier_id, year) %>%
      dplyr::mutate(ok = TRUE)
    
    suppliers <- suppliers %>%
      dplyr::left_join(suppliers_siec_ok, by = c("supplier_id", "year")) %>%
      dplyr::mutate(ok = dplyr::if_else(is.na(ok), FALSE, ok)) %>%
      dplyr::filter(ok) %>%
      dplyr::select(-ok)
  
  }
  
  # ---------------------------------------------------------------------------
  # 2) Mod: exclude EUETS suppliers
  # ---------------------------------------------------------------------------
  if (isTRUE(mods$supplier_non_euets)) {
    suppliers <- suppliers %>% dplyr::filter(is_euets == 0)
  }
  
  # ---------------------------------------------------------------------------
  # 3) Mod: supplier NACE filter (high / high+medium)
  # ---------------------------------------------------------------------------
  if (!identical(mods$supplier_nace_filter, "none")) {
    
    sup_nace <- aux$aa_nace %>%
      dplyr::transmute(supplier_id = firm_id, nace4d) %>%
      dplyr::left_join(aux$likelihood_tbl, by = "nace4d")
    
    suppliers <- suppliers %>% dplyr::left_join(sup_nace, by = "supplier_id")
    
    if (identical(mods$supplier_nace_filter, "high")) {
      suppliers <- suppliers %>% dplyr::filter(likelihood == "high")
    } else if (identical(mods$supplier_nace_filter, "high_medium")) {
      suppliers <- suppliers %>% dplyr::filter(likelihood %in% c("high", "medium"))
    } else {
      stop("Unknown supplier_nace_filter: ", mods$supplier_nace_filter)
    }
    
    suppliers <- suppliers %>% dplyr::select(-nace4d, -likelihood)
  }
  
  # ---------------------------------------------------------------------------
  # 4) Join B2B edges to suppliers (this is the baseline purchases component)
  # ---------------------------------------------------------------------------
  b2b_edges <- aux$b2b %>%
    dplyr::inner_join(suppliers, by = c("supplier_id", "year"))
  
  # ---------------------------------------------------------------------------
  # 5) Mod: buyer-sector-specific fuel restriction (THIS BLOCK MUST BE SELF-CONTAINED)
  # ---------------------------------------------------------------------------
  if (isTRUE(mods$buyer_sector_siec)) {
    
    buyers_nace2d <- aux$aa_nace %>%
      dplyr::transmute(buyer_id = firm_id, nace2d) %>%
      dplyr::distinct()
    
    buyer_year_allowed <- b2b_edges %>%
      dplyr::distinct(buyer_id, year) %>%
      dplyr::left_join(buyers_nace2d, by = "buyer_id") %>%
      dplyr::left_join(aux$buyer_sector_siec, by = "nace2d") %>%
      dplyr::filter(!is.na(siec_code)) %>%
      dplyr::select(buyer_id, year, siec_code) %>%
      dplyr::distinct()
    
    eligible_pairs <- buyer_year_allowed %>%
      dplyr::inner_join(supplier_year_siec_tbl, by = c("year", "siec_code")) %>%
      dplyr::distinct(buyer_id, supplier_id, year)
    
    b2b_edges <- b2b_edges %>%
      dplyr::inner_join(eligible_pairs, by = c("buyer_id", "supplier_id", "year"))
  }
  
  # ---------------------------------------------------------------------------
  # 6) Emissions-weighted vs value-weighted contributions
  # ---------------------------------------------------------------------------
  if (isTRUE(mods$emissions_weighted)) {
    b2b_edges <- b2b_edges %>% dplyr::mutate(contrib = sales_ij * intensity_kg_per_eur)
  } else {
    b2b_edges <- b2b_edges %>% dplyr::mutate(contrib = sales_ij)
  }
  
  purchases_component <- b2b_edges %>%
    dplyr::group_by(buyer_id, year) %>%
    dplyr::summarise(purchases = sum(contrib, na.rm = TRUE), .groups = "drop")
  
  # Buyer’s own imports component depends on fuel_def + emissions_weighted
  if (identical(mods$fuel_def, "broad_ch27")) {
    
    buyer_own <- aux$firm_year_fuel_totals %>%
      dplyr::transmute(
        buyer_id = firm_id,
        year,
        own_imports_value = imports_value_total,
        own_co2_kg = co2_kg_total
      )
    
  } else { # strict_cn8
    buyer_own <- aux$firm_year_cn8 %>%
      dplyr::inner_join(aux$cn8_strict, by = "cn8") %>%
      dplyr::group_by(supplier_id, year) %>%
      dplyr::summarise(
        own_imports_value = sum(imports_value, na.rm = TRUE),
        own_co2_kg = sum(co2_kg, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        buyer_id = supplier_id,
        year,
        own_imports_value,
        own_co2_kg
      )
  }
  
  proxy <- purchases_component %>%
    dplyr::full_join(buyer_own, by = c("buyer_id", "year")) %>%
    dplyr::mutate(
      purchases = dplyr::coalesce(purchases, 0),
      own_imports_value = dplyr::coalesce(own_imports_value, 0),
      own_co2_kg = dplyr::coalesce(own_co2_kg, 0),
      fuel_proxy = if (isTRUE(mods$emissions_weighted)) purchases + own_co2_kg else purchases + own_imports_value
    ) %>%
    dplyr::select(buyer_id, year, fuel_proxy)
  
  proxy
}
