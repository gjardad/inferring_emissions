###############################################################################
# 01_preprocess/19_build_hs_cn_to_ipcc_mapping.R
#
# PURPOSE
#   Build mapping from HS6/CN8 customs codes to IPCC fuel categories for
#   emission-factor weighting and fuel-type identification.
#
# CLASSIFICATION SOURCE
#   LLM-generated (Claude Opus 4.6) using Prompt 4 in
#   prompts_for_llm_generated_crosswalks.md.
#
# OUTPUTS
#   - data/processed/hs6_to_ipcc_fuel_categories.RData
#   - data/processed/cn8_to_ipcc_fuel_categories.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/19_build_hs_cn_to_ipcc_mapping.R
###############################################################################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(tibble)
library(dplyr)

# =============================================================================
# Part A — HS6-level mapping to IPCC 2006 fuel categories (Chapter 27)
# =============================================================================

hs6_to_ipcc_fuel_categories <- tribble(
  ~hs6,     ~ipcc_fuel,                          ~energy_use,   ~needs_cn8, ~comment,

  # --- Coal (2701) ---
  "270111", "Anthracite",                         "core",        FALSE,      "Direct match to IPCC Anthracite",
  "270112", "Other Bituminous Coal",              "core",        FALSE,      "Includes coking coal; no CN8 split available to separate Coking Coal",
  "270119", "Sub-Bituminous Coal",                "core",        FALSE,      "Residual coal category; primarily sub-bituminous",
  "270120", "Patent Fuel",                        "core",        FALSE,      "Briquettes/ovoids from coal = IPCC Patent Fuel",

  # --- Lignite (2702) ---
  "270210", "Lignite",                            "core",        FALSE,      "Lignite, not agglomerated",
  "270220", "Brown Coal Briquettes",              "core",        FALSE,      "Agglomerated lignite = IPCC Brown Coal Briquettes",

  # --- Peat (2703) ---
  "270300", "Peat",                               "core",        FALSE,      "Direct match to IPCC Peat",

  # --- Coke (2704) ---
  "270400", "Coke Oven Coke and Lignite Coke",   "core",        TRUE,       "CN8 needed: coal coke vs lignite coke vs retort carbon",

  # --- Coal gas (2705) ---
  "270500", "Gas Works Gas",                      "core",        FALSE,      "Coal gas, water gas, producer gas → Gas Works Gas",

  # --- Tar (2706) ---
  "270600", "Coal Tar",                           "borderline",  FALSE,      "Tar from coal/lignite/peat; sometimes burned, often feedstock",

  # --- Coal-tar distillates (2707) ---
  "270710", "Naphtha",                            "borderline",  FALSE,      "Benzol (benzene); coal-tar origin, feedstock use; mapped as Naphtha",
  "270720", "Naphtha",                            "borderline",  FALSE,      "Toluol (toluene); coal-tar aromatic feedstock",
  "270730", "Naphtha",                            "borderline",  FALSE,      "Xylol (xylenes); coal-tar aromatic feedstock",
  "270740", "Naphtha",                            "borderline",  FALSE,      "Naphthalene; coal-tar derived, primarily chemical feedstock",
  "270750", "Naphtha",                            "borderline",  FALSE,      "Other aromatic HC mixtures from coal tar distillation",
  "270791", "Other Petroleum Products",           "borderline",  FALSE,      "Creosote oils; sometimes burned, often wood preservation",
  "270799", "Other Petroleum Products",           "borderline",  FALSE,      "Other coal-tar distillates; mixed uses",

  # --- Pitch and pitch coke (2708) ---
  "270810", "Coal Tar",                           "borderline",  FALSE,      "Pitch from coal tar; residue of tar distillation",
  "270820", "Coke Oven Coke and Lignite Coke",   "borderline",  FALSE,      "Pitch coke; coal-derived coke product",

  # --- Crude petroleum (2709) ---
  "270900", "Crude Oil",                          "core",        FALSE,      "Direct match to IPCC Crude Oil",

  # --- Petroleum oils (2710) ---
  "271012", "Motor Gasoline",                     "core",        TRUE,       "CN8 needed: motor gasoline, avgas, jet gasoline, naphtha, white spirit",
  "271019", "Gas/Diesel Oil",                     "core",        TRUE,       "CN8 needed: kerosene, gas oil, fuel oil, lubricants",
  "271020", "Gas/Diesel Oil",                     "core",        TRUE,       "CN8 needed: biodiesel blends of gas oil and fuel oil",
  "271091", "Waste Oils",                         "non-energy",  FALSE,      "Waste oils with PCBs/PCTs/PBBs; hazardous, not for combustion",
  "271099", "Waste Oils",                         "borderline",  FALSE,      "Other waste oils; may be burned for energy recovery",

  # --- Petroleum gases (2711) ---
  "271111", "Natural Gas",                        "core",        FALSE,      "LNG = IPCC Natural Gas (liquefied for transport)",
  "271112", "Liquefied Petroleum Gases",          "core",        FALSE,      "Liquefied propane → LPG",
  "271113", "Liquefied Petroleum Gases",          "core",        FALSE,      "Liquefied butanes → LPG",
  "271114", "Naphtha",                            "borderline",  FALSE,      "Ethylene/propylene/butylene/butadiene; primarily petrochemical feedstock",
  "271119", "Liquefied Petroleum Gases",          "core",        FALSE,      "Other LPG n.e.s.",
  "271121", "Natural Gas",                        "core",        FALSE,      "Natural gas in gaseous state; direct IPCC match",
  "271129", "Refinery Gas",                       "borderline",  FALSE,      "Other gaseous hydrocarbons; likely refinery off-gases",

  # --- Petroleum jelly and waxes (2712) ---
  "271210", "Paraffin Waxes",                     "non-energy",  FALSE,      "Petroleum jelly; non-energy product",
  "271220", "Paraffin Waxes",                     "non-energy",  FALSE,      "Paraffin wax < 0.75% oil",
  "271290", "Paraffin Waxes",                     "non-energy",  FALSE,      "Other mineral waxes, including microcrystalline",

  # --- Petroleum coke, bitumen, residues (2713) ---
  "271311", "Petroleum Coke",                     "core",        FALSE,      "Petroleum coke, not calcined; used as fuel",
  "271312", "Petroleum Coke",                     "borderline",  FALSE,      "Petroleum coke, calcined; mainly electrode manufacture (non-energy)",
  "271320", "Bitumen",                            "non-energy",  FALSE,      "Petroleum bitumen; road paving, roofing (non-energy)",
  "271390", "Other Petroleum Products",           "borderline",  FALSE,      "Other residues of petroleum oils; mixed uses",

  # --- Oil shale, tar sands, natural bitumen (2714) ---
  "271410", "Oil Shale and Tar Sands",            "core",        FALSE,      "Bituminous or oil shale and tar sands",
  "271490", "Bitumen",                            "non-energy",  FALSE,      "Natural bitumen, asphaltites; primarily non-energy",

  # --- Bituminous mixtures (2715) ---
  "271500", "Bitumen",                            "non-energy",  FALSE,      "Bituminous mixtures; road/construction use"
) %>%
  mutate(hs6 = as.character(hs6))

# =============================================================================
# Part B — CN8-level overrides for HS6 headings with needs_cn8 = TRUE
# =============================================================================

cn8_to_ipcc_fuel_categories <- tribble(
  ~cn8,         ~ipcc_fuel,                          ~include, ~comment,

  # --- CN8 under 2704 (coke/semi-coke) ---
  "27040010",   "Coke Oven Coke and Lignite Coke",   TRUE,    "Coke of coal → Coke Oven Coke",
  "27040030",   "Coke Oven Coke and Lignite Coke",   TRUE,    "Coke of lignite → Lignite Coke (same IPCC category)",
  "27040090",   "Coke Oven Coke and Lignite Coke",   FALSE,   "Retort carbon; industrial non-energy use",

  # --- CN8 under 271012 (light petroleum oils, not crude) ---
  "27101211",   "Naphtha",                            TRUE,    "Light oils for specific process (refinery feedstock)",
  "27101215",   "Naphtha",                            FALSE,   "Light oils for chemical transformation (feedstock, non-energy)",
  "27101221",   "White Spirit and SBP",               TRUE,    "White spirit",
  "27101225",   "White Spirit and SBP",               TRUE,    "Other special boiling point spirits (SBP)",
  "27101231",   "Aviation Gasoline",                   TRUE,    "Aviation spirit (avgas)",
  "27101241",   "Motor Gasoline",                      TRUE,    "Motor gasoline, RON < 95",
  "27101245",   "Motor Gasoline",                      TRUE,    "Motor gasoline, RON 95–98",
  "27101249",   "Motor Gasoline",                      TRUE,    "Motor gasoline, RON >= 98",
  "27101250",   "Motor Gasoline",                      TRUE,    "Motor gasoline, leaded",
  "27101270",   "Jet Gasoline",                        TRUE,    "Spirit type (naphtha-based) jet fuel",
  "27101290",   "Naphtha",                             TRUE,    "Other light oils and preparations n.e.s.",

  # --- CN8 under 271019 (other petroleum oils, medium/heavy) ---
  "27101911",   "Other Kerosene",                      FALSE,   "Medium oils for specific process (non-energy use)",
  "27101915",   "Other Kerosene",                      FALSE,   "Medium oils for chemical transformation (feedstock)",
  "27101921",   "Jet Kerosene",                        TRUE,    "Kerosene-type jet fuel (aviation turbine fuel)",
  "27101925",   "Other Kerosene",                      TRUE,    "Other kerosene (heating, illuminating)",
  "27101929",   "Other Kerosene",                      TRUE,    "Other medium oils n.e.s.",
  "27101931",   "Gas/Diesel Oil",                      FALSE,   "Gas oils for specific process (non-energy use)",
  "27101935",   "Gas/Diesel Oil",                      FALSE,   "Gas oils for chemical transformation (feedstock)",
  "27101943",   "Gas/Diesel Oil",                      TRUE,    "Gas oils, sulphur <= 0.001% (ULSD)",
  "27101946",   "Gas/Diesel Oil",                      TRUE,    "Gas oils, sulphur > 0.001% to 0.002%",
  "27101947",   "Gas/Diesel Oil",                      TRUE,    "Gas oils, sulphur > 0.002% to 0.1%",
  "27101948",   "Gas/Diesel Oil",                      TRUE,    "Gas oils, sulphur > 0.1%",
  "27101951",   "Residual Fuel Oil",                   FALSE,   "Fuel oils for specific process (non-energy use)",
  "27101955",   "Residual Fuel Oil",                   FALSE,   "Fuel oils for chemical transformation (feedstock)",
  "27101962",   "Residual Fuel Oil",                   TRUE,    "Fuel oils, sulphur <= 0.1% (LSFO)",
  "27101966",   "Residual Fuel Oil",                   TRUE,    "Fuel oils, sulphur > 0.1% to 0.5%",
  "27101967",   "Residual Fuel Oil",                   TRUE,    "Fuel oils, sulphur > 0.5% (HSFO)",
  "27101971",   "Lubricants",                          FALSE,   "Lubricating oils for specific process (non-energy)",
  "27101975",   "Lubricants",                          FALSE,   "Lubricating oils for chemical transformation (non-energy)",
  "27101981",   "Lubricants",                          FALSE,   "Motor/compressor/turbine lube oils (non-energy)",
  "27101983",   "Lubricants",                          FALSE,   "Hydraulic oils (non-energy)",
  "27101985",   "Lubricants",                          FALSE,   "White oils, liquid paraffin (non-energy)",
  "27101987",   "Lubricants",                          FALSE,   "Gear oils and reductor oils (non-energy)",
  "27101991",   "Lubricants",                          FALSE,   "Metal-working compounds, mould-release oils (non-energy)",
  "27101993",   "Lubricants",                          FALSE,   "Electrical insulating oils (non-energy)",
  "27101999",   "Lubricants",                          FALSE,   "Other lubricating oils (non-energy)",

  # --- CN8 under 271020 (petroleum oils containing biodiesel) ---
  "27102011",   "Gas/Diesel Oil",                      TRUE,    "Gas oils w/ biodiesel, S <= 0.001%; bio-fraction → Biodiesels",
  "27102016",   "Gas/Diesel Oil",                      TRUE,    "Gas oils w/ biodiesel, S > 0.001% to 0.1%; bio-fraction → Biodiesels",
  "27102019",   "Gas/Diesel Oil",                      TRUE,    "Gas oils w/ biodiesel, S > 0.1%; bio-fraction → Biodiesels",
  "27102032",   "Residual Fuel Oil",                   TRUE,    "Fuel oils w/ biodiesel, S <= 0.5%; bio-fraction → Biodiesels",
  "27102038",   "Residual Fuel Oil",                   TRUE,    "Fuel oils w/ biodiesel, S > 0.5%; bio-fraction → Biodiesels",
  "27102090",   "Other Petroleum Products",            TRUE,    "Other petroleum oils w/ biodiesel; bio-fraction → Biodiesels"
)

# =============================================================================
# Save
# =============================================================================

save(hs6_to_ipcc_fuel_categories, file = paste0(PROC_DATA, "/hs6_to_ipcc_fuel_categories.RData"))
save(cn8_to_ipcc_fuel_categories, file = paste0(PROC_DATA, "/cn8_to_ipcc_fuel_categories.RData"))
