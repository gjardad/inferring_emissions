###############################################################################
# 01_preprocess/19_build_hs_cn_to_ipcc_mapping.R
#
# PURPOSE
#   Build mapping from HS6/CN8 customs codes to IPCC fuel categories for emission-factor weighting and fuel-type identification.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/19_build_hs_cn_to_ipcc_mapping.R
###############################################################################

#### HEADER -------

## This code creates mapping between HS/CN 6/8-digit codes and fuel categories
# consistent with IPCC tables on emission factors

#####################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "config", "paths.R"))


# Setup ------


# ==================
# Build mapping ----
# ==================

library(tibble)
library(dplyr)

# HS 6-digit codes -------

hs6_to_ipcc_fuel_categories <- tribble(
  ~hs6,     ~ipcc_fuel,                               ~energy_use,   ~needs_cn8, ~comment,
  
  # ---- COAL & SOLID FUELS ----
  "270111", "Anthracite",                              "core",        FALSE,     "Coal; anthracite (explicit).",
  "270112", "Other Bituminous Coal",                   "core",        FALSE,     "Coal; bituminous. HS6 doesn't separate coking vs other; treat as bituminous (or split later with CN8 if you have it).",
  "270119", "Sub-Bituminous Coal",                     "core",        FALSE,     "Coal; other than anthracite/bituminous. Best proxy in IPCC list is sub-bituminous (residual coal rank).",
  "270120", "Brown Coal Briquettes",                   "borderline",  FALSE,     "Briquettes/ovoids manufactured from coal. Could also be 'Patent Fuel' depending on interpretation; choose one convention and document it.",
  
  # ---- LIGNITE / BROWN COAL ----
  "270210", "Lignite",                                 "core",        FALSE,     "Lignite; not agglomerated.",
  "270220", "Lignite",                                 "core",        FALSE,     "Lignite; agglomerated. (Important: this is NOT 'brown coal briquettes' in CN/HS 2022.)",
  
  # ---- PEAT ----
  "270300", "Peat",                                    "core",        FALSE,     "Peat.",
  
  # ---- COKE & SEMI-COKE ----
  "270400", "Coke Oven Coke and Lignite Coke",         "core",        TRUE,      "Coke and semi-coke incl. retort carbon. HS6 doesn't separate gas coke; CN8 may allow finer split.",
  
  # ---- MANUFACTURED GASES (RARELY TRADED; OFTEN ON-SITE) ----
  "270500", "Gas Works Gas",                            "borderline",  FALSE,     "Coal/water/producer gas etc. In IPCC these are typically on-site industrial gases; treat carefully or exclude unless you truly observe trade flows.",
  
  # ---- COAL TAR & RELATED ----
  "270600", "Coal Tar",                                 "borderline",  FALSE,     "Tar distilled from coal/lignite/peat/mineral tars. Often feedstock; if combusted, IPCC 'Coal Tar' EF applies.",
  
  # ---- COAL TAR DISTILLATES (BENZENE/TOLUENE/XYLENE/ETC.) ----
  # Not cleanly represented in your IPCC list; closest is Coal Tar, but most use is chemical feedstock.
  "270710", NA_character_,                              "non-energy",  FALSE,     "Benzol (benzene): typically chemical feedstock; no direct match in your 53 IPCC fuels. Recommend exclude from combustion-based EF mapping.",
  "270720", NA_character_,                              "non-energy",  FALSE,     "Toluol (toluene): typically chemical feedstock; exclude for combustion EF mapping.",
  "270730", NA_character_,                              "non-energy",  FALSE,     "Xylol (xylenes): typically chemical feedstock; exclude for combustion EF mapping.",
  "270740", NA_character_,                              "non-energy",  FALSE,     "Naphthalene: chemical product; exclude.",
  "270750", NA_character_,                              "non-energy",  FALSE,     "Aromatic hydrocarbon mixtures: typically chemical; exclude.",
  "270791", NA_character_,                              "non-energy",  FALSE,     "Creosote oils: often preservative/chemical; exclude unless you have evidence of combustion use.",
  "270799", NA_character_,                              "non-energy",  FALSE,     "Other coal-tar distillates: exclude by default.",
  
  # ---- PITCH / PITCH COKE ----
  "270810", "Coal Tar",                                 "borderline",  FALSE,     "Pitch obtained from coal tar/mineral tars. Often non-energy use; closest IPCC category is Coal Tar.",
  "270820", "Coke Oven Coke and Lignite Coke",          "borderline",  FALSE,     "Pitch coke: solid carbon. Closest IPCC category is coke; treat as borderline unless you know it's combusted.",
  
  # ---- CRUDE OIL ----
  "270900", "Crude Oil",                                "core",        FALSE,     "Crude petroleum oils.",
  
  # ---- PETROLEUM OILS (HS6 TOO COARSE) ----
  "271012", "Motor Gasoline",                           "core",        TRUE,      "HS6 'not crude' light oils. Could include naphtha/gasoline; CN8 needed to split Motor Gasoline vs Naphtha cleanly.",
  "271019", "Other Petroleum Products",                 "core",        TRUE,      "HS6 'other oils' includes diesel/gasoil, kerosene, residual fuel oil, lubricants etc. CN8 needed for clean mapping to Gas/Diesel Oil, Jet/Other Kerosene, Residual Fuel Oil, Lubricants.",
  "271020", "Biodiesels",                               "borderline",  TRUE,      "Petroleum oils containing biodiesel. EF depends on blend share (fossil vs bio). CN8/extra info needed to do this right.",
  
  # ---- WASTE OILS ----
  "271091", "Waste Oils",                               "borderline",  FALSE,     "Waste oils containing PCBs etc (treat carefully).",
  "271099", "Waste Oils",                               "borderline",  FALSE,     "Other waste oils.",
  
  # ---- PETROLEUM GASES ----
  "271111", "Natural Gas",                              "core",        FALSE,     "Natural gas, liquefied (LNG) in CN/HS 2022.",
  "271112", "Liquefied Petroleum Gases",                "core",        FALSE,     "Liquefied propane.",
  "271113", "Liquefied Petroleum Gases",                "core",        FALSE,     "Liquefied butanes.",
  "271114", "Natural Gas Liquids",                      "borderline",  FALSE,     "Ethylene/propylene/butylene/butadiene. Not a perfect IPCC match; NGL is closest proxy. Flag for sensitivity.",
  "271119", "Liquefied Petroleum Gases",                "core",        FALSE,     "Other liquefied petroleum gases (n.e.c.).",
  "271121", "Natural Gas",                              "core",        FALSE,     "Natural gas in gaseous state.",
  "271129", "Liquefied Petroleum Gases",                "borderline",  FALSE,     "Other gaseous hydrocarbons (not natural gas). Often LPG-like; verify if material in your data.",
  
  # ---- MINERAL WAXES / PETROLEUM JELLY ----
  "271210", "Paraffin Waxes",                           "borderline",  FALSE,     "Petroleum jelly. Closest IPCC category is Paraffin Waxes; usually non-energy use.",
  "271220", "Paraffin Waxes",                           "borderline",  FALSE,     "Paraffin wax (<0.75% oil). Usually non-energy.",
  "271290", "Paraffin Waxes",                           "borderline",  FALSE,     "Other mineral waxes. Usually non-energy.",
  
  # ---- PETROLEUM COKE / BITUMEN / RESIDUES ----
  "271311", "Petroleum Coke",                           "core",        FALSE,     "Petroleum coke, not calcined.",
  "271312", "Petroleum Coke",                           "core",        FALSE,     "Petroleum coke, calcined.",
  "271320", "Bitumen",                                  "borderline",  FALSE,     "Petroleum bitumen; mostly construction. Include only if you justify combustion.",
  "271390", "Other Petroleum Products",                 "borderline",  FALSE,     "Other residues of petroleum oils; may include heavy residues. Borderline.",
  
  # ---- BITUMINOUS MINERALS / TAR SANDS ----
  "271410", "Oil Shale and Tar Sands",                  "borderline",  FALSE,     "Bituminous/oil shale and tar sands.",
  "271490", "Other Petroleum Products",                 "borderline",  FALSE,     "Asphaltites/asphaltic rocks. Often non-energy.",
  
  # ---- BITUMINOUS MIXTURES ----
  "271500", "Other Petroleum Products",                 "borderline",  FALSE,     "Bituminous mixtures (mastics/cut-backs). Typically non-energy."
) %>%
  mutate(hs6 = as.character(hs6))

# ==============================
# CN 8-digit codes -------------
# ==============================

# CN 8-digit codes for 2704 -----

cn8_2704 <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,                         ~include, ~comment,
  "27040010","Coke Oven Coke and Lignite Coke",  TRUE,     "Coke and semi-coke of coal.",
  "27040030","Coke Oven Coke and Lignite Coke",  TRUE,     "Coke and semi-coke of lignite.",
  "27040090","Gas Coke",                         TRUE,     "Other coke/semi-coke incl. retort carbon."
)

# CN 8-digit codes for 2710 -----

cn8_2710_light <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,        ~include, ~comment,
  "27101215","White Spirit and SBP", FALSE, "Special spirits / white spirit.",
  "27101221","White Spirit and SBP", FALSE, "White spirit.",
  "27101225","Motor Gasoline",  TRUE,  "Motor spirit (gasoline).",
  "27101231","Motor Gasoline",  TRUE,  "Motor gasoline, low aromatics.",
  "27101241","Motor Gasoline",  TRUE,  "Motor gasoline RON <95.",
  "27101245","Motor Gasoline",  TRUE,  "Motor gasoline RON 95–98.",
  "27101249","Motor Gasoline",  TRUE,  "Motor gasoline RON ≥98.",
  "27101250","Motor Gasoline",  TRUE,  "Motor gasoline, other.",
  "27101270","Jet Gasoline",    TRUE,  "Jet fuel (gasoline type).",
  "27101290","Other Petroleum Products", FALSE, "Other light oils; non-fuel catch-all."
)

cn8_2710_medium <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,        ~include, ~comment,
  "27101915","Other Kerosene",  TRUE,  "Kerosene (non-jet).",
  "27101921","Jet Kerosene",    TRUE,  "Jet fuel (kerosene type).",
  "27101925","Gas/Diesel Oil",  TRUE,  "Medium oils.",
  "27101929","Gas/Diesel Oil",  TRUE,  "Gas oils (diesel)."
)

cn8_2710_heavy <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,            ~include, ~comment,
  "27101943","Residual Fuel Oil",   TRUE,  "Fuel oils, sulphur ≤0.001%.",
  "27101946","Residual Fuel Oil",   TRUE,  "Fuel oils, sulphur 0.001–0.002%.",
  "27101947","Residual Fuel Oil",   TRUE,  "Fuel oils, sulphur 0.002–0.1%.",
  "27101948","Residual Fuel Oil",   TRUE,  "Fuel oils, sulphur >0.1%."
)

cn8_2710_lubes <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,     ~include, ~comment,
  "27101967","Lubricants",   FALSE, "Lubricating oils.",
  "27101981","Lubricants",   FALSE, "Motor/ compressor / turbine oils.",
  "27101983","Lubricants",   FALSE, "Hydraulic oils.",
  "27101985","Lubricants",   FALSE, "White oils / liquid paraffin.",
  "27101987","Lubricants",   FALSE, "Gear oils.",
  "27101991","Lubricants",   FALSE, "Metal-working compounds.",
  "27101993","Lubricants",   FALSE, "Electrical insulating oils.",
  "27101999","Lubricants",   FALSE, "Other lubricating oils."
)

cn8_2710_other <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,     ~include, ~comment,
  "27102011","Biodiesels",   FALSE, "Fuel containing biodiesel; fossil/bio split unknown.",
  "27102016","Biodiesels",   FALSE, "Fuel containing biodiesel.",
  "27102019","Biodiesels",   FALSE, "Fuel containing biodiesel.",
  "27102032","Residual Fuel Oil", TRUE, "Fuel oils, sulphur ≤0.5%.",
  "27102038","Residual Fuel Oil", TRUE, "Fuel oils, sulphur >0.5%.",
  "27102090","Other Petroleum Products", FALSE, "Other oils.",
  "27109100","Waste Oils",   FALSE, "Waste oils with PCBs.",
  "27109900","Waste Oils",   FALSE, "Other waste oils."
)

# CN 8-digit codes for 2711 -----

cn8_2711 <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,                  ~include, ~comment,
  "27111100","Natural Gas",               TRUE,  "Natural gas (liquefied).",
  "27112111","Natural Gas",               TRUE,  "Natural gas for power/heating.",
  "27112119","Natural Gas",               TRUE,  "Natural gas, other.",
  "27111297","Liquefied Petroleum Gases", TRUE,  "Propane.",
  "27111397","Liquefied Petroleum Gases", TRUE,  "Butanes.",
  "27111400","Natural Gas Liquids",       FALSE, "Ethylene/propylene/etc.; chemical feedstocks.",
  "27111900","Liquefied Petroleum Gases", TRUE,  "Other LPG.",
  "27112900","Liquefied Petroleum Gases", FALSE, "Other gaseous hydrocarbons; ambiguous."
)

# CN 8-digit codes for 2713 -----

cn8_2713 <- tibble::tribble(
  ~cn8,      ~ipcc_fuel,        ~include, ~comment,
  "27131100","Petroleum Coke", TRUE,  "Petroleum coke, not calcined.",
  "27131200","Petroleum Coke", TRUE,  "Petroleum coke, calcined.",
  "27132000","Bitumen",        FALSE, "Petroleum bitumen (construction use).",
  "27139010","Other Petroleum Products", FALSE, "Residues for manufacture.",
  "27139090","Other Petroleum Products", FALSE, "Other residues."
)

# Combine all CN 8-digit codes ----------

cn8_to_ipcc_fuel_categories <- dplyr::bind_rows(
  cn8_2704,
  cn8_2710_light,
  cn8_2710_medium,
  cn8_2710_heavy,
  cn8_2710_lubes,
  cn8_2710_other,
  cn8_2711,
  cn8_2713
)

# save it
save(cn8_to_ipcc_fuel_categories, file = paste0(PROC_DATA, "/cn8_to_ipcc_fuel_categories.RData"))
save(hs6_to_ipcc_fuel_categories, file = paste0(PROC_DATA, "/hs6_to_ipcc_fuel_categories.RData"))
