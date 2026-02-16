###############################################################################
# 01_preprocess/30_build_hs_to_siec_map.R
#
# PURPOSE
#   Build many-to-many crosswalk from HS codes to SIEC v1.0 fuel categories.
#
# CLASSIFICATION SOURCE
#   LLM-generated (Claude Opus 4.6) using Prompt 5 in
#   prompts_for_llm_generated_crosswalks.md.
#   Reference: IRES 2018, Table 3.1
#
# OUTPUTS
#   - data/processed/hs_to_siec_map.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/30_build_hs_to_siec_map.R
###############################################################################

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
source(file.path(REPO_DIR, "paths.R"))

library(tibble)

# =============================================================================
# HS <-> SIEC v1.0 many-to-many crosswalk
# Reference: IRES 2018, Table 3.1
# Scope: Chapter 27 + selected codes from Ch. 22, 28, 29
# =============================================================================

hs_to_siec_map <- tribble(
  ~hs_code,  ~siec_code,      ~siec_description,

  # --- Coal ---
  "270111",  "C0110",         "Anthracite",
  "270112",  "C0121",         "Coking coal",
  "270112",  "C0129",         "Other bituminous coal",
  "270119",  "C0129",         "Other bituminous coal",
  "270119",  "C0210",         "Sub-bituminous coal",
  "270120",  "C0320",         "Patent fuel",

  # --- Lignite ---
  "270210",  "C0220",         "Lignite",
  "270220",  "C0330",         "Brown coal briquettes (BKB)",

  # --- Peat (single HS 4-digit covers all peat forms) ---
  "2703",    "P1110",         "Sod peat",
  "2703",    "P1120",         "Milled peat",
  "2703",    "P1210",         "Peat briquettes",
  "2703",    "P1290",         "Other peat products",

  # --- Coke and semi-coke ---
  "2704",    "C0311",         "Coke oven coke",
  "2704",    "C0312",         "Gas coke",
  "2704",    "C0313",         "Coke breeze",
  "2704",    "C0314",         "Semi-cokes",

  # --- Coal gas, water gas, producer gas ---
  "2705",    "C0350",         "Coke oven gas",
  "2705",    "C0360",         "Gas works gas",
  "2705",    "C0371",         "Blast furnace gas",
  "2705",    "C0372",         "Basic oxygen steel furnace gas",
  "2705",    "C0379",         "Other recovered gases",

  # --- Coal tar ---
  "2706",    "C0340",         "Coal tar",

  # --- Oils from coal tar distillation ---
  "2707",    "C0390",         "Other coal products",

  # --- Pitch and pitch coke ---
  "270810",  "C0390",         "Other coal products",
  "270820",  "C0390",         "Other coal products",

  # --- Crude petroleum ---
  "2709",    "O4100_TOT",     "Crude oil (conventional)",
  "2709",    "O4200",         "Natural gas liquids (NGL)",

  # --- Light petroleum oils (>= 90% vol. distils <= 210 C) ---
  #     271011 = HS 2002/07 edition; 271012 = HS 2012+ replacement
  "271011",  "O4640",         "Naphtha",
  "271011",  "O4651",         "Aviation gasoline",
  "271011",  "O4652",         "Motor gasoline",
  "271011",  "O4653",         "Gasoline-type jet fuel",
  "271011",  "O4691",         "White spirit and SBP",

  "271012",  "O4640",         "Naphtha",
  "271012",  "O4651",         "Aviation gasoline",
  "271012",  "O4652",         "Motor gasoline",
  "271012",  "O4653",         "Gasoline-type jet fuel",
  "271012",  "O4691",         "White spirit and SBP",

  # --- Other (medium / heavy) petroleum oils ---
  "271019",  "O4661",         "Kerosene-type jet fuel",
  "271019",  "O4669",         "Other kerosene",
  "271019",  "O4671",         "Gas oil and diesel oil",
  "271019",  "O4680",         "Fuel oil",
  "271019",  "O4692",         "Lubricants",
  "271019",  "O4699",         "Other oil products n.e.c.",

  # --- Petroleum oils containing biodiesel ---
  "271020",  "O4671",         "Gas oil and diesel oil",

  # --- Waste oils ---
  "271091",  "O4699",         "Other oil products n.e.c.",
  "271099",  "O4699",         "Other oil products n.e.c.",

  # --- LNG ---
  "271111",  "G3000",         "Natural gas",

  # --- Liquefied propane / butanes / other LPG ---
  "271112",  "O4630",         "Liquefied petroleum gases (LPG)",
  "271113",  "O4630",         "Liquefied petroleum gases (LPG)",
  "271119",  "O4630",         "Liquefied petroleum gases (LPG)",

  # --- Ethylene, propylene, butylene, butadiene ---
  "271114",  "O4620",         "Ethane",
  "271114",  "O4500",         "Other hydrocarbons",

  # --- Natural gas (gaseous state) ---
  "271121",  "G3000",         "Natural gas",

  # --- Other gaseous hydrocarbons ---
  "271129",  "O4610",         "Refinery gas",

  # --- Petroleum jelly & paraffin waxes ---
  "271210",  "O4693",         "Paraffin waxes",
  "271220",  "O4693",         "Paraffin waxes",
  "271290",  "O4693",         "Paraffin waxes",

  # --- Petroleum coke ---
  "271311",  "O4694",         "Petroleum coke",
  "271312",  "O4694",         "Petroleum coke",

  # --- Petroleum bitumen ---
  "271320",  "O4695",         "Bitumen",

  # --- Other residues of petroleum oils ---
  "271390",  "O4699",         "Other oil products n.e.c.",

  # --- Oil shale / tar sands / natural bitumen ---
  "271410",  "S2000",         "Oil shale / oil sands",
  "271490",  "S2000",         "Oil shale / oil sands",
  "271490",  "O4695",         "Bitumen",

  # --- Bituminous mixtures ---
  "271500",  "O4695",         "Bitumen",
  "271500",  "O4699",         "Other oil products n.e.c.",

  # --- Hydrogen ---
  "280410",  "O4500",         "Other hydrocarbons",

  # --- Additives and oxygenates ---
  "220720",  "O4400",         "Additives and oxygenates",
  "290511",  "O4400",         "Additives and oxygenates",
  "290919",  "O4400",         "Additives and oxygenates"
)

# =============================================================================
# Save
# =============================================================================

save(hs_to_siec_map, file = paste0(PROC_DATA, "/hs_to_siec_map.RData"))
