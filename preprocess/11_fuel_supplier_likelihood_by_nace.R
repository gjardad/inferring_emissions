###############################################################################
# 01_preprocess/11_fuel_supplier_likelihood_by_nace.R
#
# PURPOSE
#   Classify NACE sectors by likelihood of being a fuel supplier (high/medium/low) for use in proxy restrictions and robustness checks.
#
# INPUTS
#   - data/processed/firm_cncode_year_physical_qty.RData
#
# OUTPUTS
#   - data/processed/likelihood_of_being_fuel_supplier_by_nace.RData
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/11_fuel_supplier_likelihood_by_nace.R
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
source(file.path(REPO_DIR, "config", "paths.R"))


# ------------------------------------------------------------------------------
# Fuel-supplier likelihood classification
#
# Logic:
#   - Identify set of sectors present in set of firms flagged as fuel importers
#
#   Among those, classify into
#   - HIGH: sectors whose business model plausibly involves selling fuel
#   - MEDIUM: intermediaries where fuel can be bundled / pass-through (logistics, retail fuel)
#   - LOW: everything else
#
# Input:  nace5d must be a character of 5 digits (e.g. "35220", "46710")
# Output: adds nace4 (4-digit), nace2 (2-digit), and fuel_supplier_likelihood
# ------------------------------------------------------------------------------

# ------------------
# Set up -----------
# ------------------


library(dplyr)
library(stringr)

# =======================
# Classifier function --------------
# =======================

classify_fuel_supplier_likelihood <- function(df, nace_col = "nace5d") {
  
  nace_sym <- rlang::sym(nace_col)
  
  df %>%
    mutate(
      # ensure clean numeric-only strings
      nace5d = as.character(!!nace_sym),
      nace5d = str_replace_all(nace5d, "\\D", ""),     # drop non-digits if any
      nace5d = str_pad(nace5d, width = 5, side = "left", pad = "0"),
      
      nace4 = substr(nace5d, 1, 4),
      nace2 = substr(nace5d, 1, 2),
      
      fuel_supplier_likelihood = case_when(
        # -----------------------
        # HIGH likelihood
        # -----------------------
        nace4 %in% c(
          "1920",  # refined petroleum products
          "3522",  # distribution of gaseous fuels
          "3523",  # trade of gas through mains
          "4671",  # wholesale of solid, liquid and gaseous fuels
          "4950"   # transport via pipelines (often tightly linked to fuel/gas)
        ) ~ "high",
        
        # -----------------------
        # MEDIUM likelihood
        # -----------------------
        # Retail fuel stations
        nace4 == "4730" ~ "medium",
        
        # Electricity generation: can include utilities that sell energy (but not always "fuel").
        # Keep as HIGH if you want "energy suppliers" broadly, or downgrade to MEDIUM if you want
        # "fossil fuel suppliers" strictly.
        str_detect(nace4, "^351") ~ "medium",
        
        # Transport & storage intermediaries (fuel may be bundled / pass-through)
        nace2 %in% c("49", "52", "53", "50", "51") &
          nace4 != "4950" ~ "medium",
        
        # Wholesale trade: many 46xx are not fuel, but can be intermediaries.
        # Keep non-4671 46xx as medium only if you want a permissive net.
        #nace2 == "46" & nace4 != "4671" ~ "medium",
        
        # -----------------------
        # LOW likelihood
        # -----------------------
        TRUE ~ "low"
      )
    )
}

# -----------------------
# Implement it ----------
# -----------------------

load(paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))

# unique_nace must have column nace5d (as in your object)
unique_nace_classified <- classify_fuel_supplier_likelihood(fuel_qty, "nace5d")

# If you want the mapping table (unique nace4 with likelihood):
nace4_mapping <- unique_nace_classified %>%
  distinct(nace4, fuel_supplier_likelihood) %>%
  arrange(nace4) %>%
  rename(nace4d = nace4, likelihood = fuel_supplier_likelihood) %>% 
  filter(!is.na(nace4d))

likelihood_of_being_fuel_supplier_by_nace <- nace4_mapping

# save it
save(likelihood_of_being_fuel_supplier_by_nace,
     file = paste0(PROC_DATA, "/likelihood_of_being_fuel_supplier_by_nace.RData"))
