###############################################################################
# 01_preprocess/11_fuel_supplier_likelihood_by_nace.R
#
# PURPOSE
#   Classify NACE sectors by likelihood of being a fuel supplier
#   (high/medium/low) for use in proxy restrictions and robustness checks.
#
# INPUTS
#   - data/processed/firm_cncode_year_physical_qty.RData
#
# OUTPUTS
#   - data/processed/likelihood_of_being_fuel_supplier_by_nace.RData
#
# CLASSIFICATION SOURCE
#   LLM-generated (Claude Opus 4.6) using Prompt 2 in
#   prompts_for_llm_generated_crosswalks.md. The prompt feeds all 615
#   NACE Rev. 2 four-digit codes with non-tautological classification
#   criteria and Belgian institutional context.
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
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(tibble)
library(stringr)

# ===========================================================================
# LLM-generated classification (Prompt 2, with Belgium context)
#
# HIGH:   sector's defining activity involves purchasing and reselling fuel
# MEDIUM: fuel sales plausible as secondary activity or intermediation
# LOW:    default for any NACE code not listed below
# ===========================================================================

fuel_supplier_classification <- tribble(
  ~nace4d, ~likelihood, ~comment,

  # --- HIGH ---
  "0510", "high", "Coal mining; primary output is coal sold B2B",
  "0520", "high", "Lignite mining; primary output is lignite sold B2B",
  "0610", "high", "Crude petroleum extraction; sells crude to refiners/traders",
  "0620", "high", "Natural gas extraction; sells gas B2B",
  "1910", "high", "Coke oven products; produces and sells coke, coal tar, etc.",
  "1920", "high", "Refining; sells diesel, gasoline, fuel oil, naphtha, LPG, petcoke B2B (Antwerp cluster)",
  "3521", "high", "Manufacture of gas; produces and sells manufactured/synthesis gas",
  "3522", "high", "Distribution of gaseous fuels through mains; sells/delivers gas to end-users incl. firms",
  "3523", "high", "Trade of gas through mains; explicit gas trading activity (Zeebrugge hub)",
  "4612", "high", "Agents/brokers for fuels, ores, metals, chemicals; fuel brokerage is a core activity",
  "4671", "high", "Wholesale of solid, liquid and gaseous fuels; the defining fuel wholesale code",

  # --- MEDIUM ---
  "0892", "medium", "Peat extraction; peat is a fossil fuel, sold B2B but niche in Belgium",
  "0910", "medium", "Support activities for petroleum/gas extraction; facilitates fuel supply chain, may broker",
  "4619", "medium", "Agents in sale of variety of goods; may include fuel among mixed product portfolio",
  "4675", "medium", "Wholesale of chemical products; overlap with petroleum-derived chemicals/solvents",
  "4676", "medium", "Wholesale of other intermediate products; can include petroleum-based intermediates",
  "4690", "medium", "Non-specialised wholesale; may include fuel among diversified product lines",
  "4730", "medium", "Retail sale of automotive fuel; primarily B2C but some B2B sales to fleets/businesses",
  "4950", "medium", "Pipeline transport; in Belgian context (Fluxys/Zeebrugge), closely linked to gas balancing/supply",
  "5020", "medium", "Sea/coastal freight transport; bunkering fuel sales to vessels are B2B fuel transactions",
  "5210", "medium", "Warehousing/storage; includes fuel tank terminals (Antwerp/Ghent) that may trade stored fuel",
  "5222", "medium", "Service activities incidental to water transport; port-based bunkering and fuel provisioning",
  "6612", "medium", "Commodity contracts brokerage; includes fossil fuel commodity trading/derivatives"
)

# ===========================================================================
# Classify firms
# ===========================================================================

load(paste0(PROC_DATA, "/firm_cncode_year_physical_qty.RData"))

fuel_qty <- fuel_qty %>%
  mutate(
    nace5d = as.character(nace5d),
    nace5d = str_replace_all(nace5d, "\\D", ""),
    nace5d = str_pad(nace5d, width = 5, side = "left", pad = "0"),
    nace4d = substr(nace5d, 1, 4)
  ) %>%
  left_join(fuel_supplier_classification %>% select(nace4d, likelihood),
            by = "nace4d") %>%
  mutate(likelihood = if_else(is.na(likelihood), "low", likelihood))

# Mapping table: unique nace4d with likelihood
likelihood_of_being_fuel_supplier_by_nace <- fuel_qty %>%
  distinct(nace4d, likelihood) %>%
  arrange(nace4d) %>%
  filter(!is.na(nace4d))

# Save
save(likelihood_of_being_fuel_supplier_by_nace,
     file = paste0(PROC_DATA, "/likelihood_of_being_fuel_supplier_by_nace.RData"))
