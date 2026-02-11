###############################################################################
# preprocess/00_build_downsampled_raw_data.R
#
# PURPOSE
#   Create downsampled versions of the three large raw .dta files
#   (Annual_Accounts, B2B, import_export) for local development on LD2.
#
#   Strategy: sample ~1,000 firms that appear in ALL three datasets,
#   seeded with:
#     1. EU ETS-regulated firms (so the sample has firms with emissions data)
#     2. Fuel importers (preserve supply-chain structure)
#     3. Downstream B2B buyers of fuel importers
#     4. Random firms to pad to target
#
# OUTPUTS  (saved to RAW_DATA/NBB/downsampled/)
#   Annual_Accounts_MASTER_ANO_sample.dta
#   B2B_ANO_sample.dta
#   import_export_ANO_sample.dta
#   sampled_firms.rds          (the firm IDs, for reference)
#
# RUN ON: RMD (jardang)
###############################################################################

# ---- Setup ----
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(haven)
library(dplyr)

set.seed(42)
TARGET_N_FIRMS <- 2000
RAW_NBB        <- file.path(RAW_DATA, "NBB")
OUT_DIR        <- file.path(RAW_NBB, "downsampled")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ===========================================================================
# Step 1: Collect firm IDs from each dataset
# ===========================================================================
cat("=== Step 1: Collect firm IDs from each dataset ===\n")

# ---- 1a. Annual Accounts: read just vat_ano ----
cat("  Reading firm IDs from Annual_Accounts...\n")
aa_ids <- read_dta(file.path(RAW_NBB, "Annual_Accounts_MASTER_ANO.dta"),
                   col_select = "vat_ano") %>%
  distinct(vat_ano) %>%
  pull(vat_ano)
cat("    Unique firms in Annual_Accounts:", length(aa_ids), "\n")

# ---- 1b. Import/Export: read vat_ano, flow, cncode to identify fuel importers ----
cat("  Reading firm IDs + fuel info from import_export...\n")
trade_slim <- read_dta(file.path(RAW_NBB, "import_export_ANO.dta"),
                       col_select = c("vat_ano", "flow", "cncode"))

trade_ids <- trade_slim %>% distinct(vat_ano) %>% pull(vat_ano)
cat("    Unique firms in import_export:", length(trade_ids), "\n")

# Identify fuel importers (Chapter 27, excluding 2716)
fuel_importers <- trade_slim %>%
  filter(flow == "I",
         substr(cncode, 1, 2) == "27",
         substr(cncode, 1, 4) != "2716") %>%
  distinct(vat_ano) %>%
  pull(vat_ano)
cat("    Fuel importers identified:", length(fuel_importers), "\n")

rm(trade_slim)
gc()

# ---- 1c. B2B: read just supplier and buyer IDs ----
cat("  Reading firm IDs from B2B...\n")
b2b_ids_raw <- read_dta(file.path(RAW_NBB, "B2B_ANO.dta"),
                        col_select = c("vat_i_ano", "vat_j_ano"))

b2b_firms <- union(
  b2b_ids_raw %>% distinct(vat_i_ano) %>% pull(vat_i_ano),
  b2b_ids_raw %>% distinct(vat_j_ano) %>% pull(vat_j_ano)
)
cat("    Unique firms in B2B:", length(b2b_firms), "\n")

# ---- 1d. EU ETS firms ----
cat("  Reading EU ETS firm IDs from EUTL_Belgium...\n")
euets_firms <- read_dta(file.path(RAW_NBB, "EUTL_Belgium.dta")) %>%
  filter(!is.na(vat_ano) & vat_ano != "") %>%
  distinct(vat_ano) %>%
  pull(vat_ano)
cat("    EU ETS firms with valid vat_ano:", length(euets_firms), "\n")

# ===========================================================================
# Step 2: Find fuel importers' downstream B2B buyers
# ===========================================================================
cat("\n=== Step 2: Identify downstream buyers of fuel importers ===\n")

fuel_buyers <- b2b_ids_raw %>%
  filter(vat_i_ano %in% fuel_importers) %>%
  distinct(vat_j_ano) %>%
  pull(vat_j_ano)
cat("  Downstream buyers of fuel importers:", length(fuel_buyers), "\n")

rm(b2b_ids_raw)
gc()

# ===========================================================================
# Step 3: Intersect across all three datasets
# ===========================================================================
cat("\n=== Step 3: Intersect firm IDs across all three datasets ===\n")

firms_in_all_three <- Reduce(intersect, list(aa_ids, trade_ids, b2b_firms))
cat("  Firms present in all three datasets:", length(firms_in_all_three), "\n")

# ===========================================================================
# Step 4: Build the sample with priority seeding
# ===========================================================================
cat("\n=== Step 4: Build sample of ~", TARGET_N_FIRMS, " firms ===\n")

# Priority 1: EU ETS firms (need emissions for the outcome variable)
seed_euets <- intersect(euets_firms, firms_in_all_three)
cat("  [Priority 1] EU ETS firms in all three datasets:", length(seed_euets), "\n")

# Priority 2: Fuel importers (not already in EU ETS seed)
seed_importers <- intersect(fuel_importers, firms_in_all_three)
seed_importers <- setdiff(seed_importers, seed_euets)
cat("  [Priority 2] Fuel importers (excl. EU ETS):", length(seed_importers), "\n")

# Priority 3: Downstream buyers of fuel importers
seed_buyers <- intersect(fuel_buyers, firms_in_all_three)
seed_buyers <- setdiff(seed_buyers, c(seed_euets, seed_importers))
cat("  [Priority 3] Fuel buyers (excl. above):", length(seed_buyers), "\n")

# Assemble: start with all EU ETS firms
sampled_firms <- seed_euets

# Add fuel importers (all if room, subsample if not)
if (length(sampled_firms) < TARGET_N_FIRMS) {
  n_add <- min(length(seed_importers), TARGET_N_FIRMS - length(sampled_firms))
  sampled_firms <- c(sampled_firms, sample(seed_importers, n_add))
}

# Add fuel buyers
if (length(sampled_firms) < TARGET_N_FIRMS) {
  n_add <- min(length(seed_buyers), TARGET_N_FIRMS - length(sampled_firms))
  sampled_firms <- c(sampled_firms, sample(seed_buyers, n_add))
}

# Pad with random firms from the intersection
if (length(sampled_firms) < TARGET_N_FIRMS) {
  remaining_pool <- setdiff(firms_in_all_three, sampled_firms)
  n_pad <- min(length(remaining_pool), TARGET_N_FIRMS - length(sampled_firms))
  sampled_firms <- c(sampled_firms, sample(remaining_pool, n_pad))
}

sampled_firms <- unique(sampled_firms)

# Report composition
n_euets_in_sample <- sum(sampled_firms %in% euets_firms)
n_fuel_imp_in_sample <- sum(sampled_firms %in% fuel_importers)
cat("\n  Final sample:", length(sampled_firms), "firms\n")
cat("    of which EU ETS:", n_euets_in_sample, "\n")
cat("    of which fuel importers:", n_fuel_imp_in_sample, "\n")

# Save the firm list for reference
saveRDS(sampled_firms, file.path(OUT_DIR, "sampled_firms.rds"))

# ===========================================================================
# Step 5: Filter and save downsampled datasets
# ===========================================================================
cat("\n=== Step 5: Filter and save downsampled datasets ===\n")

# 5a. Annual Accounts
cat("  Filtering Annual_Accounts...\n")
aa_sample <- read_dta(file.path(RAW_NBB, "Annual_Accounts_MASTER_ANO.dta")) %>%
  filter(vat_ano %in% sampled_firms)
cat("    Rows:", nrow(aa_sample), "\n")
write_dta(aa_sample, file.path(OUT_DIR, "Annual_Accounts_MASTER_ANO_sample.dta"))
rm(aa_sample)
gc()

# 5b. Import/Export
cat("  Filtering import_export...\n")
trade_sample <- read_dta(file.path(RAW_NBB, "import_export_ANO.dta")) %>%
  filter(vat_ano %in% sampled_firms)
cat("    Rows:", nrow(trade_sample), "\n")
write_dta(trade_sample, file.path(OUT_DIR, "import_export_ANO_sample.dta"))
rm(trade_sample)
gc()

# 5c. B2B — keep rows where EITHER supplier or buyer is in the sample
cat("  Filtering B2B...\n")
b2b_sample <- read_dta(file.path(RAW_NBB, "B2B_ANO.dta")) %>%
  filter(vat_i_ano %in% sampled_firms | vat_j_ano %in% sampled_firms)
cat("    Rows:", nrow(b2b_sample), "\n")
write_dta(b2b_sample, file.path(OUT_DIR, "B2B_ANO_sample.dta"))
rm(b2b_sample)
gc()

cat("\n=== Done! Downsampled files saved to:", OUT_DIR, "===\n")
cat("  - Annual_Accounts_MASTER_ANO_sample.dta\n")
cat("  - B2B_ANO_sample.dta\n")
cat("  - import_export_ANO_sample.dta\n")
cat("  - sampled_firms.rds\n")
