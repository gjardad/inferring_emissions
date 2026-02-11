###############################################################################
# preprocess/00b_refilter_b2b_downsampled.R
#
# PURPOSE
#   Re-filter B2B using the already-generated sampled_firms.rds,
#   with a stricter AND filter (both supplier and buyer in sample).
#   Run this standalone after 00_build_downsampled_raw_data.R has already
#   produced sampled_firms.rds.
#
# RUN ON: RMD (jardang)
###############################################################################

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

RAW_NBB <- file.path(RAW_DATA, "NBB")
OUT_DIR <- file.path(RAW_NBB, "downsampled")

# Load the firm list from the previous run
sampled_firms <- readRDS(file.path(OUT_DIR, "sampled_firms.rds"))
cat("Loaded", length(sampled_firms), "sampled firms\n")

# Re-filter B2B with AND (both supplier and buyer in sample)
cat("Filtering B2B (both sides in sample)...\n")
b2b_sample <- read_dta(file.path(RAW_NBB, "B2B_ANO.dta")) %>%
  filter(vat_i_ano %in% sampled_firms & vat_j_ano %in% sampled_firms)
cat("  Rows:", nrow(b2b_sample), "\n")

write_dta(b2b_sample, file.path(OUT_DIR, "B2B_ANO.dta"))
cat("Done! Saved to:", file.path(OUT_DIR, "B2B_ANO.dta"), "\n")
