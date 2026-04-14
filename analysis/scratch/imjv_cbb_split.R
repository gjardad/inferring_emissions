# =============================================================================
# CBB = ondernemingsnummer (10 digits) + vestigingsnummer (4 digits)
# Verify this split using matched IMJV-EUTL firms where we know the BvD ID
# =============================================================================

DATA_DIR <- "c:/Users/jota_/Documents/NBB_data"

# Load IMJV raw (character cbb)
imjv_raw <- read.delim(file.path(DATA_DIR, "raw/IMJV/imjv_co2_lucht.tsv"),
                       colClasses = "character")

# Load IMJV-EUTL match
load(file.path(DATA_DIR, "processed", "imjv_eutl_match.RData"))

# Build cbb lookup from raw: firm_name -> cbb_number (character)
cbb_lookup <- unique(imjv_raw[, c("firm_name", "cbb_number")])

# Get matched firms
matched <- imjv_eutl[imjv_eutl$matched == TRUE, ]

# Join cbb_number (character) by name
matched$cbb_char <- cbb_lookup$cbb_number[match(matched$imjv_name, cbb_lookup$firm_name)]

# Load EUTL account -> BvD
library(haven)
eutl_be <- read_dta(file.path(DATA_DIR, "raw/NBB/EUTL_Belgium.dta"))

# installation.csv for name matching
inst <- read.csv(file.path(DATA_DIR, "raw/EUTL/Oct_2024_version/installation.csv"),
                 colClasses = "character")

# Match eutl_name -> installation -> bvdid
normalize <- function(x) tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", x)))
matched$eutl_norm <- normalize(matched$eutl_name)
inst$inst_norm <- normalize(inst$name)

# Join to installation
matched$inst_id <- inst$id[match(matched$eutl_norm, inst$inst_norm)]

# Join to EUTL_Belgium for BvD
eutl_bvd <- unique(eutl_be[, c("installation_id", "bvdid")])
eutl_bvd <- eutl_bvd[!is.na(eutl_bvd$bvdid) & eutl_bvd$bvdid != "", ]
matched$bvdid <- eutl_bvd$bvdid[match(matched$inst_id, eutl_bvd$installation_id)]

# Extract enterprise number from BvD (strip BE prefix)
matched$ent_from_bvd <- gsub("^BE", "", matched$bvdid)

# Now test: does first 10 digits of cbb_char == ent_from_bvd?
has_both <- matched[!is.na(matched$cbb_char) & !is.na(matched$ent_from_bvd), ]
has_both$cbb_first10 <- substr(has_both$cbb_char, 1, 10)
has_both$cbb_last4 <- substr(has_both$cbb_char, 11, 14)

has_both$match_10_4 <- has_both$cbb_first10 == has_both$ent_from_bvd

cat("=== Testing CBB[1:10] == enterprise number ===\n")
cat("Match:", sum(has_both$match_10_4), "/", nrow(has_both), "\n\n")

# Show side by side
cat("=== Side-by-side (first 20) ===\n")
for (i in 1:min(20, nrow(has_both))) {
  cat(sprintf("CBB: %s -> first10: %s | BvD ent: %s | match: %s | suffix: %s | %s\n",
              has_both$cbb_char[i],
              has_both$cbb_first10[i],
              has_both$ent_from_bvd[i],
              has_both$match_10_4[i],
              has_both$cbb_last4[i],
              has_both$imjv_name[i]))
}

# If that doesn't work, also check if enterprise number is embedded differently
# Maybe it's 0 + enterprise_number (without leading 0) + suffix
cat("\n=== Alternative: strip leading 0 from enterprise, check if contained ===\n")
has_both$ent_stripped <- sub("^0", "", has_both$ent_from_bvd)
has_both$cbb_contains_ent <- mapply(function(cbb, ent) grepl(ent, cbb, fixed = TRUE),
                                     has_both$cbb_char, has_both$ent_stripped)
cat("CBB contains enterprise (stripped):", sum(has_both$cbb_contains_ent), "/", nrow(has_both), "\n")

# Show matches
if (any(has_both$cbb_contains_ent)) {
  cat("\nMatches:\n")
  hits <- has_both[has_both$cbb_contains_ent, ]
  for (i in 1:min(10, nrow(hits))) {
    cat(sprintf("CBB: %s | enterprise: %s | %s\n",
                hits$cbb_char[i], hits$ent_from_bvd[i], hits$imjv_name[i]))
  }
}
