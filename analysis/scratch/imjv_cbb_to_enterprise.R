# =============================================================================
# Reverse-engineer the IMJV 14-digit CBB number -> CBE enterprise number mapping
# =============================================================================

library(haven)

DATA_DIR <- "c:/Users/jota_/Documents/NBB_data"

# --- 1. Load IMJV CO2 data directly (to get cbb_number as character) ---
imjv_raw <- read.delim(file.path(DATA_DIR, "raw/IMJV/imjv_co2_lucht.tsv"),
                       colClasses = "character")
cat("IMJV raw columns:", paste(names(imjv_raw), collapse = ", "), "\n")
cat("Sample cbb_numbers:\n")
print(head(unique(imjv_raw$cbb_number), 10))
cat("Nchar of cbb_number:", unique(nchar(imjv_raw$cbb_number)), "\n\n")

# --- 2. Load IMJV-EUTL match (for the name-based matching) ---
load(file.path(DATA_DIR, "processed", "imjv_eutl_match.RData"))

# cbb_number was stored as numeric -- recover from raw
imjv_eutl$cbb_char <- sprintf("%.0f", imjv_eutl$cbb_number)
# Pad to 14 digits
imjv_eutl$cbb_char <- sprintf("%014s", imjv_eutl$cbb_char)

matched <- imjv_eutl[imjv_eutl$matched == TRUE, ]
cat("Matched firms:", nrow(matched), "\n\n")

# --- 3. Load EUTL data and build installation -> BvD ID mapping ---
# EUTL_Belgium has installation_id and bvdid
eutl_be <- read_dta(file.path(DATA_DIR, "raw/NBB/EUTL_Belgium.dta"))

# Get Belgian installations with BvD IDs
eutl_bvd <- eutl_be[!is.na(eutl_be$bvdid) & eutl_be$bvdid != "", ]
eutl_bvd <- eutl_bvd[!is.na(eutl_bvd$installation_id) & eutl_bvd$installation_id != "", ]

# Also get installation names from installation.csv
inst <- read.csv(file.path(DATA_DIR, "raw/EUTL/Oct_2024_version/installation.csv"),
                 colClasses = "character")

# Build lookup: installation name -> BvD ID (via EUTL_Belgium)
# First, get unique installation_id -> bvdid from eutl_be
inst_bvd <- unique(eutl_bvd[, c("installation_id", "bvdid", "name")])
cat("EUTL_Belgium installations with BvD:", nrow(inst_bvd), "\n")

# Also get installation name from installation.csv
inst_names <- inst[, c("id", "name", "city")]
names(inst_names) <- c("installation_id", "inst_name", "inst_city")

# --- 4. Try to match IMJV eutl_name to EUTL installation name -> BvD ---
# The imjv_eutl$eutl_name should correspond to names in the EUTL data
# Let's try exact matching first

# Normalize names for matching
normalize <- function(x) tolower(trimws(gsub("[^a-zA-Z0-9 ]", "", x)))

matched$eutl_name_norm <- normalize(matched$eutl_name)
inst_names$inst_name_norm <- normalize(inst_names$inst_name)

# Join
merged <- merge(matched, inst_names,
                by.x = "eutl_name_norm", by.y = "inst_name_norm",
                all.x = TRUE)

# Now join to get BvD ID
merged2 <- merge(merged, inst_bvd[, c("installation_id", "bvdid")],
                 by = "installation_id", all.x = TRUE)

# Show results
has_bvd <- merged2[!is.na(merged2$bvdid) & merged2$bvdid != "", ]
cat("\nMatched firms with BvD ID:", nrow(has_bvd), "\n\n")

# Extract enterprise number from BvD ID (strip "BE" prefix)
has_bvd$enterprise_from_bvd <- gsub("^BE", "", has_bvd$bvdid)

# Show the key columns side by side
cat("=== CBB number vs Enterprise number (from BvD) ===\n")
result <- has_bvd[, c("cbb_char", "enterprise_from_bvd", "bvdid",
                       "imjv_name", "eutl_name")]
result <- result[!duplicated(result$cbb_char), ]
result <- result[order(result$cbb_char), ]
print(result, row.names = FALSE)

cat("\n\n=== Pattern analysis ===\n")
cat("CBB nchar:", unique(nchar(result$cbb_char)), "\n")
cat("Enterprise nchar:", unique(nchar(result$enterprise_from_bvd)), "\n\n")

# Check if first 10 digits of CBB = enterprise number
result$cbb_first10 <- substr(result$cbb_char, 1, 10)
result$cbb_mid10 <- substr(result$cbb_char, 2, 11)
result$cbb_last10 <- substr(result$cbb_char, 5, 14)

result$match_first10 <- result$cbb_first10 == result$enterprise_from_bvd
result$match_mid10 <- result$cbb_mid10 == result$enterprise_from_bvd
result$match_last10 <- result$cbb_last10 == result$enterprise_from_bvd

cat("First 10 digits match:", sum(result$match_first10), "/", nrow(result), "\n")
cat("Digits 2-11 match:", sum(result$match_mid10), "/", nrow(result), "\n")
cat("Last 10 digits match:", sum(result$match_last10), "/", nrow(result), "\n\n")

# Show side by side for manual inspection
cat("=== Side-by-side comparison ===\n")
for (i in 1:min(20, nrow(result))) {
  cat(sprintf("CBB: %s | BvD enterprise: %s | first10: %s | mid10: %s\n",
              result$cbb_char[i], result$enterprise_from_bvd[i],
              result$cbb_first10[i], result$cbb_mid10[i]))
}
