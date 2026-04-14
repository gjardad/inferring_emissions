# =============================================================================
# Match IMJV 14-digit CBB to CBE establishment.csv
# Then follow through to EnterpriseNumber
# =============================================================================

DATA_DIR <- "c:/Users/jota_/Documents/NBB_data"

# --- 1. Load IMJV CBB numbers ---
imjv <- read.delim(file.path(DATA_DIR, "raw/IMJV/imjv_co2_lucht.tsv"),
                   colClasses = "character")
cbbs <- unique(imjv$cbb_number)
cat("Unique IMJV CBB numbers:", length(cbbs), "\n\n")

# --- 2. Load CBE establishment.csv ---
estab <- read.csv(file.path(DATA_DIR, "raw/CBE/CBE/establishment.csv"),
                  colClasses = "character")
cat("CBE establishment.csv:\n")
cat("  Columns:", paste(names(estab), collapse = ", "), "\n")
cat("  Rows:", nrow(estab), "\n")
cat("  Sample EstablishmentNumber:", head(estab$EstablishmentNumber, 5), "\n")
cat("  Nchar:", unique(nchar(estab$EstablishmentNumber))[1:3], "\n\n")

# --- 3. Try matching ---

# Strip dots from CBE EstablishmentNumber
estab$estab_clean <- gsub("\\.", "", estab$EstablishmentNumber)
cat("CBE estab_clean sample:", head(estab$estab_clean, 5), "\n")
cat("CBE estab_clean nchar:", unique(nchar(estab$estab_clean))[1:3], "\n\n")

# Hypothesis 1: first 10 digits of CBB = EstablishmentNumber (without dots)
imjv_first10 <- substr(cbbs, 1, 10)

# Hypothesis 2: digits 2-11 of CBB
imjv_2to11 <- substr(cbbs, 2, 11)

# Hypothesis 3: strip leading "0" then take first 10
imjv_strip0 <- sub("^0+", "", cbbs)
imjv_strip0_10 <- substr(imjv_strip0, 1, 10)

h1 <- sum(imjv_first10 %in% estab$estab_clean)
h2 <- sum(imjv_2to11 %in% estab$estab_clean)
h3 <- sum(imjv_strip0_10 %in% estab$estab_clean)

cat("Hypothesis 1 (first 10): ", h1, "/", length(cbbs), "match\n")
cat("Hypothesis 2 (digits 2-11): ", h2, "/", length(cbbs), "match\n")
cat("Hypothesis 3 (strip leading 0, first 10): ", h3, "/", length(cbbs), "match\n\n")

# Hypothesis 4: the full 14-digit CBB with dots inserted as 9.999.999.999.999
# matches something? Probably not, CBE is 10 digits.

# Hypothesis 5: CBB digits 3-12
imjv_3to12 <- substr(cbbs, 3, 12)
h5 <- sum(imjv_3to12 %in% estab$estab_clean)
cat("Hypothesis 5 (digits 3-12): ", h5, "/", length(cbbs), "match\n")

# Hypothesis 6: try formatted - insert dot at position 2
# CBE format: 9.999.999.999 (1 digit, dot, 3, dot, 3, dot, 3)
# CBB without leading zeros: might be the establishment number
for (cbb in head(cbbs, 5)) {
  stripped <- sub("^0+", "", cbb)
  cat(sprintf("CBB: %s -> stripped: %s (len %d)\n", cbb, stripped, nchar(stripped)))
}

# Let's just try all possible 10-char windows
cat("\n=== Brute force: all 10-char windows ===\n")
for (offset in 0:4) {
  window <- substr(cbbs, 1 + offset, 10 + offset)
  matches <- sum(window %in% estab$estab_clean)
  cat(sprintf("  Offset %d (chars %d-%d): %d / %d match\n",
              offset, 1+offset, 10+offset, matches, length(cbbs)))
}

# Maybe the relationship goes through EnterpriseNumber not EstablishmentNumber?
cat("\n=== Check against EnterpriseNumber ===\n")
ent_clean <- gsub("\\.", "", estab$EnterpriseNumber)
for (offset in 0:4) {
  window <- substr(cbbs, 1 + offset, 10 + offset)
  matches <- sum(window %in% ent_clean)
  cat(sprintf("  Offset %d (chars %d-%d): %d / %d match enterprise\n",
              offset, 1+offset, 10+offset, matches, length(cbbs)))
}
