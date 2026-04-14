# =============================================================================
# Explore the structure of 14-digit IMJV CBB numbers
# and check against CBE establishment.csv format
# =============================================================================

DATA_DIR <- "c:/Users/jota_/Documents/NBB_data"

# Load IMJV with cbb as character
imjv <- read.delim(file.path(DATA_DIR, "raw/IMJV/imjv_co2_lucht.tsv"),
                   colClasses = "character")

cbbs <- unique(imjv$cbb_number)
cat("Unique CBB numbers:", length(cbbs), "\n")
cat("All 14 chars?", all(nchar(cbbs) == 14), "\n\n")

# Show a sample
cat("=== Sample CBB numbers ===\n")
print(head(cbbs, 20))

# Break down the 14-digit structure
cat("\n=== Structural analysis ===\n")
for (cbb in head(cbbs, 20)) {
  # Try different splits
  cat(sprintf("CBB: %s | 0+10+4: %s.%s.%s + %s | 4+10: %s + %s\n",
              cbb,
              substr(cbb, 2, 4), substr(cbb, 5, 7), substr(cbb, 8, 10), substr(cbb, 11, 14),
              substr(cbb, 1, 4), substr(cbb, 5, 14)))
}

# The CBE establishment number format is 9.999.999.999 (10 digits with dots)
# Let's see if digits 1-10 of CBB could be the establishment number
cat("\n=== First 10 digits as potential establishment number ===\n")
for (cbb in head(cbbs, 20)) {
  first10 <- substr(cbb, 1, 10)
  # Format as CBE style: 9.999.999.999
  formatted <- paste0(substr(first10, 1, 1), ".",
                       substr(first10, 2, 4), ".",
                       substr(first10, 5, 7), ".",
                       substr(first10, 8, 10))
  last4 <- substr(cbb, 11, 14)
  cat(sprintf("CBB: %s -> EstNum: %s (suffix: %s)\n", cbb, formatted, last4))
}

# Check if CBE establishment.csv exists on local 1
kbo_dir <- list.files("c:/Users/jota_/Documents", pattern = "KBO|kbo|CBE|cbe",
                      recursive = FALSE, full.names = TRUE)
cat("\n=== KBO/CBE directories found ===\n")
print(kbo_dir)

# Search more broadly
kbo_files <- list.files("c:/Users/jota_/Documents", pattern = "establishment",
                        recursive = TRUE, full.names = TRUE)
cat("\n=== establishment files found ===\n")
print(kbo_files)
