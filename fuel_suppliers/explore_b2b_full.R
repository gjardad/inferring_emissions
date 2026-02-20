library(haven)

cat("Reading full B2B data...\n")
b2b <- read_dta("C:/Users/jota_/Documents/NBB_data/raw/NBB/B2B_ANO.dta")

cat("Total rows:", nrow(b2b), "\n")
cat("Year range:", min(b2b$year), "-", max(b2b$year), "\n")
cat("Unique sellers (vat_i):", length(unique(b2b$vat_i_ano)), "\n")
cat("Unique buyers (vat_j):", length(unique(b2b$vat_j_ano)), "\n")

cat("\nRows per year:\n")
print(table(b2b$year))

cat("\nMedian sellers per buyer:", median(table(b2b$vat_j_ano)), "\n")
cat("Mean sellers per buyer:", round(mean(table(b2b$vat_j_ano)), 1), "\n")

cat("\nMedian buyers per seller:", median(table(b2b$vat_i_ano)), "\n")
cat("Mean buyers per seller:", round(mean(table(b2b$vat_i_ano)), 1), "\n")

cat("\nDone.\n")
