library(haven)

cat("Reading B2B data (50K rows)...\n")
b2b <- read_dta("C:/Users/jota_/Documents/NBB_data/raw/NBB/B2B_ANO.dta", n_max = 50000)

cat("Rows:", nrow(b2b), "\n")
cat("Year range:", min(b2b$year), "-", max(b2b$year), "\n")
cat("Unique sellers (vat_i):", length(unique(b2b$vat_i_ano)), "\n")
cat("Unique buyers (vat_j):", length(unique(b2b$vat_j_ano)), "\n")

cat("\nYears present:\n")
print(sort(unique(b2b$year)))

cat("\nSales summary:\n")
print(summary(b2b$sales_ij))

cat("\nCorrected sales summary:\n")
print(summary(b2b$corr_sales_ij))

cat("\nCorrelation raw vs corrected:", cor(b2b$sales_ij, b2b$corr_sales_ij), "\n")

cat("\nDone with 50K chunk.\n")
