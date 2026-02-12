library(eurostat)

# Get the code list for nrg_bal dimension
cat("Fetching nrg_bal_c metadata...\n")
dic <- get_eurostat_dic("nrg_bal")

# Filter to codes starting with FC, TI, or NRG and ending with _E
pattern <- "^(FC|TI|NRG).*_E$"
matches <- dic[grepl(pattern, dic[[1]]), ]

cat("\nMatching energy balance codes:\n")
for (i in seq_len(nrow(matches))) {
  cat(sprintf("  %-25s  %s\n", matches[[1]][i], matches[[2]][i]))
}
cat("\nTotal matches:", nrow(matches), "\n")
