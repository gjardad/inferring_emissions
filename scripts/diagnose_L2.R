###############################################################################
# scripts/diagnose_L2.R
#
# PURPOSE
#   Understand which EU ETS firms are lost when restricting to the
#   De Loecker/Dhyne annual accounts sample, and how much emissions they carry.
#
# RUNS ON: local 1 (training_sample is full; annual accounts are downsampled
#          so we can't check AA filter criteria, but we can characterize the
#          lost firms from firm_year_belgian_euets)
###############################################################################

load("C:/Users/jota_/Documents/NBB_data/processed/firm_year_belgian_euets.RData")
load("C:/Users/jota_/Documents/NBB_data/processed/training_sample.RData")

library(dplyr)

YEARS <- c(2005, 2010, 2015, 2020)

# --- Identify lost firms per year ---
lost_list <- lapply(YEARS, function(y) {
  vat_euets <- firm_year_belgian_euets %>%
    filter(year == y, !is.na(emissions)) %>%
    pull(vat) %>% unique()
  vat_sample <- training_sample %>%
    filter(year == y, euets == 1) %>%
    pull(vat) %>% unique()

  lost_vats <- setdiff(vat_euets, vat_sample)
  firm_year_belgian_euets %>%
    filter(year == y, vat %in% lost_vats)
})
names(lost_list) <- YEARS

# --- Summary per year ---
cat("=== L2 Summary: EU ETS firms with VAT but NOT in training sample ===\n\n")
for (y in YEARS) {
  df <- lost_list[[as.character(y)]]
  cat(sprintf("--- %d ---\n", y))
  cat(sprintf("  Firms lost: %d\n", n_distinct(df$vat)))
  cat(sprintf("  Total emissions lost: %.0f kt\n", sum(df$emissions, na.rm = TRUE) / 1e3))
  cat(sprintf("  Mean emissions per firm: %.0f t\n", mean(df$emissions, na.rm = TRUE)))
  cat(sprintf("  Median emissions per firm: %.0f t\n", median(df$emissions, na.rm = TRUE)))

  # in_sample flag from build_firm_year_euets
  if ("in_sample" %in% names(df)) {
    cat(sprintf("  With in_sample == 1: %d\n", sum(df$in_sample == 1, na.rm = TRUE)))
    cat(sprintf("  With in_sample == NA: %d\n", sum(is.na(df$in_sample))))
  }
  cat("\n")
}

# --- Why are they lost? Check what data is missing ---
cat("\n=== Characterizing lost firms (pooled across years) ===\n\n")
lost_all <- bind_rows(lost_list)

cat("Missing annual accounts variables:\n")
cat(sprintf("  revenue (NA):     %d / %d\n", sum(is.na(lost_all$revenue)), nrow(lost_all)))
cat(sprintf("  wage_bill (NA):   %d / %d\n", sum(is.na(lost_all$wage_bill)), nrow(lost_all)))
cat(sprintf("  capital (NA):     %d / %d\n", sum(is.na(lost_all$capital)), nrow(lost_all)))
cat(sprintf("  value_added (NA): %d / %d\n", sum(is.na(lost_all$value_added)), nrow(lost_all)))
cat(sprintf("  fte (NA):         %d / %d\n", sum(is.na(lost_all$fte)), nrow(lost_all)))
cat(sprintf("  nace5d (NA):      %d / %d\n", sum(is.na(lost_all$nace5d)), nrow(lost_all)))

cat(sprintf("\n  revenue == 0:     %d / %d\n", sum(lost_all$revenue == 0, na.rm = TRUE), nrow(lost_all)))
cat(sprintf("  wage_bill == 0:   %d / %d\n", sum(lost_all$wage_bill == 0, na.rm = TRUE), nrow(lost_all)))
cat(sprintf("  value_added <= 0: %d / %d\n", sum(lost_all$value_added <= 0, na.rm = TRUE), nrow(lost_all)))

# --- NACE sectors of lost firms ---
cat("\n=== NACE 2-digit sectors of lost firms ===\n\n")
lost_by_nace <- lost_all %>%
  mutate(nace2d = substr(nace5d, 1, 2)) %>%
  group_by(nace2d) %>%
  summarise(
    n_firm_years = n(),
    n_firms = n_distinct(vat),
    total_emissions_kt = sum(emissions, na.rm = TRUE) / 1e3,
    .groups = "drop"
  ) %>%
  arrange(desc(total_emissions_kt))

print(lost_by_nace, n = 30)

# --- Emissions size distribution of lost vs kept ---
cat("\n=== Emissions distribution: lost vs kept (2020) ===\n\n")
y <- 2020
kept <- training_sample %>% filter(year == y, euets == 1)
lost <- lost_list[["2020"]]

cat("Kept firms:\n")
cat(sprintf("  N: %d, Mean: %.0f t, Median: %.0f t, Max: %.0f t\n",
            nrow(kept), mean(kept$emissions), median(kept$emissions), max(kept$emissions)))
cat("Lost firms:\n")
cat(sprintf("  N: %d, Mean: %.0f t, Median: %.0f t, Max: %.0f t\n",
            nrow(lost), mean(lost$emissions, na.rm=TRUE),
            median(lost$emissions, na.rm=TRUE), max(lost$emissions, na.rm=TRUE)))

# --- Top 10 lost firms by emissions (2020) ---
cat("\n=== Top 10 lost firms by emissions (2020) ===\n\n")
top_lost <- lost %>%
  select(vat, emissions, revenue, wage_bill, value_added, nace5d, in_sample) %>%
  arrange(desc(emissions)) %>%
  head(10)
print(top_lost)
