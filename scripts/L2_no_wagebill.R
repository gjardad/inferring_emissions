load("C:/Users/jota_/Documents/NBB_data/processed/firm_year_belgian_euets.RData")
load("C:/Users/jota_/Documents/NBB_data/processed/training_sample.RData")
library(dplyr)

YEARS <- c(2005, 2010, 2015, 2020)

# Current training sample EU ETS firms
ts_euets <- training_sample %>% filter(euets == 1)

# All EU ETS firms with VAT
all_euets <- firm_year_belgian_euets %>% filter(!is.na(emissions))

# Lost firms
lost <- all_euets %>%
  anti_join(ts_euets, by = c("vat", "year"))

cat("=== Lost EU ETS firm-years by reason ===\n\n")

# Categorize why they're lost
lost_diagnosed <- lost %>%
  mutate(
    no_aa_match    = is.na(nace5d),  # no annual accounts data at all
    has_aa         = !is.na(nace5d),
    fail_wagebill  = has_aa & (is.na(wage_bill) | wage_bill <= 0),
    fail_fte       = has_aa & (is.na(fte) | fte <= 1),
    fail_revenue   = has_aa & (is.na(revenue) | revenue <= 0),
    fail_capital   = has_aa & (is.na(capital) | capital <= 100),
    fail_sector    = has_aa & (startsWith(nace5d, "64") | startsWith(nace5d, "65") |
                               startsWith(nace5d, "66") | startsWith(nace5d, "84"))
  )

cat(sprintf("Total lost firm-years: %d\n", nrow(lost_diagnosed)))
cat(sprintf("  No AA match at all:       %d (%.0f kt)\n",
            sum(lost_diagnosed$no_aa_match),
            sum(lost_diagnosed$emissions[lost_diagnosed$no_aa_match], na.rm=TRUE)/1e3))
cat(sprintf("  Has AA but fails filters: %d (%.0f kt)\n",
            sum(lost_diagnosed$has_aa),
            sum(lost_diagnosed$emissions[lost_diagnosed$has_aa], na.rm=TRUE)/1e3))

cat("\n  Among those with AA match:\n")
cat(sprintf("    Fail wage_bill:  %d\n", sum(lost_diagnosed$fail_wagebill)))
cat(sprintf("    Fail FTE:        %d\n", sum(lost_diagnosed$fail_fte)))
cat(sprintf("    Fail revenue:    %d\n", sum(lost_diagnosed$fail_revenue)))
cat(sprintf("    Fail capital:    %d\n", sum(lost_diagnosed$fail_capital)))
cat(sprintf("    Fail sector:     %d\n", sum(lost_diagnosed$fail_sector)))

# What would we recover by dropping wage_bill and FTE restrictions?
# i.e., keep firms that have AA match and pass: revenue > 0, capital > 100,
# not financial/public, positive total assets
would_recover <- lost_diagnosed %>%
  filter(has_aa,
         !fail_revenue,
         !fail_capital,
         !fail_sector)

cat(sprintf("\n=== If we drop wage_bill > 0 and FTE > 1 restrictions ===\n"))
cat(sprintf("Would recover: %d firm-years, %d firms\n",
            nrow(would_recover), n_distinct(would_recover$vat)))
cat(sprintf("Recovered emissions: %.0f kt\n",
            sum(would_recover$emissions, na.rm=TRUE)/1e3))

cat("\nBy year:\n")
for (y in YEARS) {
  r <- would_recover %>% filter(year == y)
  em_sample <- ts_euets %>% filter(year == y) %>% summarise(em = sum(emissions)) %>% pull(em)
  cat(sprintf("  %d: +%d firms, +%.0f kt (%.1f%% of current sample)\n",
              y, n_distinct(r$vat), sum(r$emissions, na.rm=TRUE)/1e3,
              sum(r$emissions, na.rm=TRUE)/em_sample*100))
}
