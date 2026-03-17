load("C:/Users/jota_/Documents/NBB_data/processed/firm_year_belgian_euets.RData")
load("C:/Users/jota_/Documents/NBB_data/processed/training_sample.RData")
library(dplyr)

YEARS <- c(2005, 2010, 2015, 2020)

for (y in YEARS) {
  em_sample <- training_sample %>%
    filter(year == y, euets == 1) %>%
    summarise(em = sum(emissions, na.rm = TRUE)) %>% pull(em)

  vat_sample <- training_sample %>% filter(year == y, euets == 1) %>% pull(vat) %>% unique()
  vat_all <- firm_year_belgian_euets %>% filter(year == y, !is.na(emissions)) %>% pull(vat) %>% unique()
  lost_vats <- setdiff(vat_all, vat_sample)

  em_lost <- firm_year_belgian_euets %>%
    filter(year == y, vat %in% lost_vats) %>%
    summarise(em = sum(emissions, na.rm = TRUE)) %>% pull(em)

  cat(sprintf("%d: sample = %.0f kt, lost = %.0f kt, lost/sample = %.1f%%\n",
              y, em_sample/1e3, em_lost/1e3, em_lost/em_sample*100))
}
