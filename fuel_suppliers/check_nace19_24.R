source("paths.R")
library(dplyr)

load(file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData"))

lhs %>%
  filter(nace2d %in% c("19", "24")) %>%
  mutate(emit = as.integer(y > 0)) %>%
  group_by(nace2d, year) %>%
  summarise(
    n_firms    = n(),
    n_emitters = sum(emit),
    n_nonemit  = sum(1 - emit),
    .groups = "drop"
  ) %>%
  arrange(nace2d, year) %>%
  print(n = 100)
