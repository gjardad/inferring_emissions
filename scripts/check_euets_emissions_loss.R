###############################################################################
# scripts/check_euets_emissions_loss.R
#
# PURPOSE
#   Diagnose whether restricting to the De Loecker/Dhyne sample loses any
#   EU ETS emissions. Compares total verified emissions at three levels:
#     1. All Belgian EU ETS installations (installation_year_in_belgium)
#     2. Firms matched to VAT (firm_year_belgian_euets)
#     3. Firms in training sample (training_sample, euets == 1)
#
# RUNS ON: local 1
###############################################################################

load("C:/Users/jota_/Documents/NBB_data/processed/installation_year_in_belgium.RData")
load("C:/Users/jota_/Documents/NBB_data/processed/firm_year_belgian_euets.RData")
load("C:/Users/jota_/Documents/NBB_data/processed/training_sample.RData")

library(dplyr)

YEARS <- c(2005, 2010, 2015, 2020)

# --- Level 1: All Belgian installations ---
inst <- installation_year_in_belgium %>%
  filter(year %in% YEARS) %>%
  group_by(year) %>%
  summarise(
    n_installations = n_distinct(installation_id),
    emissions_inst = sum(verified, na.rm = TRUE),
    n_na_verified = sum(is.na(verified)),
    .groups = "drop"
  )

# --- Level 2: Firms matched to VAT ---
firm <- firm_year_belgian_euets %>%
  filter(year %in% YEARS) %>%
  group_by(year) %>%
  summarise(
    n_firms_vat = n_distinct(vat),
    emissions_vat = sum(emissions, na.rm = TRUE),
    n_na_emissions = sum(is.na(emissions)),
    .groups = "drop"
  )

# --- Level 3: Training sample (EU ETS firms only) ---
ts_euets <- training_sample %>%
  filter(euets == 1, year %in% YEARS) %>%
  group_by(year) %>%
  summarise(
    n_firms_sample = n_distinct(vat),
    emissions_sample = sum(emissions, na.rm = TRUE),
    .groups = "drop"
  )

# --- Combine ---
comparison <- inst %>%
  left_join(firm, by = "year") %>%
  left_join(ts_euets, by = "year") %>%
  mutate(
    loss_inst_to_vat_kt = emissions_inst - emissions_vat,
    loss_inst_to_vat_pct = (1 - emissions_vat / emissions_inst) * 100,
    loss_vat_to_sample_kt = emissions_vat - emissions_sample,
    loss_vat_to_sample_pct = (1 - emissions_sample / emissions_vat) * 100,
    loss_total_kt = emissions_inst - emissions_sample,
    loss_total_pct = (1 - emissions_sample / emissions_inst) * 100
  )

cat("\n=== Emissions Loss Diagnosis (kt CO2eq) ===\n\n")
cat(sprintf("%-6s %12s %12s %12s | %8s %8s | %8s %8s | %8s %8s\n",
            "Year", "Inst(kt)", "VAT(kt)", "Sample(kt)",
            "L1(kt)", "L1(%)", "L2(kt)", "L2(%)", "Tot(kt)", "Tot(%)"))
cat(paste(rep("-", 110), collapse=""), "\n")

for (i in seq_len(nrow(comparison))) {
  r <- comparison[i, ]
  cat(sprintf("%-6d %12.1f %12.1f %12.1f | %8.1f %7.2f%% | %8.1f %7.2f%% | %8.1f %7.2f%%\n",
              r$year, r$emissions_inst, r$emissions_vat, r$emissions_sample,
              r$loss_inst_to_vat_kt, r$loss_inst_to_vat_pct,
              r$loss_vat_to_sample_kt, r$loss_vat_to_sample_pct,
              r$loss_total_kt, r$loss_total_pct))
}

cat("\nL1 = loss from installations to VAT-matched firms (missing BvD/VAT)\n")
cat("L2 = loss from VAT-matched firms to training sample (De Loecker/Dhyne filter)\n")
cat("Tot = total loss from installations to training sample\n")

# --- Which firms are lost between VAT and training sample? ---
cat("\n\n=== Firms in firm_year_belgian_euets but NOT in training sample ===\n")
for (y in YEARS) {
  vat_firms <- firm_year_belgian_euets %>% filter(year == y) %>% pull(vat) %>% unique()
  sample_firms <- training_sample %>% filter(euets == 1, year == y) %>% pull(vat) %>% unique()
  lost <- setdiff(vat_firms, sample_firms)
  lost_emissions <- firm_year_belgian_euets %>%
    filter(year == y, vat %in% lost) %>%
    summarise(em = sum(emissions, na.rm = TRUE), n = n())
  cat(sprintf("  %d: %d firms lost, %.1f kt emissions\n", y, length(lost), lost_emissions$em))
}
