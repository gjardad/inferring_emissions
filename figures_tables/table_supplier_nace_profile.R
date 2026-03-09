###############################################################################
# figures_tables/table_supplier_nace_profile.R
#
# PURPOSE
#   What sectors do the EN-selected suppliers belong to?
#   Reports NACE 2-digit distribution of positive-coefficient suppliers
#   from the enet_asinh model at lambda.min, and compares coverage with
#   Tabachova's six NACE 4-digit codes.
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS
#   - {OUTPUT_DIR}/supplier_nace_profile.csv
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))


# ── Filter to enet_asinh, lambda.min, positive coefficients ──────────────────
suppliers <- supplier_summary_pooled %>%
  filter(model == "enet_asinh", lambda == "min", coef > 0)

cat("\nEN-selected suppliers (enet_asinh, lambda.min, coef > 0):",
    nrow(suppliers), "\n")


# ── Build NACE lookup (modal NACE 5-digit per firm) ──────────────────────────
nace_lookup <- df_annual_accounts_selected_sample_key_variables %>%
  filter(!is.na(nace5d)) %>%
  group_by(vat, nace5d) %>%
  summarise(n_years = n(), .groups = "drop") %>%
  group_by(vat) %>%
  slice_max(n_years, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(nace4d = substr(nace5d, 1, 4),
         nace2d = substr(nace5d, 1, 2)) %>%
  select(vat, nace5d, nace4d, nace2d)


# ── Merge suppliers with NACE codes ──────────────────────────────────────────
suppliers <- suppliers %>%
  left_join(nace_lookup, by = c("vat_i_ano" = "vat"))

n_matched   <- sum(!is.na(suppliers$nace4d))
n_unmatched <- sum(is.na(suppliers$nace4d))
cat("NACE match rate:", n_matched, "matched,",
    n_unmatched, "unmatched (",
    round(100 * n_unmatched / nrow(suppliers), 1), "% missing)\n\n")


# ── Tabachova NACE 4-digit codes ─────────────────────────────────────────────
tabachova_nace4d <- c("1920", "3521", "3522", "3523", "4671", "4730")
tabachova_nace2d <- unique(substr(tabachova_nace4d, 1, 2))


# ── NACE 2-digit table ──────────────────────────────────────────────────────
total_weight <- sum(suppliers$coef)

nace2d_table <- suppliers %>%
  filter(!is.na(nace2d)) %>%
  group_by(nace2d) %>%
  summarise(
    n_suppliers  = n(),
    weight_sum   = sum(coef),
    .groups = "drop"
  ) %>%
  mutate(
    share_suppliers = round(100 * n_suppliers / sum(n_suppliers), 2),
    share_weight    = round(100 * weight_sum / total_weight, 2),
    tabachova_overlap = if_else(nace2d %in% tabachova_nace2d, "yes", "")
  ) %>%
  arrange(desc(share_weight))

cat(strrep("=", 70), "\n")
cat("  NACE 2-digit profile of EN-selected suppliers\n")
cat(strrep("=", 70), "\n\n")

cat(sprintf("%-8s  %12s  %16s  %14s  %10s\n",
            "NACE2d", "N suppliers", "Share suppliers%", "Share weight%", "Tabachova"))
cat(strrep("-", 70), "\n")
for (i in seq_len(nrow(nace2d_table))) {
  cat(sprintf("%-8s  %12d  %15.2f%%  %13.2f%%  %10s\n",
              nace2d_table$nace2d[i],
              nace2d_table$n_suppliers[i],
              nace2d_table$share_suppliers[i],
              nace2d_table$share_weight[i],
              nace2d_table$tabachova_overlap[i]))
}


# ── EN vs Tabachova comparison ───────────────────────────────────────────────
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  EN vs Tabachova supplier comparison\n")
cat(strrep("=", 70), "\n\n")

# EN suppliers inside/outside Tabachova codes
en_in_tabachova  <- sum(suppliers$nace4d %in% tabachova_nace4d, na.rm = TRUE)
en_out_tabachova <- sum(!suppliers$nace4d %in% tabachova_nace4d | is.na(suppliers$nace4d))

# Weight shares
weight_in_tabachova  <- sum(suppliers$coef[suppliers$nace4d %in% tabachova_nace4d], na.rm = TRUE)
weight_out_tabachova <- total_weight - weight_in_tabachova

cat("EN-selected suppliers inside Tabachova NACE codes:", en_in_tabachova,
    "(", round(100 * en_in_tabachova / nrow(suppliers), 1), "% of suppliers,",
    round(100 * weight_in_tabachova / total_weight, 1), "% of proxy weight)\n")
cat("EN-selected suppliers outside Tabachova NACE codes:", en_out_tabachova,
    "(", round(100 * en_out_tabachova / nrow(suppliers), 1), "% of suppliers,",
    round(100 * weight_out_tabachova / total_weight, 1), "% of proxy weight)\n\n")

# Tabachova suppliers in the data NOT selected by EN
# Identify all firms with modal NACE 4-digit in Tabachova codes
# that are also eligible sellers
tabachova_firms_in_data <- nace_lookup %>%
  filter(nace4d %in% tabachova_nace4d, vat %in% eligible_sellers)

n_tabachova_in_data  <- nrow(tabachova_firms_in_data)
n_tabachova_selected <- sum(tabachova_firms_in_data$vat %in% suppliers$vat_i_ano)
n_tabachova_missed   <- n_tabachova_in_data - n_tabachova_selected

cat("Tabachova-classified eligible sellers in data:", n_tabachova_in_data, "\n")
cat("  of which selected by EN:", n_tabachova_selected,
    "(", round(100 * n_tabachova_selected / n_tabachova_in_data, 1), "%)\n")
cat("  of which NOT selected by EN:", n_tabachova_missed,
    "(", round(100 * n_tabachova_missed / n_tabachova_in_data, 1), "%)\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(nace2d_table,
          file.path(OUTPUT_DIR, "supplier_nace_profile.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "supplier_nace_profile.csv"), "\n")
