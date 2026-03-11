###############################################################################
# figures_tables/table_supplier_nace_profile.R
#
# PURPOSE
#   Characterize the suppliers selected by the full-sample elastic net
#   (enet_asinh at lambda.min). Reports:
#     - Aggregate counts: selected (pos/neg) out of total candidates
#     - NACE 2-digit breakdown with headcount and coefficient-weighted shares,
#       split by positive vs negative coefficients
#     - Tabachova NACE overlap flags
#     - Reverse Tabachova comparison: of Tabachova-classified eligible sellers,
#       how many does the EN select?
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS
#   - {OUTPUT_DIR}/supplier_nace_profile.csv
#   - Console output with all numbers needed for the paper text
#
# RUNS ON: RMD (full annual accounts needed for NACE matching)
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


# ── Filter to enet_asinh, lambda.min, all non-zero coefficients ──────────────
suppliers <- supplier_summary_pooled %>%
  filter(model == "enet_asinh", lambda == "min", coef != 0) %>%
  mutate(sign = if_else(coef > 0, "pos", "neg"))

n_total     <- nrow(suppliers)
n_pos       <- sum(suppliers$sign == "pos")
n_neg       <- sum(suppliers$sign == "neg")
n_candidates <- length(eligible_sellers)

cat("\n")
cat(strrep("=", 70), "\n")
cat("  AGGREGATE SUPPLIER COUNTS\n")
cat(strrep("=", 70), "\n\n")
cat("Total eligible sellers (candidates):", n_candidates, "\n")
cat("EN-selected suppliers (coef != 0):  ", n_total, "\n")
cat("  with positive coefficient:        ", n_pos, "\n")
cat("  with negative coefficient:        ", n_neg, "\n")


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
cat("\nNACE match rate:", n_matched, "matched,",
    n_unmatched, "unmatched (",
    round(100 * n_unmatched / nrow(suppliers), 1), "% missing)\n")


# ── Tabachova NACE 4-digit codes ─────────────────────────────────────────────
tabachova_nace4d <- c("1920", "3521", "3522", "3523", "4671", "4730")
tabachova_nace2d <- unique(substr(tabachova_nace4d, 1, 2))


# ── NACE 2-digit table ──────────────────────────────────────────────────────
# Totals for weight shares
total_weight_pos <- sum(suppliers$coef[suppliers$sign == "pos"])
total_weight_neg <- sum(abs(suppliers$coef[suppliers$sign == "neg"]))
total_weight_all <- sum(abs(suppliers$coef))

matched <- suppliers %>% filter(!is.na(nace2d))

nace2d_table <- matched %>%
  group_by(nace2d) %>%
  summarise(
    # Headcounts
    n_all  = n(),
    n_pos  = sum(sign == "pos"),
    n_neg  = sum(sign == "neg"),
    # Coefficient sums
    weight_pos = sum(coef[sign == "pos"]),
    weight_neg = sum(abs(coef[sign == "neg"])),
    .groups = "drop"
  ) %>%
  mutate(
    # Share of ALL suppliers in this sector
    share_all      = round(100 * n_all / sum(n_all), 2),
    # Share of all positive-coef suppliers that are in this sector
    share_of_pos   = round(100 * n_pos / sum(n_pos), 2),
    # Share of all negative-coef suppliers that are in this sector
    share_of_neg   = round(100 * n_neg / sum(n_neg), 2),
    # Within this sector: what fraction have positive vs negative coef
    pct_pos_within = round(100 * n_pos / n_all, 1),
    pct_neg_within = round(100 * n_neg / n_all, 1),
    # Same three shares but coefficient-weighted
    wshare_all     = round(100 * (weight_pos + weight_neg) / total_weight_all, 2),
    wshare_of_pos  = round(100 * weight_pos / total_weight_pos, 2),
    wshare_of_neg  = if (total_weight_neg > 0)
                       round(100 * weight_neg / total_weight_neg, 2) else 0,
    wpct_pos_within = round(100 * weight_pos / (weight_pos + weight_neg), 1),
    wpct_neg_within = round(100 * weight_neg / (weight_pos + weight_neg), 1),
    # Tabachova flag
    tabachova = if_else(nace2d %in% tabachova_nace2d, "yes", "")
  ) %>%
  arrange(desc(wshare_all))


# ── Print NACE 2-digit table ─────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 90), "\n")
cat("  NACE 2-DIGIT PROFILE OF EN-SELECTED SUPPLIERS\n")
cat(strrep("=", 90), "\n\n")

# Part A: Headcount shares
cat("--- HEADCOUNT ---\n\n")
cat(sprintf("%-6s  %5s %5s %5s | %7s %7s %7s | %6s %6s | %s\n",
            "NACE", "All", "Pos", "Neg",
            "Sh.All", "Sh.Pos", "Sh.Neg",
            "%PosIn", "%NegIn", "Tab."))
cat(strrep("-", 90), "\n")
for (i in seq_len(nrow(nace2d_table))) {
  r <- nace2d_table[i, ]
  cat(sprintf("%-6s  %5d %5d %5d | %6.2f%% %6.2f%% %6.2f%% | %5.1f%% %5.1f%% | %s\n",
              r$nace2d, r$n_all, r$n_pos, r$n_neg,
              r$share_all, r$share_of_pos, r$share_of_neg,
              r$pct_pos_within, r$pct_neg_within,
              r$tabachova))
}

# Part B: Coefficient-weighted shares
cat("\n--- COEFFICIENT-WEIGHTED ---\n\n")
cat(sprintf("%-6s  %8s %8s %8s | %7s %7s %7s | %6s %6s | %s\n",
            "NACE", "W.Pos", "W.Neg", "W.All",
            "Sh.All", "Sh.Pos", "Sh.Neg",
            "%PosIn", "%NegIn", "Tab."))
cat(strrep("-", 90), "\n")
for (i in seq_len(nrow(nace2d_table))) {
  r <- nace2d_table[i, ]
  cat(sprintf("%-6s  %8.4f %8.4f %8.4f | %6.2f%% %6.2f%% %6.2f%% | %5.1f%% %5.1f%% | %s\n",
              r$nace2d, r$weight_pos, r$weight_neg, r$weight_pos + r$weight_neg,
              r$wshare_all, r$wshare_of_pos, r$wshare_of_neg,
              r$wpct_pos_within, r$wpct_neg_within,
              r$tabachova))
}


# ── Paper text summary ───────────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 70), "\n")
cat("  PAPER TEXT SUMMARY\n")
cat(strrep("=", 70), "\n\n")

n_sectors_with_suppliers <- n_distinct(matched$nace2d)
core_sectors <- c("19", "35", "46")
n_core_all <- sum(nace2d_table$n_all[nace2d_table$nace2d %in% core_sectors])
pct_core_all <- round(100 * n_core_all / sum(nace2d_table$n_all), 1)
n_other_sectors <- sum(!nace2d_table$nace2d %in% core_sectors)

cat(sprintf("Selected suppliers span %d distinct NACE 2-digit sectors\n", n_sectors_with_suppliers))
cat(sprintf("%.1f%% of selected suppliers in NACE 19/35/46 (core fuel sectors)\n", pct_core_all))
cat(sprintf("Remaining suppliers scattered across %d other sectors\n", n_other_sectors))


# ── EN vs Tabachova comparison ───────────────────────────────────────────────
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  EN vs TABACHOVA SUPPLIER COMPARISON\n")
cat(strrep("=", 70), "\n\n")

# EN suppliers inside/outside Tabachova 4-digit codes
en_in_tab  <- sum(suppliers$nace4d %in% tabachova_nace4d, na.rm = TRUE)
en_out_tab <- n_total - en_in_tab

cat("EN suppliers inside Tabachova NACE 4d codes:", en_in_tab,
    "(", round(100 * en_in_tab / n_total, 1), "% )\n")
cat("EN suppliers outside Tabachova NACE 4d codes:", en_out_tab,
    "(", round(100 * en_out_tab / n_total, 1), "% )\n\n")

# Reverse: Tabachova-classified eligible sellers → selected by EN?
tabachova_firms_in_data <- nace_lookup %>%
  filter(nace4d %in% tabachova_nace4d, vat %in% eligible_sellers)

n_tab_in_data  <- nrow(tabachova_firms_in_data)
n_tab_selected <- sum(tabachova_firms_in_data$vat %in% suppliers$vat_i_ano)
n_tab_missed   <- n_tab_in_data - n_tab_selected

cat("Tabachova-classified eligible sellers in data:", n_tab_in_data, "\n")
cat("  selected by EN:", n_tab_selected,
    "(", round(100 * n_tab_selected / n_tab_in_data, 1), "% )\n")
cat("  NOT selected by EN:", n_tab_missed,
    "(", round(100 * n_tab_missed / n_tab_in_data, 1), "% )\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(nace2d_table,
          file.path(OUTPUT_DIR, "supplier_nace_profile.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "supplier_nace_profile.csv"), "\n")
