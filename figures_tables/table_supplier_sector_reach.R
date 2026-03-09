###############################################################################
# figures_tables/table_supplier_sector_reach.R
#
# PURPOSE
#   Do EN-selected suppliers sell broadly across buyer sectors, or are they
#   sector-specific? Reports distribution of NACE 2-digit sector reach and
#   cross-tabulates with coefficient quartiles.
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/b2b_selected_sample.RData
#   - {PROC_DATA}/training_sample.RData
#
# OUTPUTS
#   - {OUTPUT_DIR}/supplier_sector_reach.csv
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

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

# Build buyer NACE 2-digit lookup from training sample (modal sector per firm)
buyer_nace <- training_sample %>%
  filter(!is.na(nace2d)) %>%
  count(vat, nace2d) %>%
  group_by(vat) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(vat, nace2d)
training_vats <- unique(training_sample$vat)
rm(training_sample)


# ── Filter to enet_asinh, lambda.min, positive coefficients ──────────────────
suppliers <- supplier_summary_pooled %>%
  filter(model == "enet_asinh", lambda == "min", coef > 0)

cat("\nEN-selected suppliers (enet_asinh, lambda.min, coef > 0):",
    nrow(suppliers), "\n\n")


# ── Count distinct NACE 2-digit buyer sectors per supplier ───────────────────
sector_reach <- b2b %>%
  filter(vat_i_ano %in% suppliers$vat_i_ano,
         vat_j_ano %in% training_vats) %>%
  inner_join(buyer_nace, by = c("vat_j_ano" = "vat")) %>%
  filter(!is.na(nace2d)) %>%
  group_by(vat_i_ano) %>%
  summarise(n_sectors = n_distinct(nace2d), .groups = "drop")

# Merge back to supplier list
supplier_reach <- suppliers %>%
  select(vat_i_ano, coef) %>%
  left_join(sector_reach, by = "vat_i_ano") %>%
  mutate(n_sectors = coalesce(n_sectors, 0L))


# ── Summary statistics ──────────────────────────────────────────────────────
cat(strrep("=", 60), "\n")
cat("  Sector reach distribution across selected suppliers\n")
cat(strrep("=", 60), "\n\n")

q <- quantile(supplier_reach$n_sectors, probs = c(0, 0.25, 0.50, 0.75, 1.00))

stats <- data.frame(
  statistic = c("min", "p25", "median", "mean", "p75", "max"),
  value     = c(q[1], q[2], q[3], mean(supplier_reach$n_sectors), q[4], q[5])
)

for (i in seq_len(nrow(stats))) {
  cat(sprintf("  %-8s: %8.1f\n", stats$statistic[i], stats$value[i]))
}


# ── Frequency table by bins ─────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 60), "\n")
cat("  Frequency table: sector reach bins\n")
cat(strrep("=", 60), "\n\n")

supplier_reach <- supplier_reach %>%
  mutate(bin = case_when(
    n_sectors == 0                     ~ "0",
    n_sectors == 1                     ~ "1",
    n_sectors >= 2 & n_sectors <= 5    ~ "2-5",
    n_sectors >= 6 & n_sectors <= 10   ~ "6-10",
    n_sectors > 10                     ~ "10+"
  ))

bin_order <- c("0", "1", "2-5", "6-10", "10+")
freq_table <- supplier_reach %>%
  count(bin) %>%
  mutate(bin = factor(bin, levels = bin_order)) %>%
  arrange(bin) %>%
  mutate(share_pct = round(100 * n / sum(n), 1))

cat(sprintf("  %-10s  %8s  %8s\n", "Bin", "Count", "Share%"))
cat("  ", strrep("-", 32), "\n")
for (i in seq_len(nrow(freq_table))) {
  cat(sprintf("  %-10s  %8d  %7.1f%%\n",
              as.character(freq_table$bin[i]),
              freq_table$n[i],
              freq_table$share_pct[i]))
}


# ── Cross-tabulation: coefficient quartile x median sector reach ─────────────
cat("\n")
cat(strrep("=", 60), "\n")
cat("  Median sector reach by coefficient quartile\n")
cat(strrep("=", 60), "\n\n")

supplier_reach <- supplier_reach %>%
  mutate(coef_quartile = ntile(coef, 4))

quartile_table <- supplier_reach %>%
  group_by(coef_quartile) %>%
  summarise(
    n_suppliers     = n(),
    coef_min        = round(min(coef), 6),
    coef_max        = round(max(coef), 6),
    median_sectors  = median(n_sectors),
    mean_sectors    = round(mean(n_sectors), 1),
    .groups = "drop"
  )

cat(sprintf("  %-10s  %12s  %12s  %12s  %14s  %12s\n",
            "Quartile", "N suppliers", "Coef min", "Coef max",
            "Median sectors", "Mean sectors"))
cat("  ", strrep("-", 80), "\n")
for (i in seq_len(nrow(quartile_table))) {
  cat(sprintf("  Q%-9d  %12d  %12.6f  %12.6f  %14.0f  %12.1f\n",
              quartile_table$coef_quartile[i],
              quartile_table$n_suppliers[i],
              quartile_table$coef_min[i],
              quartile_table$coef_max[i],
              quartile_table$median_sectors[i],
              quartile_table$mean_sectors[i]))
}


# ── Save ─────────────────────────────────────────────────────────────────────
out_df <- bind_rows(
  stats %>%
    mutate(table = "summary_stats", label = statistic,
           value = as.character(round(value, 1))) %>%
    select(table, label, value),
  freq_table %>%
    mutate(table = "frequency",
           label = as.character(bin),
           value = paste0(n, " (", share_pct, "%)")) %>%
    select(table, label, value),
  quartile_table %>%
    mutate(table = "quartile_reach",
           label = paste0("Q", coef_quartile),
           value = paste0("median=", median_sectors,
                          ", mean=", mean_sectors)) %>%
    select(table, label, value)
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(out_df,
          file.path(OUTPUT_DIR, "supplier_sector_reach.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "supplier_sector_reach.csv"), "\n")
