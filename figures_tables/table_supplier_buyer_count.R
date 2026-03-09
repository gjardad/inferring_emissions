###############################################################################
# figures_tables/table_supplier_buyer_count.R
#
# PURPOSE
#   How many training-sample buyers does each EN-selected supplier serve?
#   Reports summary statistics and a frequency table of buyer counts.
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/b2b_selected_sample.RData
#   - {PROC_DATA}/training_sample.RData
#
# OUTPUTS
#   - {OUTPUT_DIR}/supplier_buyer_count.csv
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
training_vats <- unique(training_sample$vat)
rm(training_sample)


# ── Filter to enet_asinh, lambda.min, positive coefficients ──────────────────
suppliers <- supplier_summary_pooled %>%
  filter(model == "enet_asinh", lambda == "min", coef > 0)

cat("\nEN-selected suppliers (enet_asinh, lambda.min, coef > 0):",
    nrow(suppliers), "\n\n")


# ── Count distinct training-sample buyers per supplier ───────────────────────
buyer_counts <- b2b %>%
  filter(vat_i_ano %in% suppliers$vat_i_ano,
         vat_j_ano %in% training_vats) %>%
  group_by(vat_i_ano) %>%
  summarise(n_buyers = n_distinct(vat_j_ano), .groups = "drop")

# Merge back to supplier list (suppliers with 0 training-sample buyers)
supplier_buyers <- suppliers %>%
  select(vat_i_ano, coef) %>%
  left_join(buyer_counts, by = "vat_i_ano") %>%
  mutate(n_buyers = coalesce(n_buyers, 0L))


# ── Summary statistics ──────────────────────────────────────────────────────
cat(strrep("=", 60), "\n")
cat("  Buyer count distribution across selected suppliers\n")
cat(strrep("=", 60), "\n\n")

q <- quantile(supplier_buyers$n_buyers, probs = c(0, 0.25, 0.50, 0.75, 1.00))

stats <- data.frame(
  statistic = c("min", "p25", "median", "mean", "p75", "max"),
  value     = c(q[1], q[2], q[3], mean(supplier_buyers$n_buyers), q[4], q[5])
)

for (i in seq_len(nrow(stats))) {
  cat(sprintf("  %-8s: %10.1f\n", stats$statistic[i], stats$value[i]))
}


# ── Frequency table by bins ─────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 60), "\n")
cat("  Frequency table: buyer count bins\n")
cat(strrep("=", 60), "\n\n")

supplier_buyers <- supplier_buyers %>%
  mutate(bin = case_when(
    n_buyers == 0             ~ "0",
    n_buyers >= 1 & n_buyers <= 5   ~ "1-5",
    n_buyers >= 6 & n_buyers <= 10  ~ "6-10",
    n_buyers >= 11 & n_buyers <= 20 ~ "11-20",
    n_buyers > 20                   ~ "20+"
  ))

bin_order <- c("0", "1-5", "6-10", "11-20", "20+")
freq_table <- supplier_buyers %>%
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
    select(table, label, value)
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(out_df,
          file.path(OUTPUT_DIR, "supplier_buyer_count.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "supplier_buyer_count.csv"), "\n")
