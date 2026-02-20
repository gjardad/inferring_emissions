###############################################################################
# fuel_suppliers/descriptives/cn8_random_baseline.R
#
# PURPOSE
#   Assess how informative the CN8 validation is by comparing the elastic-net-
#   selected suppliers against a random baseline. Draw N random samples of the
#   same size from eligible sellers and compute the same precision metrics.
#
#   If random samples achieve similar precision, the validation is uninformative
#   (i.e. nearly all firms in the economy are "fuel-linked").
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {INT_DATA}/fuel_suppliers_cn8_validation.RData
#
# OUTPUTS (to console + OUTPUT_DIR)
#   - enet_cn8_random_baseline.csv
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

set.seed(42)
N_DRAWS <- 1000


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading CN8 validation data...\n")
load(file.path(INT_DATA, "fuel_suppliers_cn8_validation.RData"))


# ── Reference sets ───────────────────────────────────────────────────────────
# eligible_sellers comes from elastic net results
n_eligible <- length(eligible_sellers)

# cn8_importers, downstream_buyers, fuel_linked come from cn8 validation
n_cn8_importers    <- sum(eligible_sellers %in% cn8_importers)
n_downstream       <- sum(eligible_sellers %in% downstream_buyers)
n_fuel_linked      <- sum(eligible_sellers %in% fuel_linked)

cat("\nEligible sellers:                     ", n_eligible, "\n")
cat("  of which CN8 importers:             ", n_cn8_importers,
    sprintf("(%.1f%%)\n", 100 * n_cn8_importers / n_eligible))
cat("  of which 1-degree downstream:       ", n_downstream,
    sprintf("(%.1f%%)\n", 100 * n_downstream / n_eligible))
cat("  of which fuel-linked (union):       ", n_fuel_linked,
    sprintf("(%.1f%%)\n", 100 * n_fuel_linked / n_eligible))


# ── Elastic net actual selection (pooled) ────────────────────────────────────
selected_pooled <- supplier_summary_pooled %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

n_selected <- length(selected_pooled)

actual_cn8      <- sum(selected_pooled %in% cn8_importers)
actual_downstream <- sum(selected_pooled %in% downstream_buyers)
actual_linked   <- sum(selected_pooled %in% fuel_linked)

cat("\n══════════════════════════════════════════════\n")
cat("  Elastic net selection (pooled): ", n_selected, " suppliers\n")
cat("══════════════════════════════════════════════\n")
cat("  CN8 importers:         ", actual_cn8,
    sprintf("(%.1f%%)\n", 100 * actual_cn8 / n_selected))
cat("  1-degree downstream:   ", actual_downstream,
    sprintf("(%.1f%%)\n", 100 * actual_downstream / n_selected))
cat("  Fuel-linked:           ", actual_linked,
    sprintf("(%.1f%%)\n", 100 * actual_linked / n_selected))


# ── Random baseline ──────────────────────────────────────────────────────────
cat("\nDrawing", N_DRAWS, "random samples of size", n_selected, "...\n")

random_results <- matrix(NA_real_, nrow = N_DRAWS, ncol = 3,
                          dimnames = list(NULL,
                                          c("cn8_importers", "downstream", "fuel_linked")))

for (i in seq_len(N_DRAWS)) {
  draw <- sample(eligible_sellers, n_selected, replace = FALSE)
  random_results[i, "cn8_importers"] <- sum(draw %in% cn8_importers)
  random_results[i, "downstream"]    <- sum(draw %in% downstream_buyers)
  random_results[i, "fuel_linked"]   <- sum(draw %in% fuel_linked)
}

random_df <- as.data.frame(random_results) %>%
  mutate(across(everything(), ~ . / n_selected * 100))  # convert to percentages


# ── Summary ──────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════\n")
cat("  Random baseline (", N_DRAWS, " draws of ", n_selected, " from ",
    n_eligible, " eligible)\n", sep = "")
cat("══════════════════════════════════════════════\n\n")

summary_table <- data.frame(
  metric = c("CN8 importers (%)", "1-degree downstream (%)", "Fuel-linked (%)"),
  elastic_net = round(c(actual_cn8, actual_downstream, actual_linked) / n_selected * 100, 1),
  random_mean = round(colMeans(random_df), 1),
  random_p5   = round(apply(random_df, 2, quantile, 0.05), 1),
  random_p95  = round(apply(random_df, 2, quantile, 0.95), 1),
  p_value     = c(
    mean(random_df$cn8_importers >= 100 * actual_cn8 / n_selected),
    mean(random_df$downstream >= 100 * actual_downstream / n_selected),
    mean(random_df$fuel_linked >= 100 * actual_linked / n_selected)
  ),
  stringsAsFactors = FALSE
)

cat(sprintf("%-25s  Elastic net   Random mean   Random [5%%, 95%%]   p-value\n",
            "Metric"))
cat(strrep("-", 85), "\n")
for (r in seq_len(nrow(summary_table))) {
  cat(sprintf("%-25s  %8.1f%%     %8.1f%%     [%5.1f%%, %5.1f%%]    %.3f\n",
              summary_table$metric[r],
              summary_table$elastic_net[r],
              summary_table$random_mean[r],
              summary_table$random_p5[r],
              summary_table$random_p95[r],
              summary_table$p_value[r]))
}

cat("\nInterpretation:\n")
cat("  p-value = share of random draws with >= elastic net's precision.\n")
cat("  If p-value is high, the elastic net does no better than random on that metric.\n")
cat("  If p-value is low, the elastic net is selectively picking that category.\n")

# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
write.csv(summary_table,
          file.path(OUTPUT_DIR, "enet_cn8_random_baseline.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "enet_cn8_random_baseline.csv"), "\n")
