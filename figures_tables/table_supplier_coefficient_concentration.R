###############################################################################
# figures_tables/table_supplier_coefficient_concentration.R
#
# PURPOSE
#   How concentrated is the proxy — do a few suppliers dominate?
#   Reports cumulative share of total proxy weight by supplier rank and
#   coefficient summary statistics.
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#
# OUTPUTS
#   - {OUTPUT_DIR}/supplier_coefficient_concentration.csv
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


# ── Filter to enet_asinh, lambda.min, positive coefficients ──────────────────
suppliers <- supplier_summary_pooled %>%
  filter(model == "enet_asinh", lambda == "min", coef > 0) %>%
  arrange(desc(coef))

cat("\nEN-selected suppliers (enet_asinh, lambda.min, coef > 0):",
    nrow(suppliers), "\n\n")


# ── Cumulative weight by supplier rank ───────────────────────────────────────
total_weight <- sum(suppliers$coef)

suppliers <- suppliers %>%
  mutate(
    rank          = row_number(),
    cum_weight    = cumsum(coef),
    cum_share_pct = round(100 * cum_weight / total_weight, 2)
  )

cat(strrep("=", 60), "\n")
cat("  Cumulative proxy weight by supplier rank\n")
cat(strrep("=", 60), "\n\n")

milestones <- c(5, 10, 20, 50)
for (m in milestones) {
  if (m <= nrow(suppliers)) {
    share <- suppliers$cum_share_pct[m]
  } else {
    share <- 100.0
  }
  cat(sprintf("  Top %3d suppliers: %6.2f%% of total weight\n", m, share))
}
cat(sprintf("  All  %3d suppliers: 100.00%% of total weight\n", nrow(suppliers)))


# ── Coefficient summary statistics ───────────────────────────────────────────
cat("\n")
cat(strrep("=", 60), "\n")
cat("  Coefficient summary statistics\n")
cat(strrep("=", 60), "\n\n")

q <- quantile(suppliers$coef, probs = c(0, 0.25, 0.50, 0.75, 1.00))

stats <- data.frame(
  statistic = c("min", "p25", "median", "mean", "p75", "max"),
  value     = c(q[1], q[2], q[3], mean(suppliers$coef), q[4], q[5])
)

for (i in seq_len(nrow(stats))) {
  cat(sprintf("  %-8s: %12.6f\n", stats$statistic[i], stats$value[i]))
}

cat(sprintf("\n  Total weight (sum of coefficients): %.4f\n", total_weight))
cat(sprintf("  N suppliers: %d\n", nrow(suppliers)))


# ── Build output table ───────────────────────────────────────────────────────
# Concentration milestones
concentration <- data.frame(
  top_n     = milestones,
  cum_share = sapply(milestones, function(m) {
    if (m <= nrow(suppliers)) suppliers$cum_share_pct[m] else 100.0
  })
)

out <- list(
  concentration = concentration,
  summary_stats = stats
)

# Save as a combined CSV
out_df <- bind_rows(
  concentration %>%
    mutate(table = "concentration",
           label = paste0("top_", top_n),
           value = cum_share) %>%
    select(table, label, value),
  stats %>%
    mutate(table = "summary_stats",
           label = statistic) %>%
    select(table, label, value)
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(out_df,
          file.path(OUTPUT_DIR, "supplier_coefficient_concentration.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "supplier_coefficient_concentration.csv"), "\n")
