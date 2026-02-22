###############################################################################
# fuel_suppliers/descriptives/cn8_random_baseline.R
#
# PURPOSE
#   Assess how informative the CN8 validation is by comparing the elastic-net-
#   selected suppliers against a random baseline. Draw N random samples of the
#   same size from eligible sellers and compute the same precision metrics.
#
#   Three CN8 tiers are tested:
#     Broad  (Ch.27 excl. 2716): all Chapter 27 products except electricity.
#     Strict (LLM-curated):     ~61 CN8 codes for stationary combustion fuels.
#     Core   (edge_case=FALSE):  ~31 unambiguous stationary combustion fuel codes.
#
#   If random samples achieve similar precision, the validation is uninformative
#   (i.e. nearly all firms in the economy are "fuel-linked" under that tier).
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
n_eligible <- length(eligible_sellers)

# ── Elastic net actual selection (pooled) ────────────────────────────────────
selected_pooled <- supplier_summary_pooled %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

n_selected <- length(selected_pooled)


# ── Helper: run baseline for one CN8 tier ────────────────────────────────────
run_baseline <- function(tier_label, importers, ds_buyers, fl_set) {

  n_imp_eligible <- sum(eligible_sellers %in% importers)
  n_ds_eligible  <- sum(eligible_sellers %in% ds_buyers)
  n_fl_eligible  <- sum(eligible_sellers %in% fl_set)

  cat(sprintf("\n%s\n  %s tier\n%s\n", strrep("=", 60), tier_label, strrep("=", 60)))
  cat("Eligible sellers:                     ", n_eligible, "\n")
  cat("  of which CN8 importers:             ", n_imp_eligible,
      sprintf("(%.1f%%)\n", 100 * n_imp_eligible / n_eligible))
  cat("  of which 1-degree downstream:       ", n_ds_eligible,
      sprintf("(%.1f%%)\n", 100 * n_ds_eligible / n_eligible))
  cat("  of which fuel-linked (union):       ", n_fl_eligible,
      sprintf("(%.1f%%)\n", 100 * n_fl_eligible / n_eligible))

  # Elastic net actual
  actual_cn8 <- sum(selected_pooled %in% importers)
  actual_ds  <- sum(selected_pooled %in% ds_buyers)
  actual_fl  <- sum(selected_pooled %in% fl_set)

  cat("\nElastic net selection (pooled):", n_selected, "suppliers\n")
  cat("  CN8 importers:       ", actual_cn8,
      sprintf("(%.1f%%)\n", 100 * actual_cn8 / n_selected))
  cat("  1-degree downstream: ", actual_ds,
      sprintf("(%.1f%%)\n", 100 * actual_ds / n_selected))
  cat("  Fuel-linked:         ", actual_fl,
      sprintf("(%.1f%%)\n", 100 * actual_fl / n_selected))

  # Random draws
  cat("\nDrawing", N_DRAWS, "random samples of size", n_selected, "...\n")

  random_results <- matrix(NA_real_, nrow = N_DRAWS, ncol = 3,
                           dimnames = list(NULL,
                                           c("cn8_importers", "downstream", "fuel_linked")))

  for (i in seq_len(N_DRAWS)) {
    draw <- sample(eligible_sellers, n_selected, replace = FALSE)
    random_results[i, "cn8_importers"] <- sum(draw %in% importers)
    random_results[i, "downstream"]    <- sum(draw %in% ds_buyers)
    random_results[i, "fuel_linked"]   <- sum(draw %in% fl_set)
  }

  random_df <- as.data.frame(random_results) %>%
    mutate(across(everything(), ~ . / n_selected * 100))

  summary_tbl <- data.frame(
    tier        = tier_label,
    metric      = c("CN8 importers (%)", "1-degree downstream (%)", "Fuel-linked (%)"),
    elastic_net = round(c(actual_cn8, actual_ds, actual_fl) / n_selected * 100, 1),
    random_mean = round(colMeans(random_df), 1),
    random_p5   = round(apply(random_df, 2, quantile, 0.05), 1),
    random_p95  = round(apply(random_df, 2, quantile, 0.95), 1),
    p_value     = c(
      mean(random_df$cn8_importers >= 100 * actual_cn8 / n_selected),
      mean(random_df$downstream    >= 100 * actual_ds  / n_selected),
      mean(random_df$fuel_linked   >= 100 * actual_fl  / n_selected)
    ),
    stringsAsFactors = FALSE
  )

  cat(sprintf("\n%-25s  Elastic net   Random mean   Random [5%%, 95%%]   p-value\n",
              "Metric"))
  cat(strrep("-", 85), "\n")
  for (r in seq_len(nrow(summary_tbl))) {
    cat(sprintf("%-25s  %8.1f%%     %8.1f%%     [%5.1f%%, %5.1f%%]    %.3f\n",
                summary_tbl$metric[r],
                summary_tbl$elastic_net[r],
                summary_tbl$random_mean[r],
                summary_tbl$random_p5[r],
                summary_tbl$random_p95[r],
                summary_tbl$p_value[r]))
  }

  summary_tbl
}


# ══════════════════════════════════════════════════════════════════════════════
#   Run baseline for BOTH tiers
# ══════════════════════════════════════════════════════════════════════════════
summary_broad <- run_baseline("broad",
                              cn8_importers, downstream_buyers, fuel_linked)

summary_strict <- run_baseline("strict",
                               cn8_importers_strict, downstream_buyers_strict,
                               fuel_linked_strict)

summary_core <- run_baseline("core",
                             cn8_importers_core, downstream_buyers_core,
                             fuel_linked_core)


# ── Interpretation ─────────────────────────────────────────────────────────
cat("\n\nInterpretation:\n")
cat("  p-value = share of random draws with >= elastic net's precision.\n")
cat("  If p-value is high, the elastic net does no better than random on that metric.\n")
cat("  If p-value is low, the elastic net is selectively picking that category.\n")
cat("  The STRICT and CORE tiers are more discriminating because fewer eligible\n")
cat("  sellers are fuel-linked, so random precision is lower and a significant\n")
cat("  result is more meaningful.\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

summary_all <- bind_rows(summary_broad, summary_strict, summary_core)

write.csv(summary_all,
          file.path(OUTPUT_DIR, "enet_cn8_random_baseline.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "enet_cn8_random_baseline.csv"), "\n")
