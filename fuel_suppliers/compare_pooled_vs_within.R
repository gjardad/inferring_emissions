###############################################################################
# fuel_suppliers/compare_pooled_vs_within.R
#
# PURPOSE
#   Compare supplier selection between the pooled (sector FE) and
#   within-buyer (buyer FE) elastic net specifications.
#
#   Key diagnostic: if both specifications select similar suppliers,
#   the pooled model is capturing genuine fuel/production signal rather
#   than cross-sectional firm-size proxies. If they diverge, the pooled
#   model is mostly exploiting between-buyer size variation.
#
# INPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#
# OUTPUT
#   Console diagnostics (no files saved)
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


# ── Load results ─────────────────────────────────────────────────────────────
cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))


# =============================================================================
#   1. OVERLAP AT lambda.min (positive coefficients only)
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# 1. OVERLAP: Pooled vs Within-buyer (lambda.min, coef > 0)  #\n",
    "##############################################################\n\n")

# Unique suppliers selected with positive coef at lambda.min across ANY model
pooled_selected <- supplier_summary_pooled %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

fe_selected <- supplier_summary_fe %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

both_selected    <- intersect(pooled_selected, fe_selected)
only_pooled      <- setdiff(pooled_selected, fe_selected)
only_fe          <- setdiff(fe_selected, pooled_selected)

cat("Pooled selected:        ", length(pooled_selected), "unique suppliers\n")
cat("Within-buyer selected:  ", length(fe_selected), "unique suppliers\n")
cat("Selected by both:       ", length(both_selected), "\n")
cat("Only in pooled:         ", length(only_pooled), "\n")
cat("Only in within-buyer:   ", length(only_fe), "\n")

if (length(pooled_selected) > 0 & length(fe_selected) > 0) {
  union_all <- union(pooled_selected, fe_selected)
  jaccard <- length(both_selected) / length(union_all)
  cat("Jaccard similarity:     ", round(jaccard, 3), "\n")
}


# =============================================================================
#   2. MODEL-BY-MODEL OVERLAP
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# 2. MODEL-BY-MODEL OVERLAP (lambda.min, coef > 0)           #\n",
    "##############################################################\n\n")

model_names <- c("lasso_raw", "lasso_asinh", "enet_raw", "enet_asinh")

for (m in model_names) {
  p <- supplier_summary_pooled %>%
    filter(model == m, lambda == "min", coef > 0) %>%
    pull(vat_i_ano)
  f <- supplier_summary_fe %>%
    filter(model == m, lambda == "min", coef > 0) %>%
    pull(vat_i_ano)
  overlap <- length(intersect(p, f))
  cat(sprintf("  %-15s  pooled: %3d  within: %3d  overlap: %3d\n",
              m, length(p), length(f), overlap))
}


# =============================================================================
#   3. ROBUSTNESS COMPARISON (how many models select each supplier)
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# 3. ROBUSTNESS: Suppliers selected by >= 3 models           #\n",
    "##############################################################\n\n")

robust_pooled <- robustness_pooled %>%
  filter(n_models >= 3) %>%
  pull(vat_i_ano)
robust_fe <- robustness_fe %>%
  filter(n_models >= 3) %>%
  pull(vat_i_ano)

cat("Robust pooled (>=3 models):       ", length(robust_pooled), "\n")
cat("Robust within-buyer (>=3 models): ", length(robust_fe), "\n")
cat("Overlap of robust sets:           ", length(intersect(robust_pooled, robust_fe)), "\n")


# =============================================================================
#   4. COEFFICIENT COMPARISON FOR OVERLAPPING SUPPLIERS
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# 4. COEFFICIENT COMPARISON (overlapping suppliers)           #\n",
    "##############################################################\n\n")

if (length(both_selected) > 0) {

  # Average coefficient across models at lambda.min for overlapping suppliers
  avg_pooled <- supplier_summary_pooled %>%
    filter(lambda == "min", vat_i_ano %in% both_selected) %>%
    group_by(vat_i_ano) %>%
    summarise(mean_coef_pooled = mean(coef), .groups = "drop")

  avg_fe <- supplier_summary_fe %>%
    filter(lambda == "min", vat_i_ano %in% both_selected) %>%
    group_by(vat_i_ano) %>%
    summarise(mean_coef_fe = mean(coef), .groups = "drop")

  coef_comparison <- inner_join(avg_pooled, avg_fe, by = "vat_i_ano") %>%
    arrange(desc(mean_coef_pooled))

  cat("Overlapping suppliers (top 20 by pooled coef):\n")
  print(head(coef_comparison, 20), row.names = FALSE)

  if (nrow(coef_comparison) >= 3) {
    cat("\nCorrelation of mean coefs (pooled vs within-buyer):",
        round(cor(coef_comparison$mean_coef_pooled,
                  coef_comparison$mean_coef_fe), 3), "\n")
  }
} else {
  cat("No overlapping suppliers to compare.\n")
}


# =============================================================================
#   5. CV PERFORMANCE COMPARISON
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# 5. CV PERFORMANCE (RMSE at lambda.min)                     #\n",
    "##############################################################\n\n")

print(cv_summary %>% select(spec, model, rmse_min) %>%
        tidyr::pivot_wider(names_from = spec, values_from = rmse_min),
      row.names = FALSE)


# ── Export comparison summary to OUTPUT_DIR ───────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

comparison_summary <- data.frame(
  metric = c("pooled_selected", "within_buyer_selected",
             "selected_by_both", "only_pooled", "only_within_buyer",
             "jaccard_similarity",
             "robust_pooled_ge3", "robust_within_ge3", "robust_overlap"),
  value  = c(length(pooled_selected), length(fe_selected),
             length(both_selected), length(only_pooled), length(only_fe),
             ifelse(length(pooled_selected) > 0 & length(fe_selected) > 0,
                    round(length(both_selected) / length(union(pooled_selected, fe_selected)), 4),
                    NA_real_),
             length(robust_pooled), length(robust_fe),
             length(intersect(robust_pooled, robust_fe))),
  stringsAsFactors = FALSE
)

write.csv(comparison_summary,
          file.path(OUTPUT_DIR, "fuel_suppliers_comparison.csv"),
          row.names = FALSE)

cat("\n══════════════════════════════════════════════\n")
cat("Comparison complete. Summary exported to:", OUTPUT_DIR, "\n")
cat("══════════════════════════════════════════════\n")
