###############################################################################
# fuel_suppliers/validate_against_cn8.R
#
# PURPOSE
#   Validate elastic-net-identified suppliers against CN8 customs data.
#   Two validation levels:
#     (a) Direct: is the selected supplier a CN8 fuel importer?
#     (b) 1-degree downstream: does the selected supplier buy from a CN8
#         fuel importer in the B2B network? (i.e., is it a domestic
#         fuel distributor that resells imported fuel?)
#
# INPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   {PROC_DATA}/fuel_imported_by_firm_year.RData
#   {PROC_DATA}/b2b_selected_sample.RData
#
# OUTPUT
#   {INT_DATA}/fuel_suppliers_cn8_validation.RData
#     - validation_pooled, validation_fe: per-supplier validation flags
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

cat("Loading CN8 fuel importer data...\n")
load(file.path(PROC_DATA, "fuel_imported_by_firm_year.RData"))

cat("Loading B2B data (for 1-degree downstream)...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)


# ── Build validation reference sets ──────────────────────────────────────────
cat("\nBuilding validation reference sets...\n")

# (a) Direct CN8 fuel importers: unique VATs that ever imported fuel
cn8_importers <- fuel_imported_by_firm_year %>%
  distinct(vat_ano) %>%
  pull(vat_ano)

cat("CN8 fuel importers (ever):", length(cn8_importers), "\n")
cat("  of which in eligible_sellers:", sum(cn8_importers %in% eligible_sellers), "\n")

# (b) 1-degree downstream: firms that buy from CN8 importers in B2B
#     These are potential domestic fuel distributors.
#     Filter to years >= 2005 for consistency with the elastic net sample.
downstream_buyers <- b2b %>%
  filter(vat_i_ano %in% cn8_importers, year >= 2005) %>%
  distinct(vat_j_ano) %>%
  pull(vat_j_ano)

cat("1-degree downstream buyers from CN8 importers:", length(downstream_buyers), "\n")
cat("  of which in eligible_sellers:", sum(downstream_buyers %in% eligible_sellers), "\n")

# Combined: either a direct importer OR a 1-degree downstream buyer
fuel_linked <- union(cn8_importers, downstream_buyers)
cat("Fuel-linked firms (importer OR 1-degree downstream):", length(fuel_linked), "\n")
cat("  of which in eligible_sellers:", sum(fuel_linked %in% eligible_sellers), "\n")

rm(b2b)  # free memory


# ── Helper: validate one specification ───────────────────────────────────────
validate_spec <- function(supplier_summary, spec_label) {

  cat(sprintf("\n══════════════════════════════════════════════\n"))
  cat(sprintf("  Validation: %s\n", spec_label))
  cat(sprintf("══════════════════════════════════════════════\n\n"))

  # Selected suppliers at lambda.min with positive coefficient
  selected <- supplier_summary %>%
    filter(lambda == "min", coef > 0) %>%
    distinct(vat_i_ano) %>%
    mutate(
      is_cn8_importer       = vat_i_ano %in% cn8_importers,
      is_downstream_buyer   = vat_i_ano %in% downstream_buyers,
      is_fuel_linked        = vat_i_ano %in% fuel_linked
    )

  n_sel <- nrow(selected)
  if (n_sel == 0) {
    cat("  No suppliers selected. Skipping.\n")
    return(selected)
  }

  n_importer   <- sum(selected$is_cn8_importer)
  n_downstream <- sum(selected$is_downstream_buyer)
  n_linked     <- sum(selected$is_fuel_linked)
  n_neither    <- n_sel - n_linked

  cat("Selected suppliers (lambda.min, coef > 0):", n_sel, "\n\n")

  cat("  CN8 fuel importers:                 ", n_importer,
      sprintf("(%4.1f%%)\n", 100 * n_importer / n_sel))
  cat("  1-degree downstream from importers: ", n_downstream,
      sprintf("(%4.1f%%)\n", 100 * n_downstream / n_sel))
  cat("  Fuel-linked (importer OR downstream):", n_linked,
      sprintf("(%4.1f%%)\n", 100 * n_linked / n_sel))
  cat("  Neither:                            ", n_neither,
      sprintf("(%4.1f%%)\n", 100 * n_neither / n_sel))

  # Precision/recall framing (using fuel-linked as "ground truth positive")
  # among eligible sellers
  n_fuel_linked_eligible <- sum(eligible_sellers %in% fuel_linked)
  precision <- n_linked / n_sel
  recall    <- n_linked / n_fuel_linked_eligible

  cat(sprintf("\n  Precision (fuel-linked / selected):     %.1f%%\n",
              100 * precision))
  cat(sprintf("  Recall    (fuel-linked selected / total fuel-linked eligible): %.1f%%\n",
              100 * recall))

  # Model-by-model breakdown
  cat("\n  Model-by-model breakdown (lambda.min, coef > 0):\n")
  model_names <- c("lasso_raw", "lasso_asinh", "enet_raw", "enet_asinh")
  for (m in model_names) {
    m_sel <- supplier_summary %>%
      filter(model == m, lambda == "min", coef > 0) %>%
      pull(vat_i_ano)
    n_m <- length(m_sel)
    n_m_linked <- sum(m_sel %in% fuel_linked)
    cat(sprintf("    %-15s  selected: %3d  fuel-linked: %3d (%4.1f%%)\n",
                m, n_m, n_m_linked,
                ifelse(n_m > 0, 100 * n_m_linked / n_m, 0)))
  }

  selected
}


# =============================================================================
#   VALIDATE BOTH SPECIFICATIONS
# =============================================================================
validation_pooled <- validate_spec(supplier_summary_pooled, "POOLED (sector FE)")
validation_fe     <- validate_spec(supplier_summary_fe, "WITHIN-BUYER (buyer FE)")


# =============================================================================
#   COMPARATIVE SUMMARY
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# COMPARATIVE SUMMARY                                        #\n",
    "##############################################################\n\n")

# Among suppliers selected by BOTH specifications
if (nrow(validation_pooled) > 0 & nrow(validation_fe) > 0) {
  both <- intersect(validation_pooled$vat_i_ano, validation_fe$vat_i_ano)
  if (length(both) > 0) {
    n_both_linked <- sum(both %in% fuel_linked)
    cat("Suppliers selected by BOTH specifications:", length(both), "\n")
    cat("  of which fuel-linked:", n_both_linked,
        sprintf("(%.1f%%)\n", 100 * n_both_linked / length(both)))
  }

  # Among suppliers selected by pooled ONLY
  only_pooled <- setdiff(validation_pooled$vat_i_ano, validation_fe$vat_i_ano)
  if (length(only_pooled) > 0) {
    n_op_linked <- sum(only_pooled %in% fuel_linked)
    cat("\nSuppliers selected ONLY by pooled:", length(only_pooled), "\n")
    cat("  of which fuel-linked:", n_op_linked,
        sprintf("(%.1f%%)\n", 100 * n_op_linked / length(only_pooled)))
  }

  # Among suppliers selected by within-buyer ONLY
  only_fe <- setdiff(validation_fe$vat_i_ano, validation_pooled$vat_i_ano)
  if (length(only_fe) > 0) {
    n_of_linked <- sum(only_fe %in% fuel_linked)
    cat("\nSuppliers selected ONLY by within-buyer:", length(only_fe), "\n")
    cat("  of which fuel-linked:", n_of_linked,
        sprintf("(%.1f%%)\n", 100 * n_of_linked / length(only_fe)))
  }
}


# ── Save ─────────────────────────────────────────────────────────────────────
OUT_PATH <- file.path(INT_DATA, "fuel_suppliers_cn8_validation.RData")

save(
  validation_pooled, validation_fe,
  cn8_importers, downstream_buyers, fuel_linked,
  file = OUT_PATH
)

# ── Export validation summary to OUTPUT_DIR ───────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Build a tidy summary table
build_validation_row <- function(val_df, spec_label) {
  n_sel <- nrow(val_df)
  if (n_sel == 0) return(data.frame(spec = spec_label, n_selected = 0,
    n_cn8_importer = 0, n_downstream = 0, n_fuel_linked = 0, n_neither = 0,
    pct_fuel_linked = NA_real_, stringsAsFactors = FALSE))
  data.frame(
    spec            = spec_label,
    n_selected      = n_sel,
    n_cn8_importer  = sum(val_df$is_cn8_importer),
    n_downstream    = sum(val_df$is_downstream_buyer),
    n_fuel_linked   = sum(val_df$is_fuel_linked),
    n_neither       = n_sel - sum(val_df$is_fuel_linked),
    pct_fuel_linked = round(100 * mean(val_df$is_fuel_linked), 1),
    stringsAsFactors = FALSE
  )
}

validation_summary <- bind_rows(
  build_validation_row(validation_pooled, "pooled"),
  build_validation_row(validation_fe,     "within_buyer")
)

write.csv(validation_summary,
          file.path(OUTPUT_DIR, "fuel_suppliers_cn8_validation.csv"),
          row.names = FALSE)

cat("\n══════════════════════════════════════════════\n")
cat("Validation results saved to:", OUT_PATH, "\n")
cat("Validation summary exported to:", OUTPUT_DIR, "\n")
cat("══════════════════════════════════════════════\n")
