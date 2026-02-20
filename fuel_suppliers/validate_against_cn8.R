###############################################################################
# fuel_suppliers/validate_against_cn8.R
#
# PURPOSE
#   Validate elastic-net-identified suppliers against CN8 customs data.
#   Two validation tiers:
#     Broad (Ch.27 excl. 2716): all Chapter 27 products except electricity.
#     Strict (LLM-curated):     ~58 CN8 codes for stationary combustion fuels,
#                                built in preprocess/build_cn8_fossil_fuel_list.R.
#
#   Within each tier, two validation levels:
#     (a) Direct: is the selected supplier a CN8 fuel importer?
#     (b) 1-degree downstream: does the selected supplier buy from a CN8
#         fuel importer in the B2B network? (i.e., is it a domestic
#         fuel distributor that resells imported fuel?)
#
# INPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   {PROC_DATA}/fuel_imported_by_firm_year.RData
#   {PROC_DATA}/cn8digit_codes_for_fossil_fuels.RData
#   {PROC_DATA}/b2b_selected_sample.RData
#
# OUTPUT
#   {INT_DATA}/fuel_suppliers_cn8_validation.RData
#     - validation_pooled, validation_fe: per-supplier validation flags
#     - cn8_importers, downstream_buyers, fuel_linked: broad tier reference sets
#     - cn8_importers_strict, downstream_buyers_strict, fuel_linked_strict: strict tier
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

cat("Loading strict CN8 fuel code list...\n")
load(file.path(PROC_DATA, "cn8digit_codes_for_fossil_fuels.RData"))

cat("Loading B2B data (for 1-degree downstream)...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)


# ── Build validation reference sets ──────────────────────────────────────────

# ── BROAD tier (Ch.27 excl. 2716 — current definition) ──────────────────────
cat("\nBuilding validation reference sets (BROAD: Ch.27 excl. 2716)...\n")

# (a) Direct CN8 fuel importers: unique VATs that ever imported fuel
cn8_importers <- fuel_imported_by_firm_year %>%
  distinct(vat_ano) %>%
  pull(vat_ano)

cat("  CN8 importers (ever):", length(cn8_importers), "\n")
cat("    of which in eligible_sellers:", sum(cn8_importers %in% eligible_sellers), "\n")

# (b) 1-degree downstream: firms that buy from CN8 importers in B2B
downstream_buyers <- b2b %>%
  filter(vat_i_ano %in% cn8_importers, year >= 2005) %>%
  distinct(vat_j_ano) %>%
  pull(vat_j_ano)

cat("  1-degree downstream:", length(downstream_buyers), "\n")
cat("    of which in eligible_sellers:", sum(downstream_buyers %in% eligible_sellers), "\n")

# Combined: either a direct importer OR a 1-degree downstream buyer
fuel_linked <- union(cn8_importers, downstream_buyers)
cat("  Fuel-linked (union):", length(fuel_linked), "\n")
cat("    of which in eligible_sellers:", sum(fuel_linked %in% eligible_sellers), "\n")


# ── STRICT tier (LLM-curated stationary combustion fuels) ────────────────────
cat("\nBuilding validation reference sets (STRICT: ~",
    nrow(cn8digit_codes_for_fossil_fuels), " curated CN8 codes)...\n", sep = "")

strict_codes <- cn8digit_codes_for_fossil_fuels$cn_code

cn8_importers_strict <- fuel_imported_by_firm_year %>%
  filter(cncode %in% strict_codes) %>%
  distinct(vat_ano) %>%
  pull(vat_ano)

cat("  CN8 importers (strict):", length(cn8_importers_strict), "\n")
cat("    of which in eligible_sellers:", sum(cn8_importers_strict %in% eligible_sellers), "\n")

downstream_buyers_strict <- b2b %>%
  filter(vat_i_ano %in% cn8_importers_strict, year >= 2005) %>%
  distinct(vat_j_ano) %>%
  pull(vat_j_ano)

cat("  1-degree downstream (strict):", length(downstream_buyers_strict), "\n")
cat("    of which in eligible_sellers:", sum(downstream_buyers_strict %in% eligible_sellers), "\n")

fuel_linked_strict <- union(cn8_importers_strict, downstream_buyers_strict)
cat("  Fuel-linked (strict union):", length(fuel_linked_strict), "\n")
cat("    of which in eligible_sellers:", sum(fuel_linked_strict %in% eligible_sellers), "\n")

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
      # Broad tier
      is_cn8_importer       = vat_i_ano %in% cn8_importers,
      is_downstream_buyer   = vat_i_ano %in% downstream_buyers,
      is_fuel_linked        = vat_i_ano %in% fuel_linked,
      # Strict tier
      is_cn8_importer_strict     = vat_i_ano %in% cn8_importers_strict,
      is_downstream_buyer_strict = vat_i_ano %in% downstream_buyers_strict,
      is_fuel_linked_strict      = vat_i_ano %in% fuel_linked_strict
    )

  n_sel <- nrow(selected)
  if (n_sel == 0) {
    cat("  No suppliers selected. Skipping.\n")
    return(selected)
  }

  # ── Broad tier ──
  n_importer   <- sum(selected$is_cn8_importer)
  n_downstream <- sum(selected$is_downstream_buyer)
  n_linked     <- sum(selected$is_fuel_linked)
  n_neither    <- n_sel - n_linked

  cat("Selected suppliers (lambda.min, coef > 0):", n_sel, "\n\n")

  cat("  BROAD (Ch.27 excl. 2716):\n")
  cat("    CN8 fuel importers:                 ", n_importer,
      sprintf("(%4.1f%%)\n", 100 * n_importer / n_sel))
  cat("    1-degree downstream from importers: ", n_downstream,
      sprintf("(%4.1f%%)\n", 100 * n_downstream / n_sel))
  cat("    Fuel-linked (importer OR downstream):", n_linked,
      sprintf("(%4.1f%%)\n", 100 * n_linked / n_sel))
  cat("    Neither:                            ", n_neither,
      sprintf("(%4.1f%%)\n", 100 * n_neither / n_sel))

  n_fuel_linked_eligible <- sum(eligible_sellers %in% fuel_linked)
  precision_b <- n_linked / n_sel
  recall_b    <- n_linked / n_fuel_linked_eligible

  cat(sprintf("\n    Precision: %.1f%%   Recall: %.1f%%\n",
              100 * precision_b, 100 * recall_b))

  # ── Strict tier ──
  n_importer_s   <- sum(selected$is_cn8_importer_strict)
  n_downstream_s <- sum(selected$is_downstream_buyer_strict)
  n_linked_s     <- sum(selected$is_fuel_linked_strict)
  n_neither_s    <- n_sel - n_linked_s

  cat("\n  STRICT (LLM-curated stationary combustion fuels):\n")
  cat("    CN8 fuel importers:                 ", n_importer_s,
      sprintf("(%4.1f%%)\n", 100 * n_importer_s / n_sel))
  cat("    1-degree downstream from importers: ", n_downstream_s,
      sprintf("(%4.1f%%)\n", 100 * n_downstream_s / n_sel))
  cat("    Fuel-linked (importer OR downstream):", n_linked_s,
      sprintf("(%4.1f%%)\n", 100 * n_linked_s / n_sel))
  cat("    Neither:                            ", n_neither_s,
      sprintf("(%4.1f%%)\n", 100 * n_neither_s / n_sel))

  n_fuel_linked_eligible_s <- sum(eligible_sellers %in% fuel_linked_strict)
  precision_s <- n_linked_s / n_sel
  recall_s    <- n_linked_s / n_fuel_linked_eligible_s

  cat(sprintf("\n    Precision: %.1f%%   Recall: %.1f%%\n",
              100 * precision_s, 100 * recall_s))

  # Model-by-model breakdown (broad + strict)
  cat("\n  Model-by-model breakdown (lambda.min, coef > 0):\n")
  model_names <- c("lasso_raw", "lasso_asinh", "enet_raw", "enet_asinh")
  cat(sprintf("    %-15s  %5s  %11s  %11s\n",
              "Model", "N sel", "Broad FL%", "Strict FL%"))
  cat("    ", strrep("-", 50), "\n")
  for (m in model_names) {
    m_sel <- supplier_summary %>%
      filter(model == m, lambda == "min", coef > 0) %>%
      pull(vat_i_ano)
    n_m <- length(m_sel)
    n_m_linked   <- sum(m_sel %in% fuel_linked)
    n_m_linked_s <- sum(m_sel %in% fuel_linked_strict)
    cat(sprintf("    %-15s  %5d  %9.1f%%  %9.1f%%\n",
                m, n_m,
                ifelse(n_m > 0, 100 * n_m_linked / n_m, 0),
                ifelse(n_m > 0, 100 * n_m_linked_s / n_m, 0)))
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
  cn8_importers_strict, downstream_buyers_strict, fuel_linked_strict,
  file = OUT_PATH
)

# ── Export validation summary to OUTPUT_DIR ───────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# Build a tidy summary table (one row per spec × tier)
build_validation_row <- function(val_df, spec_label, tier_label,
                                  imp_col, ds_col, fl_col) {
  n_sel <- nrow(val_df)
  if (n_sel == 0) return(data.frame(spec = spec_label, tier = tier_label,
    n_selected = 0, n_cn8_importer = 0, n_downstream = 0,
    n_fuel_linked = 0, n_neither = 0, pct_fuel_linked = NA_real_,
    stringsAsFactors = FALSE))
  data.frame(
    spec            = spec_label,
    tier            = tier_label,
    n_selected      = n_sel,
    n_cn8_importer  = sum(val_df[[imp_col]]),
    n_downstream    = sum(val_df[[ds_col]]),
    n_fuel_linked   = sum(val_df[[fl_col]]),
    n_neither       = n_sel - sum(val_df[[fl_col]]),
    pct_fuel_linked = round(100 * mean(val_df[[fl_col]]), 1),
    stringsAsFactors = FALSE
  )
}

validation_summary <- bind_rows(
  build_validation_row(validation_pooled, "pooled", "broad",
                       "is_cn8_importer", "is_downstream_buyer", "is_fuel_linked"),
  build_validation_row(validation_pooled, "pooled", "strict",
                       "is_cn8_importer_strict", "is_downstream_buyer_strict", "is_fuel_linked_strict"),
  build_validation_row(validation_fe, "within_buyer", "broad",
                       "is_cn8_importer", "is_downstream_buyer", "is_fuel_linked"),
  build_validation_row(validation_fe, "within_buyer", "strict",
                       "is_cn8_importer_strict", "is_downstream_buyer_strict", "is_fuel_linked_strict")
)

write.csv(validation_summary,
          file.path(OUTPUT_DIR, "fuel_suppliers_cn8_validation.csv"),
          row.names = FALSE)

cat("\n══════════════════════════════════════════════\n")
cat("Validation results saved to:", OUT_PATH, "\n")
cat("Validation summary exported to:", OUTPUT_DIR, "\n")
cat("══════════════════════════════════════════════\n")
