###############################################################################
# fuel_suppliers/execute_elastic_net_pipeline.R
#
# PURPOSE
#   Run the full elastic net pipeline for fuel-supplier identification:
#     Step 1: Build sparse design matrices (pooled + within-buyer)
#     Step 2: Run cv.glmnet models (4 variants x 2 specifications)
#     Step 3: Compare pooled vs within-buyer supplier selection
#     Step 4: Validate selected suppliers against CN8 fuel importers
#     Step 5: Build proxies from identified suppliers + group k-fold CV
#     Step 6: Generate paper-ready LaTeX tables from CV results
#     Step 7: Elastic net proxy diagnostics (regressions, summary stats, densities)
#     Step 8: Supplier selection diagnostics (CN8 validation, baseline,
#             NACE profile, stability, placebo, within-firm test)
#
# INPUT  (consumed by the sourced scripts)
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#   {PROC_DATA}/fuel_imported_by_firm_year.RData
#
# OUTPUT (produced by the sourced scripts)
#   {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   {INT_DATA}/fuel_suppliers_cn8_validation.RData
#   {OUTPUT_DIR}/fuel_suppliers_selected_pooled.csv
#   {OUTPUT_DIR}/fuel_suppliers_selected_within_buyer.csv
#   {OUTPUT_DIR}/fuel_suppliers_comparison.csv
#   {OUTPUT_DIR}/fuel_suppliers_cn8_validation.csv
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance.csv
#   {OUTPUT_DIR}/enet_cv_performance.tex
#   {OUTPUT_DIR}/enet_fp_severity.tex
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

SCRIPT_DIR <- file.path(REPO_DIR, "fuel_suppliers")

pipeline_t0 <- Sys.time()


# ── Step 1: Build design matrices ────────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 1: Building design matrices (pooled + within-buyer)   #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "build_design_matrix.R"))


# ── Step 2: Run elastic net models ───────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 2: Running elastic net models (8 total)               #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "run_elastic_net.R"))


# ── Step 3: Compare pooled vs within-buyer ───────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 3: Comparing pooled vs within-buyer selection          #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "compare_pooled_vs_within.R"))


# ── Step 4: Validate against CN8 fuel importers ─────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 4: Validating against CN8 fuel importers              #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "validate_against_cn8.R"))


# ── Step 5: Build proxies and run group k-fold CV ────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 5: Building proxies + group k-fold CV                 #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "build_proxy_and_cv.R"))


# ── Step 6: Paper-ready LaTeX tables ──────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 6: Generating paper-ready LaTeX tables                #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "descriptives", "table_cv_performance.R"))


# ── Step 7: Proxy diagnostics ────────────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 7: Elastic net proxy diagnostics                      #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "descriptives", "elastic_net_proxy_diagnostics.R"))


# ── Step 8: Supplier selection diagnostics ────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 8: Supplier selection diagnostics                     #\n",
    "#   (CN8 validation, baseline, NACE, stability,              #\n",
    "#    placebo, within-firm test)                               #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "descriptives", "execute_supplier_diagnostics.R"))


# ── Done ─────────────────────────────────────────────────────────────────────
elapsed <- round(difftime(Sys.time(), pipeline_t0, units = "mins"), 1)
cat("\n",
    "##############################################################\n",
    sprintf("# Pipeline complete. Total time: %s min                    \n", elapsed),
    "#                                                            #\n",
    "# Outputs in:", OUTPUT_DIR, "\n",
    "#   fuel_suppliers_selected_pooled.csv                        \n",
    "#   fuel_suppliers_selected_within_buyer.csv                  \n",
    "#   fuel_suppliers_comparison.csv                             \n",
    "#   fuel_suppliers_cn8_validation.csv                         \n",
    "#   fuel_suppliers_cv_performance.csv                         \n",
    "#   enet_cv_performance.tex                                   \n",
    "#   enet_fp_severity.tex                                      \n",
    "##############################################################\n")
