###############################################################################
# fuel_suppliers/execute_elastic_net_pipeline.R
#
# PURPOSE
#   Run the full elastic net pipeline for fuel-supplier identification:
#     Step 1: Build the sparse design matrix (01_build_design_matrix.R)
#     Step 2: Run cv.glmnet models (02_run_elastic_net.R)
#
# INPUT  (consumed by the sourced scripts)
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#
# OUTPUT (produced by the sourced scripts)
#   {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
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


# ── Step 1: Build design matrix ─────────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 1: Building design matrix                             #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "build_design_matrix.R"))


# ── Step 2: Run elastic net models ───────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# STEP 2: Running elastic net models                         #\n",
    "##############################################################\n\n")

source(file.path(SCRIPT_DIR, "run_elastic_net.R"))


cat("\n",
    "##############################################################\n",
    "# Pipeline complete.                                         #\n",
    "##############################################################\n")
