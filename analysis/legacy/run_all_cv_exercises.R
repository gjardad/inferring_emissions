###############################################################################
# analysis/active/run_all_cv_exercises.R
#
# PURPOSE
#   Run all three CV exercises sequentially:
#     B1: LOSO (leave-one-sector-out)
#     B2: Firm-fold CV (K=10, stratified by sector)
#     C1: Climate TRACE EN (satellite-derived LHS)
#
#   After all three complete, compute cross-exercise supplier overlap
#   and save a combined summary.
#
#   Parallelization happens within each exercise via doParallel + cv.glmnet.
#
# OUTPUT
#   Individual outputs:
#     {PROC_DATA}/loso_proxy.RData
#     {PROC_DATA}/firmfoldcv_proxy.RData
#     {PROC_DATA}/enet_climate_trace_results.RData
#   Combined:
#     {PROC_DATA}/cv_exercises_summary.RData
#       Contains: cross_exercise_overlap, exercise_timing
#
# RUNS ON: RMD (requires full B2B data)
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

SCRIPT_DIR <- file.path(REPO_DIR, "analysis", "active")

cat("==========================================================\n")
cat("  RUNNING ALL CV EXERCISES\n")
cat("  Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("==========================================================\n\n")


# =============================================================================
# B1: Leave-one-sector-out
# =============================================================================
cat("\n##########################################################\n")
cat("## B1: LEAVE-ONE-SECTOR-OUT                             ##\n")
cat("##########################################################\n\n")

t0_b1 <- Sys.time()
source(file.path(SCRIPT_DIR, "build_loso_proxy.R"), local = new.env())
t_b1 <- round(difftime(Sys.time(), t0_b1, units = "mins"), 1)
cat("\n>>> B1 completed in", t_b1, "minutes\n\n")
gc()


# =============================================================================
# B2: Firm-fold CV (K=10, stratified)
# =============================================================================
cat("\n##########################################################\n")
cat("## B2: FIRM-FOLD CV (K=10)                              ##\n")
cat("##########################################################\n\n")

t0_b2 <- Sys.time()
source(file.path(SCRIPT_DIR, "build_firmfoldcv_proxy.R"), local = new.env())
t_b2 <- round(difftime(Sys.time(), t0_b2, units = "mins"), 1)
cat("\n>>> B2 completed in", t_b2, "minutes\n\n")
gc()


# =============================================================================
# C1: Climate TRACE EN
# =============================================================================
cat("\n##########################################################\n")
cat("## C1: CLIMATE TRACE ELASTIC NET                        ##\n")
cat("##########################################################\n\n")

t0_c1 <- Sys.time()
source(file.path(SCRIPT_DIR, "enet_climate_trace.R"), local = new.env())
t_c1 <- round(difftime(Sys.time(), t0_c1, units = "mins"), 1)
cat("\n>>> C1 completed in", t_c1, "minutes\n\n")
gc()


# =============================================================================
# Cross-exercise supplier overlap
# =============================================================================
cat("\n##########################################################\n")
cat("## CROSS-EXERCISE SUPPLIER OVERLAP                      ##\n")
cat("##########################################################\n\n")

library(dplyr)

# Load supplier lists from saved outputs
load(file.path(PROC_DATA, "loso_proxy.RData"))
loso_suppliers <- loso_supplier_overlap
# Get union of all LOSO suppliers from the overlap object
loso_sup_union <- character(0)
# Re-extract from the proxy panel: any supplier selected in any fold
# We need the actual supplier VATs — load diagnostics won't have them.
# Instead, use the saved overlap object.
loso_n_any <- loso_supplier_overlap$n_in_any_fold
rm(loso_proxy_panel, loso_sector_map, loso_fold_diagnostics, syt)

load(file.path(PROC_DATA, "firmfoldcv_proxy.RData"))
firmfoldcv_n_any <- firmfoldcv_supplier_overlap$n_in_any_fold
rm(firmfoldcv_proxy_panel, firmfoldcv_fold_map, firmfoldcv_fold_diagnostics, syt)

load(file.path(PROC_DATA, "enet_climate_trace_results.RData"))
ct_sup_pos <- ct_coef_pos$vat_i_ano
ct_sup_all <- ct_coef_lookup$vat_i_ano
rm(ct_proxy_panel, ct_train_panel, ct_eutl_match, ct_fold_assignment)

# For proper cross-exercise overlap, we need the actual supplier VAT lists.
# Re-run the LOSO and firmfoldcv scripts to save supplier lists, or
# extract from the coefficient objects. Since the scripts ran in local envs,
# we need to re-load and re-extract.
#
# The cleanest approach: load B1 output, re-extract supplier union from
# the Jaccard matrix (diagonal = 1 means the supplier set is non-empty).
# But we don't have the raw lists. Instead, we note the counts and
# compute overlap between CT suppliers and the LOSOCV (K=5) proxy
# if fold_specific_proxy.RData exists.

cat("-- Supplier counts by exercise --\n")
cat("  B1 (LOSO) suppliers in any fold:      ", loso_n_any, "\n")
cat("  B2 (Firm-fold CV) suppliers in any fold:", firmfoldcv_n_any, "\n")
cat("  C1 (CT EN) positive suppliers:         ", length(ct_sup_pos), "\n")
cat("  C1 (CT EN) all non-zero suppliers:     ", length(ct_sup_all), "\n")

# Compare CT suppliers vs LOSOCV (K=5) if available
losocv_overlap <- NULL
losocv_path <- file.path(PROC_DATA, "fold_specific_proxy.RData")
if (file.exists(losocv_path)) {
  # The LOSOCV proxy doesn't save supplier lists directly.
  # But we can check if the data was saved with supplier info.
  cat("\n  LOSOCV (K=5) proxy found — but supplier lists not saved in that file.\n")
  cat("  To compute CT vs LOSOCV overlap, re-run build_fold_specific_proxy.R\n")
  cat("  with supplier list saving enabled.\n")
}

# Timing summary
exercise_timing <- data.frame(
  exercise = c("B1_LOSO", "B2_FirmFoldCV", "C1_ClimatTrace"),
  runtime_min = c(as.numeric(t_b1), as.numeric(t_b2), as.numeric(t_c1)),
  stringsAsFactors = FALSE
)

cat("\n-- Timing summary --\n")
print(exercise_timing)
cat("Total:", sum(exercise_timing$runtime_min), "minutes\n")

# Save combined summary
cross_exercise_overlap <- list(
  loso_n_suppliers_any_fold      = loso_n_any,
  firmfoldcv_n_suppliers_any_fold = firmfoldcv_n_any,
  ct_n_suppliers_pos             = length(ct_sup_pos),
  ct_n_suppliers_all             = length(ct_sup_all),
  ct_supplier_vats_pos           = ct_sup_pos,
  ct_supplier_vats_all           = ct_sup_all
)

OUT_PATH <- file.path(PROC_DATA, "cv_exercises_summary.RData")
save(cross_exercise_overlap, exercise_timing, file = OUT_PATH)

cat("\nSaved combined summary to:", OUT_PATH, "\n")

cat("\n==========================================================\n")
cat("  ALL CV EXERCISES COMPLETE\n")
cat("  Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("  Total runtime:", sum(exercise_timing$runtime_min), "minutes\n")
cat("==========================================================\n")
