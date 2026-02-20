###############################################################################
# fuel_suppliers/descriptives/execute_supplier_diagnostics.R
#
# PURPOSE
#   Run all supplier-selection diagnostic scripts in one go:
#     1) CN8 random baseline comparison
#     2) NACE profile of selected suppliers
#     3) Supplier selection stability (jackknife)
#     4) Placebo test (shuffled emissions, size variables, shuffled within sector-year)
#     5) Within-firm signal test (firm FE + first differences)
#
# INPUTS
#   All inputs are loaded by the individual scripts from {INT_DATA} and {PROC_DATA}.
#
# OUTPUTS
#   All outputs are saved by the individual scripts to {OUTPUT_DIR}.
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

DIAG_DIR <- file.path(REPO_DIR, "fuel_suppliers", "descriptives")
diag_t0 <- Sys.time()


# ── 1) CN8 random baseline ──────────────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# 1/5: CN8 random baseline comparison                        #\n",
    "##############################################################\n\n")

source(file.path(DIAG_DIR, "cn8_random_baseline.R"))


# ── 2) NACE profile ─────────────────────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# 2/5: NACE profile of selected suppliers                    #\n",
    "##############################################################\n\n")

source(file.path(DIAG_DIR, "selected_supplier_nace_profile.R"))


# ── 3) Supplier selection stability ─────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# 3/5: Supplier selection stability (jackknife)              #\n",
    "##############################################################\n\n")

source(file.path(DIAG_DIR, "supplier_selection_stability.R"))


# ── 4) Placebo test ──────────────────────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# 4/5: Placebo test                                          #\n",
    "##############################################################\n\n")

source(file.path(DIAG_DIR, "placebo_test.R"))


# ── 5) Within-firm signal test ────────────────────────────────────────────────
cat("\n",
    "##############################################################\n",
    "# 5/5: Within-firm signal test (firm FE + first differences) #\n",
    "##############################################################\n\n")

source(file.path(DIAG_DIR, "within_firm_signal_test.R"))


# ── Done ─────────────────────────────────────────────────────────────────────
elapsed <- round(difftime(Sys.time(), diag_t0, units = "mins"), 1)
cat("\n",
    "##############################################################\n",
    sprintf("# Supplier diagnostics complete. Total time: %s min\n", elapsed),
    "#\n",
    "# Outputs in:", OUTPUT_DIR, "\n",
    "#   enet_cn8_random_baseline.csv\n",
    "#   enet_supplier_nace_profile.csv\n",
    "#   enet_supplier_stability.csv\n",
    "#   enet_placebo_test.csv\n",
    "#   enet_placebo_shuffled_detail.csv\n",
    "#   enet_placebo_within_sy_detail.csv\n",
    "#   enet_within_firm_test.csv\n",
    "##############################################################\n")
