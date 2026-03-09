###############################################################################
# analysis/active/run_all_asinh_cv.R
#
# PURPOSE
#   Run all three asinh~asinh CV proxy exercises sequentially:
#     1. Sector-fold K=5
#     2. LOSO (leave-one-sector-out)
#     3. Firm-fold K=10
#
# OUTPUTS (all in PROC_DATA)
#   fold_specific_proxy_asinh.RData
#   loso_proxy_asinh.RData
#   firmfoldcv_proxy_asinh.RData
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
cat("  RUNNING ALL ASINH~ASINH CV EXERCISES\n")
cat("  Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("==========================================================\n\n")


# =============================================================================
# 1. Sector-fold K=5 (asinh LHS)
# =============================================================================
cat("\n##########################################################\n")
cat("## 1/3: SECTOR-FOLD K=5 (asinh LHS)                     ##\n")
cat("##########################################################\n\n")

t0 <- Sys.time()
t0_1 <- t0
source(file.path(SCRIPT_DIR, "build_fold_specific_proxy_asinh.R"), local = new.env())
t_1 <- round(difftime(Sys.time(), t0_1, units = "mins"), 1)
cat("\n>>> Sector-fold completed in", t_1, "minutes\n\n")
gc()


# =============================================================================
# 2. LOSO (asinh LHS)
# =============================================================================
cat("\n##########################################################\n")
cat("## 2/3: LOSO (asinh LHS)                                ##\n")
cat("##########################################################\n\n")

t0_2 <- Sys.time()
source(file.path(SCRIPT_DIR, "build_loso_proxy_asinh.R"), local = new.env())
t_2 <- round(difftime(Sys.time(), t0_2, units = "mins"), 1)
cat("\n>>> LOSO completed in", t_2, "minutes\n\n")
gc()


# =============================================================================
# 3. Firm-fold K=10 (asinh LHS)
# =============================================================================
cat("\n##########################################################\n")
cat("## 3/3: FIRM-FOLD K=10 (asinh LHS)                      ##\n")
cat("##########################################################\n\n")

t0_3 <- Sys.time()
source(file.path(SCRIPT_DIR, "build_firmfoldcv_proxy_asinh.R"), local = new.env())
t_3 <- round(difftime(Sys.time(), t0_3, units = "mins"), 1)
cat("\n>>> Firm-fold completed in", t_3, "minutes\n\n")
gc()


# =============================================================================
# Summary
# =============================================================================
exercise_timing <- data.frame(
  exercise    = c("sector_fold_k5", "loso", "firmfold_k10"),
  lhs         = "asinh",
  runtime_min = c(as.numeric(t_1), as.numeric(t_2), as.numeric(t_3)),
  stringsAsFactors = FALSE
)

cat("\n==========================================================\n")
cat("  ALL ASINH CV EXERCISES COMPLETE\n")
cat("  Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\n  Timing:\n")
print(exercise_timing)
cat("\n  Total:", sum(exercise_timing$runtime_min), "minutes\n")
cat("\n  Outputs (copy to local 1):\n")
cat("    -", file.path(PROC_DATA, "fold_specific_proxy_asinh.RData"), "\n")
cat("    -", file.path(PROC_DATA, "loso_proxy_asinh.RData"), "\n")
cat("    -", file.path(PROC_DATA, "firmfoldcv_proxy_asinh.RData"), "\n")
cat("==========================================================\n")
