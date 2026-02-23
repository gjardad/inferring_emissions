###############################################################################
# fuel_suppliers/execute_today.R
#
# PURPOSE
#   Run all scripts needed to produce updated results and paper-ready .tex
#   tables for the elastic net exercise. Meant to be run on RMD after recent
#   code changes (LOSOCV, within-sector-year FP severity, core CN8 tier).
#
# EXECUTION ORDER
#   --- Heavy computation ---
#   1. build_proxy_and_cv.R         Updated CV pipeline + LOSOCV
#   2. validate_against_cn8.R       CN8 validation with core tier
#   3. cn8_random_baseline.R        Random baseline (broad/strict/core)
#
#   --- Table generation (fast, reads CSVs) ---
#   4. table_cv_performance.R       T1: prediction performance (5 rows)
#   5. table_placebo_test.R         T2: placebo tests
#   6. table_within_firm_signal.R   T3: within-firm signal
#   7. table_cn8_validation.R       TA1: CN8 validation vs random baseline
#   8. table_cn8_fossil_fuel_codes.R     CN8 fossil fuel codes longtable
#
# OUTPUTS (all to OUTPUT_DIR)
#   CSV:  fuel_suppliers_cv_performance.csv (updated)
#         fuel_suppliers_cn8_validation.csv (updated)
#         enet_cn8_random_baseline.csv (updated)
#   TEX:  enet_cv_performance.tex
#         enet_placebo_test_table.tex
#         enet_within_firm_table.tex
#         enet_cn8_validation_table.tex
#         cn8_fossil_fuel_codes.tex
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

FS_DIR   <- file.path(REPO_DIR, "fuel_suppliers")
DESC_DIR <- file.path(FS_DIR, "descriptives")

scripts <- c(
  # --- Heavy computation ---
  file.path(FS_DIR,   "build_proxy_and_cv.R"),
  file.path(FS_DIR,   "validate_against_cn8.R"),
  file.path(DESC_DIR, "cn8_random_baseline.R"),

  # --- Table generation ---
  file.path(DESC_DIR, "table_cv_performance.R"),
  file.path(DESC_DIR, "table_placebo_test.R"),
  file.path(DESC_DIR, "table_within_firm_signal.R"),
  file.path(DESC_DIR, "table_cn8_validation.R"),
  file.path(DESC_DIR, "table_cn8_fossil_fuel_codes.R")
)


# ── Execute ──────────────────────────────────────────────────────────────────
t_start <- Sys.time()

for (i in seq_along(scripts)) {
  s <- scripts[i]

  cat("\n\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf("  [%d/%d] %s\n", i, length(scripts), basename(s)))
  cat(strrep("=", 70), "\n\n")

  if (!file.exists(s)) {
    cat("  *** FILE NOT FOUND — skipping ***\n")
    next
  }

  t0 <- Sys.time()
  tryCatch(
    source(s, local = new.env(parent = globalenv())),
    error = function(e) {
      cat(sprintf("\n  *** ERROR in %s ***\n  %s\n", basename(s), conditionMessage(e)))
    }
  )
  elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
  cat(sprintf("\n  Finished %s in %.1f min\n", basename(s), elapsed))
}

total <- round(difftime(Sys.time(), t_start, units = "mins"), 1)
cat("\n\n")
cat(strrep("=", 70), "\n")
cat(sprintf("  All done. Total time: %.1f min\n", total))
cat(strrep("=", 70), "\n")
