###############################################################################
# preprocess/run_full_pipeline.R
#
# PURPOSE
#   Run the entire preprocessing + elastic net + proxy-building pipeline in
#   one go. This script sources each step sequentially, so it must be run on
#   RMD (needs full B2B data and annual accounts).
#
# WHAT IT DOES (in order)
#   1. build_installation_year.R    → installation_year_emissions.RData
#   2. build_firm_year_emissions.R  → firm_year_emissions.RData
#   3. build_firm_year_euets.R      → firm_year_belgian_euets.RData
#   4. build_loocv_training_sample.R → loocv_training_sample.RData
#   5. build_design_matrix.R        → fuel_suppliers_elastic_net_inputs.RData
#   6. run_elastic_net.R            → fuel_suppliers_elastic_net_results.RData
#   7. build_proxy.R                → training_sample.RData (with foldid,
#                                     K_FOLDS, syt, and proxy_weighted_old)
#
# KEY FIX
#   Steps 1-4 fix the pre-regulation zero-emission mislabeling: EU ETS
#   firm-years where verified emissions were NA (installation not yet
#   regulated) are now dropped instead of coded as y = 0.
#
# AFTER RUNNING
#   Copy {PROC_DATA}/training_sample.RData to local 1.
#   The file includes proxy_weighted_old (from the pre-fix run) for comparison.
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


# ── Save old proxy_weighted for comparison ───────────────────────────────────
old_ts_path <- file.path(PROC_DATA, "training_sample.RData")
if (file.exists(old_ts_path)) {
  cat("Loading old training_sample to preserve proxy_weighted_old...\n")
  old_env <- new.env()
  load(old_ts_path, envir = old_env)
  proxy_weighted_old_lookup <- old_env$training_sample[, c("vat", "year", "proxy_weighted")]
  names(proxy_weighted_old_lookup)[3] <- "proxy_weighted_old"
  cat("  Old training sample:", nrow(proxy_weighted_old_lookup), "rows\n")
  rm(old_env)
} else {
  proxy_weighted_old_lookup <- NULL
  cat("No existing training_sample.RData found — skipping proxy_weighted_old.\n")
}


# ══════════════════════════════════════════════════════════════════════════════
#   STEP 1: build_installation_year.R
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  STEP 1: build_installation_year.R\n")
cat(strrep("=", 70), "\n\n")
source(file.path(REPO_DIR, "preprocess", "build_installation_year.R"))


# ══════════════════════════════════════════════════════════════════════════════
#   STEP 2: build_firm_year_emissions.R
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  STEP 2: build_firm_year_emissions.R\n")
cat(strrep("=", 70), "\n\n")
source(file.path(REPO_DIR, "preprocess", "build_firm_year_emissions.R"))


# ══════════════════════════════════════════════════════════════════════════════
#   STEP 3: build_firm_year_euets.R
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  STEP 3: build_firm_year_euets.R\n")
cat(strrep("=", 70), "\n\n")
source(file.path(REPO_DIR, "preprocess", "build_firm_year_euets.R"))


# ══════════════════════════════════════════════════════════════════════════════
#   STEP 4: build_loocv_training_sample.R
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  STEP 4: build_loocv_training_sample.R\n")
cat(strrep("=", 70), "\n\n")
source(file.path(REPO_DIR, "preprocess", "build_loocv_training_sample.R"))


# ══════════════════════════════════════════════════════════════════════════════
#   STEP 5: build_design_matrix.R
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  STEP 5: build_design_matrix.R\n")
cat(strrep("=", 70), "\n\n")
source(file.path(REPO_DIR, "preprocess", "build_design_matrix.R"))


# ══════════════════════════════════════════════════════════════════════════════
#   STEP 6: run_elastic_net.R
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  STEP 6: run_elastic_net.R\n")
cat(strrep("=", 70), "\n\n")
source(file.path(REPO_DIR, "analysis", "run_elastic_net.R"))


# ══════════════════════════════════════════════════════════════════════════════
#   STEP 7: build_proxy.R
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  STEP 7: build_proxy.R\n")
cat(strrep("=", 70), "\n\n")
source(file.path(REPO_DIR, "preprocess", "build_proxy.R"))


# ══════════════════════════════════════════════════════════════════════════════
#   POST: merge proxy_weighted_old into training_sample.RData
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 70), "\n")
cat("  POST: Adding proxy_weighted_old to training_sample.RData\n")
cat(strrep("=", 70), "\n\n")

# Reload the freshly saved training_sample
load(file.path(PROC_DATA, "training_sample.RData"))

if (!is.null(proxy_weighted_old_lookup)) {
  library(dplyr)
  training_sample <- training_sample %>%
    left_join(proxy_weighted_old_lookup, by = c("vat", "year"))

  # Report comparison
  n_matched <- sum(!is.na(training_sample$proxy_weighted_old))
  n_new_obs <- sum(is.na(training_sample$proxy_weighted_old))
  cat("Matched old proxy_weighted for", n_matched, "obs\n")
  cat("New obs (no old proxy):", n_new_obs, "\n")

  # For obs that dropped out (in old but not new), report count
  n_dropped <- nrow(proxy_weighted_old_lookup) - n_matched
  cat("Old obs dropped from new sample:", n_dropped, "\n")

  # Correlation between old and new
  both <- training_sample[!is.na(training_sample$proxy_weighted_old), ]
  if (nrow(both) > 0) {
    cat(sprintf("Correlation(proxy_weighted, proxy_weighted_old): %.4f\n",
                cor(both$proxy_weighted, both$proxy_weighted_old)))
  }
} else {
  training_sample$proxy_weighted_old <- NA_real_
  cat("No old proxy_weighted available — column set to NA.\n")
}

# Re-save with the old proxy column included
save(training_sample, foldid, K_FOLDS, syt,
     file = file.path(PROC_DATA, "training_sample.RData"))

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("PIPELINE COMPLETE.\n")
cat("  training_sample.RData saved to:", file.path(PROC_DATA, "training_sample.RData"), "\n")
cat("  Contents: training_sample, foldid, K_FOLDS, syt\n")
cat("  training_sample includes proxy_weighted_old for comparison.\n")
cat("\n  Next: copy training_sample.RData to local 1.\n")
cat("══════════════════════════════════════════════════════════════════\n")
