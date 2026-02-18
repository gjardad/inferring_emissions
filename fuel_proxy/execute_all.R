###############################################################################
# fuel_proxy/execute_all.R
#
# PURPOSE
#   Master orchestrator: run the entire fuel_proxy pipeline from raw data
#   through to final descriptive outputs.
#
#   Stage 1: Preprocessing  — build processed datasets and 96 proxy variants
#   Stage 2: Models         — PPML benchmarks, hurdle top-K search, LOSOCV
#   Stage 3: Descriptives   — tables, figures, diagnostics
#
# USAGE
#   source("fuel_proxy/execute_all.R")
#
# NOTES
#   - Stages are sourced at the global level (not in isolated environments)
#     because execute_models.R uses on.exit() for cluster cleanup, which
#     does not work correctly inside source(local = new.env(...)).
#   - Errors within a stage are handled by that stage's own error trapping;
#     a stage-level failure here stops the pipeline (downstream stages depend
#     on upstream outputs).
###############################################################################

# ====================
# Define paths -------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))


# =====================================================================
# Helper: run a stage
# =====================================================================

run_stage <- function(script_path, stage_label) {
  cat("\n", strrep("#", 70), "\n")
  cat("  STAGE:", stage_label, "\n")
  cat(strrep("#", 70), "\n\n")
  t0 <- Sys.time()
  source(script_path, local = FALSE)
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  cat(sprintf("\n  [%s completed in %.1f min]\n", stage_label, elapsed))
}

pipeline_start <- Sys.time()


# =====================================================================
# Stage 1: Preprocessing
# =====================================================================

run_stage(
  file.path(REPO_DIR, "fuel_proxy", "execute_preprocessing.R"),
  "PREPROCESSING"
)


# =====================================================================
# Stage 2: Models
# =====================================================================

run_stage(
  file.path(REPO_DIR, "fuel_proxy", "models", "execute_models.R"),
  "MODELS"
)


# =====================================================================
# Stage 3: Descriptives
# =====================================================================

run_stage(
  file.path(REPO_DIR, "fuel_proxy", "descriptives", "execute_descriptives.R"),
  "DESCRIPTIVES"
)


# =====================================================================
pipeline_elapsed <- round(as.numeric(difftime(Sys.time(), pipeline_start, units = "mins")), 1)
cat("\n", strrep("#", 70), "\n")
cat("  ALL STAGES COMPLETE\n")
cat(sprintf("  Total elapsed: %.1f min\n", pipeline_elapsed))
cat(strrep("#", 70), "\n")
cat("Outputs in:", OUTPUT_DIR, "\n\n")
