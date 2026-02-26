###############################################################################
# fuel_suppliers/build_training_sample.R
#
# PURPOSE
#   One-off script to create a complete training sample for local
#   experimentation. Merges the analysis panel (proxies + sector + emissions)
#   with ALL annual accounts variables (~228 columns).
#
#   Run this on RMD after build_proxy_and_cv.R has produced
#   fuel_suppliers_analysis_panel.RData. Then copy training_sample.RData
#   to local desktop.
#
# INPUT
#   {PROC_DATA}/fuel_suppliers_analysis_panel.RData
#   {PROC_DATA}/annual_accounts_selected_sample.RData
#
# OUTPUT
#   {PROC_DATA}/training_sample.RData
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
cat("Loading analysis panel...\n")
load(file.path(PROC_DATA, "fuel_suppliers_analysis_panel.RData"))

cat("Loading full annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample.RData"))

# ── Merge ────────────────────────────────────────────────────────────────────
# Annual accounts uses vat_ano; analysis panel uses vat
aa_all <- df_annual_accounts_selected_sample %>%
  rename(vat = vat_ano) %>%
  semi_join(analysis_panel, by = c("vat", "year"))
rm(df_annual_accounts_selected_sample)

# Drop columns that already exist in analysis_panel (avoid duplicates on join)
overlap_cols <- setdiff(intersect(names(aa_all), names(analysis_panel)),
                        c("vat", "year"))
if (length(overlap_cols) > 0) {
  cat("Dropping overlapping columns:", paste(overlap_cols, collapse = ", "), "\n")
  aa_all <- aa_all %>% select(-all_of(overlap_cols))
}

training_sample <- analysis_panel %>%
  left_join(aa_all, by = c("vat", "year"))
rm(aa_all, analysis_panel)

# ── Save ─────────────────────────────────────────────────────────────────────
save(training_sample, file = file.path(PROC_DATA, "training_sample.RData"))
cat("Saved:", nrow(training_sample), "rows x", ncol(training_sample), "cols\n")
cat("File:", file.path(PROC_DATA, "training_sample.RData"), "\n")
