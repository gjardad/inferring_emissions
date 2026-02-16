###############################################################################
# preprocess/00_download_ncv_from_energy_balance.R
#
# PURPOSE
#   Download net calorific values (nrg_bal_cv) from Eurostat for Belgium,
#   filtered to MJ/tonne and NCV_AVG / NCV_IMP / NCV_EXP.
#   Used by script 12.
#
# REPRODUCIBILITY
#   Uses the eurostat R package to pull directly from Eurostat bulk download.
###############################################################################

# ====================
# Define paths -------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

# ====================
# Setup --------------
# ====================

library(eurostat)
library(dplyr)
library(readr)

# ====================
# Download -----------
# ====================

cat("Downloading nrg_bal_cv from Eurostat...\n")
raw_cv <- get_eurostat("nrg_bal_cv",
                       filters = list(geo = "BE", unit = "MJ_T_NCV",
                                      calval = c("NCV_AVG", "NCV_IMP", "NCV_EXP")))
cat("Download complete:", nrow(raw_cv), "rows\n")

# ====================
# Get labels ---------
# ====================

dic_calval <- get_eurostat_dic("calval") %>%
  setNames(c("code", "label"))

dic_siec <- get_eurostat_dic("siec") %>%
  setNames(c("code", "label"))

# Build "CODE:Label" format columns
raw_cv <- raw_cv %>%
  left_join(dic_calval, by = c("calval" = "code")) %>%
  mutate(calval_labelled = paste0(calval, ":", label)) %>%
  select(-label) %>%
  left_join(dic_siec, by = c("siec" = "code")) %>%
  mutate(siec_labelled = paste0(siec, ":", label)) %>%
  select(-label)

# ====================
# Format and save ----
# ====================

out_cv <- raw_cv %>%
  transmute(
    calval      = calval_labelled,
    unit        = "MJ_T_NCV:Megajoule per tonne (net calorific value - NCV)",
    siec        = siec_labelled,
    TIME_PERIOD = as.integer(format(time, "%Y")),
    OBS_VALUE   = values
  )

outdir <- file.path(RAW_DATA, "Eurostat")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

write_csv(out_cv, file.path(outdir, "ncv_by_fuel_year.csv"))
cat("Saved:", file.path(outdir, "ncv_by_fuel_year.csv"), "\n")
cat("Done.\n")
