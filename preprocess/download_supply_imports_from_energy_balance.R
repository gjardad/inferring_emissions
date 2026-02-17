###############################################################################
# preprocess/download_supply_imports_from_energy_balance.R
#
# PURPOSE
#   Download primary production (PPRD) and imports (IMP) from the Eurostat
#   energy balance (nrg_bal_c) for Belgium, all SIEC codes, in TJ.
#   Used to show that Belgium's fuel supply is dominated by imports.
#
# OUTPUTS
#   - RAW_DATA/Eurostat/supply_imports_energy_balance.csv
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

# ====================
# Setup --------------
# ====================

library(eurostat)
library(dplyr)
library(readr)

# ====================
# Download -----------
# ====================

cat("Downloading nrg_bal_c (PPRD + IMP) from Eurostat...\n")
raw <- get_eurostat("nrg_bal_c",
                    filters = list(geo = "BE", unit = "TJ",
                                   nrg_bal = c("PPRD", "IMP")))
cat("Download complete:", nrow(raw), "rows\n")

# ====================
# Filter years -------
# ====================

df <- raw %>%
  mutate(year = as.integer(format(time, "%Y"))) %>%
  filter(year >= 2005, year <= 2022)

cat("After filtering to 2005-2022:", nrow(df), "rows\n")

# ====================
# Get SIEC labels ----
# ====================

dic_siec <- get_eurostat_dic("siec") %>%
  setNames(c("code", "label"))

df <- df %>%
  left_join(dic_siec, by = c("siec" = "code")) %>%
  rename(siec_label = label)

# ====================
# Format and save ----
# ====================

out <- df %>%
  transmute(
    nrg_bal,
    siec,
    siec_label,
    year,
    value_tj = values
  )

outdir <- file.path(RAW_DATA, "Eurostat")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

write_csv(out, file.path(outdir, "supply_imports_energy_balance.csv"))
cat("Saved:", file.path(outdir, "supply_imports_energy_balance.csv"), "\n")
cat("Done.\n")
