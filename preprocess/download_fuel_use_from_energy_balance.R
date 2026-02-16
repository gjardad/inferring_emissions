###############################################################################
# preprocess/00_download_fuel_use_from_energy_balance.R
#
# PURPOSE
#   Download complete energy balance data (nrg_bal_c) from Eurostat for Belgium,
#   filtered to TJ and energy-use balance entries (FC_*_E, TI_*_E, NRG_*_E).
#   Used by scripts 15 and 16.
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
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

# ====================
# Setup --------------
# ====================

library(eurostat)
library(dplyr)
library(stringr)
library(readr)

# ====================
# Download -----------
# ====================

cat("Downloading nrg_bal_c from Eurostat (this may take a few minutes)...\n")
raw <- get_eurostat("nrg_bal_c", filters = list(geo = "BE", unit = "TJ"))
cat("Download complete:", nrow(raw), "rows\n")

# ====================
# Filter to energy-use balance entries
# ====================

# Keep only nrg_bal codes that start with FC, TI, or NRG and end with _E
energy_use_pattern <- "^(FC|TI|NRG).*_E$"

df <- raw %>%
  filter(str_detect(nrg_bal, energy_use_pattern))

cat("After filtering to energy-use entries:", nrow(df), "rows\n")

# ====================
# Get labels ---------
# ====================

dic_nrg_bal <- get_eurostat_dic("nrg_bal") %>%
  setNames(c("code", "label"))

dic_siec <- get_eurostat_dic("siec") %>%
  setNames(c("code", "label"))

# Build "CODE: Label" format columns
df <- df %>%
  left_join(dic_nrg_bal, by = c("nrg_bal" = "code")) %>%
  mutate(nrg_bal_labelled = paste0(nrg_bal, ": ", label)) %>%
  select(-label) %>%
  left_join(dic_siec, by = c("siec" = "code")) %>%
  mutate(siec_labelled = paste0(siec, ": ", label)) %>%
  select(-label)

# ====================
# Format and save ----
# ====================

out <- df %>%
  transmute(
    nrg_bal = nrg_bal_labelled,
    siec    = siec_labelled,
    TIME_PERIOD = as.integer(format(time, "%Y")),
    OBS_VALUE   = values
  )

outdir <- file.path(RAW_DATA, "Eurostat")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

write_csv(out, file.path(outdir, "energy_balance_belgium.csv"))
cat("Saved:", file.path(outdir, "energy_balance_belgium.csv"), "\n")
cat("Done.\n")
