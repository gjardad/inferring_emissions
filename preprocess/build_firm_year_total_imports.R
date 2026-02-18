###############################################################################
# 01_preprocess/build_firm_year_total_imports.R
#
# PURPOSE
#   Compute total import value per firm-year from customs data.
#
# INPUTS
#   - data/processed/df_trade.RData
#
# OUTPUTS
#   - data/processed/firm_year_total_imports.RData
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

library(dplyr)

load(file.path(PROC_DATA, "df_trade.RData"))

firm_year_total_imports <- df_trade %>%
  filter(flow == "I") %>%
  group_by(vat_ano, year) %>%
  summarise(total_imports = sum(cn_value, na.rm = TRUE),
            .groups = "drop")

save(firm_year_total_imports,
     file = file.path(PROC_DATA, "firm_year_total_imports.RData"))

message("Saved firm_year_total_imports with ",
        nrow(firm_year_total_imports), " rows.")
