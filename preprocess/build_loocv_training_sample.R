###############################################################################
# 01_preprocess/06_build_loocv_training_sample.R
#
# PURPOSE
#   Construct the canonical LOOCV training sample used by downstream models.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/06_build_loocv_training_sample.R
###############################################################################

#### HEADER -------

# This code builds the LOOCV training data set:

# EUETS firms + NACE2d C19/C24 non-ETS firms

#####################

# ====================
# Define paths ------------------
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


# Setup ------


library(dplyr)

# ===================
# Load data ---------
# ===================

load(paste0(PROC_DATA, "/firm_year_belgian_euets.RData"))

load(paste0(PROC_DATA, "/annual_accounts_selected_sample_key_variables.RData"))

euets <- firm_year_belgian_euets %>% 
  select(vat, year, emissions) %>% 
  mutate(euets = 1)

loocv_training_sample <- df_annual_accounts_selected_sample_key_variables %>% 
  left_join(euets, by = c("vat", "year")) %>% 
  mutate(nace2d = substr(nace5d, 1, 2),
         euets = coalesce(euets, 0)) %>% 
  filter(euets == 1 | nace2d %in% c("19", "24"),
         year >= 2005) %>% 
  mutate(emissions = if_else(is.na(emissions) & nace2d %in% c("19", "24"), 0, emissions))

# save it
save(loocv_training_sample, file = paste0(PROC_DATA, "/loocv_training_sample.RData"))


