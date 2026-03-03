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
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
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

loocv_training_sample_pre <- df_annual_accounts_selected_sample_key_variables %>%
  left_join(euets, by = c("vat", "year")) %>%
  mutate(nace2d = substr(nace5d, 1, 2),
         euets = coalesce(euets, 0)) %>%
  filter(euets == 1 | nace2d %in% c("19", "24"),
         year >= 2005) %>%
  # For non-ETS NACE 19/24 firms: emissions = 0 (by construction, EU ETS
  # covers ~100% of fuel-combustion emissions in these sectors)
  mutate(emissions = if_else(is.na(emissions) & nace2d %in% c("19", "24"), 0, emissions))

n_na_euets <- sum(is.na(loocv_training_sample_pre$emissions))
cat("EU ETS firm-years with NA emissions (not yet regulated):", n_na_euets, "\n")

# Drop EU ETS firm-years where emissions is NA. These are years when the
# firm's installations were not yet regulated (e.g., pre-Phase III entrants).
# Treating them as zero emitters is incorrect — the firm was emitting,
# it just wasn't in the compliance system yet.
loocv_training_sample <- loocv_training_sample_pre %>%
  filter(!is.na(emissions))

cat("Training sample:", nrow(loocv_training_sample), "firm-years",
    "(dropped", n_na_euets, "pre-regulation obs)\n")
rm(loocv_training_sample_pre)

# save it
save(loocv_training_sample, file = paste0(PROC_DATA, "/loocv_training_sample.RData"))


