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
#
# EU ETS firms (observed emissions) + non-ETS firms assigned zero combustion
# emissions from sectors where EU ETS coverage is near-complete:
#   - C19: Oil refining (100% coverage, CRF 1.A.1.b)
#   - C17/C18: Pulp, paper and print (~97% coverage, CRF 1.A.2.d)
#   - C24 iron & steel only: NACE 24.1, 24.2, 24.3, 24.51, 24.52
#     (~95% coverage, CRF 1.A.2.a)
#     Excludes non-ferrous metals (24.4x, 24.53, 24.54) where EU ETS
#     covers only ~70% of combustion emissions (CRF 1.A.2.b).

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

# Define non-ETS zero-emission sectors (see header for justification)
ZERO_NACE2D <- c("17", "18", "19")                                    # full 2-digit sectors
ZERO_C24_NACE4D <- c("2410", "2420", "2431", "2432", "2433", "2434",  # C24 iron & steel only
                      "2451", "2452")

loocv_training_sample_pre <- df_annual_accounts_selected_sample_key_variables %>%
  left_join(euets, by = c("vat", "year")) %>%
  mutate(nace2d = substr(nace5d, 1, 2),
         nace4d = substr(nace5d, 1, 4),
         euets = coalesce(euets, 0),
         is_zero_sector = nace2d %in% ZERO_NACE2D |
                          (nace2d == "24" & nace4d %in% ZERO_C24_NACE4D)) %>%
  filter(euets == 1 | is_zero_sector,
         year >= 2005) %>%
  # For non-ETS firms in zero-emission sectors: emissions = 0
  mutate(emissions = if_else(is.na(emissions) & is_zero_sector, 0, emissions)) %>%
  select(-nace4d, -is_zero_sector)

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


