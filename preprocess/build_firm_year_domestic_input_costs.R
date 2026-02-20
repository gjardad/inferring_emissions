###############################################################################
# 01_preprocess/04_build_firm_year_domestic_input_costs.R
#
# PURPOSE
#   Build firm-year domestic input costs from annual accounts.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/04_build_firm_year_domestic_input_costs.R
###############################################################################

#### HEADER -------

## This code creates data set with firm-year total domestic input costs

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


# Libraries ----

library(tidyverse)
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately

# Import data ------
load(paste0(PROC_DATA, "/b2b_selected_sample.RData"))
df_b2b <- df_b2b_selected_sample

# Clean data ------

firm_year_domestic_input_cost <- df_b2b %>%
  group_by(vat_j_ano, year) %>% # Group by j and year
  summarize(input_cost = sum(corr_sales_ij, na.rm = TRUE)) %>% 
  rename(vat = vat_j_ano)

# Save it -----
save(firm_year_domestic_input_cost, file = paste0(PROC_DATA,"/firm_year_domestic_input_cost.RData"))  

