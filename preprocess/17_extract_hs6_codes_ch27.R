###############################################################################
# 01_preprocess/17_extract_hs6_codes_ch27.R
#
# PURPOSE
#   Extract HS6 codes under Chapter 27 used to define fuel goods in customs data.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/17_extract_hs6_codes_ch27.R
###############################################################################

#### HEADER -------

## Creates data set with HS 6-digit codes and descriptions of all goods in chapter 27

#####################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  DATA_DIR <- "C:/Users/jota_/Documents/NBB_projects/data"
  REPO_DIR <- "C:/Users/jota_/Documents/NBB_projects/inferring_emissions"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

UTILS_DIR <- file.path(REPO_DIR, "utils")
LOOCV_DIR <- file.path(REPO_DIR, "loocv")

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}


# ==============
# Set up -------
# ==============


library(readxl)

# ==============
# Import data --
# ==============

hs_codes_for_ch27 <- read_xlsx(
  paste0(RAW_DATA, "/Correspondences_and_dictionaries/HSCodeandDescription.xlsx"),
  sheet = 1
) %>% 
  filter(substr(Code, 1, 2) == "27") %>% 
  select(-c(Classification)) %>% 
  rename(hs_code = 1, description = 2, parent_code = 3, level = 4, is_basic_leve = 5)

# save it
save(hs_codes_for_ch27, file = paste0(PROC_DATA, "/hs_code_for_ch27_goods.RData"))
