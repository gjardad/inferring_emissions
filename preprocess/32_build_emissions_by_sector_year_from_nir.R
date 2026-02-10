###############################################################################
# 01_preprocess/32_build_emissions_by_sector_year_from_nir.R
#
# PURPOSE
#   Build sector-year emissions from the National Inventory Report (NIR).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/32_build_emissions_by_sector_year_from_nir.R
###############################################################################

#### HEADER -------

## Create data set with emissions by year by CRF category
# from NIR

# For each CRF category, include corresponding NACE code and nace_group
# (see crf_to_nace_map for nace_groups)

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


## Setup ------


# Libraries ----

library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(purrr)
library(stringr)

# CRF to NACE map ------
  load(paste0(PROC_DATA, "crf_to_nace_map.RData"))

# Create data set with rows at the CRF-year level ------

  years  <- 2005:2022

  df_expanded <- crf_to_nace_map %>%
    tidyr::expand_grid(year = years)
  
  valid_crf_codes <- crf_to_nace_map$crf %>%
    unique() %>%
    ifelse(grepl("\\.$", .), ., paste0(., "."))

# Setup up .xlsx files and sheets I'm interested on -------
  
  sheets <- c(
    "Table1.A(a)s1",
    "Table1.A(a)s2",
    "Table1.A(a)s4",
    "Table1.B.2",
    "Table2(I).A-H"
  )
  
  nir_folder <- file.path(RAW_DATA, "/NIR/")
  
  file_for_year <- function(y) {
    file.path(
      nir_folder,
      sprintf("BEL-CRT-2024-V1.0-%d-20241217-192810_awaiting submission.xlsx", y)
    )
  }
  
  ## Extract CRF code from the start of the text, always ending with "."
  extract_crf_code <- function(x) {
    # e.g. "1.A.1.a. Public electricity..." -> "1.A.1.a."
    code <- str_extract(x, "^[0-9](?:\\.[0-9A-Za-z]+)*\\.?")
    # ensure trailing dot
    code <- ifelse(!is.na(code) & !grepl("\\.$", code),
                   paste0(code, "."),
                   code)
    code
  }
  
# Look for CO2 emissions for the valid CRF codes ------
  
  co2_lookup <- map_dfr(
    years,
    \(y) {
      path <- file_for_year(y)
      
      map_dfr(
        sheets,
        \(sh) {
          df_sheet <- tryCatch(
            read_excel(path, sheet = sh, col_types = "text"),
            error = function(e) NULL
          )
          if (is.null(df_sheet) || ncol(df_sheet) < 7) return(tibble())
          
          raw_b <- df_sheet[[1]]   # CRF text (column B in Excel)
          raw_v <- df_sheet[[7]]   # value column (column G/H in Excel)
          
          tibble(
            year          = y,
            crf           = extract_crf_code(raw_b),
            co2_emissions = suppressWarnings(as.numeric(raw_v))
          ) %>%
            filter(
              !is.na(crf),
              crf %in% valid_crf_codes
            )
        }
      )
    }
  )
  
  emissions_by_crf_year <- df_expanded %>%
    left_join(co2_lookup, by = c("year", "crf"))

# Save it ------
save(emissions_by_crf_year, file = paste0(PROC_DATA, "emissions_by_crf_year.RData"))
  

