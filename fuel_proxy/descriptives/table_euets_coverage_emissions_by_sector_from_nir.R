###############################################################################
# 05_analysis/07_table_euets_coverage_emissions_by_sector_from_nir.R
#
# PURPOSE
#   Create table comparing EU ETS coverage to NIR emissions by sector.
#
# DATA SOURCE
#   Annex XII xlsx files downloaded from EIONET:
#   2024: https://cdr.eionet.europa.eu/be/eu/govreg/annex/envzaasda/
#   2025: https://cdr.eionet.europa.eu/be/eu/govreg/annex/envz4jzgg/
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/05_analysis/07_table_euets_coverage_emissions_by_sector_from_nir.R
###############################################################################

#### HEADER -------

## This code generates table that reports EUETS coverage of emissions by sector

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
library(tidyr)

# Import data ----

library(readxl)
annexx_xii_2024 <- read_excel(paste0(RAW_DATA, "/NIR/BE_2024_Art14_AnnexXII_Consistency_with_ETS_280224.xlsx")) %>% 
  select(1,4) %>% 
  rename(crf = 1, share = 2) %>% 
  filter(!is.na(crf) & !is.na(share)) %>% 
  filter(share != "NO") %>% 
  separate(crf, into = c("crf_code", "crf_text"), sep = " ",
           extra = "merge", remove = TRUE) %>% 
  filter(!crf_code %in% c("Greenhouse", "CO2", "Iron")) %>% 
  mutate(share_2024 = as.numeric(share)*100) %>% 
  select(crf_code, crf_text, share_2024)

annexx_xii_2025 <- read_excel(paste0(RAW_DATA, "/NIR/BE_2025_Art14_Annex XII-Consistency with ETS_2025_1281_Final_130325.xlsx")) %>% 
  select(1,4) %>% 
  rename(crf = 1, share = 2) %>% 
  filter(!is.na(crf) & !is.na(share)) %>% 
  separate(crf, into = c("crf_code", "crf_text"), sep = " ",
           extra = "merge", remove = TRUE) %>% 
  filter(!crf_code %in% c("Greenhouse", "CO2", "Iron")) %>%
  mutate(share = if_else(share == "NA", "", share)) %>% 
  mutate(share_2025 = as.numeric(share))

df_table <- annexx_xii_2024 %>% 
  left_join(annexx_xii_2025 %>% select(crf_code, crf_text, share_2025),
            by = c("crf_code", "crf_text")) %>% 
  mutate(
    share_2024 = sprintf("%.2f", share_2024),
    share_2025 = sprintf("%.2f", share_2025)
  )

# =====================================================================
# Generate LaTeX table with hierarchical indentation
# =====================================================================

# Row definitions: CRF code, text pattern (to disambiguate duplicates),
#                  display label, indent level (0/1/2)
row_defs <- list(
  list(code = "1.A",   pattern = "total",      label = "Fuel combustion activities, total",                indent = 0),
  list(code = "1.A",   pattern = "stationary",  label = "Fuel combustion activities, stationary combustion", indent = 1),
  list(code = "1.A.1", pattern = NULL,          label = "Energy industries",                                indent = 2),
  list(code = "1.A.2", pattern = NULL,          label = "Manufacturing industries and construction",        indent = 2),
  list(code = "1.A.3", pattern = NULL,          label = "Transport",                                        indent = 2),
  list(code = "1.A.4", pattern = NULL,          label = "Other sectors",                                    indent = 2),
  list(code = "1.B",   pattern = NULL,          label = "Fugitive emissions from fuels",                    indent = 0),
  list(code = NA,      pattern = NULL,          label = "Industrial processes",                              indent = 0),
  list(code = "2.A",   pattern = NULL,          label = "Mineral products",                                  indent = 2),
  list(code = "2.B",   pattern = NULL,          label = "Chemical industry",                                 indent = 2),
  list(code = "2.C",   pattern = NULL,          label = "Metal production",                                  indent = 2),
  list(code = "2D3",   pattern = NULL,          label = "Non-energy products from fuels and solvent use",    indent = 2),
  list(code = "2.H",   pattern = NULL,          label = "Other",                                             indent = 2)
)

# Lookup helper: find value from df_table by code (+ optional text pattern)
lookup_val <- function(code, pattern, col) {
  if (is.na(code)) return(NA_character_)
  rows <- df_table %>% filter(crf_code == code)
  if (!is.null(pattern)) rows <- rows %>% filter(grepl(pattern, crf_text, ignore.case = TRUE))
  if (nrow(rows) == 1) rows[[col]] else NA_character_
}

# Formatting helpers
fmt_val <- function(x) if (is.na(x) || x %in% c("NaN", "NA")) "-" else x

indent_tex <- function(level) {
  switch(as.character(level),
         "1" = "\\hspace{3mm}",
         "2" = "\\hspace{6mm}",
         "")
}

# Build LaTeX lines
tex_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "Category & 2024 (\\%) & 2025 (\\%)\\\\",
  "\\midrule"
)

for (rd in row_defs) {
  lbl <- paste0(indent_tex(rd$indent), rd$label)
  v24 <- fmt_val(lookup_val(rd$code, rd$pattern, "share_2024"))
  v25 <- fmt_val(lookup_val(rd$code, rd$pattern, "share_2025"))
  tex_lines <- c(tex_lines, sprintf("%s & %s & %s \\\\", lbl, v24, v25))
}

tex_lines <- c(tex_lines, "\\bottomrule", "\\end{tabular}")

writeLines(tex_lines, file.path(OUTPUT_DIR, "euets_coverage_by_sector.tex"))
cat("\nSaved to", file.path(OUTPUT_DIR, "euets_coverage_by_sector.tex"), "\n")

