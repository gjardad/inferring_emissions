###############################################################################
# 05_analysis/07_table_euets_coverage_emissions_by_sector_from_nir.R
#
# PURPOSE
#   Create table comparing EU ETS coverage to NIR emissions by sector.
#
# DATA SOURCE
#   Annex XII xlsx files downloaded from EIONET:
#   2024: https://cdr.eionet.europa.eu/be/eu/govreg/annex/envzfqxiw/
#   2025: https://cdr.eionet.europa.eu/be/eu/govreg/inventory/envz9p5nw/index_html
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
  REPO_DIR <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))


# Setup ------


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

annexx_xii_2025 <- read_excel(paste0(RAW_DATA, "/NIR/BE_2025_Art14_Annex_XII-Consistency_with_ETS_2025_1281_Final_130325.xlsx")) %>% 
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

df_table_short <- df_table %>%
  filter(crf_code %in% c("1.A", "1.A.1", "1.A.2",
                         "1.A.3", "1.A.4", "1.B",
                         "2.A", "2.B", "2.C", "2D3", "2.H")) %>% 
  select(
    Category = crf_text,
    share_2024,
    share_2025
  )

# Generate table ------

library(knitr)
library(kableExtra)

df_table_short %>%
  kable(
    format = "latex",
    col.names = c("Category", "Share in 2024", "Share in 2025"),
    booktabs = TRUE,
    align = c("l", "r", "r")
  ) %>%
  kable_styling(latex_options = "hold_position")

