###############################################################################
# 01_preprocess/18_extract_cn8_codes_ch27.R
#
# PURPOSE
#   Extract CN8 codes under Chapter 27 (excluding electricity) used to define fuel goods in customs data.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/18_extract_cn8_codes_ch27.R
###############################################################################

#### HEADER -------

## This code creates list of all CN 8-digit codes for goods listed in chapter 27 of World HS

# For each CN 8-digit code it includes the product description and the units it is reported in customs

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# 1. Path to your downloaded PDF -------
pdf_path <- paste0(raw_data, "/Correspondences_and_dictionaries/COMBINED_NOMENCLATURE_2022_EN.pdf")

# 2. Read all pages as text -------
all_pages <- pdf_text(pdf_path)

pages_ch27 <- all_pages[169:174]

# 3. Turn into a data frame: one row per line
raw_lines <- tibble(
  # local page index within chapter 27 (1–6) or keep absolute if you prefer
  page = 169:174,
  text = pages_ch27
) |>
  separate_rows(text, sep = "\n", convert = FALSE) |>
  group_by(page) |>
  mutate(
    line_in_page = row_number()
  ) |>
  ungroup() |>
  mutate(
    line_in_ch27 = row_number(),      
    text = str_squish(text)             
  )

# narrow to lines that matter
raw_lines <- raw_lines |>
   filter(line_in_ch27 >= 25, line_in_ch27 <= 381)

# ---- 2. Mark empty lines and detect code starts (4-, 6-, and 8-digit) ----

# Patterns at start of line:
#  - 8-digit: "XXXX XX XX"
#  - 6-digit: "XXXX XX"
#  - 4-digit: "XXXX"
pat_cn8 <- "^\\d{4}\\s\\d{2}\\s\\d{2}\\b"
pat_hs6 <- "^\\d{4}\\s\\d{2}\\b"
pat_hs4 <- "^\\d{4}\\b"

lines_flagged <- raw_lines |>
  mutate(
    is_empty = text == "" | is.na(text),
    
    code8 = str_extract(text, pat_cn8),
    code6 = if_else(is.na(code8), str_extract(text, pat_hs6), NA_character_),
    code4 = if_else(is.na(code8) & is.na(code6),
                    str_extract(text, pat_hs4),
                    NA_character_),
    
    code_level = case_when(
      !is.na(code8) ~ "CN8",
      !is.na(code6) ~ "HS6",
      !is.na(code4) ~ "HS4",
      TRUE          ~ NA_character_
    ),
    is_code_start = !is.na(code_level)
  )

# ---- 3. Group: each code start begins a new block ----
# Lines get the same group_id as the last code start above them.
lines_grouped <- lines_flagged |>
  mutate(
    group_id = cumsum(is_code_start)
  ) |>
  filter(group_id > 0)  # drop anything before the first code

# ---- 4. For each group (code block), keep from the first line
#        down to the line BEFORE first empty line (if any) ----
blocks <- lines_grouped |>
  group_by(group_id) |>
  summarise(
    text_vec      = list(text),
    is_empty_vec  = list(is_empty),
    code8_vec     = list(code8),
    code6_vec     = list(code6),
    code4_vec     = list(code4),
    code_level_vec = list(code_level),
    .groups = "drop"
  ) |>
  mutate(
    # find the last index to keep (cut at first empty after the first line)
    end_idx = map2_int(text_vec, is_empty_vec, ~{
      n <- length(.x)
      empties_after_first <- which(.y & seq_len(n) > 1)
      if (length(empties_after_first) == 0) n else min(empties_after_first) - 1
    }),
    # collapse lines 1:end_idx into one block of text
    block_text = map2_chr(text_vec, end_idx,
                          ~paste(.x[seq_len(.y)], collapse = " "))
  ) |>
  mutate(
    # choose the code and level from the FIRST line in the block
    code_raw = map_chr(code8_vec, ~.x[1]) %>%
      if_else(. == "" | is.na(.),
              map_chr(code6_vec, ~.x[1]),
              .) %>%
      if_else(. == "" | is.na(.),
              map_chr(code4_vec, ~.x[1]),
              .),
    
    level = map_chr(code_level_vec, ~.x[1]),
    
    # normalize code: remove spaces
    code = str_replace_all(code_raw, "\\s", ""),
    
    # remove the code from the block_text to get description
    Description = case_when(
      level == "CN8" ~ str_squish(str_replace(block_text, pat_cn8, "")),
      level == "HS6" ~ str_squish(str_replace(block_text, pat_hs6, "")),
      level == "HS4" ~ str_squish(str_replace(block_text, pat_hs4, "")),
      TRUE           ~ block_text
    )
  )

ch27_all_levels <- blocks |>
  select(code, code_raw, level, Description) |>
  arrange(code)

ch27_all_levels

# Identify units ---------

ch27_all_levels <- ch27_all_levels %>%
  mutate(
    # 1) raw unit only for CN8 rows: capture everything after the *last* dot.
    unit_raw = if_else(
      level == "CN8",
      # str_match returns a matrix; [,2] is the captured group
      str_match(Description, "\\.([^.]*)$")[, 2],
      NA_character_
    ),
    # 2) clean it up: collapse multiple spaces, trim, and normalize m³ -> m3
    unit = unit_raw %>%
      str_replace_all("\\s+", " ") %>%  # multiple spaces -> single
      str_trim() %>%
      str_replace_all("³", "3")
  )

# Cleaning ---------

ch27_all_levels2 <- ch27_all_levels %>%
  mutate(
    # remove -, ., :, spaces and lower-case for a clean check
    desc_no_punct = Description %>%
      str_replace_all("[-\\.:]", " ") %>%
      str_squish() %>%
      str_replace_all(" ", "") %>%
      str_to_lower(),
    is_other_only = (level == "CN8" & desc_no_punct == "other")
  )

get_base_name <- function(desc) {
  if (is.na(desc) || desc == "") return(NA_character_)
  
  # 1. split by ":" into hierarchical segments
  segs <- str_split(desc, ":", simplify = FALSE)[[1]]
  segs <- segs[nzchar(trimws(segs))]  # drop empty segments
  
  if (length(segs) == 0) return(NA_character_)
  
  # 2. choose the last segment whose cleaned text is not "other"
  pick_idx <- NA_integer_
  for (i in seq_along(segs)) {
    idx <- length(segs) + 1L - i   # walk backwards
    seg <- segs[idx]
    seg_clean <- seg %>%
      str_replace_all("[-\\.:]", " ") %>%
      str_squish() %>%
      str_replace_all(" ", "") %>%
      str_to_lower()
    if (seg_clean != "" && seg_clean != "other") {
      pick_idx <- idx
      break
    }
  }
  
  if (is.na(pick_idx)) {
    # everything is empty or "Other"
    seg <- segs[length(segs)]
  } else {
    seg <- segs[pick_idx]
  }
  
  # 3. clean the chosen segment to a nice label
  x <- seg
  
  # remove dot leaders (". . . . .") -> just spaces
  x <- str_replace_all(x, "\\.(\\s*\\.)+", " ")
  
  # remove a trailing "Other" bullet, like "- - - - Other"
  x <- str_replace(x, "(-\\s*)*Other\\s*$", "")
  
  # remove leading bullet hyphens "- - -"
  x <- str_replace(x, "^(-\\s*)+", "")
  
  # remove trailing hyphens-only segments
  x <- str_replace(x, "(-\\s*)+$", "")
  
  # final tidy
  x <- str_squish(x)
  
  if (x == "") NA_character_ else x
}

parents <- ch27_all_levels2 %>%
  filter(level %in% c("HS4", "HS6")) %>%
  mutate(
    parent_base = map_chr(Description, get_base_name)
  ) %>%
  select(level, code, parent_base)

ch27_final <- ch27_all_levels2 %>%
  # prepare HS parent codes for CN8 rows
  mutate(
    hs6_code = if_else(level == "CN8", substr(code, 1, 6), NA_character_),
    hs4_code = if_else(level == "CN8", substr(code, 1, 4), NA_character_)
  ) %>%
  # join HS6 parent labels
  left_join(
    parents %>%
      filter(level == "HS6") %>%
      select(code, parent_base_hs6 = parent_base),
    by = c("hs6_code" = "code")
  ) %>%
  # join HS4 parent labels (fallback)
  left_join(
    parents %>%
      filter(level == "HS4") %>%
      select(code, parent_base_hs4 = parent_base),
    by = c("hs4_code" = "code")
  ) %>%
  mutate(
    parent_base = coalesce(parent_base_hs6, parent_base_hs4),
    
    base_name_self = if_else(
      level == "CN8",
      map_chr(Description, get_base_name),
      NA_character_
    ),
    
    fuel_name = case_when(
      # CN8 that are pure "Other" -> "Other" + parent base label
      level == "CN8" & is_other_only & !is.na(parent_base) ~
        str_c("Other ", parent_base),
      
      level == "CN8" & is_other_only &  is.na(parent_base) ~
        "Other",  # safety fallback
      
      # CN8 not pure "Other" -> use own base name (e.g. "Phenols")
      level == "CN8" ~ base_name_self,
      
      # for HS4/HS6 we can also store base name if you like:
      TRUE ~ NA_character_
    )
  )

ch27_cn8_final <- ch27_final %>% 
  filter(level == "CN8") %>% 
  select(code, fuel_name, unit) %>% 
  rename(cncode = code)


