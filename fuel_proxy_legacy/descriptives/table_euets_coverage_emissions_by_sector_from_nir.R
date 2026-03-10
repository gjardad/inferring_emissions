###############################################################################
# table_euets_coverage_emissions_by_sector_from_nir.R
#
# PURPOSE
#   Create table comparing EU ETS coverage to NIR emissions by sector,
#   showing both raw kt CO2eq and ETS coverage share for 2022 and 2023.
#
# DATA SOURCE
#   Annex XII xlsx files downloaded from EIONET:
#   2024 (year 2022): https://cdr.eionet.europa.eu/be/eu/govreg/annex/envzaasda/
#   2025 (year 2023): https://cdr.eionet.europa.eu/be/eu/govreg/annex/envz4jzgg/
###############################################################################

# ====================
# Define paths
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

# ====================
# Import data from pre-extracted TSVs
# (Original xlsx files segfault in readxl on local 1; TSVs were extracted via
#  PowerShell COM — see comments at top for xlsx source URLs.)
# Columns: crf (CRF label), kt_co2eq (inventory kt CO2eq), share (ETS %)
# ====================

read_annex_tsv <- function(path) {
  df <- read.delim(path, sep = "\t", stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8-BOM")
  df %>%
    filter(crf != "") %>%
    mutate(
      crf_trimmed = trimws(crf),
      kt_co2eq = suppressWarnings(as.numeric(gsub(",", "", kt_co2eq))),
      share = suppressWarnings(as.numeric(gsub("%", "", share)))
    )
}

annex_2024 <- read_annex_tsv(file.path(PROC_DATA, "annex_xii_2024.tsv"))
annex_2025 <- read_annex_tsv(file.path(PROC_DATA, "annex_xii_2025.tsv"))

# ====================
# Lookup helper: search by text pattern in the CRF label column
# ====================

lookup <- function(df, pattern) {
  row <- df %>% filter(grepl(pattern, crf_trimmed, ignore.case = TRUE, perl = TRUE))
  if (nrow(row) == 0) return(list(kt = NA_real_, share = NA_real_))
  if (nrow(row) > 1) row <- row[1, ]
  list(kt = row$kt_co2eq, share = row$share)
}

# ====================
# Row definitions: pattern (regex on crf_trimmed), display label, indent level
# Excludes: Transport (1.A.3), 1.A.2.g Other, 2.H Other
# ====================

# Each entry: pat (regex), label, indent, and optional "before" spacing
# before = "addlinespace" inserts \addlinespace, "midrule" inserts \midrule
row_defs <- list(
  list(pat = "^1\\.A\\s+Fuel combustion.*stationary",
       label = "Fuel combustion, stationary",          indent = 0),
  # -- Energy industries --
  list(pat = "^1\\.A\\.1\\s+Energy industries",
       label = "Energy industries",                    indent = 1),
  list(pat = "1\\.A\\.1\\.a\\s+Public electricity",
       label = "Public electricity and heat production", indent = 2),
  list(pat = "1\\.A\\.1\\.b\\s+Petroleum refining",
       label = "Petroleum refining",                   indent = 2),
  list(pat = "1\\.A\\.1\\.c\\s+Manufacture of solid fuels",
       label = "Manufacture of solid fuels \\& other",  indent = 2),
  # -- Manufacturing --
  list(pat = "^1\\.A\\.2\\.?\\s+Manufacturing",
       label = "Manufacturing industries and construction", indent = 1,
       before = "addlinespace"),
  list(pat = "1\\.A\\.2\\.a\\s+Iron",
       label = "Iron and steel",                       indent = 2),
  list(pat = "1\\.A\\.2\\.b\\s+Non-ferrous",
       label = "Non-ferrous metals",                   indent = 2),
  list(pat = "1\\.A\\.2\\.c\\s+Chemicals",
       label = "Chemicals",                            indent = 2),
  list(pat = "1\\.A\\.2\\.d\\s+Pulp",
       label = "Pulp, paper and print",                indent = 2),
  list(pat = "1\\.A\\.2\\.e\\s+Food",
       label = "Food processing, beverages and tobacco", indent = 2),
  list(pat = "1\\.A\\.2\\.f\\s+Non-metallic",
       label = "Non-metallic minerals",                indent = 2),
  # -- Other sectors --
  list(pat = "^1\\.A\\.4\\s+Other sectors",
       label = "Other sectors",                        indent = 1,
       before = "addlinespace"),
  list(pat = "1\\.A\\.4\\.a\\s+Commercial",
       label = "Commercial / Institutional",           indent = 2),
  list(pat = "1\\.A\\.4\\.c\\s+Agriculture",
       label = "Agriculture / Forestry / Fisheries",   indent = 2),
  # -- Industrial processes --
  list(pat = NA,
       label = "Industrial processes",                 indent = 0,
       before = "midrule"),
  list(pat = "^2\\.A\\s+Mineral",
       label = "Mineral products",                     indent = 1),
  list(pat = "^2\\.B\\s+Chemical",
       label = "Chemical industry",                    indent = 1),
  list(pat = "^2\\.C\\s+Metal",
       label = "Metal production",                     indent = 1),
  list(pat = "^2D3\\s+Non-energy",
       label = "Non-energy products from fuels and solvent use", indent = 1)
)

# ====================
# Formatting helpers
# ====================

fmt_kt <- function(x) {
  if (is.na(x)) return("--")
  formatC(x, format = "f", digits = 1, big.mark = ",")
}

fmt_pct <- function(x) {
  if (is.na(x)) return("--")
  sprintf("%.1f", x)
}

indent_tex <- function(level) {
  switch(as.character(level),
         "1" = "\\hspace{5mm}",
         "2" = "\\hspace{10mm}",
         "")
}

# ====================
# Build LaTeX table
# ====================

tex_lines <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{2022} & \\multicolumn{2}{c}{2023} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Category & kt CO\\textsubscript{2}eq & ETS (\\%) & kt CO\\textsubscript{2}eq & ETS (\\%) \\\\",
  "\\midrule"
)

for (rd in row_defs) {
  # Insert spacing before this row if requested
  bef <- rd$before %||% "none"
  if (bef == "addlinespace") tex_lines <- c(tex_lines, "\\addlinespace")
  if (bef == "midrule")      tex_lines <- c(tex_lines, "\\midrule")

  lbl <- paste0(indent_tex(rd$indent), rd$label)

  if (is.na(rd$pat)) {
    # Section header with no data (Industrial processes)
    tex_lines <- c(tex_lines,
      sprintf("%s & & & & \\\\", lbl))
    next
  }

  v24 <- lookup(annex_2024, rd$pat)
  v25 <- lookup(annex_2025, rd$pat)

  tex_lines <- c(tex_lines,
    sprintf("%s & %s & %s & %s & %s \\\\",
            lbl, fmt_kt(v24$kt), fmt_pct(v24$share),
            fmt_kt(v25$kt), fmt_pct(v25$share)))
}

tex_lines <- c(tex_lines, "\\bottomrule", "\\end{tabular}")

writeLines(tex_lines, file.path(OUTPUT_DIR, "euets_coverage_by_sector.tex"))
cat("\nSaved to", file.path(OUTPUT_DIR, "euets_coverage_by_sector.tex"), "\n")

