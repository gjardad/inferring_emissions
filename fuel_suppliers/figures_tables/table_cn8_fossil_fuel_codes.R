###############################################################################
# fuel_suppliers/descriptives/table_cn8_fossil_fuel_codes.R
#
# PURPOSE
#   Generate a paper-ready LaTeX longtable listing all CN8 fossil fuel codes
#   used to identify fuel importers in the customs data.
#
# INPUTS
#   {PROC_DATA}/cn8digit_codes_for_fossil_fuels.RData
#
# OUTPUTS
#   {OUTPUT_DIR}/cn8_fossil_fuel_codes.tex
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))


# ── Load CN8 codes ─────────────────────────────────────────────────────────
load(file.path(PROC_DATA, "cn8digit_codes_for_fossil_fuels.RData"))
codes <- cn8digit_codes_for_fossil_fuels

# Escape LaTeX special characters in descriptions
escape_tex <- function(x) {
  x <- gsub("%", "\\\\%", x)
  x <- gsub("&", "\\\\&", x)
  x <- gsub("#", "\\\\#", x)
  x <- gsub("<=", "$\\\\leq$", x)
  x <- gsub(">=", "$\\\\geq$", x)
  x <- gsub(">", "$>$", x)
  x <- gsub("<", "$<$", x)
  x
}

# Format CN8 code with thin space: XXXX XXXX
fmt_cn8 <- function(x) paste0(substr(x, 1, 4), "\\,", substr(x, 5, 8))


# ── Build LaTeX table ──────────────────────────────────────────────────────
tex <- c(
  "\\begin{longtable}{l p{0.65\\textwidth} c}",
  "\\caption{List of CN Codes and HS Labels for Fossil Fuels}",
  "\\label{tab: list of fuels} \\\\",
  "\\hline",
  "\\textbf{CN code} & \\textbf{Name in HS} & \\textbf{Core fuel} \\\\",
  "\\hline",
  "\\endfirsthead",
  "",
  "\\hline",
  "\\textbf{CN code} & \\textbf{Name in HS} & \\textbf{Core fuel} \\\\",
  "\\hline",
  "\\endhead",
  "",
  "\\hline",
  "\\multicolumn{3}{p{0.85\\textwidth}}{\\footnotesize",
  "\\textit{Notes}: Table lists the CN 8-digit codes and corresponding names in the World Harmonized Structure (HS) of all fossil fuels used to generate emissions in stationary installations. ``Core fuel'' indicates codes that unambiguously correspond to fuels combusted for energy; non-core codes are edge cases that may serve primarily as feedstock, solvent, or other non-energy uses.",
  "} \\\\",
  "\\endlastfoot"
)

for (i in seq_len(nrow(codes))) {
  r <- codes[i, ]
  core <- if (!r$edge_case) "Y" else "N"
  tex <- c(tex, sprintf(
    "%s & %s & %s \\\\",
    fmt_cn8(r$cn_code),
    escape_tex(r$description),
    core
  ))
}

tex <- c(tex, "", "\\end{longtable}")


# ── Save ───────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out_path <- file.path(OUTPUT_DIR, "cn8_fossil_fuel_codes.tex")
writeLines(tex, out_path)
cat("Saved CN8 fossil fuel codes table to:", out_path, "\n")
cat("  Total codes:", nrow(codes), "\n")
cat("  Core fuels:", sum(!codes$edge_case), "\n")
cat("  Edge cases:", sum(codes$edge_case), "\n")
