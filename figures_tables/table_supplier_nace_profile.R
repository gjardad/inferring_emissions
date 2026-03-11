###############################################################################
# figures_tables/table_supplier_nace_profile.R
#
# PURPOSE
#   Characterize the suppliers selected by the full-sample elastic net
#   (enet_asinh at lambda.min). Produces:
#     - Console diagnostics with all numbers needed for the paper text
#     - LaTeX table with two panels:
#         Panel A: 6 Tabachova NACE 4-digit fuel sectors
#         Panel B: Top 10 non-Tabachova NACE 2-digit sectors + "Other"
#     - CSV with full NACE 2-digit breakdown
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS
#   - {OUTPUT_DIR}/supplier_nace_profile.tex
#   - {OUTPUT_DIR}/supplier_nace_profile.csv
#   - Console output with all numbers needed for the paper text
#
# RUNS ON: RMD (full annual accounts needed for NACE matching)
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

library(dplyr)


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))


# ── Filter to enet_asinh, lambda.min, all non-zero coefficients ──────────────
suppliers <- supplier_summary_pooled %>%
  filter(model == "enet_asinh", lambda == "min", coef != 0) %>%
  mutate(sign = if_else(coef > 0, "pos", "neg"))

n_total     <- nrow(suppliers)
n_pos       <- sum(suppliers$sign == "pos")
n_neg       <- sum(suppliers$sign == "neg")
n_candidates <- length(eligible_sellers)

cat("\n")
cat(strrep("=", 70), "\n")
cat("  AGGREGATE SUPPLIER COUNTS\n")
cat(strrep("=", 70), "\n\n")
cat("Total eligible sellers (candidates):", n_candidates, "\n")
cat("EN-selected suppliers (coef != 0):  ", n_total, "\n")
cat("  with positive coefficient:        ", n_pos, "\n")
cat("  with negative coefficient:        ", n_neg, "\n")


# ── Build NACE lookup (modal NACE 5-digit per firm) ──────────────────────────
nace_lookup <- df_annual_accounts_selected_sample_key_variables %>%
  filter(!is.na(nace5d)) %>%
  group_by(vat, nace5d) %>%
  summarise(n_years = n(), .groups = "drop") %>%
  group_by(vat) %>%
  slice_max(n_years, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(nace4d = substr(nace5d, 1, 4),
         nace2d = substr(nace5d, 1, 2)) %>%
  select(vat, nace5d, nace4d, nace2d)


# ── Merge suppliers with NACE codes ──────────────────────────────────────────
suppliers <- suppliers %>%
  left_join(nace_lookup, by = c("vat_i_ano" = "vat"))

n_matched   <- sum(!is.na(suppliers$nace4d))
n_unmatched <- sum(is.na(suppliers$nace4d))
cat("\nNACE match rate:", n_matched, "matched,",
    n_unmatched, "unmatched (",
    round(100 * n_unmatched / nrow(suppliers), 1), "% missing)\n")


# ── Tabachova NACE 4-digit codes ─────────────────────────────────────────────
tabachova_nace4d <- c("1920", "3521", "3522", "3523", "4671", "4730")
tabachova_nace2d <- unique(substr(tabachova_nace4d, 1, 2))

tabachova_descriptions <- c(
  "1920" = "Refined petroleum products",
  "3521" = "Manufacture of gas",
  "3522" = "Distribution of gaseous fuels",
  "3523" = "Trade of gas through mains",
  "4671" = "Wholesale of fuels",
  "4730" = "Retail sale of automotive fuel"
)

# NACE 2-digit descriptions for Panel B
nace2d_descriptions <- c(
  "43" = "Specialised construction",
  "71" = "Architectural \\& engineering",
  "49" = "Land transport",
  "45" = "Motor vehicle trade",
  "47" = "Retail trade",
  "25" = "Fabricated metal products",
  "62" = "Computer programming",
  "52" = "Warehousing \\& transport support",
  "78" = "Employment activities",
  "70" = "Management consultancy",
  "55" = "Accommodation",
  "27" = "Electrical equipment",
  "68" = "Real estate",
  "23" = "Other non-metallic minerals",
  "56" = "Food \\& beverage service",
  "28" = "Machinery \\& equipment",
  "81" = "Services to buildings",
  "42" = "Civil engineering",
  "31" = "Furniture",
  "41" = "Building construction",
  "79" = "Travel agency",
  "69" = "Legal \\& accounting",
  "77" = "Rental \\& leasing",
  "20" = "Chemicals",
  "18" = "Printing",
  "33" = "Repair of machinery",
  "73" = "Advertising",
  "74" = "Other professional activities",
  "10" = "Food products",
  "22" = "Rubber \\& plastic products",
  "63" = "Information services",
  "61" = "Telecommunications",
  "59" = "Film \\& TV production",
  "26" = "Computer \\& electronics",
  "16" = "Wood products",
  "38" = "Waste collection",
  "82" = "Office support",
  "29" = "Motor vehicles",
  "58" = "Publishing",
  "13" = "Textiles",
  "32" = "Other manufacturing",
  "14" = "Wearing apparel",
  "80" = "Security \\& investigation",
  "08" = "Other mining",
  "53" = "Postal \\& courier",
  "36" = "Water collection",
  "39" = "Remediation",
  "50" = "Water transport",
  "46" = "Wholesale trade",
  "35" = "Electricity \\& gas",
  "19" = "Refined petroleum",
  "01" = "Crop \\& animal production",
  "02" = "Forestry",
  "17" = "Paper products"
)


# ── Total weight (for shares) ───────────────────────────────────────────────
total_weight_all <- sum(abs(suppliers$coef))
matched <- suppliers %>% filter(!is.na(nace2d))


# ── NACE 2-digit table (full, for console + CSV) ────────────────────────────
total_weight_pos <- sum(suppliers$coef[suppliers$sign == "pos"])
total_weight_neg <- sum(abs(suppliers$coef[suppliers$sign == "neg"]))

nace2d_table <- matched %>%
  group_by(nace2d) %>%
  summarise(
    n_all  = n(),
    n_pos  = sum(sign == "pos"),
    n_neg  = sum(sign == "neg"),
    weight_pos = sum(coef[sign == "pos"]),
    weight_neg = sum(abs(coef[sign == "neg"])),
    .groups = "drop"
  ) %>%
  mutate(
    share_all      = round(100 * n_all / sum(n_all), 2),
    share_of_pos   = round(100 * n_pos / sum(n_pos), 2),
    share_of_neg   = round(100 * n_neg / sum(n_neg), 2),
    pct_pos_within = round(100 * n_pos / n_all, 1),
    pct_neg_within = round(100 * n_neg / n_all, 1),
    wshare_all     = round(100 * (weight_pos + weight_neg) / total_weight_all, 2),
    wshare_of_pos  = round(100 * weight_pos / total_weight_pos, 2),
    wshare_of_neg  = if (total_weight_neg > 0)
                       round(100 * weight_neg / total_weight_neg, 2) else 0,
    wpct_pos_within = round(100 * weight_pos / (weight_pos + weight_neg), 1),
    wpct_neg_within = round(100 * weight_neg / (weight_pos + weight_neg), 1),
    tabachova = if_else(nace2d %in% tabachova_nace2d, "yes", "")
  ) %>%
  arrange(desc(wshare_all))


# ── Print full NACE 2-digit table to console ─────────────────────────────────
cat("\n")
cat(strrep("=", 90), "\n")
cat("  NACE 2-DIGIT PROFILE OF EN-SELECTED SUPPLIERS\n")
cat(strrep("=", 90), "\n\n")

cat("--- HEADCOUNT ---\n\n")
cat(sprintf("%-6s  %5s %5s %5s | %7s %7s %7s | %6s %6s | %s\n",
            "NACE", "All", "Pos", "Neg",
            "Sh.All", "Sh.Pos", "Sh.Neg",
            "%PosIn", "%NegIn", "Tab."))
cat(strrep("-", 90), "\n")
for (i in seq_len(nrow(nace2d_table))) {
  r <- nace2d_table[i, ]
  cat(sprintf("%-6s  %5d %5d %5d | %6.2f%% %6.2f%% %6.2f%% | %5.1f%% %5.1f%% | %s\n",
              r$nace2d, r$n_all, r$n_pos, r$n_neg,
              r$share_all, r$share_of_pos, r$share_of_neg,
              r$pct_pos_within, r$pct_neg_within,
              r$tabachova))
}

cat("\n--- COEFFICIENT-WEIGHTED ---\n\n")
cat(sprintf("%-6s  %8s %8s %8s | %7s %7s %7s | %6s %6s | %s\n",
            "NACE", "W.Pos", "W.Neg", "W.All",
            "Sh.All", "Sh.Pos", "Sh.Neg",
            "%PosIn", "%NegIn", "Tab."))
cat(strrep("-", 90), "\n")
for (i in seq_len(nrow(nace2d_table))) {
  r <- nace2d_table[i, ]
  cat(sprintf("%-6s  %8.4f %8.4f %8.4f | %6.2f%% %6.2f%% %6.2f%% | %5.1f%% %5.1f%% | %s\n",
              r$nace2d, r$weight_pos, r$weight_neg, r$weight_pos + r$weight_neg,
              r$wshare_all, r$wshare_of_pos, r$wshare_of_neg,
              r$wpct_pos_within, r$wpct_neg_within,
              r$tabachova))
}


# ── Paper text summary ───────────────────────────────────────────────────────
cat("\n")
cat(strrep("=", 70), "\n")
cat("  PAPER TEXT SUMMARY\n")
cat(strrep("=", 70), "\n\n")

n_sectors_with_suppliers <- n_distinct(matched$nace2d)
core_sectors <- c("19", "35", "46")
n_core_all <- sum(nace2d_table$n_all[nace2d_table$nace2d %in% core_sectors])
pct_core_all <- round(100 * n_core_all / sum(nace2d_table$n_all), 1)
n_other_sectors <- sum(!nace2d_table$nace2d %in% core_sectors)

cat(sprintf("Selected suppliers span %d distinct NACE 2-digit sectors\n", n_sectors_with_suppliers))
cat(sprintf("%.1f%% of selected suppliers in NACE 19/35/46 (core fuel sectors)\n", pct_core_all))
cat(sprintf("Remaining suppliers scattered across %d other sectors\n", n_other_sectors))


# ── EN vs Tabachova comparison ───────────────────────────────────────────────
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  EN vs TABACHOVA SUPPLIER COMPARISON\n")
cat(strrep("=", 70), "\n\n")

en_in_tab  <- sum(suppliers$nace4d %in% tabachova_nace4d, na.rm = TRUE)
en_out_tab <- n_total - en_in_tab

cat("EN suppliers inside Tabachova NACE 4d codes:", en_in_tab,
    "(", round(100 * en_in_tab / n_total, 1), "% )\n")
cat("EN suppliers outside Tabachova NACE 4d codes:", en_out_tab,
    "(", round(100 * en_out_tab / n_total, 1), "% )\n\n")

tabachova_firms_in_data <- nace_lookup %>%
  filter(nace4d %in% tabachova_nace4d, vat %in% eligible_sellers)

n_tab_in_data  <- nrow(tabachova_firms_in_data)
n_tab_selected <- sum(tabachova_firms_in_data$vat %in% suppliers$vat_i_ano)
n_tab_missed   <- n_tab_in_data - n_tab_selected

cat("Tabachova-classified eligible sellers in data:", n_tab_in_data, "\n")
cat("  selected by EN:", n_tab_selected,
    "(", round(100 * n_tab_selected / n_tab_in_data, 1), "% )\n")
cat("  NOT selected by EN:", n_tab_missed,
    "(", round(100 * n_tab_missed / n_tab_in_data, 1), "% )\n")


# ── Build LaTeX table ────────────────────────────────────────────────────────
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  GENERATING LATEX TABLE\n")
cat(strrep("=", 70), "\n\n")

# Helper: format number with comma separator
fmt_n <- function(x) format(x, big.mark = ",")

# --- Panel A: Tabachova 4-digit codes ---
# Build 4-digit breakdown for EN-selected suppliers
nace4d_tab_data <- matched %>%
  filter(nace4d %in% tabachova_nace4d) %>%
  group_by(nace4d) %>%
  summarise(
    n_pos = sum(sign == "pos"),
    n_neg = sum(sign == "neg"),
    weight_share = round(100 * sum(abs(coef)) / total_weight_all, 2),
    .groups = "drop"
  )

# Ensure all 6 codes appear (even with zeros)
panel_a <- data.frame(nace4d = tabachova_nace4d, stringsAsFactors = FALSE) %>%
  left_join(nace4d_tab_data, by = "nace4d") %>%
  mutate(
    n_pos = coalesce(n_pos, 0L),
    n_neg = coalesce(n_neg, 0L),
    weight_share = coalesce(weight_share, 0),
    description = tabachova_descriptions[nace4d]
  )

# Panel A total
pa_total_pos <- sum(panel_a$n_pos)
pa_total_neg <- sum(panel_a$n_neg)
pa_total_ws  <- round(sum(panel_a$weight_share), 2)

# --- Panel B: Top 10 NACE 2-digit sectors + Other ---
all_2d <- nace2d_table %>%
  arrange(desc(wshare_all))

top10 <- all_2d %>% head(10)
other <- all_2d %>% tail(-10)

other_n_pos <- sum(other$n_pos)
other_n_neg <- sum(other$n_neg)
other_ws    <- round(sum(other$wshare_all), 2)
n_other_sectors_pb <- nrow(other)

# --- Assemble LaTeX ---
tex <- c(
  "\\begin{tabular}{l l cc c}",
  "\\toprule",
  "NACE code & Description & $N$ ($\\hat{\\beta}_j > 0$) & $N$ ($\\hat{\\beta}_j < 0$) & Weight share (\\%) \\\\",
  "\\midrule",
  sprintf("\\multicolumn{5}{l}{\\textit{Panel A: Suppliers from fuel-related NACE 4-digit sectors}} \\\\"),
  "\\addlinespace"
)

for (i in seq_len(nrow(panel_a))) {
  r <- panel_a[i, ]
  tex <- c(tex, sprintf("%s & %s & %d & %d & %.2f \\\\",
                        r$nace4d, r$description, r$n_pos, r$n_neg, r$weight_share))
}

tex <- c(tex,
  "\\addlinespace",
  sprintf("& Total & %d & %d & %.2f \\\\", pa_total_pos, pa_total_neg, pa_total_ws),
  "\\midrule",
  sprintf("\\multicolumn{5}{l}{\\textit{Panel B: Top sectors selected by Elastic Net}} \\\\"),
  "\\addlinespace"
)

for (i in seq_len(nrow(top10))) {
  r <- top10[i, ]
  desc <- nace2d_descriptions[r$nace2d]
  if (is.na(desc)) desc <- ""
  tex <- c(tex, sprintf("%s & %s & %d & %d & %.2f \\\\",
                        r$nace2d, desc, r$n_pos, r$n_neg, r$wshare_all))
}

tex <- c(tex,
  "\\addlinespace",
  sprintf("& Other (%d sectors) & %d & %d & %.2f \\\\",
          n_other_sectors_pb, other_n_pos, other_n_neg, other_ws),
  "\\bottomrule",
  "\\end{tabular}"
)

# Print to console
cat(paste(tex, collapse = "\n"), "\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

tex_path <- file.path(OUTPUT_DIR, "supplier_nace_profile.tex")
writeLines(tex, tex_path)
cat("\nSaved LaTeX table to:", tex_path, "\n")

write.csv(nace2d_table,
          file.path(OUTPUT_DIR, "supplier_nace_profile.csv"),
          row.names = FALSE)
cat("Saved CSV to:", file.path(OUTPUT_DIR, "supplier_nace_profile.csv"), "\n")
