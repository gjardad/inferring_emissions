###############################################################################
# scripts/check_b2b_for_lost_euets.R
#
# PURPOSE
#   Check whether EU ETS firms that are lost due to missing wage bill / FTE
#   have B2B transaction data. If they do, they could potentially be added
#   to the training sample with partial covariates.
#
# RUNS ON: RMD (needs full B2B data)
###############################################################################

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
  REPO_DIR <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

# ── Load data ────────────────────────────────────────────────────────────────
load(file.path(PROC_DATA, "firm_year_belgian_euets.RData"))
load(file.path(PROC_DATA, "training_sample.RData"))
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))  # or df_b2b.RData

# ── Identify lost EU ETS firms ──────────────────────────────────────────────
ts_euets <- training_sample %>% filter(euets == 1) %>% select(vat, year)

lost <- firm_year_belgian_euets %>%
  filter(!is.na(emissions)) %>%
  anti_join(ts_euets, by = c("vat", "year")) %>%
  # Keep only those with AA match but failing wage_bill/FTE
  filter(!is.na(nace5d),
         is.na(wage_bill) | wage_bill <= 0 | is.na(fte) | fte <= 1)

cat(sprintf("Lost EU ETS firm-years (have AA, fail wage_bill/FTE): %d (%d firms)\n",
            nrow(lost), n_distinct(lost$vat)))

# ── Check B2B presence ──────────────────────────────────────────────────────
# B2B data: check if lost firms appear as buyer or supplier
# Adapt column names to whatever the B2B data uses
b2b_cols <- names(df_b2b)
cat("\nB2B columns:", paste(b2b_cols, collapse = ", "), "\n\n")

# Check as buyer
lost_vats <- unique(lost$vat)

# Firms appearing in B2B as buyer
if ("buyer" %in% b2b_cols) {
  buyer_col <- "buyer"
} else if ("vat_buyer" %in% b2b_cols) {
  buyer_col <- "vat_buyer"
} else {
  buyer_col <- b2b_cols[grep("buy", b2b_cols, ignore.case = TRUE)[1]]
}

if ("supplier" %in% b2b_cols) {
  supplier_col <- "supplier"
} else if ("vat_supplier" %in% b2b_cols) {
  supplier_col <- "vat_supplier"
} else {
  supplier_col <- b2b_cols[grep("sup|sell", b2b_cols, ignore.case = TRUE)[1]]
}

cat(sprintf("Using buyer col: '%s', supplier col: '%s'\n\n", buyer_col, supplier_col))

b2b_buyers <- unique(df_b2b[[buyer_col]])
b2b_suppliers <- unique(df_b2b[[supplier_col]])

lost_in_b2b <- lost %>%
  mutate(
    in_b2b_as_buyer    = vat %in% b2b_buyers,
    in_b2b_as_supplier = vat %in% b2b_suppliers,
    in_b2b_any         = in_b2b_as_buyer | in_b2b_as_supplier
  )

cat("=== B2B coverage of lost EU ETS firms ===\n\n")
cat(sprintf("  In B2B as buyer:    %d / %d firm-years\n",
            sum(lost_in_b2b$in_b2b_as_buyer), nrow(lost_in_b2b)))
cat(sprintf("  In B2B as supplier: %d / %d firm-years\n",
            sum(lost_in_b2b$in_b2b_as_supplier), nrow(lost_in_b2b)))
cat(sprintf("  In B2B (either):    %d / %d firm-years\n",
            sum(lost_in_b2b$in_b2b_any), nrow(lost_in_b2b)))

cat(sprintf("\n  Unique firms in B2B: %d / %d\n",
            n_distinct(lost_in_b2b$vat[lost_in_b2b$in_b2b_any]),
            n_distinct(lost_in_b2b$vat)))

cat(sprintf("  Emissions in B2B:     %.0f kt\n",
            sum(lost_in_b2b$emissions[lost_in_b2b$in_b2b_any], na.rm = TRUE) / 1e3))
cat(sprintf("  Emissions not in B2B: %.0f kt\n",
            sum(lost_in_b2b$emissions[!lost_in_b2b$in_b2b_any], na.rm = TRUE) / 1e3))

# ── Also check the no-AA-match group ────────────────────────────────────────
lost_no_aa <- firm_year_belgian_euets %>%
  filter(!is.na(emissions)) %>%
  anti_join(ts_euets, by = c("vat", "year")) %>%
  filter(is.na(nace5d))

cat(sprintf("\n\n=== No-AA-match group: %d firm-years (%d firms) ===\n",
            nrow(lost_no_aa), n_distinct(lost_no_aa$vat)))

lost_no_aa <- lost_no_aa %>%
  mutate(
    in_b2b_as_buyer    = vat %in% b2b_buyers,
    in_b2b_as_supplier = vat %in% b2b_suppliers,
    in_b2b_any         = in_b2b_as_buyer | in_b2b_as_supplier
  )

cat(sprintf("  In B2B (either):    %d / %d firm-years\n",
            sum(lost_no_aa$in_b2b_any), nrow(lost_no_aa)))
cat(sprintf("  Unique firms in B2B: %d / %d\n",
            n_distinct(lost_no_aa$vat[lost_no_aa$in_b2b_any]),
            n_distinct(lost_no_aa$vat)))
cat(sprintf("  Emissions in B2B:     %.0f kt\n",
            sum(lost_no_aa$emissions[lost_no_aa$in_b2b_any], na.rm = TRUE) / 1e3))
