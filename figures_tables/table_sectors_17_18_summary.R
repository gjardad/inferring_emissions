###############################################################################
# figures_tables/table_sectors_17_18_summary.R
#
# PURPOSE
#   Descriptive statistics for firms in NACE 17 (Paper) and NACE 18 (Printing)
#   split by EU ETS status. Same structure as table_sectors_19_24_summary.R.
#
#   Motivation: EU ETS covers ~97% of combustion emissions in C17/18 (from CRF
#   category 1.A.2.d Pulp, paper and print). If non-ETS firms in 17/18 are a
#   small share of sector revenue, we could justify adding them as zeros too.
#
# INPUTS
#   {RAW_DATA}/NBB/Annual_Accounts_MASTER_ANO.dta   (full annual accounts)
#   {PROC_DATA}/training_sample.RData               (to identify ETS firms)
#
# OUTPUTS
#   {OUTPUT_DIR}/sectors_17_18_summary.tex
#   Console output with summary stats
#
# RUNS ON: RMD (needs full annual accounts)
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
library(haven)

# ── Load ETS firm identifiers from training sample ──────────────────────────
load(file.path(PROC_DATA, "training_sample.RData"))

ets_vats <- training_sample %>%
  filter(euets == 1) %>%
  distinct(vat) %>%
  pull(vat)

rm(training_sample)
gc()

# ── Load full annual accounts ────────────────────────────────────────────────
cat("Loading annual accounts...\n")
aa <- read_dta(file.path(RAW_DATA, "NBB", "Annual_Accounts_MASTER_ANO.dta"),
               col_select = c("vat", "year", "nace5d", "turnover_VAT"))
gc()

# Derive NACE 2-digit
aa <- aa %>%
  mutate(nace2d = as.integer(substr(as.character(nace5d), 1, 2))) %>%
  filter(nace2d %in% c(17, 18)) %>%
  mutate(euets = as.integer(vat %in% ets_vats))

cat("Firms in NACE 17/18:", n_distinct(aa$vat), "\n")
cat("Of which ETS:", n_distinct(aa$vat[aa$euets == 1]), "\n")

# ── Compute summary stats ────────────────────────────────────────────────────
stats <- aa %>%
  mutate(group = ifelse(euets == 1, "ETS", "Non-ETS")) %>%
  group_by(nace2d, group) %>%
  summarise(
    firms    = n_distinct(vat),
    obs      = n(),
    mean_rev = mean(turnover_VAT, na.rm = TRUE),
    total_rev = sum(turnover_VAT, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(nace2d) %>%
  mutate(share_rev = total_rev / sum(total_rev) * 100) %>%
  ungroup()

# ── Print to console ────────────────────────────────────────────────────────
cat("\n=== Summary stats for NACE 17/18 by ETS status ===\n")
print(as.data.frame(stats))

# ── Build LaTeX table (same format as 19/24) ────────────────────────────────
fmt_int <- function(x) format(x, big.mark = ",")
fmt_rev <- function(x) formatC(x / 1e6, format = "f", digits = 0, big.mark = ",")
fmt_pct <- function(x) sprintf("%.1f", x)

get_val <- function(nace, grp) {
  row <- stats %>% filter(nace2d == nace, group == grp)
  if (nrow(row) == 0) return(list(firms = 0, obs = 0, mean_rev = NA, share_rev = 0))
  row
}

s17_ets    <- get_val(17, "ETS")
s17_nonets <- get_val(17, "Non-ETS")
s18_ets    <- get_val(18, "ETS")
s18_nonets <- get_val(18, "Non-ETS")

tex <- c(
  "\\begin{tabular}{l cccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Paper (NACE 17)} & \\multicolumn{2}{c}{Printing (NACE 18)} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & ETS & Non-ETS & ETS & Non-ETS \\\\",
  "\\midrule",
  sprintf("Firms & %s & %s & %s & %s \\\\",
          fmt_int(s17_ets$firms), fmt_int(s17_nonets$firms),
          fmt_int(s18_ets$firms), fmt_int(s18_nonets$firms)),
  sprintf("Firm-years & %s & %s & %s & %s \\\\",
          fmt_int(s17_ets$obs), fmt_int(s17_nonets$obs),
          fmt_int(s18_ets$obs), fmt_int(s18_nonets$obs)),
  sprintf("Mean revenue (M\\euro) & %s & %s & %s & %s \\\\",
          fmt_rev(s17_ets$mean_rev), fmt_rev(s17_nonets$mean_rev),
          fmt_rev(s18_ets$mean_rev), fmt_rev(s18_nonets$mean_rev)),
  sprintf("Revenue share (\\%%) & %s & %s & %s & %s \\\\",
          fmt_pct(s17_ets$share_rev), fmt_pct(s17_nonets$share_rev),
          fmt_pct(s18_ets$share_rev), fmt_pct(s18_nonets$share_rev)),
  "\\bottomrule",
  "\\end{tabular}"
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, "sectors_17_18_summary.tex")
writeLines(tex, out_path)
cat("\nSaved to:", out_path, "\n")
