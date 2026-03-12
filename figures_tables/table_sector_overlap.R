###############################################################################
# figures_tables/table_sector_overlap.R
#
# PURPOSE
#   Show overlap between NACE sectors in training vs deployment at both
#   2-digit and 5-digit granularity. Produces a compact 3-row × 2-column
#   LaTeX table motivating the LOSOCV design.
#
# INPUTS
#   - {PROC_DATA}/firm_year_panel_with_proxies.RData
#   - {PROC_DATA}/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS
#   - {OUTPUT_DIR}/sector_overlap.tex
#   - Console diagnostics
#
# RUNS ON: RMD (full annual accounts needed)
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
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

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading firm-year panel with proxies...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

cat("Loading annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))

# ── Define deployment sectors ────────────────────────────────────────────────
# Deployment = all firms in annual accounts that are NOT in the training sample
# (i.e., non-ETS firms outside zero-emission sectors 17, 18, 19, subset of 24)

all_aa <- df_annual_accounts_selected_sample_key_variables %>%
  filter(!is.na(nace5d), nace5d != "") %>%
  mutate(nace2d = substr(nace5d, 1, 2))

training_vat_years <- training_sample %>%
  select(vat, year)

deployment <- all_aa %>%
  anti_join(training_vat_years, by = c("vat", "year"))

# ── Compute sector sets ─────────────────────────────────────────────────────
train_2d <- unique(training_sample$nace2d)
deploy_2d <- unique(deployment$nace2d)

train_5d <- unique(training_sample$nace5d)
deploy_5d <- unique(deployment$nace5d)

both_2d   <- length(intersect(train_2d, deploy_2d))
train_only_2d <- length(setdiff(train_2d, deploy_2d))
deploy_only_2d <- length(setdiff(deploy_2d, train_2d))

both_5d   <- length(intersect(train_5d, deploy_5d))
train_only_5d <- length(setdiff(train_5d, deploy_5d))
deploy_only_5d <- length(setdiff(deploy_5d, train_5d))

# ── Console diagnostics ─────────────────────────────────────────────────────
cat("\n── Sector overlap ──\n")
cat("\nNACE 2-digit:\n")
cat("  Training sectors:", length(train_2d), "\n")
cat("  Deployment sectors:", length(deploy_2d), "\n")
cat("  Both:", both_2d, "\n")
cat("  Training only:", train_only_2d, "\n")
cat("  Deployment only:", deploy_only_2d, "\n")
cat("  Training-only sectors:", paste(sort(setdiff(train_2d, deploy_2d)), collapse = ", "), "\n")
cat("  Deployment-only sectors:", paste(sort(setdiff(deploy_2d, train_2d)), collapse = ", "), "\n")

cat("\nNACE 5-digit:\n")
cat("  Training sectors:", length(train_5d), "\n")
cat("  Deployment sectors:", length(deploy_5d), "\n")
cat("  Both:", both_5d, "\n")
cat("  Training only:", train_only_5d, "\n")
cat("  Deployment only:", deploy_only_5d, "\n")

# ── Build LaTeX table ────────────────────────────────────────────────────────
tex <- c(
  "\\begin{tabular}{lcc}",
  "  \\toprule",
  "  & NACE 2-digit & NACE 5-digit \\\\",
  "  \\midrule",
  sprintf("  In both             & %d & %d \\\\", both_2d, both_5d),
  sprintf("  Training only       & %d & %d \\\\", train_only_2d, train_only_5d),
  sprintf("  Deployment only     & %d & %d \\\\", deploy_only_2d, deploy_only_5d),
  "  \\midrule",
  sprintf("  Total training      & %d & %d \\\\", length(train_2d), length(train_5d)),
  sprintf("  Total deployment    & %d & %d \\\\", length(deploy_2d), length(deploy_5d)),
  "  \\bottomrule",
  "\\end{tabular}"
)

out_path <- file.path(OUTPUT_DIR, "sector_overlap.tex")
writeLines(tex, out_path)
cat("\nTable written to:", out_path, "\n")
