###############################################################################
# preprocess/package_for_local.R
#
# PURPOSE
#   Merge training sample, fold-specific proxy, Tabachova proxy, and full
#   annual accounts into a single self-contained .RData for local analysis.
#
#   This is the file that gets copied from RMD to local 1.
#
# INPUT
#   {PROC_DATA}/training_sample.RData              (from build_training_sample.R)
#   {PROC_DATA}/fold_specific_proxy_asinh.RData     (from build_fold_specific_proxy_asinh.R)
#   {PROC_DATA}/tabachova_proxy.RData               (from build_tabachova_proxy.R)
#   {PROC_DATA}/annual_accounts_selected_sample.RData (full annual accounts)
#
# OUTPUT
#   {PROC_DATA}/training_sample_ammended.RData
#     Contains: training_sample (full panel with proxies + all annual accounts),
#               syt (sector-year emission totals)
#
# RUNS ON: RMD
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

# ── Load components ──────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

cat("Loading fold-specific proxy...\n")
load(file.path(PROC_DATA, "fold_specific_proxy_asinh.RData"))

cat("Loading Tabachova proxy...\n")
load(file.path(PROC_DATA, "tabachova_proxy.RData"))

cat("Loading full annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample.RData"))
aa_all <- df_annual_accounts_selected_sample %>%
  rename(vat = vat_ano)
rm(df_annual_accounts_selected_sample)

# ── Merge proxies onto training sample ───────────────────────────────────────
cat("Merging proxies...\n")

training_sample <- training_sample %>%
  mutate(
    y           = emissions,
    emit        = as.integer(emissions > 0),
    log_revenue = log(pmax(revenue, 1e-12))
  ) %>%
  left_join(
    fs_proxy_panel_asinh %>%
      select(vat, year, fold_specific_proxy_asinh, fold_specific_proxy_all_asinh,
             primary_nace2d, fold_k),
    by = c("vat", "year")
  ) %>%
  left_join(tabachova_proxy, by = c("vat", "year")) %>%
  mutate(
    fold_specific_proxy_asinh     = coalesce(fold_specific_proxy_asinh, 0),
    fold_specific_proxy_all_asinh = coalesce(fold_specific_proxy_all_asinh, 0),
    proxy_tabachova_asinh         = coalesce(proxy_tabachova_asinh, 0)
  )

# ── Merge full annual accounts ───────────────────────────────────────────────
cat("Merging annual accounts...\n")

aa_lhs <- aa_all %>%
  semi_join(training_sample, by = c("vat", "year"))

# Drop columns that already exist in training_sample
overlap_cols <- setdiff(intersect(names(aa_lhs), names(training_sample)),
                        c("vat", "year"))
if (length(overlap_cols) > 0) {
  cat("Dropping overlapping columns:", paste(overlap_cols, collapse = ", "), "\n")
  aa_lhs <- aa_lhs %>% select(-all_of(overlap_cols))
}

training_sample <- training_sample %>%
  left_join(aa_lhs, by = c("vat", "year"))

rm(aa_all, aa_lhs, fs_proxy_panel_asinh, tabachova_proxy)

# ── Sector-year emission totals ─────────────────────────────────────────────
syt <- training_sample %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")

# ── Save ─────────────────────────────────────────────────────────────────────
OUT_PATH <- file.path(PROC_DATA, "training_sample_ammended.RData")
save(training_sample, syt, file = OUT_PATH)

cat("\n══════════════════════════════════════════════\n")
cat("Saved:", OUT_PATH, "\n")
cat("  training_sample:", nrow(training_sample), "rows x",
    ncol(training_sample), "cols\n")
cat("  Firms:", n_distinct(training_sample$vat), "\n")
cat("  EU ETS:", n_distinct(training_sample$vat[training_sample$euets == 1]), "\n")
cat("  Non-ETS zeros:", n_distinct(training_sample$vat[training_sample$euets == 0]), "\n")
cat("  syt:", nrow(syt), "sector-year cells\n")
cat("══════════════════════════════════════════════\n")
cat("\nCopy this file to local 1 for analysis.\n")
