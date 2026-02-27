###############################################################################
# fuel_suppliers/build_proxy.R
#
# PURPOSE
#   Build fuel-consumption proxies from elastic-net-identified suppliers and
#   prepare the training sample for downstream CV scripts.
#
#   This script requires the full B2B data and elastic net results, so it
#   runs on RMD only. The output (training_sample.RData) is self-contained
#   and can be copied to the local desktop for CV experimentation.
#
# INPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#   {PROC_DATA}/annual_accounts_selected_sample.RData
#
# OUTPUT
#   {PROC_DATA}/training_sample.RData
#     Contains: training_sample (data.frame), foldid, K_FOLDS, syt
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
cat("Loading elastic net inputs...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData"))

cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)


# ── Extract identified supplier sets ─────────────────────────────────────────
# Union of suppliers with positive coef in any model at lambda.min
suppliers_pooled <- supplier_summary_pooled %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

suppliers_fe <- supplier_summary_fe %>%
  filter(lambda == "min", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

cat("Identified suppliers (pooled):       ", length(suppliers_pooled), "\n")
cat("Identified suppliers (within-buyer): ", length(suppliers_fe), "\n")

# ── Phase 2A: additional supplier sets ────────────────────────────────────
# Robustness-filtered: suppliers in >= 3 of 4 models at lambda.min
suppliers_pooled_robust3 <- robustness_pooled %>%
  filter(n_models >= 3) %>%
  pull(vat_i_ano)

# Conservative lambda (1se): union of positive coefs at lambda.1se
suppliers_pooled_1se <- supplier_summary_pooled %>%
  filter(lambda == "1se", coef > 0) %>%
  distinct(vat_i_ano) %>%
  pull(vat_i_ano)

# Coefficient lookup for weighted proxy (enet_asinh model at lambda.min)
coef_lookup_pooled <- supplier_summary_pooled %>%
  filter(lambda == "min", model == "enet_asinh", coef > 0) %>%
  select(vat_i_ano, coef)

cat("Identified suppliers (pooled robust>=3):", length(suppliers_pooled_robust3), "\n")
cat("Identified suppliers (pooled 1se):      ", length(suppliers_pooled_1se), "\n")
cat("Suppliers with enet_asinh coef > 0:     ", nrow(coef_lookup_pooled), "\n")


# ── Build proxies ────────────────────────────────────────────────────────────
cat("\nBuilding fuel-consumption proxies...\n")

# Filter B2B to LHS buyers, years >= 2005
b2b_lhs <- b2b %>%
  filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)

# Helper: build proxy from a supplier set (unweighted sum of sales)
build_proxy <- function(b2b_df, supplier_set, proxy_name) {
  b2b_df %>%
    filter(vat_i_ano %in% supplier_set) %>%
    group_by(vat_j_ano, year) %>%
    summarise(proxy = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
    rename(vat = vat_j_ano) %>%
    rename_with(~ proxy_name, .cols = "proxy")
}

# Helper: coefficient-weighted proxy (beta_j x asinh(sales_ij))
build_weighted_proxy <- function(b2b_df, coef_df, proxy_name) {
  b2b_df %>%
    inner_join(coef_df, by = "vat_i_ano") %>%
    group_by(vat_j_ano, year) %>%
    summarise(proxy = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
              .groups = "drop") %>%
    rename(vat = vat_j_ano) %>%
    rename_with(~ proxy_name, .cols = "proxy")
}

# Original proxies
proxy_pooled <- build_proxy(b2b_lhs, suppliers_pooled, "proxy_pooled")
proxy_fe     <- build_proxy(b2b_lhs, suppliers_fe,     "proxy_fe")

# Phase 2A proxy variants
proxy_weighted <- build_weighted_proxy(b2b_lhs, coef_lookup_pooled, "proxy_weighted")
proxy_robust3  <- build_proxy(b2b_lhs, suppliers_pooled_robust3, "proxy_robust3")
proxy_robust3w <- build_weighted_proxy(
  b2b_lhs,
  coef_lookup_pooled %>% filter(vat_i_ano %in% suppliers_pooled_robust3),
  "proxy_robust3w"
)
proxy_1se <- build_proxy(b2b_lhs, suppliers_pooled_1se, "proxy_1se")

rm(b2b_lhs)

# Merge nace5d from loocv_training_sample
load(file.path(PROC_DATA, "loocv_training_sample.RData"))
nace5d_lookup <- loocv_training_sample %>%
  filter(year >= 2005) %>%
  select(vat, year, nace5d) %>%
  mutate(nace4d = substr(nace5d, 1, 4))
rm(loocv_training_sample)

# Load full annual accounts for training_sample export
load(file.path(PROC_DATA, "annual_accounts_selected_sample.RData"))
aa_all <- df_annual_accounts_selected_sample %>%
  rename(vat = vat_ano)
rm(df_annual_accounts_selected_sample)

# Merge proxies into LHS panel
panel <- lhs %>%
  left_join(proxy_pooled,  by = c("vat", "year")) %>%
  left_join(proxy_fe,      by = c("vat", "year")) %>%
  left_join(proxy_weighted, by = c("vat", "year")) %>%
  left_join(proxy_robust3,  by = c("vat", "year")) %>%
  left_join(proxy_robust3w, by = c("vat", "year")) %>%
  left_join(proxy_1se,      by = c("vat", "year")) %>%
  left_join(nace5d_lookup, by = c("vat", "year")) %>%
  mutate(
    proxy_pooled   = coalesce(proxy_pooled, 0),
    proxy_fe       = coalesce(proxy_fe, 0),
    proxy_weighted = coalesce(proxy_weighted, 0),
    proxy_robust3  = coalesce(proxy_robust3, 0),
    proxy_robust3w = coalesce(proxy_robust3w, 0),
    proxy_1se      = coalesce(proxy_1se, 0),
    emit           = as.integer(y > 0)
  )
rm(nace5d_lookup)

cat("Panel rows:", nrow(panel), "\n")
cat("Emitters:", sum(panel$emit), sprintf("(%.1f%%)\n", 100 * mean(panel$emit)))
cat("Proxy coverage (pooled > 0):   ", sum(panel$proxy_pooled > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_pooled > 0)))
cat("Proxy coverage (within-buyer): ", sum(panel$proxy_fe > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_fe > 0)))
cat("Proxy coverage (weighted > 0): ", sum(panel$proxy_weighted > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_weighted > 0)))
cat("Proxy coverage (robust3 > 0):  ", sum(panel$proxy_robust3 > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_robust3 > 0)))
cat("Proxy coverage (robust3w > 0): ", sum(panel$proxy_robust3w > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_robust3w > 0)))
cat("Proxy coverage (1se > 0):      ", sum(panel$proxy_1se > 0),
    sprintf("(%.1f%%)\n", 100 * mean(panel$proxy_1se > 0)))


# ── Sector-year emission totals (for calibration) ───────────────────────────
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")

cat("Sector-year cells:", nrow(syt), "\n")
cat("Cells with E_total > 0:", sum(syt$E_total > 0), "\n\n")


# ── Save training sample ───────────────────────────────────────────────────
# Merges core panel columns with ALL annual accounts variables so downstream
# scripts have everything without needing B2B or elastic net inputs.
aa_lhs <- aa_all %>%
  semi_join(panel, by = c("vat", "year"))
# Drop columns that already exist in panel
overlap_cols <- setdiff(intersect(names(aa_lhs), names(panel)), c("vat", "year"))
if (length(overlap_cols) > 0) aa_lhs <- aa_lhs %>% select(-all_of(overlap_cols))

training_sample <- panel %>%
  select(vat, year, y, log_revenue, nace2d, nace5d, euets, emit,
         proxy_pooled, proxy_fe,
         proxy_weighted, proxy_robust3, proxy_robust3w, proxy_1se) %>%
  left_join(aa_lhs, by = c("vat", "year"))

save(training_sample, foldid, K_FOLDS, syt,
     file = file.path(PROC_DATA, "training_sample.RData"))

cat("Saved training sample:", nrow(training_sample), "rows x",
    ncol(training_sample), "cols\n")
cat("Also saved: foldid (", length(foldid), " entries), K_FOLDS =", K_FOLDS,
    ", syt (", nrow(syt), " cells)\n")
cat("File:", file.path(PROC_DATA, "training_sample.RData"), "\n")
