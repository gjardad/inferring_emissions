###############################################################################
# preprocess/add_tabachova_proxy.R
#
# PURPOSE
#   Lightweight script to build the Tabachova et al. (2025) benchmark proxy
#   and patch it into the existing training_sample.RData.
#
#   The proxy sums B2B purchases from suppliers in 6 pre-selected NACE 4-digit
#   codes used by Tabachova et al. for their Hungarian emission estimation:
#     Gas: 3521, 3522, 3523
#     Oil: 1920, 4671, 4730
#
#   This is the only script that needs to run on RMD to add the proxy.
#   For full-pipeline reproducibility, build_proxy.R also includes this proxy.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/annual_accounts_selected_sample.RData
#
# OUTPUT
#   {PROC_DATA}/training_sample.RData  (overwritten with proxy_tabachova added)
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


# ── Load existing training sample ────────────────────────────────────────────
ts_path <- file.path(PROC_DATA, "training_sample.RData")
cat("Loading training sample from:", ts_path, "\n")
load(ts_path)  # loads: training_sample, foldid, K_FOLDS, syt

if ("proxy_tabachova" %in% names(training_sample)) {
  cat("proxy_tabachova already exists in training_sample. Overwriting.\n")
  training_sample <- training_sample %>% select(-proxy_tabachova)
}

cat("Training sample:", nrow(training_sample), "rows x",
    ncol(training_sample), "cols\n")


# ── Load B2B data ────────────────────────────────────────────────────────────
cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)


# ── Load annual accounts (for supplier NACE codes) ──────────────────────────
cat("Loading annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample.RData"))
aa <- df_annual_accounts_selected_sample %>%
  rename(vat = vat_ano)
rm(df_annual_accounts_selected_sample)


# ── Identify Tabachova-style suppliers by NACE 4-digit code ─────────────────
# Tabachova et al. (2025): 6 pre-selected NACE Rev 2 codes.
# Same classification system in Belgium — codes are identical.
TABACHOVA_NACE4D <- c(
  "3521", "3522", "3523",   # gas: manufacture, distribution, trade through mains
  "1920", "4671", "4730"    # oil: refining, wholesale fuels, retail automotive fuel
)

supplier_nace <- aa %>%
  distinct(vat, nace5d) %>%
  mutate(nace4d = substr(nace5d, 1, 4))

suppliers_tabachova <- supplier_nace %>%
  filter(nace4d %in% TABACHOVA_NACE4D) %>%
  pull(vat) %>%
  unique()

cat("\n── Tabachova supplier identification ──\n")
cat("NACE codes:", paste(TABACHOVA_NACE4D, collapse = ", "), "\n")
cat("Suppliers found:", length(suppliers_tabachova), "\n")

# Show breakdown by NACE code
supplier_breakdown <- supplier_nace %>%
  filter(nace4d %in% TABACHOVA_NACE4D) %>%
  count(nace4d) %>%
  arrange(nace4d)
cat("Breakdown:\n")
print(supplier_breakdown, row.names = FALSE)

rm(aa, supplier_nace)


# ── Build proxy ──────────────────────────────────────────────────────────────
cat("\nBuilding Tabachova proxy...\n")

# Filter B2B to LHS buyers (training sample firms) and years >= 2005
lhs_vats <- unique(training_sample$vat)

proxy_tabachova <- b2b %>%
  filter(vat_j_ano %in% lhs_vats, year >= 2005,
         vat_i_ano %in% suppliers_tabachova) %>%
  group_by(vat_j_ano, year) %>%
  summarise(proxy_tabachova = sum(corr_sales_ij, na.rm = TRUE),
            .groups = "drop") %>%
  rename(vat = vat_j_ano)

rm(b2b)

cat("Proxy rows (firm-years with positive purchases):", nrow(proxy_tabachova), "\n")


# ── Merge into training sample ───────────────────────────────────────────────
training_sample <- training_sample %>%
  left_join(proxy_tabachova, by = c("vat", "year")) %>%
  mutate(proxy_tabachova = coalesce(proxy_tabachova, 0))

cat("\n── Coverage ──\n")
cat("Firms with proxy_tabachova > 0:", sum(training_sample$proxy_tabachova > 0),
    sprintf("(%.1f%%)\n", 100 * mean(training_sample$proxy_tabachova > 0)))
cat("Firms with proxy_tabachova = 0:", sum(training_sample$proxy_tabachova == 0),
    sprintf("(%.1f%%)\n", 100 * mean(training_sample$proxy_tabachova == 0)))


# ── Save ─────────────────────────────────────────────────────────────────────
save(training_sample, foldid, K_FOLDS, syt, file = ts_path)

cat("\nSaved updated training sample:", nrow(training_sample), "rows x",
    ncol(training_sample), "cols\n")
cat("File:", ts_path, "\n")
