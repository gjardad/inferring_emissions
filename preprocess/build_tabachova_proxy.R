###############################################################################
# preprocess/build_tabachova_proxy.R
#
# PURPOSE
#   Build the Tabachova et al. benchmark proxy: for each buyer-year, sum of
#   asinh-transformed purchases from suppliers in fuel-related NACE codes.
#
#   Fuel-related NACE 4-digit codes (from Tabachova et al.):
#     3521, 3522, 3523  Gas distribution
#     1920              Petroleum refining
#     4671              Wholesale of fuels
#     4730              Retail sale of fuel
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/annual_accounts_selected_sample_key_variables.RData
#   {PROC_DATA}/training_sample.RData    (to restrict to training-sample buyers)
#
# OUTPUT
#   {PROC_DATA}/tabachova_proxy.RData
#     Contains: tabachova_proxy (data.frame with vat, year, proxy_tabachova_asinh)
#
# RUNS ON: RMD (requires full B2B data)
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
cat("Loading training sample (for buyer list)...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
buyer_vats <- unique(training_sample$vat)
rm(training_sample)

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading annual accounts (for supplier NACE lookup)...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))

# ── Identify Tabachova suppliers ─────────────────────────────────────────────
TABACHOVA_NACE4D <- c("3521", "3522", "3523", "1920", "4671", "4730")

supplier_nace <- df_annual_accounts_selected_sample_key_variables %>%
  distinct(vat, nace5d) %>%
  mutate(nace4d = substr(nace5d, 1, 4))
rm(df_annual_accounts_selected_sample_key_variables)

suppliers_tabachova <- supplier_nace %>%
  filter(nace4d %in% TABACHOVA_NACE4D) %>%
  pull(vat) %>%
  unique()

cat("Tabachova fuel-NACE suppliers:", length(suppliers_tabachova), "\n")

# ── Build proxy ──────────────────────────────────────────────────────────────
cat("Building proxy (asinh of purchases from fuel-NACE suppliers)...\n")

tabachova_proxy <- b2b %>%
  filter(vat_j_ano %in% buyer_vats,
         vat_i_ano %in% suppliers_tabachova,
         year >= 2005) %>%
  group_by(vat_j_ano, year) %>%
  summarise(proxy_tabachova       = sum(corr_sales_ij, na.rm = TRUE),
            proxy_tabachova_asinh = sum(asinh(corr_sales_ij), na.rm = TRUE),
            .groups = "drop") %>%
  rename(vat = vat_j_ano)

cat("Proxy built:", nrow(tabachova_proxy), "buyer-year obs\n")
cat("Buyers with proxy > 0:", n_distinct(tabachova_proxy$vat), "\n")

# ── Save ─────────────────────────────────────────────────────────────────────
save(tabachova_proxy, file = file.path(PROC_DATA, "tabachova_proxy.RData"))
cat("Saved:", file.path(PROC_DATA, "tabachova_proxy.RData"), "\n")
