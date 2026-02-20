###############################################################################
# 01_define_proxy_grid.R
#
# PURPOSE:
#   Define the full Cartesian product of fuel proxy configurations.
#
# OUTPUT:
#   Creates object: proxy_grid (tibble)
###############################################################################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))



library(tibble)
library(tidyr)
library(dplyr)

# ----------------------------
# Define option sets
# ----------------------------

fuel_def <- c("broad_ch27", "strict_cn8")

use_siec_all       <- c(FALSE, TRUE)
supplier_non_euets <- c(FALSE, TRUE)
buyer_sector_siec  <- c(FALSE, TRUE)
emissions_weighted <- c(FALSE, TRUE)

supplier_nace_filter <- c("none", "high", "high_medium")

# ----------------------------
# Full Cartesian product
# ----------------------------

proxy_grid <- tidyr::crossing(
  fuel_def            = fuel_def,
  use_siec_all        = use_siec_all,
  supplier_non_euets  = supplier_non_euets,
  buyer_sector_siec   = buyer_sector_siec,
  emissions_weighted  = emissions_weighted,
  supplier_nace_filter= supplier_nace_filter
)

# ----------------------------
# Sanity checks (fail fast)
# ----------------------------

stopifnot(nrow(proxy_grid) == 96)

stopifnot(all(proxy_grid$fuel_def %in% c("broad_ch27", "strict_cn8")))
stopifnot(all(proxy_grid$supplier_nace_filter %in% c("none", "high", "high_medium")))

stopifnot(is.logical(proxy_grid$use_siec_all))
stopifnot(is.logical(proxy_grid$supplier_non_euets))
stopifnot(is.logical(proxy_grid$buyer_sector_siec))
stopifnot(is.logical(proxy_grid$emissions_weighted))

# Stable ordering (important for caching & reproducibility)
proxy_grid <- proxy_grid %>%
  arrange(
    fuel_def,
    emissions_weighted,
    buyer_sector_siec,
    use_siec_all,
    supplier_non_euets,
    supplier_nace_filter
  ) %>%
  mutate(proxy_id = row_number())

