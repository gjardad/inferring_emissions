###############################################################################
# 02_proxies/02_build_proxies.R
#
# PURPOSE:
#   Loads proxy grid definitions and builds each proxy once.
#   Results are cached to 02_proxies/cache as .rds files.
#
# INPUTS (via aux):
#   - aux <- load_aux() 
#
# OUTPUT:
#   - Writes: proxy_*.rds to 02_proxies/cache/
###############################################################################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  DATA_DIR <- "X:/Documents/JARDANG/data"
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  DATA_DIR <- "C:/Users/jota_/Documents/NBB_projects/data"
  REPO_DIR <- "C:/Users/jota_/Documents/NBB_projects/inferring_emissions"
} else {
  stop("Define 'folder' for this user.")
}

PROC_DATA <- file.path(DATA_DIR, "data", "processed")
RAW_DATA <- file.path(DATA_DIR, "data", "raw")
INT_DATA <- file.path(DATA_DIR, "data", "intermediate")

UTILS_DIR <- file.path(REPO_DIR, "utils")
LOOCV_DIR <- file.path(REPO_DIR, "loocv")

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}



# ----------------------------
# Setup
# ----------------------------
source("X:/Documents/JARDANG/carbon_policy_networks/code/inferring_emissions/00_config/paths.R")

# ----------------------------
# Load utilities + builders
# ----------------------------
source(file.path(LOOCV_ROOT, "06_utils", "progress_utils.R"))
source(file.path(LOOCV_ROOT, "06_utils", "fuel_proxy_builders.R"))

# ----------------------------
# Load proxy grid
# ----------------------------
source(file.path(LOOCV_ROOT, "02_proxies", "01_define_proxy_grid.R"))
# expects object: proxy_grid (a tibble/data.frame: one row per proxy config)
stopifnot(exists("proxy_grid"))

# Cache directory in the new structure
CACHE_DIR <- file.path(LOOCV_ROOT, "02_proxies", "cache")
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

tic("02_build_proxies")

aux <- load_aux()

grid <- proxy_grid

# ------------------------------------------------------------------
# Naming: keep your existing naming scheme, but operate on grid rows
# ------------------------------------------------------------------
make_proxy_name <- function(row) {
  paste0(
    "proxy",
    "_fuelDef", row$fuel_def,
    "_siecAll", as.integer(row$use_siec_all),
    "_nonEUETS", as.integer(row$supplier_non_euets),
    "_buyerSIEC", as.integer(row$buyer_sector_siec),
    "_wEm", as.integer(row$emissions_weighted),
    "_nace", row$supplier_nace_filter
  )
}

start_all <- Sys.time()

for (k in seq_len(nrow(grid))) {
  progress_eta(k, nrow(grid), start_all, every = 5, prefix = "  ")
  
  row <- grid[k, , drop = FALSE]
  # `row` is a 1-row df; convert cleanly to list of scalars
  mods <- as.list(row)
  mods <- lapply(mods, function(x) if (length(x) == 1) x[[1]] else x)
  
  nm  <- make_proxy_name(mods)
  out_path <- file.path(CACHE_DIR, paste0(nm, ".rds"))
  
  if (file.exists(out_path)) next
  
  log_step(paste0("Building ", nm))
  t0 <- Sys.time()
  
  proxy <- tryCatch(
    {
      build_fuel_proxy(mods, aux) %>%
        dplyr::mutate(fuel_proxy = as.numeric(fuel_proxy)) %>%
        dplyr::group_by(buyer_id, year) %>%
        dplyr::summarise(fuel_proxy = sum(fuel_proxy, na.rm = TRUE), .groups = "drop")
    },
    error = function(e) {
      message("FAILED proxy: ", nm)
      message("mods = ", paste0(names(mods), "=", unlist(mods), collapse = ", "))
      stop(e)
    }
  )
  
  # ---- Complete proxy on full buyer-year universe ----
  buyer_year_universe <- aux$b2b %>%
    dplyr::distinct(buyer_id, year)
  
  proxy <- buyer_year_universe %>%
    dplyr::left_join(proxy, by = c("buyer_id", "year")) %>%
    dplyr::mutate(fuel_proxy = dplyr::coalesce(fuel_proxy, 0))
  
  saveRDS(list(name = nm, mods = mods, proxy = proxy), out_path)
  
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  log_step(paste0("Saved ", nm, " | build time ", round(elapsed, 1), "s"))
}

toc()
