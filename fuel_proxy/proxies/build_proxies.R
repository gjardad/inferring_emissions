###############################################################################
# build_proxies.R
#
# PURPOSE:
#   Loads proxy grid definitions and builds each proxy once.
#   Results are cached to proxies/cache as .rds files.
#
# INPUTS (via aux):
#   - aux <- load_aux()   (requires aux_*.rds in CACHE_DIR)
#
# OUTPUT:
#   - Writes: proxy_*.rds to proxies/cache/
###############################################################################

# ====================
# Define paths -------
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

# ----------------------------
# Load utilities + builders
# ----------------------------
source_try(UTILS_DIR, "progress_utils")
source_try(UTILS_DIR, "fuel_proxy_builders")

# ----------------------------
# Load proxy grid
# ----------------------------
source(file.path(REPO_DIR, "fuel_proxy", "proxies", "define_proxy_grid.R"))
# expects object: proxy_grid (a tibble/data.frame: one row per proxy config)
stopifnot(exists("proxy_grid"))

# Cache directory
if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

tic("build_proxies")

aux <- load_aux()
cat(sprintf("aux loaded: %d elements (%s)\n", length(aux), paste(names(aux), collapse = ", ")))
stopifnot(is.list(aux), length(aux) > 0)

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
      message("build_fuel_proxy args: mods=list(", length(mods), "), aux=", class(aux)[1],
              "(", length(aux), " elements)")
      message("Traceback:")
      calls <- sys.calls()
      for (cl in rev(calls)) message("  ", deparse(cl, nlines = 1))
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
