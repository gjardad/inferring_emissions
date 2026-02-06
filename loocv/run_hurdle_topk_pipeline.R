###############################################################################
# run_hurdle_topk_pipeline.R
#
# PURPOSE
#   Standalone pipeline that:
#     1) builds base data
#     2) precomputes step 1 (phat) and ranks (proxy, threshold) pairs
#     3) precomputes step 2 (muhat) and ranks proxies
#     4) evaluates hurdle model only on top-K step1 pairs x top-K step2 proxies
#        and computes RMSE for each triple
#
# REQUIRED INPUTS (must exist in the environment before sourcing this script)
#   df_run          : firm-year estimation sample
#   syt_run         : sector-year totals (sector, year, E_total)
#   proxy_cache_dir : directory with proxy_*.rds files
#   output_dir      : directory to write outputs
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

source_try <- function(dir, fname_no_ext) {
  f <- file.path(dir, paste0(fname_no_ext, ".R"))
  if (!file.exists(f)) stop("Missing file: ", normalizePath(f, winslash = "/", mustWork = FALSE))
  source(normalizePath(f, winslash = "/", mustWork = TRUE), local = TRUE)
}

repo_dir <- getwd()
utils_dir <- file.path(repo_dir, "utils")
loocv_dir <- file.path(repo_dir, "loocv")

source_try(utils_dir, "calc_metrics")
source_try(utils_dir, "step1_metrics_threshold")
source_try(loocv_dir, "hurdle_fast_prep_base_DT")
source_try(loocv_dir, "hurdle_fast_step1_ext")
source_try(loocv_dir, "hurdle_fast_step2_int")
source_try(loocv_dir, "hurdle_fast_evaluate_triples")

if (!exists("df_run")) stop("df_run not found in environment")
if (!exists("syt_run")) stop("syt_run not found in environment")
if (!exists("proxy_cache_dir")) stop("proxy_cache_dir not found in environment")
if (!exists("output_dir")) stop("output_dir not found in environment")

# --------------------------------------------------------------------------
# Configurable parameters
# --------------------------------------------------------------------------
thresholds <- seq(0, 0.45, by = 0.05)
low_fpr_max <- 0.1
high_fpr_max <- 0.2
step1_topk_per_band <- 5
step2_topk <- 5

DROP_SINGLETON_CELLS_IN_METRICS <- TRUE
FALLBACK_EQUAL_SPLIT <- TRUE
PROGRESS_EVERY <- 50

cache_dir <- file.path(output_dir, "cache_topk_pipeline")
step1_cache_dir <- file.path(cache_dir, "step1")
step2_cache_dir <- file.path(cache_dir, "step2")

# --------------------------------------------------------------------------
# Helper to read proxy RDS consistently + extract proxy name used in tags
# --------------------------------------------------------------------------
read_proxy_rds_simple <- function(path) {
  obj <- readRDS(path)
  tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(path))
  tbl <- data.table::as.data.table(tbl)
  if ("buyer_id" %in% names(tbl) && !"vat" %in% names(tbl)) data.table::setnames(tbl, "buyer_id", "vat")
  if (!all(c("vat", "year", "fuel_proxy") %in% names(tbl))) stop("Proxy missing vat/year/fuel_proxy: ", path)
  list(tbl = tbl, name = name)
}

safe_tag <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

phat_path_for <- function(proxy_name) file.path(step1_cache_dir, paste0("phat__", safe_tag(proxy_name), ".rds"))
muhat_path_for <- function(proxy_name) file.path(step2_cache_dir, paste0("muhat__", safe_tag(proxy_name), ".rds"))

# --------------------------------------------------------------------------
# 1) Build base
# --------------------------------------------------------------------------
base <- prep_hurdle_base_DT(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue"
)

DT0 <- data.table::copy(base$DT)
DT0[, id := id__]
DT0[, year := year__]

# --------------------------------------------------------------------------
# 2) Precompute step 1 (phat)
# --------------------------------------------------------------------------
if (!dir.exists(step1_cache_dir)) dir.create(step1_cache_dir, recursive = TRUE)
if (!dir.exists(step2_cache_dir)) dir.create(step2_cache_dir, recursive = TRUE)

ext_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
int_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(ext_files) == 0 || length(int_files) == 0) stop("No proxies found in: ", proxy_cache_dir)

phat_paths <- lapply(seq_along(ext_files), function(j) {
  ext <- read_proxy_rds_simple(ext_files[[j]])
  out_path <- phat_path_for(ext$name)

  if (file.exists(out_path)) return(out_path)

  phat_dt <- precompute_step1_ext(
    base = base,
    proxy_ext_tbl = ext$tbl,
    proxy_keys = c("vat", "year"),
    proxy_var = "fuel_proxy",
    partial_pooling = TRUE,
    progress_every = PROGRESS_EVERY
  )
  saveRDS(list(proxy_name = ext$name, phat_dt = phat_dt), out_path)
  out_path
})
phat_paths <- unlist(phat_paths)

# --------------------------------------------------------------------------
# 3) Step 1 ranking: (proxy, threshold) pairs
# --------------------------------------------------------------------------
step1_metrics <- step1_threshold_metrics(
  base = base,
  phat_paths = phat_paths,
  thresholds = thresholds,
  progress_every = PROGRESS_EVERY
)

step1_low <- step1_metrics[
  FPR_nonemitters <= low_fpr_max
][order(-emitter_mass_captured)][1:min(step1_topk_per_band, .N)]

step1_high <- step1_metrics[
  FPR_nonemitters > low_fpr_max & FPR_nonemitters <= high_fpr_max
][order(-emitter_mass_captured)][1:min(step1_topk_per_band, .N)]

topk_step1 <- rbindlist(list(step1_low, step1_high), fill = TRUE)
if (nrow(topk_step1) == 0) stop("No step-1 pairs after band filtering. Check thresholds or FPR bands.")

step1_pairs <- unique(topk_step1[, .(proxy_tag_ext, threshold_value = threshold)])

saveRDS(step1_metrics, file.path(output_dir, "step1_metrics_threshold_all.rds"))
fwrite(step1_metrics, file.path(output_dir, "step1_metrics_threshold_all.csv"))
saveRDS(topk_step1, file.path(output_dir, "topk_step1_pairs_by_band.rds"))

# --------------------------------------------------------------------------
# 4) Precompute step 2 (muhat) + rank proxies
# --------------------------------------------------------------------------
muhat_paths <- lapply(seq_along(int_files), function(j) {
  int <- read_proxy_rds_simple(int_files[[j]])
  out_path <- muhat_path_for(int$name)

  if (file.exists(out_path)) return(out_path)

  muhat_dt <- precompute_step2_int(
    base = base,
    proxy_int_tbl = int$tbl,
    proxy_keys = c("vat", "year"),
    proxy_var = "fuel_proxy",
    partial_pooling = TRUE,
    progress_every = PROGRESS_EVERY
  )
  saveRDS(list(proxy_name = int$name, muhat_dt = muhat_dt), out_path)
  out_path
})

muhat_paths <- unlist(muhat_paths)

step2_metrics <- rbindlist(lapply(muhat_paths, function(p) {
  obj <- readRDS(p)
  mu <- as.data.table(obj$muhat_dt)

  dt <- merge(
    DT0[, .(id, year, emit__, y__)],
    mu[, .(id, year, muhat_int_raw)],
    by = c("id", "year"),
    all.x = TRUE
  )
  dt[is.na(muhat_int_raw), muhat_int_raw := 0]
  dt <- dt[emit__ == 1]

  if (nrow(dt) == 0) {
    return(data.table(proxy_tag_int = obj$proxy_name, n = 0L, RMSE = NA_real_))
  }

  m <- calc_metrics(dt$y__, dt$muhat_int_raw, fp_threshold = 0)

  data.table(
    proxy_tag_int = obj$proxy_name,
    n = m[["n"]],
    RMSE = m[["rmse"]],
    nRMSE = m[["nrmse_sd"]],
    MAE = m[["mae"]],
    MAPE = m[["mape"]],
    spearman = m[["spearman"]]
  )
}), fill = TRUE)

setorder(step2_metrics, RMSE)

topk_step2 <- step2_metrics[1:min(step2_topk, .N)]
if (nrow(topk_step2) == 0) stop("No step-2 proxies available for ranking.")

saveRDS(step2_metrics, file.path(output_dir, "step2_metrics_all_from_precompute.rds"))
fwrite(step2_metrics, file.path(output_dir, "step2_metrics_all_from_precompute.csv"))
saveRDS(topk_step2, file.path(output_dir, "topk_step2_proxies.rds"))

# --------------------------------------------------------------------------
# 5) Build triple grid: (step1 proxy, threshold) x (step2 proxy)
# --------------------------------------------------------------------------
step1_pairs[, pair_id := .I]
step2_top <- unique(topk_step2[, .(proxy_tag_int)])

triples <- CJ(pair_id = step1_pairs$pair_id, proxy_tag_int = step2_top$proxy_tag_int, unique = TRUE)
triples <- merge(triples, step1_pairs, by = "pair_id", all.x = TRUE)
triples[, pair_id := NULL]

# Map proxy name -> cached path (phat/muhat)
phat_map <- new.env(parent = emptyenv())
for (p in phat_paths) {
  obj <- readRDS(p)
  assign(obj$proxy_name, p, envir = phat_map)
}

muhat_map <- new.env(parent = emptyenv())
for (p in muhat_paths) {
  obj <- readRDS(p)
  assign(obj$proxy_name, p, envir = muhat_map)
}

# --------------------------------------------------------------------------
# 6) Evaluate hurdle model on selected triples
# --------------------------------------------------------------------------
metrics_hurdle_topk <- evaluate_hurdle_triples(
  base = base,
  triples = triples,
  phat_map = phat_map,
  muhat_map = muhat_map,
  drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
  fallback_equal_split = FALLBACK_EQUAL_SPLIT,
  fp_threshold_metrics = 0,
  progress_every = PROGRESS_EVERY
)

saveRDS(metrics_hurdle_topk, file.path(output_dir, "hurdle_metrics_topk_triples.rds"))
fwrite(metrics_hurdle_topk, file.path(output_dir, "hurdle_metrics_topk_triples.csv"))

# --------------------------------------------------------------------------
# 7) Report best triple by RMSE (raw)
# --------------------------------------------------------------------------
PICK_VARIANT <- "raw"

best_tbl <- metrics_hurdle_topk[variant == PICK_VARIANT & is.finite(RMSE)]
setorder(best_tbl, RMSE)

best_combo <- best_tbl[1]
if (nrow(best_combo) > 0) {
  message(sprintf(
    "BEST HURDLE (%s): ext=%s | int=%s | thr=%.4f | RMSE=%.6f | nRMSE=%.6f",
    PICK_VARIANT,
    best_combo$proxy_tag_ext,
    best_combo$proxy_tag_int,
    best_combo$threshold_value,
    best_combo$RMSE,
    best_combo$nRMSE
  ))
  saveRDS(best_combo, file.path(output_dir, paste0("best_hurdle_combo_topk_", PICK_VARIANT, ".rds")))
}

###############################################################################
# END pipeline
###############################################################################
