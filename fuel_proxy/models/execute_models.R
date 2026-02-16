###############################################################################
# execution.R
#
# PURPOSE
#   Main execution script that:
#     1) loads LOOCV data, libraries, and auxiliary code
#     2) runs PPML benchmarks (FE/RE/no-proxy + proxy loop) and appends metrics
#     3) precomputes step-1 and ranks (proxy, threshold) pairs
#     4) precomputes step-2 and ranks proxies
#     5) evaluates hurdle only on topK step1 pairs x topK step2 proxies
#        and selects the best triple by RMSE
###############################################################################

# ====================
# Define paths -------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- dirname(normalizePath(sys.frame(1)$ofile, winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

# -----------------------
# Source auxiliary code
# -----------------------
source_try(UTILS_DIR, "calc_metrics")
source_try(UTILS_DIR, "build_metrics_table")
source_try(UTILS_DIR, "append_loocv_performance_metrics_log")
source_try(UTILS_DIR, "step1_metrics_threshold")

source_try(LOOCV_DIR, "ppml")
source_try(LOOCV_DIR, "hurdle_fast_prep_base_DT")
source_try(LOOCV_DIR, "hurdle_fast_step1_ext")
source_try(LOOCV_DIR, "hurdle_fast_step2_int")
source_try(LOOCV_DIR, "hurdle_fast_evaluate_triples")

# -----------------------
# Load data
# -----------------------
load(file.path(PROC_DATA, "loocv_training_sample.RData"))
library(data.table)
df_full <- as.data.table(loocv_training_sample)

sector_year_totals_full <- df_full[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

df_run <- df_full
syt_run <- sector_year_totals_full
sample_tag <- "all"

# -----------------------
# 1) PPML benchmarks
# -----------------------
ppml_metrics <- list()

# --- PPML without proxy, sector FE ---
ppml_out_fe <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  proxy_df = NULL,
  proxy_keys = c("buyer_id", "year"),
  proxy_var = "fuel_proxy",
  proxy_tag = "none",
  coalesce_proxy_to_zero = TRUE,
  partial_pooling = FALSE,
  fallback_equal_split = TRUE,
  progress_every = 50,
  drop_singleton_cells_in_metrics = TRUE
)

ppml_metrics[["ppml_woutfuelproxy_sectorFE"]] <- build_metrics_table(
  out = ppml_out_fe,
  model_family = "ppml",
  partial_pooling = "no",
  step_tag = "2",
  sample_tag = sample_tag,
  proxy_tag = "none",
  extra_id_cols = list(
    model_name = "ppml_woutfuelproxy_sectorFE",
    n_obs_est = nrow(df_run),
    n_firms_est = uniqueN(df_run$vat)
  )
)

# --- PPML without proxy, sector RE ---
ppml_out_re <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  proxy_df = NULL,
  proxy_keys = c("buyer_id", "year"),
  proxy_var = "fuel_proxy",
  proxy_tag = "none",
  coalesce_proxy_to_zero = TRUE,
  partial_pooling = TRUE,
  fallback_equal_split = TRUE,
  progress_every = 50,
  drop_singleton_cells_in_metrics = TRUE
)

ppml_metrics[["ppml_woutfuel_proxy_sectorRE"]] <- build_metrics_table(
  out = ppml_out_re,
  model_family = "ppml",
  partial_pooling = "yes",
  step_tag = "2",
  sample_tag = sample_tag,
  proxy_tag = "none",
  extra_id_cols = list(
    model_name = "ppml_woutfuel_proxy_sectorRE",
    n_obs_est = nrow(df_run),
    n_firms_est = uniqueN(df_run$vat)
  )
)

# --- PPML with proxy loop (sector RE) ---
proxy_files <- list.files(CACHE_DIR, pattern = "^proxy_.*\\.rds$", full.names = TRUE)

ppml_proxy_metrics <- lapply(proxy_files, function(proxy_file) {
  obj <- readRDS(proxy_file)
  proxy_tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(proxy_file))

  proxy_tbl <- as.data.table(proxy_tbl)
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }

  ppml_out_proxy <- poissonPP_lofo(
    df = df_run,
    sector_year_totals = syt_run,
    id_var = "vat",
    year_var = "year",
    sector_var = "nace2d",
    y_var = "emissions",
    revenue_var = "revenue",
    proxy_df = proxy_tbl,
    proxy_keys = c("vat", "year"),
    proxy_var = "fuel_proxy",
    proxy_tag = proxy_name,
    coalesce_proxy_to_zero = TRUE,
    partial_pooling = TRUE,
    fallback_equal_split = TRUE,
    progress_every = 50,
    drop_singleton_cells_in_metrics = TRUE
  )

  build_metrics_table(
    out = ppml_out_proxy,
    model_family = "ppml",
    partial_pooling = "yes",
    step_tag = "2",
    sample_tag = sample_tag,
    proxy_tag = proxy_name,
    extra_id_cols = list(
      model_name = "ppml_loop_over_proxies_sectorRE",
      n_obs_est = nrow(df_run),
      n_firms_est = uniqueN(df_run$vat)
    )
  )
})

ppml_proxy_metrics <- rbindlist(ppml_proxy_metrics, fill = TRUE)

ppml_metrics_all <- rbindlist(c(ppml_metrics, list(ppml_proxy_metrics)), fill = TRUE)

append_metrics_log(
  ppml_metrics_all,
  rds_path = METRICS_PATH_RDS,
  csv_path = METRICS_PATH_CSV,
  dedup = TRUE
)

# -----------------------
# 2) Preprocess step 1
# -----------------------
base <- prep_hurdle_base_DT(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue"
)

step1_cache_dir <- file.path(OUTPUT_DIR, "cache_step1")
if (!dir.exists(step1_cache_dir)) dir.create(step1_cache_dir, recursive = TRUE)

phat_paths <- lapply(proxy_files, function(proxy_file) {
  obj <- readRDS(proxy_file)
  proxy_tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(proxy_file))

  proxy_tbl <- as.data.table(proxy_tbl)
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }

  tag <- gsub("[^A-Za-z0-9_\\-]+", "_", proxy_name)
  out_path <- file.path(step1_cache_dir, paste0("phat__", tag, ".rds"))

  if (!file.exists(out_path)) {
    phat_dt <- precompute_step1_ext(
      base = base,
      proxy_ext_tbl = proxy_tbl,
      proxy_keys = c("vat", "year"),
      proxy_var = "fuel_proxy",
      partial_pooling = TRUE,
      progress_every = 50
    )
    saveRDS(list(proxy_name = proxy_name, phat_dt = phat_dt), out_path)
  }

  out_path
})

phat_paths <- unlist(phat_paths)

thresholds <- seq(0, 0.45, by = 0.05)
low_fpr_max <- 0.1
high_fpr_max <- 0.2
topk_per_band <- 5

step1_metrics <- step1_threshold_metrics(
  base = base,
  phat_paths = phat_paths,
  thresholds = thresholds,
  progress_every = 50
)

step1_low <- step1_metrics[
  FPR_nonemitters <= low_fpr_max
][order(-emitter_mass_captured)][1:min(topk_per_band, .N)]

step1_high <- step1_metrics[
  FPR_nonemitters > low_fpr_max & FPR_nonemitters <= high_fpr_max
][order(-emitter_mass_captured)][1:min(topk_per_band, .N)]

topk_step1 <- rbindlist(list(step1_low, step1_high), fill = TRUE)
step1_pairs <- unique(topk_step1[, .(proxy_tag_ext, threshold_value = threshold)])

saveRDS(step1_metrics, file.path(OUTPUT_DIR, "step1_metrics_threshold_all.rds"))
write.csv(step1_metrics, file.path(OUTPUT_DIR, "step1_metrics_threshold_all.csv"), row.names = FALSE)

# -----------------------
# 3) Preprocess step 2
# -----------------------
step2_cache_dir <- file.path(OUTPUT_DIR, "cache_step2")
if (!dir.exists(step2_cache_dir)) dir.create(step2_cache_dir, recursive = TRUE)

muhat_paths <- lapply(proxy_files, function(proxy_file) {
  obj <- readRDS(proxy_file)
  proxy_tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(proxy_file))

  proxy_tbl <- as.data.table(proxy_tbl)
  if ("buyer_id" %in% names(proxy_tbl) && !"vat" %in% names(proxy_tbl)) {
    setnames(proxy_tbl, "buyer_id", "vat")
  }

  tag <- gsub("[^A-Za-z0-9_\\-]+", "_", proxy_name)
  out_path <- file.path(step2_cache_dir, paste0("muhat__", tag, ".rds"))

  if (!file.exists(out_path)) {
    muhat_dt <- precompute_step2_int(
      base = base,
      proxy_int_tbl = proxy_tbl,
      proxy_keys = c("vat", "year"),
      proxy_var = "fuel_proxy",
      partial_pooling = TRUE,
      progress_every = 50
    )
    saveRDS(list(proxy_name = proxy_name, muhat_dt = muhat_dt), out_path)
  }

  out_path
})

muhat_paths <- unlist(muhat_paths)

step2_metrics <- rbindlist(lapply(muhat_paths, function(path) {
  obj <- readRDS(path)
  mu <- as.data.table(obj$muhat_dt)

  dt <- merge(
    df_run[, .(vat, year, emissions)],
    mu[, .(id, year, muhat_int_raw)],
    by.x = c("vat", "year"),
    by.y = c("id", "year"),
    all.x = TRUE
  )
  dt[is.na(muhat_int_raw), muhat_int_raw := 0]
  dt[, emit__ := as.integer(emissions > 0)]
  dt <- dt[emit__ == 1]

  if (nrow(dt) == 0) {
    return(data.table(proxy_tag_int = obj$proxy_name, n = 0L, RMSE = NA_real_))
  }

  m <- calc_metrics(dt$emissions, dt$muhat_int_raw, fp_threshold = 0)

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

topk_step2 <- step2_metrics[1:min(5, .N)]

saveRDS(step2_metrics, file.path(OUTPUT_DIR, "step2_metrics_all_from_precompute.rds"))
write.csv(step2_metrics, file.path(OUTPUT_DIR, "step2_metrics_all_from_precompute.csv"), row.names = FALSE)

# -----------------------
# 4) Evaluate hurdle on topK x topK
# -----------------------
step1_pairs[, pair_id := .I]
step2_top <- unique(topk_step2[, .(proxy_tag_int)])

triples <- CJ(pair_id = step1_pairs$pair_id, proxy_tag_int = step2_top$proxy_tag_int, unique = TRUE)
triples <- merge(triples, step1_pairs, by = "pair_id", all.x = TRUE)
triples[, pair_id := NULL]

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

metrics_hurdle_topk <- evaluate_hurdle_triples(
  base = base,
  triples = triples,
  phat_map = phat_map,
  muhat_map = muhat_map,
  drop_singleton_cells_in_metrics = TRUE,
  fallback_equal_split = TRUE,
  fp_threshold_metrics = 0,
  progress_every = 50
)

saveRDS(metrics_hurdle_topk, file.path(OUTPUT_DIR, "hurdle_metrics_topk_triples.rds"))
write.csv(metrics_hurdle_topk, file.path(OUTPUT_DIR, "hurdle_metrics_topk_triples.csv"), row.names = FALSE)

best_tbl <- metrics_hurdle_topk[variant == "raw" & is.finite(RMSE)]
setorder(best_tbl, RMSE)

best_combo <- best_tbl[1]
if (nrow(best_combo) > 0) {
  saveRDS(best_combo, file.path(OUTPUT_DIR, "best_hurdle_combo_topk_raw.rds"))
}
