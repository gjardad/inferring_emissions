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
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

# -----------------------
# Source auxiliary code
# -----------------------
source_try(UTILS_DIR, "parallel_utils")
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
# Persistent parallel cluster
# -----------------------
cl <- make_loocv_cluster()

# -----------------------
# K-fold CV: assign firms to folds
# -----------------------
fold_ids <- assign_kfold_groups(unique(df_run$vat), k = 10L, seed = 42L)
cat(sprintf("Assigned %d firms to %d folds\n", nrow(fold_ids), max(fold_ids$fold)))

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
  drop_singleton_cells_in_metrics = TRUE,
  fold_ids = fold_ids,
  cl = cl
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
  drop_singleton_cells_in_metrics = TRUE,
  fold_ids = fold_ids,
  cl = cl
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

if (length(proxy_files) == 0) {
  stop(sprintf(paste0(
    "No proxy .rds files found in CACHE_DIR:\n  %s\n",
    "The preprocessing stage likely failed to build proxies.\n",
    "Check execute_preprocessing.R output for errors in build_auxiliary_tables.R or build_proxies.R.\n",
    "Tip: run build_proxies.R standalone and look for error messages."
  ), CACHE_DIR))
}

cat(sprintf("Found %d proxy files in %s\n", length(proxy_files), CACHE_DIR))

ppml_proxy_metrics <- lapply(seq_along(proxy_files), function(j) {
  proxy_file <- proxy_files[j]
  obj <- readRDS(proxy_file)
  proxy_tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  proxy_name <- if (is.list(obj) && !is.null(obj$name)) obj$name else tools::file_path_sans_ext(basename(proxy_file))

  message(sprintf("PPML proxy %d/%d: %s", j, length(proxy_files), proxy_name))

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
    drop_singleton_cells_in_metrics = TRUE,
    fold_ids = fold_ids,
    cl = cl
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

phat_paths <- lapply(seq_along(proxy_files), function(j) {
  proxy_file <- proxy_files[j]
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
    message(sprintf("Step1 proxy %d/%d: %s", j, length(proxy_files), proxy_name))
    phat_dt <- precompute_step1_ext(
      base = base,
      proxy_ext_tbl = proxy_tbl,
      proxy_keys = c("vat", "year"),
      proxy_var = "fuel_proxy",
      partial_pooling = TRUE,
      progress_every = 50,
      fold_ids = fold_ids,
      cl = cl
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

if (nrow(step1_metrics) == 0) {
  stop(sprintf(paste0(
    "step1_threshold_metrics returned 0 rows (%d phat files were read).\n",
    "Check that phat cache files in cache_step1/ have the expected structure:\n",
    "  list(proxy_name = <string>, phat_dt = <data.table with id/year/phat_raw>)"
  ), length(phat_paths)))
}

cat(sprintf("Step1 metrics: %d rows, FPR range [%.3f, %.3f]\n",
    nrow(step1_metrics),
    min(step1_metrics$FPR_nonemitters, na.rm = TRUE),
    max(step1_metrics$FPR_nonemitters, na.rm = TRUE)))

step1_low <- head(
  step1_metrics[FPR_nonemitters <= low_fpr_max][order(-emitter_mass_captured)],
  topk_per_band
)

step1_high <- head(
  step1_metrics[FPR_nonemitters > low_fpr_max & FPR_nonemitters <= high_fpr_max][order(-emitter_mass_captured)],
  topk_per_band
)

topk_step1 <- rbindlist(list(step1_low, step1_high), fill = TRUE)

# If no pairs qualify in the specified FPR bands, fall back to the topk overall
if (nrow(topk_step1) == 0) {
  cat("WARNING: No (proxy, threshold) pairs found in FPR bands. Falling back to top-K overall.\n")
  topk_step1 <- head(
    step1_metrics[order(-emitter_mass_captured)],
    2 * topk_per_band
  )
}

step1_pairs <- unique(topk_step1[, .(proxy_tag_ext, threshold_value = threshold)])
cat(sprintf("Step1 pairs selected: %d\n", nrow(step1_pairs)))

saveRDS(step1_metrics, file.path(OUTPUT_DIR, "step1_metrics_threshold_all.rds"))
write.csv(step1_metrics, file.path(OUTPUT_DIR, "step1_metrics_threshold_all.csv"), row.names = FALSE)

# -----------------------
# 3) Preprocess step 2
# -----------------------
step2_cache_dir <- file.path(OUTPUT_DIR, "cache_step2")
if (!dir.exists(step2_cache_dir)) dir.create(step2_cache_dir, recursive = TRUE)

muhat_paths <- lapply(seq_along(proxy_files), function(j) {
  proxy_file <- proxy_files[j]
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
    message(sprintf("Step2 proxy %d/%d: %s", j, length(proxy_files), proxy_name))
    muhat_dt <- precompute_step2_int(
      base = base,
      proxy_int_tbl = proxy_tbl,
      proxy_keys = c("vat", "year"),
      proxy_var = "fuel_proxy",
      partial_pooling = TRUE,
      progress_every = 50,
      fold_ids = fold_ids,
      cl = cl
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

topk_step2 <- head(step2_metrics, 5)

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

if (nrow(metrics_hurdle_topk) > 0 && "variant" %in% names(metrics_hurdle_topk)) {
  best_tbl <- metrics_hurdle_topk[variant == "raw" & is.finite(RMSE)]
  setorder(best_tbl, RMSE)

  best_combo <- best_tbl[1]
  if (nrow(best_combo) > 0) {
    saveRDS(best_combo, file.path(OUTPUT_DIR, "best_hurdle_combo_topk_raw.rds"))
  }
} else {
  cat("WARNING: No hurdle triple metrics available. Skipping best combo selection.\n")
}

# -----------------------
# 5) LOSOCV: Leave-One-Sector-Out diagnostic for the best triple
# -----------------------
if (exists("best_combo") && nrow(best_combo) > 0) {

  losocv_ext_tag   <- best_combo$proxy_tag_ext
  losocv_int_tag   <- best_combo$proxy_tag_int
  losocv_threshold <- best_combo$threshold_value

  cat(sprintf("\n=== LOSOCV for best triple ===\n  ext = %s\n  thr = %.2f\n  int = %s\n",
              losocv_ext_tag, losocv_threshold, losocv_int_tag))

  # Load proxy tables for the best triple
  load_proxy_by_name <- function(proxy_name, proxy_files) {
    for (pf in proxy_files) {
      obj <- readRDS(pf)
      name <- if (is.list(obj) && !is.null(obj$name)) obj$name
              else tools::file_path_sans_ext(basename(pf))
      if (name == proxy_name) {
        tbl <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
        tbl <- as.data.table(tbl)
        if ("buyer_id" %in% names(tbl) && !"vat" %in% names(tbl)) {
          setnames(tbl, "buyer_id", "vat")
        }
        return(tbl)
      }
    }
    stop("Proxy not found in cache: ", proxy_name)
  }

  losocv_proxy_ext_tbl <- load_proxy_by_name(losocv_ext_tag, proxy_files)
  losocv_proxy_int_tbl <- load_proxy_by_name(losocv_int_tag, proxy_files)

  # Sector-based fold assignment
  sector_fold_result <- assign_sector_folds(
    ids     = df_run$vat,
    sectors = df_run$nace2d
  )
  losocv_fold_ids     <- sector_fold_result$fold_ids
  fold_sector_map     <- sector_fold_result$fold_sector_map

  cat(sprintf("Assigned %d firms to %d sector folds\n",
              nrow(losocv_fold_ids), nrow(fold_sector_map)))

  # Step 1: Extensive margin (logit)
  cat("Running LOSOCV Step 1 (extensive margin)...\n")
  losocv_phat_dt <- precompute_step1_ext(
    base          = base,
    proxy_ext_tbl = losocv_proxy_ext_tbl,
    proxy_keys    = c("vat", "year"),
    proxy_var     = "fuel_proxy",
    partial_pooling = TRUE,
    progress_every  = 1,
    fold_ids      = losocv_fold_ids,
    cl            = cl
  )

  # Step 2: Intensive margin (Poisson)
  cat("Running LOSOCV Step 2 (intensive margin)...\n")
  losocv_muhat_dt <- precompute_step2_int(
    base          = base,
    proxy_int_tbl = losocv_proxy_int_tbl,
    proxy_keys    = c("vat", "year"),
    proxy_var     = "fuel_proxy",
    partial_pooling = TRUE,
    progress_every  = 1,
    fold_ids      = losocv_fold_ids,
    cl            = cl
  )

  # Combine predictions
  losocv_pred <- merge(
    losocv_phat_dt[, .(id, year, sector, y_true, phat_raw)],
    losocv_muhat_dt[, .(id, year, muhat_int_raw)],
    by = c("id", "year"),
    all.x = TRUE
  )
  losocv_pred[is.na(muhat_int_raw), muhat_int_raw := 0]
  losocv_pred[, yhat_raw := pmax(as.numeric(phat_raw > losocv_threshold) * muhat_int_raw, 0)]

  # Calibrate to sector-year totals
  losocv_SYT <- copy(base$SYT)
  losocv_cellN <- copy(base$full_cellN)

  losocv_pred[, sector_key := as.character(sector)]
  losocv_pred[, year_key   := as.integer(year)]

  losocv_denom <- losocv_pred[, .(denom_full = sum(yhat_raw, na.rm = TRUE), n_full = .N),
                               by = .(sector_key, year_key)]
  losocv_denom <- merge(losocv_denom, losocv_SYT, by = c("sector_key", "year_key"), all.x = TRUE)

  losocv_pred <- merge(losocv_pred, losocv_denom, by = c("sector_key", "year_key"), all.x = TRUE)
  losocv_pred[is.na(denom_full), denom_full := 0]
  losocv_pred[is.na(n_full), n_full := 1L]

  losocv_pred <- merge(losocv_pred, losocv_cellN, by = c("sector_key", "year_key"), all.x = TRUE)
  losocv_pred[is.na(N_full), N_full := 1L]

  losocv_pred[, yhat_cal := NA_real_]
  losocv_pred[is.finite(E_total) & E_total == 0, yhat_cal := 0]
  losocv_pred[is.finite(E_total) & E_total > 0 & denom_full > 0,
       yhat_cal := E_total * (yhat_raw / denom_full)]
  losocv_pred[is.finite(E_total) & E_total > 0 &
         (is.na(yhat_cal) | !is.finite(yhat_cal)) & n_full > 0,
       yhat_cal := E_total / n_full]

  # --- Aggregate metrics (drop singleton cells) ---
  losocv_eval <- losocv_pred[N_full > 1]

  losocv_m_raw <- calc_metrics(losocv_eval$y_true, losocv_eval$yhat_raw, fp_threshold = 0)
  losocv_m_cal <- calc_metrics(losocv_eval$y_true, losocv_eval$yhat_cal, fp_threshold = 0)

  losocv_metrics_from_cm <- function(m, variant_label) {
    data.table(
      variant             = variant_label,
      proxy               = paste0("ext=", losocv_ext_tag, "|int=", losocv_int_tag),
      eval_dropped_singletons = TRUE,
      nRMSE               = m[["nrmse_sd"]],
      RMSE                = m[["rmse"]],
      MAE                 = m[["mae"]],
      MAPE                = m[["mape"]],
      MAPD_emitters       = m[["mapd_emitters"]],
      FPR_nonemitters     = m[["fpr_nonemitters"]],
      TPR_emitters        = m[["tpr_emitters"]],
      PPV_precision       = m[["ppv_precision"]],
      F1                  = m[["f1"]],
      predicted_positive_rate = m[["predicted_positive_rate"]],
      emitter_mass_captured   = m[["emitter_mass_captured"]],
      med_yhat_nonemit    = m[["p50_pred_nonemit"]],
      p90_yhat_nonemit    = m[["p90_pred_nonemit"]],
      p95_yhat_nonemit    = m[["p95_pred_nonemit"]],
      p99_yhat_nonemit    = m[["p99_pred_nonemit"]],
      mean_yhat_nonemit   = m[["mean_pred_nonemit"]],
      TP = m[["TP"]], FP = m[["FP"]], TN = m[["TN"]], FN = m[["FN"]],
      spearman            = m[["spearman"]]
    )
  }

  losocv_aggregate <- rbindlist(list(
    losocv_metrics_from_cm(losocv_m_raw, "raw"),
    losocv_metrics_from_cm(losocv_m_cal, "calibrated")
  ), fill = TRUE)

  losocv_aggregate[, model_family    := "hurdle"]
  losocv_aggregate[, partial_pooling := "yes"]
  losocv_aggregate[, step_tag        := "12"]
  losocv_aggregate[, sample_tag      := sample_tag]
  losocv_aggregate[, proxy_tag       := paste0("ext=", losocv_ext_tag, "|int=", losocv_int_tag)]
  losocv_aggregate[, proxy_tag_ext   := losocv_ext_tag]
  losocv_aggregate[, proxy_tag_int   := losocv_int_tag]
  losocv_aggregate[, model_name      := "losocv_hurdle"]
  losocv_aggregate[, threshold_value := losocv_threshold]
  losocv_aggregate[, n_obs_est       := nrow(df_run)]
  losocv_aggregate[, n_firms_est     := uniqueN(df_run$vat)]
  losocv_aggregate[, run_ts          := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]

  append_metrics_log(
    losocv_aggregate,
    rds_path = METRICS_PATH_RDS,
    csv_path = METRICS_PATH_CSV,
    dedup = TRUE
  )

  cat("\n=== LOSOCV Aggregate Metrics ===\n")
  print(losocv_aggregate[, .(variant, RMSE, nRMSE, spearman, FPR_nonemitters, TPR_emitters, F1)])

  # --- Per-sector metrics ---
  losocv_sectors <- sort(unique(losocv_pred$sector))

  losocv_per_sector_list <- lapply(losocv_sectors, function(s) {
    ps <- losocv_pred[sector == s]
    ps_eval <- ps[N_full > 1]
    if (nrow(ps_eval) == 0) ps_eval <- ps

    m_raw_s <- calc_metrics(ps_eval$y_true, ps_eval$yhat_raw, fp_threshold = 0)
    m_cal_s <- calc_metrics(ps_eval$y_true, ps_eval$yhat_cal, fp_threshold = 0)

    rbindlist(list(
      cbind(data.table(sector = s), losocv_metrics_from_cm(m_raw_s, "raw")),
      cbind(data.table(sector = s), losocv_metrics_from_cm(m_cal_s, "calibrated"))
    ), fill = TRUE)
  })

  losocv_per_sector <- rbindlist(losocv_per_sector_list, fill = TRUE)
  losocv_per_sector[, `:=`(
    model_name      = "losocv_hurdle",
    proxy_tag_ext   = losocv_ext_tag,
    proxy_tag_int   = losocv_int_tag,
    threshold_value = losocv_threshold
  )]

  cat("\n=== LOSOCV Per-Sector Metrics (raw, ordered by RMSE) ===\n")
  print(losocv_per_sector[variant == "raw"][order(RMSE),
        .(sector, n = TP + FP + TN + FN, RMSE, nRMSE, spearman, FPR_nonemitters, TPR_emitters)])

  # Save LOSOCV-specific outputs
  saveRDS(losocv_aggregate, file.path(OUTPUT_DIR, "losocv_aggregate_metrics.rds"))
  write.csv(losocv_aggregate, file.path(OUTPUT_DIR, "losocv_aggregate_metrics.csv"), row.names = FALSE)

  saveRDS(losocv_per_sector, file.path(OUTPUT_DIR, "losocv_per_sector_metrics.rds"))
  write.csv(losocv_per_sector, file.path(OUTPUT_DIR, "losocv_per_sector_metrics.csv"), row.names = FALSE)

  saveRDS(losocv_pred, file.path(OUTPUT_DIR, "losocv_predictions.rds"))

  cat(sprintf("\nLOSOCV results saved to %s\n", OUTPUT_DIR))

} else {
  cat("WARNING: No best hurdle combo available. Skipping LOSOCV.\n")
}

# -----------------------
# Cleanup
# -----------------------
stop_loocv_cluster(cl)
