###############################################################################
# _run_ppml_only.R  —  temporary runner: execution.R lines 1-186
###############################################################################

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
on.exit(stop_loocv_cluster(cl), add = TRUE)

# -----------------------
# K-fold CV: assign firms to folds
# -----------------------
fold_ids <- assign_kfold_groups(unique(df_run$vat), k = 10L, seed = 42L)
cat(sprintf("Assigned %d firms to %d folds\n", nrow(fold_ids), max(fold_ids$fold)))

# -----------------------
# Paths + outputs
# -----------------------
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# -----------------------
# 1) PPML benchmarks
# -----------------------
ppml_metrics <- list()

cat("=== PPML without proxy, sector FE ===\n")
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

cat("=== PPML without proxy, sector RE ===\n")
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

cat("=== PPML with proxy loop (sector RE) ===\n")
proxy_files <- list.files(CACHE_DIR, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
cat("Found", length(proxy_files), "proxy files\n")

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

cat("\n=== DONE — PPML section complete ===\n")
