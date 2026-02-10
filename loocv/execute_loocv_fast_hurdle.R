###############################################################################
# execute_loocv_fast_hurdle.R
#
# PURPOSE
#   One-shot driver to run and log:
#     (1) ppml_woutfuelproxy_sectorFE  (Step 2 only, no proxy, sector FE)
#     (2) ppml_woutfuelproxy_sectorRE  (Step 2 only, no proxy, sector RE)
#     (3) parallel PPML step-2-only with fuel proxy (over proxy_*.rds)
#     (4) parallel hurdle model (over proxy pairs ext x int) USING FAST PRECOMPUTE
#
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


# -----------------------
# 0) Small logger
# -----------------------
t_start <- Sys.time()

ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
elapsed_min <- function() round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 2)

log_step <- function(msg) {
  message(sprintf("[%s | +%s min] %s", ts(), elapsed_min(), msg))
}

# -----------------------
# 1) Paths
# -----------------------

PROJECT_DIR <- file.path(folder, "carbon_policy_networks")
PROC_DATA   <- file.path(PROJECT_DIR, "data", "processed")
output_dir  <- file.path(PROJECT_DIR, "output")

code_root <- file.path(PROJECT_DIR, "code", "inferring_emissions")
UTILS_DIR <- file.path(code_root, "utils")
LOOCV_DIR <- file.path(code_root, "loocv")

# Proxies live here per your tree
proxy_cache_dir <- file.path(code_root, "proxies", "cache")

# Metrics log
metrics_path_rds <- file.path(output_dir, "model_performance_metrics.rds")
metrics_path_csv <- file.path(output_dir, "model_performance_metrics.csv")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# log file path
LOG_FILE_HURDLE <- file.path(output_dir, "hurdle_progress.log")
LOG_FILE_PPML   <- file.path(output_dir, "ppml_progress.log")

# start fresh each run (optional)
if (file.exists(LOG_FILE_HURDLE)) file.remove(LOG_FILE_HURDLE)
if (file.exists(LOG_FILE_PPML))   file.remove(LOG_FILE_PPML)

# cache dir for fast hurdle artifacts
HURDLE_FAST_CACHE_DIR <- file.path(output_dir, "hurdle_fast_cache")
dir.create(HURDLE_FAST_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------
# 2) Run config
# -----------------------
TEST_MODE <- TRUE
TEST_FRAC <- 1
TEST_SEED <- 123

# Common options
FP_THRESHOLD <- 0
DROP_SINGLETON_CELLS_IN_METRICS <- TRUE
FALLBACK_EQUAL_SPLIT <- TRUE
PROGRESS_EVERY <- Inf

# Parallel
N_WORKERS <- max(1L, parallel::detectCores() - 1L)
FUTURE_PLAN <- "multisession"  # Windows-safe

# Optional slicing
PROXY_START <- 1
PROXY_END   <- 50

# For hurdle pair runs (cartesian product)
PAIR_START <- 1
PAIR_END   <- Inf

# -----------------------
# 3) Libraries
# -----------------------
suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(future.apply)
})

# -----------------------
# 4) Robust sourcer (handles hidden .R extensions)
# -----------------------
source_try <- function(dir, fname_no_ext) {
  p1 <- file.path(dir, fname_no_ext)
  p2 <- paste0(p1, ".R")
  if (file.exists(p1)) return(source(p1, local = FALSE))
  if (file.exists(p2)) return(source(p2, local = FALSE))
  stop("Could not find file to source: ", p1, " or ", p2)
}

# -----------------------
# 5) Source utils (single source of truth)
# -----------------------
log_step("Sourcing utils...")
source_try(UTILS_DIR, "calc_metrics")
source_try(UTILS_DIR, "build_metrics_table")
source_try(UTILS_DIR, "append_loocv_performance_metrics_log")
source_try(UTILS_DIR, "step1_metrics")
if (isTRUE(TEST_MODE)) source_try(UTILS_DIR, "make_lofo_subsample")

# -----------------------
# 6) Source model functions
# -----------------------
log_step("Sourcing model functions...")
source_try(LOOCV_DIR, "ppml")
source_try(LOOCV_DIR, "hurdle")            # keep the original hurdle model available

# Driver helpers (thin wrappers)
source_try(LOOCV_DIR, "run_one_proxy")
source_try(LOOCV_DIR, "run_one_proxy_pair")

# FAST hurdle pieces (place these files in LOOCV_DIR, or adjust path accordingly)
# If you keep them under LOOCV_DIR, just copy the four scripts there.
source_try(LOOCV_DIR, "hurdle_fast_prep_base_DT")
source_try(LOOCV_DIR, "hurdle_fast_step1_ext")
source_try(LOOCV_DIR, "hurdle_fast_step2_int")
source_try(LOOCV_DIR, "hurdle_fast_evaluate_pair")

# -----------------------
# 7) Load data once + build sector-year totals once
# -----------------------
log_step("Loading LOOCV training sample...")
load(file.path(PROC_DATA, "loocv_training_sample.RData"))
df_full <- as.data.table(loocv_training_sample)

sector_year_totals_full <- df_full[
  , .(E_total = sum(emissions, na.rm = TRUE)),
  by = .(nace2d, year)
]

df_run  <- df_full
syt_run <- sector_year_totals_full
sample_tag <- "all"

if (isTRUE(TEST_MODE)) {
  log_step(sprintf("TEST_MODE=TRUE: subsampling frac=%.2f seed=%d", TEST_FRAC, TEST_SEED))
  sub <- make_lofo_subsample(df = df_full, frac = TEST_FRAC, seed = TEST_SEED)
  df_run  <- as.data.table(sub$df_sub)
  syt_run <- as.data.table(sub$sector_year_totals)
  sample_tag <- "subsample"
}

log_step(sprintf("Data ready: sample_tag=%s | Nobs=%d | Nfirms=%d",
                 sample_tag, nrow(df_run), uniqueN(df_run$vat)))

# -----------------------
# 8) Append helper
# -----------------------
append_and_message <- function(metrics_tbl) {
  metrics_all <- append_metrics_log(
    as.data.table(metrics_tbl),
    rds_path = metrics_path_rds,
    csv_path = metrics_path_csv,
    dedup = TRUE
  )
  log_step(sprintf("Appended %d rows. Log now has %d rows.", nrow(metrics_tbl), nrow(metrics_all)))
  invisible(metrics_all)
}

# --------------------
# Log helper
# --------------------
ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

log_line <- function(path, ...) {
  msg <- paste0("[", ts(), "] ", paste0(..., collapse = ""))
  if (!requireNamespace("filelock", quietly = TRUE)) {
    cat(msg, "\n", file = path, append = TRUE)
    return(invisible(NULL))
  }
  lock_path <- paste0(path, ".lock")
  lck <- filelock::lock(lock_path, timeout = 60000)
  on.exit(filelock::unlock(lck), add = TRUE)
  cat(msg, "\n", file = path, append = TRUE)
  invisible(NULL)
}

###############################################################################
# (1) PPML benchmark | sector FE | no proxy
###############################################################################
log_step("[1/4] Running PPML benchmark | sector FE | no proxy")

out_ppml_fe <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  proxy_df = NULL,
  proxy_keys = c("vat","year"),
  proxy_var = "fuel_proxy",
  proxy_tag = "none",
  coalesce_proxy_to_zero = TRUE,
  partial_pooling = FALSE,
  fallback_equal_split = FALLBACK_EQUAL_SPLIT,
  drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
  fp_threshold = FP_THRESHOLD,
  progress_every = PROGRESS_EVERY
)

metrics_ppml_fe <- build_metrics_table(
  out             = out_ppml_fe,
  model_family    = "ppml",
  partial_pooling = "no",
  step_tag        = "2",
  sample_tag      = sample_tag,
  proxy_tag       = "none",
  extra_id_cols   = list(
    proxy_tag_ext = "",
    proxy_tag_int = "none",
    model_name   = "ppml_woutfuelproxy_sectorFE",
    n_obs_est    = nrow(df_run),
    n_firms_est  = uniqueN(df_run$vat)
  )
)

append_and_message(metrics_ppml_fe)

###############################################################################
# (2) PPML benchmark | sector RE | no proxy
###############################################################################
log_step("[2/4] Running PPML benchmark | sector RE | no proxy")

out_ppml_re <- poissonPP_lofo(
  df = df_run,
  sector_year_totals = syt_run,
  id_var = "vat",
  year_var = "year",
  sector_var = "nace2d",
  y_var = "emissions",
  revenue_var = "revenue",
  proxy_df = NULL,
  proxy_keys = c("vat","year"),
  proxy_var = "fuel_proxy",
  proxy_tag = "none",
  coalesce_proxy_to_zero = TRUE,
  partial_pooling = TRUE,
  fallback_equal_split = FALLBACK_EQUAL_SPLIT,
  drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
  fp_threshold = FP_THRESHOLD,
  progress_every = PROGRESS_EVERY
)

metrics_ppml_re <- build_metrics_table(
  out             = out_ppml_re,
  model_family    = "ppml",
  partial_pooling = "yes",
  step_tag        = "2",
  sample_tag      = sample_tag,
  proxy_tag       = "none",
  extra_id_cols   = list(
    proxy_tag_ext = "",
    proxy_tag_int = "none",
    model_name   = "ppml_woutfuelproxy_sectorRE",
    n_obs_est    = nrow(df_run),
    n_firms_est  = uniqueN(df_run$vat)
  )
)

append_and_message(metrics_ppml_re)

###############################################################################
# (3) Parallel PPML step-2 with proxy over proxy_*.rds
###############################################################################
log_step("[3/4] Parallel PPML step-2 w/ proxy: listing proxy files...")

if (!dir.exists(proxy_cache_dir)) stop("proxy_cache_dir does not exist: ", proxy_cache_dir)
proxy_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(proxy_files) == 0) stop("No proxy_*.rds files found in: ", proxy_cache_dir)

proxy_files <- proxy_files[seq.int(PROXY_START, min(length(proxy_files), PROXY_END))]
log_step(sprintf("PPML proxy runs: %d proxy files", length(proxy_files)))

ppml_proxy_runner <- function(df_run, syt_run, proxy_tbl, proxy_name,
                              id_var, year_var, sector_var, y_var, revenue_var,
                              proxy_keys, proxy_var, coalesce_proxy_to_zero,
                              partial_pooling, step_tag, sample_tag, model_name, ...) {

  out <- poissonPP_lofo(
    df = df_run,
    sector_year_totals = syt_run,
    id_var = id_var,
    year_var = year_var,
    sector_var = sector_var,
    y_var = y_var,
    revenue_var = revenue_var,
    proxy_df   = proxy_tbl,
    proxy_keys = proxy_keys,
    proxy_var  = proxy_var,
    proxy_tag  = proxy_name,
    coalesce_proxy_to_zero = coalesce_proxy_to_zero,
    partial_pooling = partial_pooling,
    fallback_equal_split = FALLBACK_EQUAL_SPLIT,
    drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
    fp_threshold = FP_THRESHOLD,
    progress_every = PROGRESS_EVERY
  )

  metrics <- build_metrics_table(
    out             = out,
    model_family    = "ppml",
    partial_pooling = if (isTRUE(partial_pooling)) "yes" else "no",
    step_tag        = step_tag,
    sample_tag      = sample_tag,
    proxy_tag       = proxy_name,
    extra_id_cols   = list(
      proxy_tag_ext = "",
      proxy_tag_int = proxy_name,
      model_name   = model_name,
      n_obs_est    = nrow(df_run),
      n_firms_est  = uniqueN(df_run[[id_var]])
    )
    
  )

  list(metrics = metrics, out = out)
}

future::plan(strategy = FUTURE_PLAN, workers = N_WORKERS)

log_step(sprintf("Starting parallel PPML proxy runs (workers=%d, plan=%s)", N_WORKERS, FUTURE_PLAN))

metrics_list_ppml_proxy <- future_lapply(
  seq_along(proxy_files),
  function(j) {
    pf <- proxy_files[[j]]

    log_line(
      LOG_FILE_PPML,
      sprintf("[PPML proxy %d/%d] %s", j, length(proxy_files), basename(pf))
    )

    run_one_proxy(
      proxy_file = pf,
      df_run = df_run,
      syt_run = syt_run,
      model_runner = ppml_proxy_runner,
      model_name = "ppml_step2_proxy",
      step_tag = "2",
      sample_tag = sample_tag,
      partial_pooling = TRUE,
      id_var = "vat",
      year_var = "year",
      sector_var = "nace2d",
      y_var = "emissions",
      revenue_var = "revenue",
      proxy_keys = c("vat","year"),
      proxy_var = "fuel_proxy",
      coalesce_proxy_to_zero = TRUE
    )
  },
  future.seed = TRUE
)

metrics_ppml_proxy <- rbindlist(metrics_list_ppml_proxy, fill = TRUE)
append_and_message(metrics_ppml_proxy)
###############################################################################
# (4) Parallel hurdle model over proxy pairs (FAST: precompute step1 + step2)
#     + compute step-only metrics
#     + restrict pair evaluation to TOP-K x TOP-K (based on step-only rankings)
###############################################################################
log_step("[4/4] FAST hurdle: listing proxy files...")

ext_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
int_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)

if (length(ext_files) == 0 || length(int_files) == 0) stop("No proxies found for hurdle in: ", proxy_cache_dir)

# --------------------------------------------------------------------------
# Helper to read proxy RDS consistently + extract the proxy name used in tags
# --------------------------------------------------------------------------
read_proxy_rds_simple <- function(path) {
  obj <- readRDS(path)
  tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  name <- if (is.list(obj) && !is.null(obj$name))  obj$name  else tools::file_path_sans_ext(basename(path))
  tbl <- as.data.table(tbl)
  if ("buyer_id" %in% names(tbl) && !"vat" %in% names(tbl)) setnames(tbl, "buyer_id", "vat")
  if (!all(c("vat","year","fuel_proxy") %in% names(tbl))) stop("Proxy missing vat/year/fuel_proxy: ", path)
  list(tbl = tbl, name = name)
}

# --------------------------------------------------------------------------
# Build base ONCE (used by evaluation + as template for step precomputes)
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

# --------------------------------------------------------------------------
# IMPORTANT: precompute loops over ALL proxies (not sliced pairs)
# --------------------------------------------------------------------------
ext_unique_paths <- ext_files
int_unique_paths <- int_files

log_step(sprintf("FAST hurdle: will precompute on ALL proxies: ext=%d | int=%d",
                 length(ext_unique_paths), length(int_unique_paths)))

# --------------------------------------------------------------------------
# Name -> path mapping (robust: uses the proxy name stored inside each RDS)
# --------------------------------------------------------------------------
log_step("FAST hurdle: building proxy name->path maps...")
ext_name_path <- rbindlist(lapply(ext_files, function(p) {
  x <- read_proxy_rds_simple(p)
  data.table(proxy_name = x$name, proxy_path = p)
}), fill = TRUE)

int_name_path <- rbindlist(lapply(int_files, function(p) {
  x <- read_proxy_rds_simple(p)
  data.table(proxy_name = x$name, proxy_path = p)
}), fill = TRUE)

# (Optional sanity: ensure unique mapping)
if (anyDuplicated(ext_name_path$proxy_name)) warning("Duplicate ext proxy_name detected in name->path map.")
if (anyDuplicated(int_name_path$proxy_name)) warning("Duplicate int proxy_name detected in name->path map.")

# --------------------------------------------------------------------------
# Cache file name helper (stable + filesystem-safe)
# --------------------------------------------------------------------------
safe_tag <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

phat_path_for  <- function(proxy_name) file.path(HURDLE_FAST_CACHE_DIR, paste0("phat__",  safe_tag(proxy_name), ".rds"))
muhat_path_for <- function(proxy_name) file.path(HURDLE_FAST_CACHE_DIR, paste0("muhat__", safe_tag(proxy_name), ".rds"))

###############################################################################
# 4a) Precompute Step 1 for EACH ext proxy (ALL proxies)
###############################################################################
log_step(sprintf("FAST hurdle: precomputing Step 1 (ext) over %d proxies...", length(ext_unique_paths)))

phat_paths <- future_lapply(
  seq_along(ext_unique_paths),
  function(j) {
    ext <- read_proxy_rds_simple(ext_unique_paths[[j]])
    out_path <- phat_path_for(ext$name)
    
    if (file.exists(out_path)) {
      log_line(LOG_FILE_HURDLE, sprintf("[FAST Step1 cached] ext=%s", ext$name))
      return(out_path)
    }
    
    log_line(LOG_FILE_HURDLE, sprintf("[FAST Step1 start] ext=%s", ext$name))
    phat_dt <- precompute_step1_ext(
      base = base,
      proxy_ext_tbl = ext$tbl,
      proxy_keys = c("vat","year"),
      proxy_var = "fuel_proxy",
      partial_pooling = TRUE,
      progress_every = PROGRESS_EVERY
    )
    saveRDS(list(proxy_name = ext$name, phat_dt = phat_dt), out_path)
    log_line(LOG_FILE_HURDLE, sprintf("[FAST Step1 done] ext=%s", ext$name))
    out_path
  },
  future.seed = TRUE
)

# --------------------------------------------------------------------------
# Step-1-only metrics + ranking (choose top-K ext proxies)
# --------------------------------------------------------------------------
log_step("FAST hurdle: computing Step-1-only metrics and ranking ext proxies...")

K <- 10L  # <-- set your desired top-K here

step1_out <- step1_metrics(
  base = base,
  phat_paths = phat_paths,
  fpr_targets = c(0.01, 0.05, 0.10),
  rank_by = "tpr_at_fpr",
  rank_fpr_target = 0.05,     # rank by TPR when FPR is constrained to 5%
  compute_best_f1 = FALSE,    # set TRUE if you want it
  top_k = K,                  # your chosen K
  progress_every = 50
)

step1_metrics <- step1_out$metrics
best_step1    <- step1_out$best
top1_names    <- step1_out$topk$proxy_tag_ext

log_step(sprintf(
  "BEST Step 1 proxy: %s | TPR@FPR=5%%: %.4f | AUC: %.4f",
  best_step1$proxy_tag_ext,
  best_step1[[paste0("tpr_at_fpr", 0.05)]],
  best_step1$AUC
))

saveRDS(step1_metrics, file.path(output_dir, "step1_metrics_all_from_precompute.rds"))
fwrite(step1_metrics, file.path(output_dir, "step1_metrics_all_from_precompute.csv"))
saveRDS(best_step1,    file.path(output_dir, "best_step1_proxy_from_precompute.rds"))

###############################################################################
# 4b) Precompute Step 2 for EACH int proxy (ALL proxies)
###############################################################################
log_step(sprintf("FAST hurdle: precomputing Step 2 (int) over %d proxies...", length(int_unique_paths)))

muhat_paths <- future_lapply(
  seq_along(int_unique_paths),
  function(j) {
    int <- read_proxy_rds_simple(int_unique_paths[[j]])
    out_path <- muhat_path_for(int$name)
    
    if (file.exists(out_path)) {
      log_line(LOG_FILE_HURDLE, sprintf("[FAST Step2 cached] int=%s", int$name))
      return(out_path)
    }
    
    log_line(LOG_FILE_HURDLE, sprintf("[FAST Step2 start] int=%s", int$name))
    muhat_dt <- precompute_step2_int(
      base = base,
      proxy_int_tbl = int$tbl,
      proxy_keys = c("vat","year"),
      proxy_var = "fuel_proxy",
      partial_pooling = TRUE,
      progress_every = PROGRESS_EVERY
    )
    saveRDS(list(proxy_name = int$name, muhat_dt = muhat_dt), out_path)
    log_line(LOG_FILE_HURDLE, sprintf("[FAST Step2 done] int=%s", int$name))
    out_path
  },
  future.seed = TRUE
)

# --------------------------------------------------------------------------
# Step-2-only metrics + ranking (choose top-K int proxies)
# --------------------------------------------------------------------------
log_step("FAST hurdle: computing Step-2-only metrics and ranking int proxies...")

step2_metrics <- rbindlist(lapply(muhat_paths, function(p) {
  obj <- readRDS(p)                   # list(proxy_name, muhat_dt)
  mu  <- as.data.table(obj$muhat_dt)  # expects id, year, muhat_int_raw
  
  dt <- merge(
    DT0[, .(id, year, emit__, y__)],
    mu[,   .(id, year, muhat_int_raw)],
    by = c("id","year"),
    all.x = TRUE
  )
  dt[is.na(muhat_int_raw), muhat_int_raw := 0]
  dt <- dt[emit__ == 1]
  
  if (nrow(dt) == 0) {
    return(data.table(proxy_tag_int = obj$proxy_name, n = 0L, RMSE = NA_real_, score = NA_real_))
  }
  
  m <- calc_metrics(dt$y__, dt$muhat_int_raw, fp_threshold = 0)
  
  data.table(
    proxy_tag_int = obj$proxy_name,
    n             = m[["n"]],
    RMSE          = m[["rmse"]],
    nRMSE         = m[["nrmse_sd"]],
    MAE           = m[["mae"]],
    MAPE          = m[["mape"]],
    spearman      = m[["spearman"]]
  )
}), fill = TRUE)

step2_metrics[, score := -RMSE]
setorder(step2_metrics, -score)

best_step2 <- step2_metrics[1]
log_step(sprintf("BEST Step 2 proxy: %s | RMSE=%.6f",
                 best_step2$proxy_tag_int, best_step2$RMSE))

saveRDS(step2_metrics, file.path(output_dir, "step2_metrics_all_from_precompute.rds"))
fwrite(step2_metrics, file.path(output_dir, "step2_metrics_all_from_precompute.csv"))
saveRDS(best_step2,    file.path(output_dir, "best_step2_proxy_from_precompute.rds"))

###############################################################################
# 4c) Restrict pair evaluation to TOP-K x TOP-K (based on step-only metrics)
###############################################################################

top1_names <- step1_metrics[1:min(K, .N), proxy_tag_ext]
top2_names <- step2_metrics[1:min(K, .N), proxy_tag_int]

top1_paths <- ext_name_path[proxy_name %in% top1_names, proxy_path]
top2_paths <- int_name_path[proxy_name %in% top2_names, proxy_path]

if (length(top1_paths) == 0) stop("Top-K ext proxies produced 0 paths. Check ext_name_path mapping.")
if (length(top2_paths) == 0) stop("Top-K int proxies produced 0 paths. Check int_name_path mapping.")

pairs <- CJ(ext_path = top1_paths, int_path = top2_paths, unique = TRUE)

log_step(sprintf("FAST hurdle: restricting to TOP-K grid: K1=%d, K2=%d => %d pairs",
                 length(top1_paths), length(top2_paths), nrow(pairs)))

# If you still want optional slicing AFTER restriction (rarely needed):
# pairs <- pairs[seq.int(PAIR_START, min(nrow(pairs), PAIR_END))]

###############################################################################
# 4d) Build proxy name -> cached file path maps (for fast pair evaluation)
###############################################################################
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

###############################################################################
# 4e) Evaluate TOP-K x TOP-K pairs (cheap): step1+2 (raw) and step1+2+3 (cal)
###############################################################################
log_step(sprintf("FAST hurdle: evaluating %d TOP-K pairs...", nrow(pairs)))

metrics_list_hurdle_fast <- future_lapply(
  seq_len(nrow(pairs)),
  function(k) {
    ext <- read_proxy_rds_simple(pairs$ext_path[k])
    int <- read_proxy_rds_simple(pairs$int_path[k])
    
    log_line(
      LOG_FILE_HURDLE,
      sprintf("[FAST Pair %d/%d] ext=%s | int=%s", k, nrow(pairs), ext$name, int$name)
    )
    
    phat_obj  <- readRDS(get(ext$name, envir = phat_map))
    muhat_obj <- readRDS(get(int$name, envir = muhat_map))
    
    out_fast <- evaluate_pair_fast(
      base = base,
      phat_dt = phat_obj$phat_dt,
      muhat_dt = muhat_obj$muhat_dt,
      proxy_tag_ext = ext$name,
      proxy_tag_int = int$name,
      fp_threshold = FP_THRESHOLD,
      drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
      fallback_equal_split = FALLBACK_EQUAL_SPLIT
    )
    
    combo_proxy <- paste0("ext=", ext$name, "|int=", int$name)
    
    out_like <- list(
      predictions = out_fast$predictions,
      metrics     = out_fast$metrics,   # contains both "raw" and "calibrated"
      proxy_ext   = ext$name,
      proxy_int   = int$name,
      include_proxy_ext = TRUE,
      include_proxy_int = TRUE,
      partial_pooling   = TRUE
    )
    
    build_metrics_table(
      out             = out_like,
      model_family    = "hurdle",
      partial_pooling = "yes",
      step_tag        = "12",           # raw=step1+2; calibrated=step1+2+3 via variant
      sample_tag      = sample_tag,
      proxy_tag       = combo_proxy,
      extra_id_cols   = list(
        proxy_tag_ext = ext$name,
        proxy_tag_int = int$name,
        n_obs_est     = nrow(df_run),
        n_firms_est   = uniqueN(df_run$vat),
        fast_hurdle   = "yes",
        topk_K        = as.integer(K)
      )
    )
  },
  future.seed = TRUE
)

metrics_hurdle_fast <- rbindlist(metrics_list_hurdle_fast, fill = TRUE)
append_and_message(metrics_hurdle_fast)

plan(sequential)
log_step("ALL DONE (FAST HURDLE).")
