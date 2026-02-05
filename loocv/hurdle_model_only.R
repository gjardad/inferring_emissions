###############################################################################
# (4) Parallel hurdle model over proxy pairs (FAST: precompute step1 + step2)
#     + evaluate ALL 96x96 pairs under SOFT and HARD threshold grids
#     + pick (proxy_ext, proxy_int, threshold) that minimizes RMSE
###############################################################################
log_step("[4/4] FAST hurdle: listing proxy files...")

ext_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
int_files <- list.files(proxy_cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(ext_files) == 0 || length(int_files) == 0) stop("No proxies found for hurdle in: ", proxy_cache_dir)

# --------------------------------------------------------------------------
# Helper to read proxy RDS consistently + extract proxy name used in tags
# --------------------------------------------------------------------------
read_proxy_rds_simple <- function(path) {
  obj <- readRDS(path)
  tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  name <- if (is.list(obj) && !is.null(obj$name))  obj$name  else tools::file_path_sans_ext(basename(path))
  tbl <- data.table::as.data.table(tbl)
  if ("buyer_id" %in% names(tbl) && !"vat" %in% names(tbl)) data.table::setnames(tbl, "buyer_id", "vat")
  if (!all(c("vat","year","fuel_proxy") %in% names(tbl))) stop("Proxy missing vat/year/fuel_proxy: ", path)
  list(tbl = tbl, name = name)
}

# --------------------------------------------------------------------------
# Build base ONCE
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

# base$DT is the canonical table
DT0 <- data.table::copy(base$DT)
DT0[, id := id__]
DT0[, year := year__]

# --------------------------------------------------------------------------
# Name -> path mapping (robust: uses proxy name inside each RDS)
# --------------------------------------------------------------------------
log_step("FAST hurdle: building proxy name->path maps...")

ext_name_path <- data.table::rbindlist(lapply(ext_files, function(p) {
  x <- read_proxy_rds_simple(p)
  data.table::data.table(proxy_name_ext = x$name, ext_path = p)
}), fill = TRUE)

int_name_path <- data.table::rbindlist(lapply(int_files, function(p) {
  x <- read_proxy_rds_simple(p)
  data.table::data.table(proxy_name_int = x$name, int_path = p)
}), fill = TRUE)

if (anyDuplicated(ext_name_path$proxy_name_ext)) warning("Duplicate ext proxy_name detected in name->path map.")
if (anyDuplicated(int_name_path$proxy_name_int)) warning("Duplicate int proxy_name detected in name->path map.")

# --------------------------------------------------------------------------
# Cache file name helper (stable + filesystem-safe)
# --------------------------------------------------------------------------
safe_tag <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

phat_path_for  <- function(proxy_name) file.path(HURDLE_FAST_CACHE_DIR, paste0("phat__",  safe_tag(proxy_name), ".rds"))
muhat_path_for <- function(proxy_name) file.path(HURDLE_FAST_CACHE_DIR, paste0("muhat__", safe_tag(proxy_name), ".rds"))

# --------------------------------------------------------------------------
# 4a) Precompute Step 1 phat for ALL ext proxies (once)
# --------------------------------------------------------------------------
log_step(sprintf("FAST hurdle: precomputing Step 1 (ext) over %d proxies...", length(ext_files)))

phat_paths <- future_lapply(
  seq_along(ext_files),
  function(j) {
    ext <- read_proxy_rds_simple(ext_files[[j]])
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
# 4b) Precompute Step 2 muhat for ALL int proxies (once)
# --------------------------------------------------------------------------
log_step(sprintf("FAST hurdle: precomputing Step 2 (int) over %d proxies...", length(int_files)))

muhat_paths <- future_lapply(
  seq_along(int_files),
  function(j) {
    int <- read_proxy_rds_simple(int_files[[j]])
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
# 4c) Build proxy_name -> cached file path maps (for fast evaluation)
# --------------------------------------------------------------------------
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
# 4d) Build threshold grid
#     Recommended: quantiles of *phat distribution pooled over all firms/years*
# --------------------------------------------------------------------------
log_step("FAST hurdle: building threshold grid...")

# Choose N thresholds; you can tune this.
N_THRESH <- 50L

# Use one representative phat file to build quantiles, or pool multiple.
# Pooling across all proxies is expensive; using a representative phat is fine.
# Here: use first phat as the distribution source.
ph0 <- readRDS(phat_paths[[1]])$phat_dt
ph0 <- data.table::as.data.table(ph0)
thr_grid <- unique(as.numeric(stats::quantile(ph0$phat_raw, probs = seq(0, 1, length.out = N_THRESH + 2L), na.rm = TRUE)))
thr_grid <- thr_grid[thr_grid > 0 & thr_grid < 1]   # drop 0 and 1 endpoints
thr_grid <- sort(unique(thr_grid))

log_step(sprintf("FAST hurdle: threshold grid built (N=%d). Range=[%.4f, %.4f]",
                 length(thr_grid), min(thr_grid), max(thr_grid)))

saveRDS(thr_grid, file.path(output_dir, "hurdle_threshold_grid.rds"))

# --------------------------------------------------------------------------
# Helper: evaluate SOFT + HARD(threshold grid) for one proxy pair
#   - Returns a metrics table with a 'threshold_type' and 'threshold_value'
#   - Also returns best (min RMSE) per variant if you want.
# --------------------------------------------------------------------------
evaluate_pair_threshold_grid <- function(base,
                                         phat_dt,
                                         muhat_dt,
                                         proxy_tag_ext,
                                         proxy_tag_int,
                                         thresholds,
                                         drop_singleton_cells_in_metrics = TRUE,
                                         fallback_equal_split = TRUE,
                                         fp_threshold_metrics = 0) {
  DT0 <- data.table::copy(base$DT)
  DT0[, id := id__]
  DT0[, year := year__]
  
  ph <- data.table::as.data.table(phat_dt)[, .(id, year, phat_raw)]
  mu <- data.table::as.data.table(muhat_dt)[, .(id, year, muhat_int_raw)]
  
  # Merge on (id,year)
  dt <- merge(
    DT0[, .(id, year, sector__ = sector__, y__ = y__, emit__ = emit__)],
    ph,
    by = c("id","year"),
    all.x = TRUE
  )
  dt <- merge(dt, mu, by = c("id","year"), all.x = TRUE)
  dt[is.na(phat_raw),      phat_raw      := 0]
  dt[is.na(muhat_int_raw), muhat_int_raw := 0]
  
  # Attach cell sizes N_full from base$full_cellN
  dt[, sector_key := as.character(sector__)]
  dt[, year_key   := as.integer(year)]
  
  full_cellN <- data.table::copy(base$full_cellN)  # must have sector_key, year_key, N_full
  dt <- merge(
    dt,
    full_cellN[, .(sector_key, year_key, N_full)],
    by = c("sector_key","year_key"),
    all.x = TRUE
  )
  dt[is.na(N_full), N_full := 1L]  # safe fallback
  
  # Convenience: cell-level totals for calibration, computed from base$SYT and training denom logic
  # We'll mimic evaluate_pair_fast's calibration rule but for arbitrary yhat_raw vectors.
  SYT <- data.table::copy(base$SYT)         # keyed sector_key/year_key with E_total
  full_cellN <- data.table::copy(base$full_cellN)
  
  # In the "fast" framework, denom_train is sum yhat_raw over TRAIN within fold.
  # Here, we do not have fold structure; BUT your precomputes are LOFOCV outputs:
  # phat_raw and muhat_int_raw for each (id,year) are already out-of-fold.
  # So for calibration-by-cell in deployment style, the correct analog is:
  # denom_full_cell = sum over ALL firms in cell of yhat_raw  (since each obs is already OOF)
  # This matches what evaluate_pair_fast effectively produces after combining LOFOCV pieces.
  #
  # We'll implement calibration by sector-year:
  #   yhat_cal = E_total * (yhat_raw / sum_cell_yhat_raw), with fallback equal split if denom==0.
  #
  calibrate_cell <- function(dt_in, yhat_col) {
    tmp <- data.table::copy(dt_in)
    tmp[, sector_key := as.character(sector__)]
    tmp[, year_key   := as.integer(year)]
    
    denom <- tmp[, .(denom_full = sum(get(yhat_col), na.rm = TRUE), n_full = .N),
                 by = .(sector_key, year_key)]
    denom <- merge(denom, SYT, by=c("sector_key","year_key"), all.x=TRUE)
    
    tmp <- merge(tmp, denom, by=c("sector_key","year_key"), all.x=TRUE)
    
    tmp[is.na(denom_full), denom_full := 0]
    tmp[is.na(n_full), n_full := 1L]
    
    # calibrated
    tmp[, yhat_cal := NA_real_]
    tmp[is.finite(E_total) & E_total == 0, yhat_cal := 0]
    tmp[is.finite(E_total) & E_total > 0 & denom_full > 0,
        yhat_cal := E_total * (get(yhat_col) / denom_full)]
    
    if (fallback_equal_split) {
      tmp[is.finite(E_total) & E_total > 0 &
            (is.na(yhat_cal) | !is.finite(yhat_cal)) & n_full > 0,
          yhat_cal := E_total / n_full]
    }
    
    tmp[, .(id, year, sector__, y__=y__, emit__=emit__, N_full=N_full, yhat_raw=get(yhat_col), yhat_cal=yhat_cal)]
  }
  
  # Build all prediction variants: soft + hard(t)
  out_rows <- vector("list", length(thresholds) + 1L)
  
  # SOFT
  dt[, yhat_soft := pmax(phat_raw * muhat_int_raw, 0)]
  pred_soft <- calibrate_cell(dt, "yhat_soft")
  
  # Metrics (raw + calibrated)
  Pm_raw <- pred_soft
  Pm_cal <- pred_soft
  if (isTRUE(drop_singleton_cells_in_metrics)) {
    Pm_raw <- Pm_raw[N_full > 1]
    Pm_cal <- Pm_cal[N_full > 1]
  }
  m_raw <- calc_metrics(Pm_raw$y__, Pm_raw$yhat_raw, fp_threshold = fp_threshold_metrics)
  m_cal <- calc_metrics(Pm_cal$y__, Pm_cal$yhat_cal, fp_threshold = fp_threshold_metrics)
  
  out_rows[[1]] <- data.table::rbindlist(list(
    data.table::data.table(
      variant="raw",
      threshold_type="soft",
      threshold_value=NA_real_,
      RMSE=m_raw[["rmse"]],
      nRMSE=m_raw[["nrmse_sd"]],
      spearman=m_raw[["spearman"]],
      MAPD_emitters=m_raw[["mapd_emitters"]],
      FPR_nonemitters=m_raw[["fpr_nonemitters"]],
      TPR_emitters=m_raw[["tpr_emitters"]],
      PPV_precision=m_raw[["ppv_precision"]],
      F1=m_raw[["f1"]]
    ),
    data.table::data.table(
      variant="calibrated",
      threshold_type="soft",
      threshold_value=NA_real_,
      RMSE=m_cal[["rmse"]],
      nRMSE=m_cal[["nrmse_sd"]],
      spearman=m_cal[["spearman"]],
      MAPD_emitters=m_cal[["mapd_emitters"]],
      FPR_nonemitters=m_cal[["fpr_nonemitters"]],
      TPR_emitters=m_cal[["tpr_emitters"]],
      PPV_precision=m_cal[["ppv_precision"]],
      F1=m_cal[["f1"]]
    )
  ), fill=TRUE)
  
  # HARD thresholds
  for (ii in seq_along(thresholds)) {
    t <- thresholds[[ii]]
    col <- paste0("yhat_hard_", ii)
    
    dt[, (col) := pmax(as.numeric(phat_raw > t) * muhat_int_raw, 0)]
    pred_hard <- calibrate_cell(dt, col)
    
    Pm_raw <- pred_hard
    Pm_cal <- pred_hard
    if (isTRUE(drop_singleton_cells_in_metrics)) {
      Pm_raw <- Pm_raw[N_full > 1]
      Pm_cal <- Pm_cal[N_full > 1]
    }
    
    m_raw <- calc_metrics(Pm_raw$y__, Pm_raw$yhat_raw, fp_threshold = fp_threshold_metrics)
    m_cal <- calc_metrics(Pm_cal$y__, Pm_cal$yhat_cal, fp_threshold = fp_threshold_metrics)
    
    out_rows[[ii + 1L]] <- data.table::rbindlist(list(
      data.table::data.table(
        variant="raw",
        threshold_type="hard",
        threshold_value=as.numeric(t),
        RMSE=m_raw[["rmse"]],
        nRMSE=m_raw[["nrmse_sd"]],
        spearman=m_raw[["spearman"]],
        MAPD_emitters=m_raw[["mapd_emitters"]],
        FPR_nonemitters=m_raw[["fpr_nonemitters"]],
        TPR_emitters=m_raw[["tpr_emitters"]],
        PPV_precision=m_raw[["ppv_precision"]],
        F1=m_raw[["f1"]]
      ),
      data.table::data.table(
        variant="calibrated",
        threshold_type="hard",
        threshold_value=as.numeric(t),
        RMSE=m_cal[["rmse"]],
        nRMSE=m_cal[["nrmse_sd"]],
        spearman=m_cal[["spearman"]],
        MAPD_emitters=m_cal[["mapd_emitters"]],
        FPR_nonemitters=m_cal[["fpr_nonemitters"]],
        TPR_emitters=m_cal[["tpr_emitters"]],
        PPV_precision=m_cal[["ppv_precision"]],
        F1=m_cal[["f1"]]
      )
    ), fill=TRUE)
  }
  
  M <- data.table::rbindlist(out_rows, fill=TRUE)
  M[, proxy_tag_ext := proxy_tag_ext]
  M[, proxy_tag_int := proxy_tag_int]
  M[]
}

# --------------------------------------------------------------------------
# 4e) Evaluate ALL 96 x 96 x (1 + N_THRESH) combinations
# --------------------------------------------------------------------------

pairs <- data.table::CJ(
  ext_name = ext_name_path$proxy_name_ext,
  int_name = int_name_path$proxy_name_int,
  unique = TRUE
)

# --------------------------------------------------------------------------
# Progress check: how many (ext,int) pairs already completed?
# --------------------------------------------------------------------------

PAIR_THR_CACHE_DIR <- file.path(output_dir, "pairthr_cache")

dir.create(PAIR_THR_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

safe_tag <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

pairthr_path_for <- function(ext_name, int_name) {
  file.path(PAIR_THR_CACHE_DIR,
            paste0("pairthr__", safe_tag(ext_name), "__", safe_tag(int_name), ".rds"))
}

# Expected total number of pairs
n_total_pairs <- nrow(pairs)

# Completed files on disk
pair_files_done <- list.files(
  PAIR_THR_CACHE_DIR,
  pattern = "^pairthr__.*\\.rds$",
  full.names = TRUE
)

n_done <- length(pair_files_done)
n_left <- n_total_pairs - n_done

log_step(sprintf(
  "FAST hurdle progress: %d / %d pairs completed (%.1f%%). Remaining: %d",
  n_done,
  n_total_pairs,
  100 * n_done / n_total_pairs,
  n_left
))

log_step(sprintf("FAST hurdle: evaluating ALL pairs with thresholds: %d ext x %d int = %d pairs",
                 length(ext_name_path$proxy_name_ext),
                 length(int_name_path$proxy_name_int),
                 nrow(pairs)))

metrics_list_all <- future.apply::future_lapply(
  seq_len(nrow(pairs)),
  function(k) {
    ext_name <- pairs$ext_name[[k]]
    int_name <- pairs$int_name[[k]]
    
    out_path <- pairthr_path_for(ext_name, int_name)
    
    # ---- resume: if already computed, just load and return ----
    if (file.exists(out_path)) {
      return(readRDS(out_path))
    }
    
    log_line(
      LOG_FILE_HURDLE,
      sprintf("[FAST PairThr %d/%d] ext=%s | int=%s", k, nrow(pairs), ext_name, int_name)
    )
    
    # Load cached step outputs
    phat_obj  <- readRDS(get(ext_name, envir = phat_map))
    muhat_obj <- readRDS(get(int_name, envir = muhat_map))
    
    # Compute metrics across soft + hard threshold grid
    M <- evaluate_pair_threshold_grid(
      base = base,
      phat_dt = phat_obj$phat_dt,
      muhat_dt = muhat_obj$muhat_dt,
      proxy_tag_ext = ext_name,
      proxy_tag_int = int_name,
      thresholds = thr_grid,
      drop_singleton_cells_in_metrics = DROP_SINGLETON_CELLS_IN_METRICS,
      fallback_equal_split = FALLBACK_EQUAL_SPLIT,
      fp_threshold_metrics = 0
    )
    
    # Add stable identifiers compatible with your log
    M[, model_family := "hurdle"]
    M[, partial_pooling := "yes"]
    M[, step_tag := "12"]
    M[, sample_tag := sample_tag]
    M[, proxy_tag := paste0("ext=", ext_name, "|int=", int_name)]
    M[, run_ts := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]
    
    # reorder
    id_cols <- c(
      "run_ts","model_family","partial_pooling","step_tag","sample_tag",
      "proxy_tag_ext","proxy_tag_int","proxy_tag","variant",
      "threshold_type","threshold_value"
    )
    keep <- id_cols[id_cols %in% names(M)]
    M <- M[, c(keep, setdiff(names(M), keep)), with = FALSE]
    
    # Save checkpoint (atomic-ish)
    tmp <- paste0(out_path, ".tmp")
    saveRDS(M, tmp)
    file.rename(tmp, out_path)
    
    out_path
  },
  future.seed = TRUE
)

metrics_hurdle_all <- data.table::rbindlist(lapply(expected_paths[file.exists(expected_paths)], readRDS), fill=TRUE)

# --------------------------------------------------------------------------
# 4f) Pick best triple by RMSE
#     Choose whether you pick on raw or calibrated variant.
# --------------------------------------------------------------------------
PICK_VARIANT <- "raw"
best_tbl <- metrics_hurdle_all[variant == PICK_VARIANT & is.finite(RMSE)]
data.table::setorder(best_tbl, RMSE)

best_combo <- best_tbl[1]
log_step(sprintf(
  "BEST HURDLE (%s): ext=%s | int=%s | thr=%s%s | RMSE=%.6f | nRMSE=%.6f",
  PICK_VARIANT,
  best_combo$proxy_tag_ext,
  best_combo$proxy_tag_int,
  best_combo$threshold_type,
  if (best_combo$threshold_type == "hard") paste0("(", sprintf("%.4f", best_combo$threshold_value), ")") else "",
  best_combo$RMSE,
  best_combo$nRMSE
))

saveRDS(metrics_hurdle_all, file.path(output_dir, "hurdle_metrics_all_pairs_thresholds.rds"))
data.table::fwrite(metrics_hurdle_all, file.path(output_dir, "hurdle_metrics_all_pairs_thresholds.csv"))

saveRDS(best_combo, file.path(output_dir, paste0("best_hurdle_combo_", PICK_VARIANT, ".rds")))

# Append to your global metrics log (if desired)
append_and_message(metrics_hurdle_all)

###############################################################################
# END hurdle all-pairs threshold sweep
###############################################################################
