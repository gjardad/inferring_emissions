###############################################################################
# sweep_best_proxies_step1_step2.R
#
# PURPOSE
#   Find the "best" proxy for:
#     - Step 1 (extensive margin): Pr(emit=1 | X, proxy)
#     - Step 2 (intensive margin on emitters): E[y | emit=1, X, proxy]
#   using FULL-sample LOFOCV for each proxy, and store performance metrics.
#
# DESIGN
#   - Step 1:
#       * For each proxy, run LOFOCV Step-1 model only, obtain phat_raw for all obs
#       * Evaluate against emit indicator using calc_metrics(y_true=emit, yhat=phat)
#       * Choose best proxy by a user-chosen objective (default: maximize TPR/FPR)
#
#   - Step 2:
#       * For each proxy, run LOFOCV Step-2 model only, obtain muhat_int_raw for all obs
#       * Evaluate ONLY on emitters: calc_metrics(y_true=y, yhat=muhat) among emit==1
#       * Choose best proxy by minimizing RMSE (default)
#
# INPUTS
#   - df_run: full training data (firm-year)
#   - syt_run: sector-year totals (only used by prep_hurdle_base_DT; step sweeps don't calibrate)
#   - proxy_files: list of proxy_*.rds files
#   - HURDLE_FAST_CACHE_DIR: where to cache per-proxy step predictions
#
# OUTPUTS
#   - Writes:
#       step1_metrics_all.rds / .csv
#       step2_metrics_all.rds / .csv
#       best_step1_proxy.rds
#       best_step2_proxy.rds
#   - Returns a list with metrics tables + the selected best proxies.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(future.apply)
})

# -----------------------------
# Helpers
# -----------------------------

read_proxy_rds_simple <- function(path) {
  obj <- readRDS(path)
  tbl  <- if (is.list(obj) && !is.null(obj$proxy)) obj$proxy else obj
  name <- if (is.list(obj) && !is.null(obj$name))  obj$name  else tools::file_path_sans_ext(basename(path))
  tbl <- as.data.table(tbl)
  if ("buyer_id" %in% names(tbl) && !"vat" %in% names(tbl)) setnames(tbl, "buyer_id", "vat")
  if (!all(c("vat","year","fuel_proxy") %in% names(tbl))) stop("Proxy missing vat/year/fuel_proxy: ", path)
  list(tbl = tbl, name = name, path = path)
}

safe_tag <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

# objective for step1 selection
# default: maximize TPR/(FPR+eps); you can swap for other scoring rules
score_step1 <- function(metrics_row, eps = 1e-9) {
  tpr <- as.numeric(metrics_row[["TPR_emitters"]])
  fpr <- as.numeric(metrics_row[["FPR_nonemitters"]])
  if (!is.finite(tpr) || !is.finite(fpr)) return(NA_real_)
  tpr / (fpr + eps)
}

# objective for step2 selection (default: minimize RMSE)
score_step2 <- function(metrics_row) {
  rmse <- as.numeric(metrics_row[["RMSE"]])
  if (!is.finite(rmse)) return(NA_real_)
  -rmse  # maximize negative RMSE <=> minimize RMSE
}

# -----------------------------
# Step 1 sweep (LOFOCV, full sample)
# -----------------------------
sweep_step1_all_proxies <- function(base,
                                    proxy_files,
                                    cache_dir,
                                    fp_threshold = 0.5,
                                    partial_pooling = TRUE,
                                    progress_every = 50,
                                    parallel = TRUE) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  phat_path_for <- function(proxy_name) {
    file.path(cache_dir, paste0("step1_phat__", safe_tag(proxy_name), ".rds"))
  }
  
  runner_one <- function(pf) {
    pr <- read_proxy_rds_simple(pf)
    out_path <- phat_path_for(pr$name)
    
    # cache hit
    if (file.exists(out_path)) {
      obj <- readRDS(out_path)
      phat_dt <- obj$phat_dt
    } else {
      phat_dt <- precompute_step1_ext(
        base = base,
        proxy_ext_tbl = pr$tbl,
        proxy_keys = c("vat","year"),
        proxy_var = "fuel_proxy",
        partial_pooling = partial_pooling,
        progress_every = progress_every
      )
      saveRDS(list(proxy_name = pr$name, proxy_path = pr$path, phat_dt = phat_dt), out_path)
    }
    
    # Evaluate step 1 as classification on emit indicator
    # base has y__ and emit__ already; phat_dt should have id/year + phat_raw
    # We merge to get y_true and emit__
    dt <- merge(
      base[, .(id, year, emit__, y__)],
      phat_dt[, .(id, year, phat_raw)],
      by = c("id","year"),
      all.x = TRUE
    )
    
    # If any missing predictions (shouldn't happen), coalesce to 0
    dt[is.na(phat_raw), phat_raw := 0]
    
    # Use calc_metrics with y_true=emit (0/1) and yhat=phat probability
    m <- calc_metrics(dt$emit__, dt$phat_raw, fp_threshold = fp_threshold)
    
    # standardized row
    out <- data.table(
      proxy_tag_ext = pr$name,
      proxy_path    = pr$path,
      fp_threshold  = fp_threshold,
      
      n            = m[["n"]],
      RMSE         = m[["rmse"]],
      nRMSE        = m[["nrmse_sd"]],
      MAE          = m[["mae"]],
      MAPE         = m[["mape"]],
      spearman     = m[["spearman"]],
      
      FPR_nonemitters = m[["fpr_nonemitters"]],
      TPR_emitters    = m[["tpr_emitters"]],
      PPV_precision   = m[["ppv_precision"]],
      F1              = m[["f1"]],
      
      predicted_positive_rate = m[["predicted_positive_rate"]],
      emitter_mass_captured   = m[["emitter_mass_captured"]]
    )
    
    out[, step := "1"]
    out[]
  }
  
  if (parallel) {
    out_list <- future_lapply(proxy_files, runner_one, future.seed = TRUE)
  } else {
    out_list <- lapply(proxy_files, runner_one)
  }
  
  rbindlist(out_list, fill = TRUE)
}

# -----------------------------
# Step 2 sweep (LOFOCV, emitters only)
# -----------------------------
sweep_step2_all_proxies <- function(base,
                                    proxy_files,
                                    cache_dir,
                                    fp_threshold = 0, # not very relevant for step2 on emitters, but kept for calc_metrics signature
                                    partial_pooling = TRUE,
                                    progress_every = 50,
                                    parallel = TRUE) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  muhat_path_for <- function(proxy_name) {
    file.path(cache_dir, paste0("step2_muhat__", safe_tag(proxy_name), ".rds"))
  }
  
  runner_one <- function(pf) {
    pr <- read_proxy_rds_simple(pf)
    out_path <- muhat_path_for(pr$name)
    
    # cache hit
    if (file.exists(out_path)) {
      obj <- readRDS(out_path)
      muhat_dt <- obj$muhat_dt
    } else {
      muhat_dt <- precompute_step2_int(
        base = base,
        proxy_int_tbl = pr$tbl,
        proxy_keys = c("vat","year"),
        proxy_var = "fuel_proxy",
        partial_pooling = partial_pooling,
        progress_every = progress_every
      )
      saveRDS(list(proxy_name = pr$name, proxy_path = pr$path, muhat_dt = muhat_dt), out_path)
    }
    
    # Evaluate ONLY on emitters
    dt <- merge(
      base[, .(id, year, emit__, y__)],
      muhat_dt[, .(id, year, muhat_int_raw)],
      by = c("id","year"),
      all.x = TRUE
    )
    
    dt[is.na(muhat_int_raw), muhat_int_raw := 0]
    dt_emit <- dt[emit__ == 1]
    
    if (nrow(dt_emit) == 0) {
      # no emitters -> degenerate; return NA metrics
      out <- data.table(
        proxy_tag_int = pr$name,
        proxy_path    = pr$path,
        n             = 0L,
        RMSE          = NA_real_,
        nRMSE         = NA_real_,
        MAE           = NA_real_,
        MAPE          = NA_real_,
        spearman      = NA_real_
      )
    } else {
      m <- calc_metrics(dt_emit$y__, dt_emit$muhat_int_raw, fp_threshold = fp_threshold)
      
      out <- data.table(
        proxy_tag_int = pr$name,
        proxy_path    = pr$path,
        n            = m[["n"]],
        RMSE         = m[["rmse"]],
        nRMSE        = m[["nrmse_sd"]],
        MAE          = m[["mae"]],
        MAPE         = m[["mape"]],
        spearman     = m[["spearman"]]
      )
    }
    
    out[, step := "2"]
    out[]
  }
  
  if (parallel) {
    out_list <- future_lapply(proxy_files, runner_one, future.seed = TRUE)
  } else {
    out_list <- lapply(proxy_files, runner_one)
  }
  
  rbindlist(out_list, fill = TRUE)
}

# -----------------------------
# Main orchestrator
# -----------------------------
find_best_proxies_step1_step2 <- function(df_run,
                                          syt_run,
                                          proxy_files,
                                          out_dir,
                                          cache_dir,
                                          # selection knobs
                                          fp_threshold_step1 = 0.5,
                                          step1_score_fun = score_step1,
                                          step2_score_fun = score_step2,
                                          # model knobs
                                          partial_pooling = TRUE,
                                          progress_every = 50,
                                          parallel = TRUE,
                                          future_plan = "multisession",
                                          n_workers = max(1L, parallel::detectCores() - 1L)) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  # plan parallel
  if (parallel) {
    future::plan(strategy = future_plan, workers = n_workers)
  } else {
    future::plan(sequential)
  }
  
  # Build base once
  base <- prep_hurdle_base_DT(
    df = df_run,
    sector_year_totals = syt_run,
    id_var = "vat",
    year_var = "year",
    sector_var = "nace2d",
    y_var = "emissions",
    revenue_var = "revenue"
  )
  
  # Step 1 sweep
  step1_cache <- file.path(cache_dir, "step1")
  m1 <- sweep_step1_all_proxies(
    base = base,
    proxy_files = proxy_files,
    cache_dir = step1_cache,
    fp_threshold = fp_threshold_step1,
    partial_pooling = partial_pooling,
    progress_every = progress_every,
    parallel = parallel
  )
  m1[, score := vapply(seq_len(.N), function(i) step1_score_fun(m1[i]), numeric(1))]
  setorder(m1, -score)
  
  best1 <- m1[1]
  
  saveRDS(m1, file.path(out_dir, "step1_metrics_all.rds"))
  fwrite(m1, file.path(out_dir, "step1_metrics_all.csv"))
  saveRDS(best1, file.path(out_dir, "best_step1_proxy.rds"))
  
  # Step 2 sweep
  step2_cache <- file.path(cache_dir, "step2")
  m2 <- sweep_step2_all_proxies(
    base = base,
    proxy_files = proxy_files,
    cache_dir = step2_cache,
    partial_pooling = partial_pooling,
    progress_every = progress_every,
    parallel = parallel
  )
  m2[, score := vapply(seq_len(.N), function(i) step2_score_fun(m2[i]), numeric(1))]
  setorder(m2, -score)
  
  best2 <- m2[1]
  
  saveRDS(m2, file.path(out_dir, "step2_metrics_all.rds"))
  fwrite(m2, file.path(out_dir, "step2_metrics_all.csv"))
  saveRDS(best2, file.path(out_dir, "best_step2_proxy.rds"))
  
  future::plan(sequential)
  
  list(
    step1_metrics = m1,
    step2_metrics = m2,
    best_step1 = best1,
    best_step2 = best2
  )
}
