###############################################################################
# step1_metrics.R
#
# PURPOSE
#   Compute **Step-1-only** (extensive margin) performance metrics for a set of
#   precomputed Step-1 predictions `phat_dt` (one per proxy), and rank proxies.
#
#   This is designed to plug into the FAST hurdle workflow where you:
#     1) build `base <- prep_hurdle_base_DT(...)`
#     2) precompute Step 1 for many proxies, saving each to disk as:
#          list(proxy_name = <name>, phat_dt = <data.table>)
#     3) call this function on the resulting `phat_paths` to:
#          - compute threshold-free AUC (ranking quality)
#          - compute TPR at controlled FPR targets (e.g., 1%, 5%, 10%)
#          - optionally compute best-F1 over a threshold grid
#          - rank proxies and return best + top-K
#
# WHY NOT calc_metrics()?
#   calc_metrics() defines "predicted positive" as yhat > fp_threshold, which
#   is appropriate for emissions levels (tons) but NOT for probabilities in (0,1)
#   when fp_threshold=0 (almost everyone becomes positive).
#   Here we explicitly evaluate classification from predicted probabilities.
#
# INPUTS
#   base : list
#       Output from prep_hurdle_base_DT(). Must contain base$DT as a data.table
#       with columns id__/year__/emit__.
#
#   phat_paths : character vector
#       Each path is an .rds file containing:
#         list(proxy_name = <string>, phat_dt = <data.table>)
#
#   fpr_targets : numeric vector
#       Target false-positive rates to evaluate at, e.g. c(0.01,0.05,0.10).
#       For each target, we pick a threshold based on non-emitters so that
#       realized FPR is close to the target, and report TPR at that threshold.
#
#   rank_by : character
#       How to rank proxies. Options:
#         - "tpr_at_fpr" (default) : rank by TPR at rank_fpr_target, tiebreak AUC
#         - "auc"                 : rank by AUC only
#         - "best_f1"             : rank by best-F1 only
#
#   rank_fpr_target : numeric
#       Which FPR target to use when rank_by="tpr_at_fpr" (must be in fpr_targets).
#
#   compute_best_f1 : logical
#       If TRUE, compute best-F1 over a grid of thresholds (slower).
#
# OUTPUT
#   List with:
#     $metrics  : data.table (one row per proxy)
#     $best     : 1-row data.table (best proxy under ranking rule)
#     $topk     : top-K proxies (K passed as arg)
#
###############################################################################

step1_metrics <- function(base,
                                      phat_paths,
                                      fpr_targets = c(0.01, 0.05, 0.10),
                                      rank_by = c("tpr_at_fpr", "auc", "best_f1"),
                                      rank_fpr_target = 0.05,
                                      compute_best_f1 = FALSE,
                                      top_k = 10L,
                                      progress_every = 50) {
  suppressPackageStartupMessages({
    library(data.table)
  })
  
  rank_by <- match.arg(rank_by)
  
  if (!is.list(base) || is.null(base$DT)) stop("base must be a list with base$DT.")
  DT0 <- as.data.table(base$DT)
  
  req_cols <- c("id__", "year__", "emit__")
  if (!all(req_cols %in% names(DT0))) {
    stop("base$DT is missing required columns: ", paste(setdiff(req_cols, names(DT0)), collapse = ", "))
  }
  
  if (length(phat_paths) == 0) {
    return(list(metrics = data.table(), best = data.table(), topk = data.table()))
  }
  
  if (!(rank_fpr_target %in% fpr_targets)) {
    stop("rank_fpr_target must be one of fpr_targets. Got rank_fpr_target=",
         rank_fpr_target, " fpr_targets=", paste(fpr_targets, collapse = ", "))
  }
  
  # ---- internal: AUC + TPR@FPR + optional best-F1 for probabilities ----
  eval_step1_prob <- function(y_bin, phat, fpr_targets, compute_best_f1) {
    y_bin <- as.integer(y_bin)
    phat  <- as.numeric(phat)
    
    ok <- is.finite(y_bin) & is.finite(phat)
    y_bin <- y_bin[ok]
    phat  <- phat[ok]
    
    n <- length(y_bin)
    n1 <- sum(y_bin == 1L)
    n0 <- sum(y_bin == 0L)
    
    out <- list(
      n = n,
      AUC = NA_real_,
      best_f1 = NA_real_,
      best_f1_thr = NA_real_
    )
    
    # Need both classes
    if (n1 == 0 || n0 == 0) {
      # still return threshold slots
      for (fpr in fpr_targets) {
        out[[paste0("tpr_at_fpr", fpr)]] <- NA_real_
        out[[paste0("thr_at_fpr", fpr)]] <- NA_real_
        out[[paste0("fpr_real",   fpr)]] <- NA_real_
      }
      return(out)
    }
    
    # AUC (rank-based, tie-safe)
    r <- rank(phat, ties.method = "average")
    out$AUC <- (sum(r[y_bin == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
    
    # TPR at FPR targets (threshold chosen from non-emitters)
    phat0 <- phat[y_bin == 0L]
    for (fpr in fpr_targets) {
      thr <- as.numeric(stats::quantile(phat0, probs = 1 - fpr, na.rm = TRUE, type = 7))
      pred_pos <- (phat >= thr)
      
      TP <- sum(pred_pos & (y_bin == 1L))
      FN <- sum(!pred_pos & (y_bin == 1L))
      FP <- sum(pred_pos & (y_bin == 0L))
      TN <- sum(!pred_pos & (y_bin == 0L))
      
      tpr <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
      fpr_real <- if ((FP + TN) > 0) FP / (FP + TN) else NA_real_
      
      out[[paste0("tpr_at_fpr", fpr)]] <- tpr
      out[[paste0("thr_at_fpr", fpr)]] <- thr
      out[[paste0("fpr_real",   fpr)]] <- fpr_real
    }
    
    # Optional: best F1 over a coarse threshold grid
    if (isTRUE(compute_best_f1)) {
      grid <- unique(stats::quantile(phat, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE, type = 7))
      best_f1 <- -Inf
      best_thr <- NA_real_
      for (thr in grid) {
        pred_pos <- (phat >= thr)
        TP <- sum(pred_pos & (y_bin == 1L))
        FP <- sum(pred_pos & (y_bin == 0L))
        FN <- sum(!pred_pos & (y_bin == 1L))
        
        ppv <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
        tpr <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
        f1  <- if (is.finite(ppv) && is.finite(tpr) && (ppv + tpr) > 0) 2 * ppv * tpr / (ppv + tpr) else NA_real_
        
        if (is.finite(f1) && f1 > best_f1) {
          best_f1 <- f1
          best_thr <- thr
        }
      }
      out$best_f1 <- if (is.finite(best_f1)) best_f1 else NA_real_
      out$best_f1_thr <- best_thr
    }
    
    out
  }
  
  # Standardize join keys once
  DT_join <- DT0[, .(id = id__, year = year__, emit__ = emit__)]
  
  metrics_list <- vector("list", length(phat_paths))
  
  for (i in seq_along(phat_paths)) {
    p <- phat_paths[[i]]
    if (i %% progress_every == 0) {
      message(sprintf("[step1-metrics] %d / %d : %s", i, length(phat_paths), basename(p)))
    }
    
    obj <- readRDS(p)
    if (is.null(obj$proxy_name) || is.null(obj$phat_dt)) next
    
    ph <- as.data.table(obj$phat_dt)
    if (!all(c("id", "year", "phat_raw") %in% names(ph))) {
      stop("phat_dt must contain columns id, year, phat_raw. Missing in: ", p)
    }
    
    dt <- merge(DT_join, ph[, .(id, year, phat_raw)], by = c("id","year"), all.x = TRUE)
    dt[is.na(phat_raw), phat_raw := 0]
    
    s <- eval_step1_prob(dt$emit__, dt$phat_raw, fpr_targets = fpr_targets, compute_best_f1 = compute_best_f1)
    
    row <- data.table(proxy_tag_ext = as.character(obj$proxy_name),
                      n = s$n,
                      AUC = s$AUC,
                      best_f1 = s$best_f1,
                      best_f1_thr = s$best_f1_thr)
    
    # add per-target columns
    for (fpr in fpr_targets) {
      row[, (paste0("tpr_at_fpr", fpr)) := s[[paste0("tpr_at_fpr", fpr)]]]
      row[, (paste0("thr_at_fpr", fpr)) := s[[paste0("thr_at_fpr", fpr)]]]
      row[, (paste0("fpr_real",   fpr)) := s[[paste0("fpr_real",   fpr)]]]
    }
    
    metrics_list[[i]] <- row
  }
  
  metrics <- rbindlist(metrics_list, fill = TRUE)
  
  if (nrow(metrics) == 0) {
    return(list(metrics = metrics, best = data.table(), topk = data.table()))
  }
  
  # Ranking
  if (rank_by == "auc") {
    setorder(metrics, -AUC)
  } else if (rank_by == "best_f1") {
    setorder(metrics, -best_f1, -AUC)
  } else {
    col_tpr <- paste0("tpr_at_fpr", rank_fpr_target)
    if (!(col_tpr %in% names(metrics))) stop("Missing ranking column: ", col_tpr)
    
    data.table::setorderv(
      metrics,
      cols  = c(col_tpr, "AUC"),
      order = c(-1L, -1L),
      na.last = TRUE
    )
  }
  
  best <- metrics[1]
  topk <- metrics[seq_len(min(as.integer(top_k), nrow(metrics)))]
  
  list(metrics = metrics, best = best, topk = topk)
}
