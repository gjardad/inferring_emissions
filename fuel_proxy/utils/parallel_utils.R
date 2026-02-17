###############################################################################
# parallel_utils.R
#
# PURPOSE
#   Utilities for managing a persistent PSOCK cluster across multiple LOOCV
#   calls, avoiding repeated cluster startup/teardown overhead.
###############################################################################

make_loocv_cluster <- function(n_cores = max(1L, parallel::detectCores(logical = FALSE) - 2L)) {
  if (n_cores <= 1L) return(NULL)

  n_cores <- min(n_cores, parallel::detectCores(logical = TRUE))
  message(sprintf("Starting PSOCK cluster with %d workers...", n_cores))

  cl <- parallel::makeCluster(n_cores, type = "PSOCK")

  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(data.table)
      library(mgcv)
    })
  })

  message("Cluster ready.")
  cl
}

stop_loocv_cluster <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
    message("Cluster stopped.")
  }
}

assign_kfold_groups <- function(ids, k = 10L, seed = 42L) {
  set.seed(seed)
  ids <- unique(ids)
  n <- length(ids)
  data.table::data.table(
    id   = ids,
    fold = sample(rep(seq_len(k), length.out = n))
  )
}

assign_sector_folds <- function(ids, sectors) {
  stopifnot(length(ids) == length(sectors))
  dt <- data.table::data.table(id = ids, sector = sectors)
  firm_sector <- dt[, .(sector = names(which.max(table(sector)))), by = id]
  sector_levels <- sort(unique(firm_sector$sector))
  sector_to_fold <- data.table::data.table(
    sector = sector_levels,
    fold   = seq_along(sector_levels)
  )
  firm_sector <- merge(firm_sector, sector_to_fold, by = "sector", all.x = TRUE)
  list(
    fold_ids        = firm_sector[, .(id, fold)],
    fold_sector_map = sector_to_fold
  )
}
