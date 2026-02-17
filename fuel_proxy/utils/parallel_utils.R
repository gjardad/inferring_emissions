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
