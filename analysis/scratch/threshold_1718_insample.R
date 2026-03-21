# Quick: in-sample optimal threshold for 17/18
source("paths.R")
source(file.path(UTILS_DIR, "calibration_helpers.R"))

load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"))
panel <- repeated_cv_proxy_panel
panel$primary_nace2d[panel$primary_nace2d %in% c("17","18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17","18")] <- "17/18"

idx <- which(panel$nace2d == "17/18")
M <- ncol(proxy_matrix)

# Pre-compute all reps' calibrated + demeaned yhat for 17/18
cat("Calibrating...\n")
dm_mat <- matrix(NA_real_, nrow = length(idx), ncol = M)
raw_mat <- matrix(NA_real_, nrow = length(idx), ncol = M)
for (r in 1:M) {
  fk <- assign_folds(panel, "sector", 5L, 2026L + r)
  lev <- pmax(sinh(proxy_matrix[, r]), 0)
  yh <- calibrate_sector(panel, lev, fk)[idx]
  raw_mat[, r] <- yh
  dm_mat[, r] <- ave(yh, interaction(panel$nace2d[idx], panel$year[idx]),
                     FUN = function(x) x - mean(x))
  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M))
}

emit <- panel$emit[idx]

p_grid <- seq(0, 1, by = 0.01)
obj <- numeric(length(p_grid))

cat("Grid search...\n")
for (ip in seq_along(p_grid)) {
  p <- p_grid[ip]
  diffs <- numeric(M)
  for (r in 1:M) {
    tau <- quantile(dm_mat[, r], probs = p, names = FALSE)
    pred <- as.integer(dm_mat[, r] > tau)
    tp <- sum(pred == 1 & emit == 1)
    fn <- sum(pred == 0 & emit == 1)
    fp <- sum(pred == 1 & emit == 0)
    tn <- sum(pred == 0 & emit == 0)
    diffs[r] <- tp/(tp+fn) - fp/(fp+tn)
  }
  obj[ip] <- mean(diffs)
}

best <- which.max(obj)
cat(sprintf("\nIn-sample optimal for 17/18: p*=%.2f, TPR-FPR=%.4f\n",
            p_grid[best], obj[best]))

# Naive
diffs0 <- numeric(M)
for (r in 1:M) {
  pred <- as.integer(raw_mat[, r] > 0)
  tp <- sum(pred == 1 & emit == 1)
  fn <- sum(pred == 0 & emit == 1)
  fp <- sum(pred == 1 & emit == 0)
  tn <- sum(pred == 0 & emit == 0)
  diffs0[r] <- tp/(tp+fn) - fp/(fp+tn)
}
cat(sprintf("Naive (>0) for 17/18: TPR-FPR=%.4f\n", mean(diffs0)))
