# threshold_cv.R
# Cross-validate a percentile threshold on calibrated EN predictions for
# extensive-margin classification among {17/18, 19, 24}.
#
# Pipeline:
#   For each of 200 EN reps r:
#     1. Calibrate predictions via calibrate_sector().
#     2. Year-demean calibrated yhat within sector-group.
#     3. Leave-one-sector-group-out: apply the CV'd threshold, compute TPR_r, FPR_r.
#   Report the average TPR, FPR across reps (matching table methodology).
#
# The threshold p* is selected ONCE on the training groups (not per-rep),
# since the threshold is a deployment-time choice, not a training-time one.
# Specifically: for each held-out group h, we grid-search p on the average
# of per-rep (TPR - FPR) across the 2 training groups over all 200 reps.
#
# RUNS ON: local 1

REPO_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
  error = function(e) normalizePath(getwd(), winslash = "/")
)
while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
source(file.path(UTILS_DIR, "calibration_helpers.R"))

BASE_SEED <- 2026L
K_sec     <- 5L

# ── Load data ────────────────────────────────────────────────────────────────
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel <- e_sec$repeated_cv_proxy_panel
M <- ncol(proxy_matrix_sec)
rm(e_sec)

# Combine NACE 17/18
panel$primary_nace2d[panel$primary_nace2d %in% c("17", "18")] <- "17/18"
panel$nace2d[panel$nace2d %in% c("17", "18")] <- "17/18"

# Subset to zero-emission sector groups
idx_ze <- which(panel$nace2d %in% c("17/18", "19", "24"))
panel_ze <- panel[idx_ze, ]
panel_ze$sector_group <- panel_ze$nace2d

cat("Observations per sector group:\n")
print(table(panel_ze$sector_group, panel_ze$emit))

groups <- sort(unique(panel_ze$sector_group))

# ── Helper: TPR and FPR ─────────────────────────────────────────────────────
tpr_fpr <- function(pred, actual) {
  tp <- sum(pred == 1 & actual == 1)
  fn <- sum(pred == 0 & actual == 1)
  fp <- sum(pred == 1 & actual == 0)
  tn <- sum(pred == 0 & actual == 0)
  tpr <- if ((tp + fn) > 0) tp / (tp + fn) else NA
  fpr <- if ((fp + tn) > 0) fp / (fp + tn) else NA
  c(tpr = tpr, fpr = fpr)
}

# ── Step 1: Calibrate each rep & compute per-rep demeaned yhat ───────────────
cat(sprintf("\nCalibrating %d EN repetitions and computing demeaned yhat...\n", M))

# Store per-rep demeaned yhat for the zero-emission subset (N_ze × M)
N_ze <- nrow(panel_ze)
yhat_dm_mat <- matrix(NA_real_, nrow = N_ze, ncol = M)
yhat_raw_mat <- matrix(NA_real_, nrow = N_ze, ncol = M)

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels_r <- proxy_to_levels(proxy_matrix_sec[, r])
  yhat_r <- calibrate_sector(panel, en_levels_r, fold_k_r)

  # Subset to zero-emission sectors
  yhat_ze <- yhat_r[idx_ze]
  yhat_raw_mat[, r] <- yhat_ze

  # Year-demean within sector-group
  yhat_dm <- ave(yhat_ze,
                 interaction(panel_ze$sector_group, panel_ze$year),
                 FUN = function(x) x - mean(x))
  yhat_dm_mat[, r] <- yhat_dm

  if (r %% 50 == 0) cat(sprintf("  %d/%d\n", r, M))
}

# ── Step 2: Grid search — average per-rep TPR-FPR across training groups ─────
p_grid <- seq(0, 1, by = 0.01)

results <- list()
curve_data <- list()

for (h in groups) {
  cat(sprintf("\n── Held out: %s ──\n", h))
  train_groups <- setdiff(groups, h)

  is_train <- panel_ze$sector_group %in% train_groups
  is_test  <- panel_ze$sector_group == h

  # For each p, compute avg over reps of { avg over training groups of (TPR-FPR) }
  obj <- numeric(length(p_grid))

  for (ip in seq_along(p_grid)) {
    p <- p_grid[ip]
    rep_diffs <- numeric(M)

    for (r in seq_len(M)) {
      diffs_s <- numeric(length(train_groups))
      for (is_g in seq_along(train_groups)) {
        s <- train_groups[is_g]
        in_s <- which(panel_ze$sector_group == s)
        dm_s <- yhat_dm_mat[in_s, r]
        tau_s <- quantile(dm_s, probs = p, names = FALSE)
        pred_s <- as.integer(dm_s > tau_s)
        tf_s <- tpr_fpr(pred_s, panel_ze$emit[in_s])
        diffs_s[is_g] <- tf_s["tpr"] - tf_s["fpr"]
      }
      rep_diffs[r] <- mean(diffs_s)
    }
    obj[ip] <- mean(rep_diffs)
  }

  curve_data[[h]] <- data.frame(held_out = h, p = p_grid, obj = obj)
  p_star <- p_grid[which.max(obj)]

  # ── Apply p* to held-out group, per-rep, then average metrics ────────────
  tpr_reps <- fpr_reps <- numeric(M)
  tpr_naive_reps <- fpr_naive_reps <- numeric(M)

  test_idx <- which(is_test)

  for (r in seq_len(M)) {
    dm_h <- yhat_dm_mat[test_idx, r]
    tau_h <- quantile(dm_h, probs = p_star, names = FALSE)
    pred_cv <- as.integer(dm_h > tau_h)
    tf_cv <- tpr_fpr(pred_cv, panel_ze$emit[test_idx])
    tpr_reps[r] <- tf_cv["tpr"]
    fpr_reps[r] <- tf_cv["fpr"]

    # Naive baseline: yhat > 0
    pred_naive <- as.integer(yhat_raw_mat[test_idx, r] > 0)
    tf_naive <- tpr_fpr(pred_naive, panel_ze$emit[test_idx])
    tpr_naive_reps[r] <- tf_naive["tpr"]
    fpr_naive_reps[r] <- tf_naive["fpr"]
  }

  results[[h]] <- data.frame(
    held_out     = h,
    p_star       = p_star,
    TPR_cv       = round(mean(tpr_reps), 3),
    FPR_cv       = round(mean(fpr_reps), 3),
    diff_cv      = round(mean(tpr_reps) - mean(fpr_reps), 3),
    TPR_naive    = round(mean(tpr_naive_reps), 3),
    FPR_naive    = round(mean(fpr_naive_reps), 3),
    diff_naive   = round(mean(tpr_naive_reps) - mean(fpr_naive_reps), 3),
    row.names    = NULL
  )

  cat(sprintf("  p* = %.2f\n", p_star))
  cat(sprintf("  CV threshold:  TPR = %.3f, FPR = %.3f, diff = %.3f\n",
              mean(tpr_reps), mean(fpr_reps), mean(tpr_reps) - mean(fpr_reps)))
  cat(sprintf("  Naive (>0):    TPR = %.3f, FPR = %.3f, diff = %.3f\n",
              mean(tpr_naive_reps), mean(fpr_naive_reps),
              mean(tpr_naive_reps) - mean(fpr_naive_reps)))
}

# ── Summary ──────────────────────────────────────────────────────────────────
res <- do.call(rbind, results)
res$gain <- res$diff_cv - res$diff_naive

cat("\n\n══ Summary ══════════════════════════════════════════════════════════════\n")
print(res, row.names = FALSE)

# ── Plot ─────────────────────────────────────────────────────────────────────
curves <- do.call(rbind, curve_data)

pdf(file.path(OUTPUT_DIR, "threshold_cv_curves.pdf"), width = 8, height = 4)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
for (h in groups) {
  d <- curves[curves$held_out == h, ]
  plot(d$p, d$obj, type = "l", lwd = 2,
       xlab = "Percentile p", ylab = "Mean(TPR - FPR) on training groups",
       main = paste("Held out:", h))
  p_star <- res$p_star[res$held_out == h]
  abline(v = p_star, col = "red", lty = 2)
  text(p_star, max(d$obj, na.rm = TRUE) * 0.95, sprintf("p*=%.2f", p_star),
       col = "red", pos = 4, cex = 0.8)
}
dev.off()
cat(sprintf("\nPlot saved to: %s\n", file.path(OUTPUT_DIR, "threshold_cv_curves.pdf")))
