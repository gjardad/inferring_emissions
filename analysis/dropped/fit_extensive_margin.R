###############################################################################
# analysis/fit_extensive_margin.R
#
# PURPOSE
#   Maximize classification performance (minimize FPR, maximize TPR) for the
#   extensive margin: predicting emitter vs non-emitter.
#
#   Tests multiple classifiers against the current GAM baseline, with
#   explicit attention to distribution shift between training and deployment:
#
#   DISTRIBUTION SHIFT CONCERN
#     Training data = EU ETS firms (large, heavy industry) + NACE 19/24
#     confirmed zeros. Deployment = all non-ETS firms across ALL sectors.
#     Two implications for feature selection:
#
#     (a) Sector identity is unreliable. Many deployment sectors don't
#         appear in training. A model that relies on sector will not know
#         what to do with e.g. NACE 62 (software). The GAM's sector RE
#         defaults unseen sectors to the population mean (graceful).
#         Trees have no such mechanism.
#
#     (b) Size-emissions relationship is confounded. In training, bigger
#         firms emit more (big steel > small steel). In deployment, this
#         breaks down (big tech firm ≠ more emissions than medium tech).
#         Size should predict emissions only among firms that actually
#         buy fuel — i.e., conditional on positive proxy.
#
#     The fuel-supply proxy is the feature that transfers best: whether
#     a firm buys from fuel suppliers is directly relevant to whether it
#     combusts fossil fuels, regardless of sector or firm size.
#
#   MODELS TESTED
#     1. GAM proxy-only    — no sector, no size. Tests how far the proxy
#                            alone gets us. Most deployment-safe model.
#     2. GAM baseline      — current spec: log_revenue + proxy + sector RE.
#     3. GAM enriched      — baseline + capital, FTE, capital intensity.
#     4. GAM interaction   — proxy × revenue interaction. Tests whether
#                            size matters only among proxy-positive firms.
#                            If the main effect of log_revenue ≈ 0 and the
#                            interaction is positive, size helps classify
#                            only when there's a fuel-supply signal — exactly
#                            what we want for deployment.
#     5. XGBoost full      — gradient boosted trees with all features
#                            (including sector). Maximum in-sample flexibility.
#     6. XGBoost no-sector — same features minus sector identity. Tests
#                            whether sector helps or whether the model
#                            can classify using proxy + firm characteristics
#                            alone. Directly relevant to deployment.
#     7. Random forest     — alternative ensemble method, all features.
#
#   All use the same 10-fold group CV (grouped by firm, seed=42) as
#   fit_cv_lofocv.R.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/extensive_margin_comparison.csv   (summary per model)
#   {OUTPUT_DIR}/extensive_margin_roc_data.csv     (full threshold sweep)
#   {OUTPUT_DIR}/extensive_margin_feature_importance.csv (XGBoost)
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(mgcv)

# Check for optional packages
has_xgboost <- requireNamespace("xgboost", quietly = TRUE)
has_ranger  <- requireNamespace("ranger", quietly = TRUE)

if (!has_xgboost) cat("NOTE: xgboost not installed. Run install.packages('xgboost') to enable GBT.\n")
if (!has_ranger)  cat("NOTE: ranger not installed. Run install.packages('ranger') to enable RF.\n")

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)


# ── Rename annual accounts variables (if needed) ────────────────────────────
if ("turnover_VAT" %in% names(panel) && !"revenue" %in% names(panel))
  panel <- panel %>% rename(revenue = turnover_VAT)
if ("v_0022_27" %in% names(panel) && !"capital" %in% names(panel))
  panel <- panel %>% rename(capital = v_0022_27)
if ("v_0001003" %in% names(panel) && !"fte" %in% names(panel))
  panel <- panel %>% rename(fte = v_0001003)
if ("v_0001023" %in% names(panel) && !"wage_bill" %in% names(panel))
  panel <- panel %>% rename(wage_bill = v_0001023)
if ("v_0009800" %in% names(panel) && !"value_added" %in% names(panel))
  panel <- panel %>% rename(value_added = v_0009800)


# ── Engineer features ────────────────────────────────────────────────────────
panel <- panel %>%
  mutate(
    emit         = as.integer(y > 0),
    log_revenue  = ifelse(is.na(log_revenue), 0, log_revenue),
    year_f       = factor(year),
    nace2d_f     = factor(nace2d),

    # Proxy features
    proxy_pos    = as.integer(proxy_weighted > 0),
    asinh_proxy  = asinh(proxy_weighted),

    # Annual accounts features (asinh for scale-robustness, 0 for missing)
    asinh_capital     = asinh(coalesce(capital, 0)),
    asinh_fte         = asinh(coalesce(fte, 0)),
    asinh_wage        = asinh(coalesce(wage_bill, 0)),
    asinh_va          = asinh(coalesce(value_added, 0)),
    capital_intensity = asinh(coalesce(capital, 0) / pmax(exp(log_revenue), 1)),

    # Integer-encode sector for tree models. Trees split at integer
    # boundaries, so they can isolate any single sector or group of
    # sectors. With ~20 categories this works well in practice.
    nace2d_int   = as.integer(factor(nace2d))
  )

cat(sprintf("Panel: %d rows, %d firms\n", nrow(panel), n_distinct(panel$vat)))
cat(sprintf("Emitters: %d (%.1f%%)\n", sum(panel$emit), 100 * mean(panel$emit)))
cat(sprintf("Has capital: %d (%.1f%%)\n",
    sum(!is.na(panel$capital) & panel$capital > 0),
    100 * mean(!is.na(panel$capital) & panel$capital > 0)))
cat(sprintf("Has FTE: %d (%.1f%%)\n",
    sum(!is.na(panel$fte) & panel$fte > 0),
    100 * mean(!is.na(panel$fte) & panel$fte > 0)))


# ── CV folds (identical to fit_cv_lofocv.R) ──────────────────────────────────
K_FOLDS <- 10L
set.seed(42)
unique_firms <- unique(panel$vat)
firm_folds   <- sample(rep(1:K_FOLDS, length.out = length(unique_firms)))
names(firm_folds) <- unique_firms
foldid <- unname(firm_folds[panel$vat])

cat(sprintf("Fold sizes: %s\n\n", paste(table(foldid), collapse = ", ")))


# ── Sector-year totals for calibration ────────────────────────────────────────
if (!exists("syt")) {
  syt <- panel %>%
    group_by(nace2d, year) %>%
    summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")
}


# ── Feature matrices for tree-based models ────────────────────────────────────
# Full feature set (includes sector identity).
tree_features_full <- c(
  "log_revenue",
  "proxy_pos", "asinh_proxy",           # proxy
  "asinh_capital", "asinh_fte",          # firm size
  "asinh_wage", "asinh_va",             # annual accounts
  "capital_intensity",                    # intensity ratio
  "nace2d_int", "year"                   # sector + year
)
X_tree_full <- as.matrix(panel[, tree_features_full])

# No-sector feature set: drops nace2d_int. Tests whether classification
# works without sector identity — directly relevant to deployment where
# many sectors are unseen.
tree_features_nosec <- setdiff(tree_features_full, "nace2d_int")
X_tree_nosec <- as.matrix(panel[, tree_features_nosec])


# ── Pre-allocate prediction columns ──────────────────────────────────────────
model_names <- c("gam_proxy_only", "gam_baseline", "gam_enriched",
                 "gam_interaction")
if (has_xgboost) model_names <- c(model_names, "xgb_full", "xgb_nosec")
if (has_ranger)  model_names <- c(model_names, "rf")

for (nm in model_names) {
  panel[[paste0("phat_", nm)]] <- NA_real_
}


# ── XGBoost hyperparameters ──────────────────────────────────────────────────
#
# How XGBoost works (brief):
#   Builds a sequence of shallow decision trees, each correcting the
#   errors of the previous ones. Think of each tree as asking a series
#   of yes/no questions about the features ("is proxy > 5?", "is
#   nace2d = 24?") and assigning a score at the bottom.
#
# Key hyperparameters and why we chose these values:
#
#   max_depth = 3
#     Each tree has at most 3 levels of splits, so it can capture
#     at most 3-way interactions (e.g., proxy > 0 AND nace2d = 19
#     AND revenue > X). Deeper trees risk overfitting with ~5K
#     training obs. Depth 3 is standard for small datasets.
#
#   eta = 0.05 (learning rate)
#     Each tree contributes only 5% of its prediction to the running
#     total. Lower = slower learning = less overfitting, but needs
#     more rounds. 0.05 is conservative; 0.1-0.3 is common for
#     larger datasets.
#
#   nrounds = 300
#     Number of sequential trees. With eta=0.05, 300 rounds means
#     the model sees 300 x 0.05 = 15 "effective" passes. Combined
#     with the regularization below, this is conservative.
#
#   subsample = 0.8
#     Each tree is trained on a random 80% of the training data.
#     Reduces overfitting (similar to bootstrap in random forests).
#
#   colsample_bytree = 0.8
#     Each tree sees a random 80% of the features. Forces the model
#     to not rely exclusively on the strongest feature (proxy).
#
#   min_child_weight = 20
#     A leaf node must contain >= 20 observations. Prevents the tree
#     from creating very specific rules that fit noise. With ~5K
#     training obs and 3 levels, this is quite conservative.
#
#   gamma = 1
#     Minimum loss reduction to make a further split. Acts as a
#     "significance threshold" for splits — a split must improve
#     the objective by at least 1.0. Pruning weak splits.
#
xgb_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  max_depth        = 3,
  eta              = 0.05,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 20,
  gamma            = 1,
  nthread          = 4
)
XGB_NROUNDS <- 300


# ── CV loop ──────────────────────────────────────────────────────────────────
cat("Running ", K_FOLDS, "-fold group CV...\n\n", sep = "")
t0_total <- Sys.time()

# Store feature importance across folds (XGBoost full and no-sector)
xgb_importance_full  <- list()
xgb_importance_nosec <- list()

for (k in 1:K_FOLDS) {
  cat(sprintf("  Fold %d/%d ...", k, K_FOLDS))
  t0 <- Sys.time()

  train_idx <- which(foldid != k)
  test_idx  <- which(foldid == k)
  train <- panel[train_idx, ]
  test  <- panel[test_idx, ]

  # ── 1. GAM proxy-only ──
  # No sector RE, no size variables. Only the fuel-supply proxy and year FE.
  # This is the most deployment-safe model: it relies solely on whether a
  # firm buys from identified fuel suppliers. At deployment, this feature
  # transfers across sectors without modification.
  fit <- tryCatch(
    gam(emit ~ I(proxy_weighted > 0) + asinh(proxy_weighted) + year_f,
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    phat <- as.numeric(predict(fit, newdata = test, type = "response"))
    panel$phat_gam_proxy_only[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  # ── 2. GAM baseline (current extensive margin spec) ──
  fit <- tryCatch(
    gam(emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +
          year_f + s(nace2d_f, bs = "re"),
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    phat <- as.numeric(predict(fit, newdata = test, type = "response"))
    panel$phat_gam_baseline[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  # ── 3. GAM enriched (+ capital, FTE, capital intensity) ──
  fit <- tryCatch(
    gam(emit ~ log_revenue + I(proxy_weighted > 0) + asinh(proxy_weighted) +
          asinh_capital + asinh_fte + capital_intensity +
          year_f + s(nace2d_f, bs = "re"),
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    phat <- as.numeric(predict(fit, newdata = test, type = "response"))
    panel$phat_gam_enriched[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  # ── 4. GAM interaction (proxy × revenue) ──
  # Tests whether size (log_revenue) helps classify emitters only among
  # firms that buy from fuel suppliers. The formula expands to:
  #   log_revenue + proxy_pos + log_revenue:proxy_pos + asinh(proxy)
  # If the main effect of log_revenue ≈ 0 and the interaction is positive,
  # it means size predicts emissions ONLY when the proxy is positive — a
  # big steel producer with fuel purchases emits, a big tech firm without
  # them doesn't. This is exactly the deployment-safe pattern.
  fit <- tryCatch(
    gam(emit ~ log_revenue * proxy_pos + asinh(proxy_weighted) +
          year_f + s(nace2d_f, bs = "re"),
        data = train, family = binomial(link = "logit"), method = "REML"),
    error = function(e) NULL
  )
  if (!is.null(fit)) {
    phat <- as.numeric(predict(fit, newdata = test, type = "response"))
    panel$phat_gam_interaction[test_idx] <- pmin(pmax(phat, 0), 1)
  }

  # ── 5. XGBoost full (all features including sector) ──
  if (has_xgboost) {
    dtrain <- xgboost::xgb.DMatrix(X_tree_full[train_idx, ],
                                    label = panel$emit[train_idx])
    dtest  <- xgboost::xgb.DMatrix(X_tree_full[test_idx, ])

    fit <- xgboost::xgb.train(xgb_params, dtrain, nrounds = XGB_NROUNDS,
                               verbose = 0)
    panel$phat_xgb_full[test_idx] <- predict(fit, dtest)

    imp <- xgboost::xgb.importance(feature_names = tree_features_full,
                                    model = fit)
    imp$fold <- k
    xgb_importance_full[[k]] <- imp
  }

  # ── 6. XGBoost no-sector ──
  # Drops sector identity. Forces the model to discriminate using proxy
  # and firm characteristics alone. If this performs comparably to the
  # full model, sector identity is not adding much — good news for
  # deployment to unseen sectors.
  if (has_xgboost) {
    dtrain_ns <- xgboost::xgb.DMatrix(X_tree_nosec[train_idx, ],
                                       label = panel$emit[train_idx])
    dtest_ns  <- xgboost::xgb.DMatrix(X_tree_nosec[test_idx, ])

    fit_ns <- xgboost::xgb.train(xgb_params, dtrain_ns, nrounds = XGB_NROUNDS,
                                  verbose = 0)
    panel$phat_xgb_nosec[test_idx] <- predict(fit_ns, dtest_ns)

    imp_ns <- xgboost::xgb.importance(feature_names = tree_features_nosec,
                                       model = fit_ns)
    imp_ns$fold <- k
    xgb_importance_nosec[[k]] <- imp_ns
  }

  # ── 7. Random forest ──
  #
  # How random forests work (brief):
  #   Grows many independent decision trees (500 here), each on a
  #   random bootstrap sample of the training data, and at each split
  #   considers only a random subset of features. The final prediction
  #   is the average across all 500 trees. This "wisdom of crowds"
  #   approach reduces overfitting compared to a single deep tree.
  #
  # Key hyperparameters:
  #
  #   num.trees = 500
  #     Number of independent trees. More trees = more stable predictions,
  #     at the cost of computation. 500 is standard; beyond ~500 there
  #     are usually diminishing returns.
  #
  #   min.node.size = 10
  #     Minimum observations in a leaf node. Like min_child_weight in
  #     XGBoost — prevents overly specific leaves. 10 is moderate.
  #
  #   mtry (not set, uses default)
  #     Number of features considered at each split. Default for
  #     classification is sqrt(p) ~ sqrt(11) ~ 3. This forces each
  #     tree to explore different feature combinations.
  #
  if (has_ranger) {
    train_rf <- data.frame(
      emit = factor(panel$emit[train_idx]),
      X_tree_full[train_idx, ]
    )
    test_rf <- data.frame(X_tree_full[test_idx, ])

    fit <- ranger::ranger(
      emit ~ ., data = train_rf,
      probability = TRUE, num.trees = 500,
      min.node.size = 10, importance = "impurity"
    )

    panel$phat_rf[test_idx] <- predict(fit, data = test_rf)$predictions[, "1"]
  }

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf(" done (%.1fs)\n", elapsed))
}

elapsed_total <- round(difftime(Sys.time(), t0_total, units = "mins"), 1)
cat(sprintf("\nCV complete (%.1f min)\n\n", elapsed_total))


# ══════════════════════════════════════════════════════════════════════════════
# POST-CV ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════

# ── Classification metrics at threshold grid ──────────────────────────────────
cat("Computing classification metrics...\n")

THRESHOLDS <- seq(0.01, 0.99, by = 0.01)

compute_class_metrics <- function(y, phat, thresholds) {
  is_emit <- (y > 0)
  is_non  <- !is_emit
  n_emit  <- sum(is_emit)
  n_non   <- sum(is_non)

  tpr <- fpr <- precision <- f1 <- youdenJ <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    pred_pos <- (phat > thresholds[i])

    TP <- sum(pred_pos & is_emit)
    FP <- sum(pred_pos & is_non)

    tpr_i <- if (n_emit > 0) TP / n_emit else NA_real_
    fpr_i <- if (n_non > 0)  FP / n_non  else NA_real_
    ppv_i <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_

    tpr[i] <- tpr_i
    fpr[i] <- fpr_i
    precision[i] <- ppv_i
    f1[i] <- if (is.finite(ppv_i) && is.finite(tpr_i) && (ppv_i + tpr_i) > 0) {
      2 * ppv_i * tpr_i / (ppv_i + tpr_i)
    } else NA_real_
    youdenJ[i] <- if (is.finite(tpr_i) && is.finite(fpr_i)) tpr_i - fpr_i else NA_real_
  }

  # AUC-ROC (trapezoidal rule)
  ord <- order(fpr, tpr)
  auc <- abs(sum(diff(fpr[ord]) * (tpr[ord][-1] + tpr[ord][-length(tpr[ord])]) / 2))

  list(
    df = data.frame(
      threshold = thresholds, TPR = tpr, FPR = fpr,
      precision = precision, F1 = f1, youdenJ = youdenJ,
      stringsAsFactors = FALSE
    ),
    auc = auc
  )
}


# ── Compute hybrid pipeline metrics at a single threshold ────────────────────
compute_hybrid_at_thr <- function(phat, proxy, y, emit, nace2d, year, syt, thr) {
  ok <- !is.na(phat) & !is.na(proxy) & !is.na(y)
  yhat_hard <- pmax(as.numeric(phat[ok] > thr) * proxy[ok], 0)

  yhat_cap <- calibrate_with_cap(
    yhat_hard, emit[ok], y[ok], nace2d[ok], year[ok], syt
  )

  m <- calc_metrics(y[ok], yhat_cap, nace2d = nace2d[ok], year = year[ok])

  data.frame(
    threshold = thr,
    nRMSE = m$nrmse_sd, rmse = m$rmse,
    fpr_hybrid = m$fpr_nonemitters, tpr_hybrid = m$tpr_emitters,
    spearman = m$spearman,
    within_sy_rho_med = m$within_sy_rho_med,
    rho_pooled = m$rho_pooled,
    stringsAsFactors = FALSE
  )
}


# ── Evaluate each model ──────────────────────────────────────────────────────
roc_all     <- list()
summary_all <- list()

cat("\n")
for (nm in model_names) {
  phat <- panel[[paste0("phat_", nm)]]
  ok   <- !is.na(phat) & !is.na(panel$y)

  if (sum(ok) == 0) {
    cat(sprintf("  %s: SKIPPED (no predictions)\n", nm))
    next
  }

  # Pure classification metrics
  class_res <- compute_class_metrics(panel$y[ok], phat[ok], THRESHOLDS)
  class_m   <- class_res$df
  auc       <- class_res$auc
  class_m$model <- nm
  roc_all[[nm]] <- class_m

  # Optimal thresholds by different criteria
  best_J  <- class_m[which.max(class_m$youdenJ), ]
  best_F1 <- class_m[which.max(class_m$F1), ]

  # TPR >= 0.95 with minimum FPR
  cands_95 <- class_m[!is.na(class_m$TPR) & class_m$TPR >= 0.95, ]
  best_95  <- if (nrow(cands_95) > 0) cands_95[which.min(cands_95$FPR), ] else best_J

  # Hybrid pipeline metrics at the classification-optimal thresholds
  hybrid_J <- compute_hybrid_at_thr(
    phat, panel$proxy_weighted, panel$y, panel$emit,
    panel$nace2d, panel$year, syt, best_J$threshold
  )
  hybrid_95 <- compute_hybrid_at_thr(
    phat, panel$proxy_weighted, panel$y, panel$emit,
    panel$nace2d, panel$year, syt, best_95$threshold
  )

  # Hybrid-RMSE-optimal threshold (comparable to fit_cv_lofocv.R)
  hybrid_sweep <- list()
  for (thr in seq(0.01, 0.80, by = 0.01)) {
    hybrid_sweep[[length(hybrid_sweep) + 1]] <- compute_hybrid_at_thr(
      phat, panel$proxy_weighted, panel$y, panel$emit,
      panel$nace2d, panel$year, syt, thr
    )
  }
  hybrid_sweep_df <- bind_rows(hybrid_sweep)
  best_rmse_row <- hybrid_sweep_df[which.min(hybrid_sweep_df$rmse), ]

  cat(sprintf("  %s (AUC=%.3f):\n", nm, auc))
  cat(sprintf("    Youden J:     thr=%.2f  FPR=%.3f  TPR=%.3f  F1=%.3f\n",
              best_J$threshold, best_J$FPR, best_J$TPR, best_J$F1))
  cat(sprintf("    F1 optimal:   thr=%.2f  FPR=%.3f  TPR=%.3f  F1=%.3f\n",
              best_F1$threshold, best_F1$FPR, best_F1$TPR, best_F1$F1))
  cat(sprintf("    TPR>=0.95:    thr=%.2f  FPR=%.3f  TPR=%.3f\n",
              best_95$threshold, best_95$FPR, best_95$TPR))
  cat(sprintf("    Hybrid (J):   thr=%.2f  nRMSE=%.3f  FPR=%.3f  TPR=%.3f  rho_s=%.3f\n",
              hybrid_J$threshold, hybrid_J$nRMSE,
              hybrid_J$fpr_hybrid, hybrid_J$tpr_hybrid,
              hybrid_J$within_sy_rho_med))
  cat(sprintf("    Hybrid (95):  thr=%.2f  nRMSE=%.3f  FPR=%.3f  TPR=%.3f  rho_s=%.3f\n",
              hybrid_95$threshold, hybrid_95$nRMSE,
              hybrid_95$fpr_hybrid, hybrid_95$tpr_hybrid,
              hybrid_95$within_sy_rho_med))
  cat(sprintf("    Hybrid RMSE:  thr=%.2f  nRMSE=%.3f  FPR=%.3f  TPR=%.3f  rho_s=%.3f\n\n",
              best_rmse_row$threshold, best_rmse_row$nRMSE,
              best_rmse_row$fpr_hybrid, best_rmse_row$tpr_hybrid,
              best_rmse_row$within_sy_rho_med))

  summary_all[[nm]] <- data.frame(
    model = nm, auc = auc,
    # Youden J optimal
    thr_J = best_J$threshold, FPR_J = best_J$FPR, TPR_J = best_J$TPR, F1_J = best_J$F1,
    # F1 optimal
    thr_F1 = best_F1$threshold, FPR_F1 = best_F1$FPR, TPR_F1 = best_F1$TPR, F1_F1 = best_F1$F1,
    # TPR >= 0.95 constrained
    thr_95 = best_95$threshold, FPR_95 = best_95$FPR, TPR_95 = best_95$TPR,
    # Hybrid pipeline at Youden J threshold
    hybrid_J_nRMSE = hybrid_J$nRMSE, hybrid_J_FPR = hybrid_J$fpr_hybrid,
    hybrid_J_TPR = hybrid_J$tpr_hybrid, hybrid_J_rho_s = hybrid_J$within_sy_rho_med,
    # Hybrid pipeline at TPR>=0.95 threshold
    hybrid_95_nRMSE = hybrid_95$nRMSE, hybrid_95_FPR = hybrid_95$fpr_hybrid,
    hybrid_95_TPR = hybrid_95$tpr_hybrid, hybrid_95_rho_s = hybrid_95$within_sy_rho_med,
    # Hybrid RMSE-optimal (comparable to fit_cv_lofocv.R output)
    hybrid_rmse_thr = best_rmse_row$threshold,
    hybrid_rmse_nRMSE = best_rmse_row$nRMSE,
    hybrid_rmse_FPR = best_rmse_row$fpr_hybrid,
    hybrid_rmse_TPR = best_rmse_row$tpr_hybrid,
    hybrid_rmse_rho_s = best_rmse_row$within_sy_rho_med,
    stringsAsFactors = FALSE
  )
}


# ── Feature importance (XGBoost, averaged across folds) ──────────────────────
avg_importance <- function(imp_list, label) {
  if (length(imp_list) == 0) return(invisible(NULL))

  imp_all <- bind_rows(imp_list)
  imp_avg <- imp_all %>%
    group_by(Feature) %>%
    summarise(
      Gain_mean      = mean(Gain, na.rm = TRUE),
      Gain_sd        = sd(Gain, na.rm = TRUE),
      Cover_mean     = mean(Cover, na.rm = TRUE),
      Frequency_mean = mean(Frequency, na.rm = TRUE),
      n_folds        = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(Gain_mean))

  cat(sprintf("═══ %s feature importance (averaged across folds) ═══\n\n", label))
  cat("  Gain = fraction of total loss reduction attributable to this feature.\n")
  cat("  Cover = fraction of training observations affected by splits on it.\n")
  cat("  Frequency = fraction of all splits that use this feature.\n\n")
  print(as.data.frame(imp_avg), row.names = FALSE)
  cat("\n")

  imp_avg
}

imp_avg_full  <- NULL
imp_avg_nosec <- NULL

if (has_xgboost) {
  imp_avg_full  <- avg_importance(xgb_importance_full,  "XGBoost full")
  imp_avg_nosec <- avg_importance(xgb_importance_nosec, "XGBoost no-sector")
}


# ── Print summary table ──────────────────────────────────────────────────────
cat("\n═══════════════════════════════════════════════════════════════\n")
cat("  Extensive margin classification comparison (LOFOCV)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

summary_df <- bind_rows(summary_all)

cat("-- Pure classification (phat thresholds) --\n")
print(
  summary_df %>%
    select(model, auc,
           thr_J, FPR_J, TPR_J, F1_J,
           thr_95, FPR_95, TPR_95) %>%
    mutate(across(where(is.numeric), ~ round(., 3))),
  row.names = FALSE
)

cat("\n-- Hybrid pipeline (after calibrate_with_cap) --\n")
print(
  summary_df %>%
    select(model,
           hybrid_rmse_thr, hybrid_rmse_nRMSE,
           hybrid_rmse_FPR, hybrid_rmse_TPR, hybrid_rmse_rho_s,
           hybrid_J_nRMSE, hybrid_J_FPR, hybrid_J_TPR, hybrid_J_rho_s) %>%
    mutate(across(where(is.numeric), ~ round(., 3))),
  row.names = FALSE
)


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(summary_df,
          file.path(OUTPUT_DIR, "extensive_margin_comparison.csv"),
          row.names = FALSE)

roc_df <- bind_rows(roc_all)
write.csv(roc_df,
          file.path(OUTPUT_DIR, "extensive_margin_roc_data.csv"),
          row.names = FALSE)

if (!is.null(imp_avg_full)) {
  write.csv(imp_avg_full,
            file.path(OUTPUT_DIR, "extensive_margin_feature_importance_full.csv"),
            row.names = FALSE)
}
if (!is.null(imp_avg_nosec)) {
  write.csv(imp_avg_nosec,
            file.path(OUTPUT_DIR, "extensive_margin_feature_importance_nosec.csv"),
            row.names = FALSE)
}

cat(sprintf("\nResults saved to: %s\n", OUTPUT_DIR))
cat("═══════════════════════════════════════════════════════════════\n")
