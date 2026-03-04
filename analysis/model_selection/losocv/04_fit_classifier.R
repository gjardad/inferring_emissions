###############################################################################
# analysis/model_selection/losocv/04_fit_classifier.R
#
# PURPOSE
#   Fit emitter/non-emitter classifiers under LOSOCV and compare them.
#   Adapted from fit_extensive_margin.R (which uses LOFOCV).
#
#   Models tested:
#     1. GAM baseline     — log_revenue + proxy + sector RE (current spec)
#     2. GAM enriched     — baseline + capital, FTE, capital intensity
#     3. XGBoost full     — all ~220 annual accounts + proxy + sector + year
#     4. XGBoost no-sector— same minus sector identity (deployment-robust)
#     5. Random forest    — all features (same as XGBoost full)
#
#   GAM models use a handful of covariates. Tree models get the full ~220
#   feature set. If GAM performs comparably, the specification is robust.
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/classifier_phat_{model}.csv
#   {OUTPUT_DIR}/model_selection/losocv/classifier_comparison.csv
#   {OUTPUT_DIR}/model_selection/losocv/classifier_feature_importance_full.csv
#   {OUTPUT_DIR}/model_selection/losocv/classifier_feature_importance_nosec.csv
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
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

has_xgboost <- requireNamespace("xgboost", quietly = TRUE)
has_ranger  <- requireNamespace("ranger", quietly = TRUE)

if (!has_xgboost) cat("NOTE: xgboost not installed. Run install.packages('xgboost').\n")
if (!has_ranger)  cat("NOTE: ranger not installed. Run install.packages('ranger').\n")

source(file.path(UTILS_DIR, "calc_metrics.R"))
source(file.path(UTILS_DIR, "calibration.R"))

OUT_DIR <- file.path(OUTPUT_DIR, "model_selection", "losocv")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

# Rename annual accounts variables (if needed)
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

    # Annual accounts features (for GAM enriched)
    asinh_capital     = asinh(coalesce(capital, 0)),
    asinh_fte         = asinh(coalesce(fte, 0)),
    capital_intensity = asinh(coalesce(capital, 0) / pmax(exp(log_revenue), 1)),

    # Integer-encode sector for tree models
    nace2d_int   = as.integer(factor(nace2d))
  )

cat(sprintf("Panel: %d rows, %d firms\n", nrow(panel), n_distinct(panel$vat)))
cat(sprintf("Emitters: %d (%.1f%%)\n", sum(panel$emit), 100 * mean(panel$emit)))


# ── Build tree feature matrices (all ~220 financial covariates) ──────────────
v_cols <- grep("^v_[0-9]", names(panel), value = TRUE)
# Exclude columns already renamed to avoid duplication
v_cols <- setdiff(v_cols, c("v_0022_27", "v_0001003", "v_0001023", "v_0009800"))

# Build a matrix of all financial covariates (asinh-transformed)
fin_for_trees <- as.matrix(panel[, v_cols])
fin_for_trees[is.na(fin_for_trees)] <- 0
fin_for_trees <- asinh(fin_for_trees)

# Add the renamed key variables and derived features
tree_fin_extra <- cbind(
  log_revenue       = panel$log_revenue,
  asinh_capital     = panel$asinh_capital,
  asinh_fte         = panel$asinh_fte,
  capital_intensity = panel$capital_intensity,
  proxy_pos         = panel$proxy_pos,
  asinh_proxy       = panel$asinh_proxy
)

# Full feature set (includes sector)
X_tree_full <- cbind(fin_for_trees, tree_fin_extra,
                     nace2d_int = panel$nace2d_int, year = panel$year)

# No-sector feature set
X_tree_nosec <- X_tree_full[, colnames(X_tree_full) != "nace2d_int"]

tree_features_full  <- colnames(X_tree_full)
tree_features_nosec <- colnames(X_tree_nosec)

cat(sprintf("Tree features: %d (full), %d (no-sector)\n",
            ncol(X_tree_full), ncol(X_tree_nosec)))


# ── LOSOCV: assign firms to sectors ─────────────────────────────────────────
firm_sector <- panel %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

panel <- panel %>%
  left_join(firm_sector, by = "vat")

sector_levels <- sort(unique(firm_sector$primary_nace2d))
n_folds <- length(sector_levels)
cat(sprintf("LOSOCV: %d sector folds\n\n", n_folds))

# Full factor levels for GAM (so unseen sector RE defaults to population mean)
all_nace2d_levels <- levels(panel$nace2d_f)
all_year_levels   <- levels(panel$year_f)


# ── Sector-year totals ──────────────────────────────────────────────────────
syt <- panel %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# ── XGBoost hyperparameters ──────────────────────────────────────────────────
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


# ── Pre-allocate prediction columns ──────────────────────────────────────────
model_names <- c("gam_baseline", "gam_enriched")
if (has_xgboost) model_names <- c(model_names, "xgb_full", "xgb_nosec")
if (has_ranger)  model_names <- c(model_names, "rf")

for (nm in model_names) {
  panel[[paste0("phat_", nm)]] <- NA_real_
}


# ── LOSOCV loop ──────────────────────────────────────────────────────────────
cat("Running LOSOCV classifier comparison...\n")
t0_total <- Sys.time()

xgb_importance_full  <- list()
xgb_importance_nosec <- list()

for (s in seq_along(sector_levels)) {
  sec <- sector_levels[s]
  cat(sprintf("  Fold %d/%d (sector %s) ...", s, n_folds, sec))
  t0 <- Sys.time()

  train_idx <- which(panel$primary_nace2d != sec)
  test_idx  <- which(panel$primary_nace2d == sec)
  train <- panel[train_idx, ]
  test  <- panel[test_idx, ]

  # Set factor levels to full panel levels (critical for GAM sector RE)
  train$nace2d_f <- factor(train$nace2d, levels = all_nace2d_levels)
  test$nace2d_f  <- factor(test$nace2d,  levels = all_nace2d_levels)
  train$year_f   <- factor(train$year,   levels = all_year_levels)
  test$year_f    <- factor(test$year,    levels = all_year_levels)

  # ── 1. GAM baseline ──
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

  # ── 2. GAM enriched ──
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

  # ── 3. XGBoost full ──
  if (has_xgboost) {
    dtrain <- xgboost::xgb.DMatrix(X_tree_full[train_idx, ],
                                    label = panel$emit[train_idx])
    dtest  <- xgboost::xgb.DMatrix(X_tree_full[test_idx, ])

    fit <- xgboost::xgb.train(xgb_params, dtrain, nrounds = XGB_NROUNDS,
                               verbose = 0)
    panel$phat_xgb_full[test_idx] <- predict(fit, dtest)

    imp <- xgboost::xgb.importance(feature_names = tree_features_full,
                                    model = fit)
    imp$fold <- s
    xgb_importance_full[[s]] <- imp
  }

  # ── 4. XGBoost no-sector ──
  if (has_xgboost) {
    dtrain_ns <- xgboost::xgb.DMatrix(X_tree_nosec[train_idx, ],
                                       label = panel$emit[train_idx])
    dtest_ns  <- xgboost::xgb.DMatrix(X_tree_nosec[test_idx, ])

    fit_ns <- xgboost::xgb.train(xgb_params, dtrain_ns, nrounds = XGB_NROUNDS,
                                  verbose = 0)
    panel$phat_xgb_nosec[test_idx] <- predict(fit_ns, dtest_ns)

    imp_ns <- xgboost::xgb.importance(feature_names = tree_features_nosec,
                                       model = fit_ns)
    imp_ns$fold <- s
    xgb_importance_nosec[[s]] <- imp_ns
  }

  # ── 5. Random forest ──
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
  cat(sprintf(" %d obs, %.1fs\n", length(test_idx), elapsed))
}

elapsed_total <- round(difftime(Sys.time(), t0_total, units = "mins"), 1)
cat(sprintf("\nLOSOCV complete (%.1f min)\n\n", elapsed_total))


# ══════════════════════════════════════════════════════════════════════════════
# POST-CV ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════

# ── Classification metrics at threshold grid ──────────────────────────────────
THRESHOLDS <- seq(0.01, 0.99, by = 0.01)

compute_class_metrics <- function(y, phat, thresholds) {
  is_emit <- (y > 0)
  is_non  <- !is_emit
  n_emit  <- sum(is_emit)
  n_non   <- sum(is_non)

  tpr <- fpr <- numeric(length(thresholds))
  for (i in seq_along(thresholds)) {
    pred_pos <- (phat > thresholds[i])
    TP <- sum(pred_pos & is_emit)
    FP <- sum(pred_pos & is_non)
    tpr[i] <- if (n_emit > 0) TP / n_emit else NA_real_
    fpr[i] <- if (n_non > 0)  FP / n_non  else NA_real_
  }

  # AUC-ROC (trapezoidal rule)
  ord <- order(fpr, tpr)
  auc <- abs(sum(diff(fpr[ord]) * (tpr[ord][-1] + tpr[ord][-length(tpr[ord])]) / 2))

  list(
    df = data.frame(threshold = thresholds, TPR = tpr, FPR = fpr,
                    stringsAsFactors = FALSE),
    auc = auc
  )
}


# ── Evaluate each model ──────────────────────────────────────────────────────
summary_all <- list()

for (nm in model_names) {
  phat <- panel[[paste0("phat_", nm)]]
  ok   <- !is.na(phat) & !is.na(panel$y)

  if (sum(ok) == 0) {
    cat(sprintf("  %s: SKIPPED (no predictions)\n", nm))
    next
  }

  class_res <- compute_class_metrics(panel$y[ok], phat[ok], THRESHOLDS)
  auc <- class_res$auc

  # TPR >= 0.95 with minimum FPR
  df_c <- class_res$df
  cands_95 <- df_c[!is.na(df_c$TPR) & df_c$TPR >= 0.95, ]
  best_95 <- if (nrow(cands_95) > 0) cands_95[which.min(cands_95$FPR), ] else
    df_c[which.max(df_c$TPR - df_c$FPR), ]

  # Youden J
  df_c$J <- df_c$TPR - df_c$FPR
  best_J <- df_c[which.max(df_c$J), ]

  cat(sprintf("  %s: AUC=%.3f  Youden(thr=%.2f FPR=%.3f TPR=%.3f)  TPR95(thr=%.2f FPR=%.3f)\n",
              nm, auc,
              best_J$threshold, best_J$FPR, best_J$TPR,
              best_95$threshold, best_95$FPR, best_95$TPR))

  summary_all[[nm]] <- data.frame(
    model = nm, auc = auc,
    thr_J = best_J$threshold, FPR_J = best_J$FPR, TPR_J = best_J$TPR,
    thr_95 = best_95$threshold, FPR_95 = best_95$FPR, TPR_95 = best_95$TPR,
    stringsAsFactors = FALSE
  )

  # Save phat for this model
  write.csv(
    panel %>%
      filter(ok) %>%
      transmute(vat, nace2d, year, y, emit, phat = !!sym(paste0("phat_", nm))),
    file.path(OUT_DIR, sprintf("classifier_phat_%s.csv", nm)),
    row.names = FALSE
  )
}


# ── Feature importance (XGBoost) ─────────────────────────────────────────────
avg_importance <- function(imp_list, label) {
  if (length(imp_list) == 0) return(NULL)
  imp_all <- bind_rows(imp_list)
  imp_avg <- imp_all %>%
    group_by(Feature) %>%
    summarise(
      Gain_mean      = mean(Gain, na.rm = TRUE),
      Cover_mean     = mean(Cover, na.rm = TRUE),
      Frequency_mean = mean(Frequency, na.rm = TRUE),
      n_folds        = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(Gain_mean))
  cat(sprintf("\n── %s feature importance (top 15) ──\n", label))
  print(head(as.data.frame(imp_avg), 15), row.names = FALSE)
  imp_avg
}

imp_full  <- avg_importance(xgb_importance_full,  "XGBoost full")
imp_nosec <- avg_importance(xgb_importance_nosec, "XGBoost no-sector")


# ── Save summary ─────────────────────────────────────────────────────────────
summary_df <- bind_rows(summary_all)
write.csv(summary_df, file.path(OUT_DIR, "classifier_comparison.csv"), row.names = FALSE)

if (!is.null(imp_full)) {
  write.csv(imp_full, file.path(OUT_DIR, "classifier_feature_importance_full.csv"),
            row.names = FALSE)
}
if (!is.null(imp_nosec)) {
  write.csv(imp_nosec, file.path(OUT_DIR, "classifier_feature_importance_nosec.csv"),
            row.names = FALSE)
}

cat("\n── Classifier comparison (LOSOCV) ──\n")
print(summary_df %>% mutate(across(where(is.numeric), ~ round(., 3))), row.names = FALSE)
cat(sprintf("\nSaved to: %s\n", OUT_DIR))
