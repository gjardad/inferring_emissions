###############################################################################
# analysis/active/build_firmfoldcv_proxy_asinh.R
#
# PURPOSE
#   Identical to build_firmfoldcv_proxy.R but with asinh(y) on LHS.
#   The elastic net is trained on asinh(emissions) instead of raw emissions.
#   All diagnostics (AUC, correlations) are computed against original y
#   so results are comparable with the levels-LHS variant.
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/training_sample.RData
#
# OUTPUT
#   {PROC_DATA}/firmfoldcv_proxy_asinh.RData
#     Contains: firmfoldcv_proxy_panel_asinh, firmfoldcv_fold_map_asinh,
#               firmfoldcv_fold_diagnostics_asinh, firmfoldcv_supplier_overlap_asinh,
#               syt_asinh
#
# RUNS ON: RMD (requires full B2B data)
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(Matrix)
library(glmnet)

# Try to set up parallel backend for cv.glmnet
USE_PARALLEL <- FALSE
n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 2L)
if (n_cores > 1L && requireNamespace("doParallel", quietly = TRUE)) {
  doParallel::registerDoParallel(cores = n_cores)
  USE_PARALLEL <- TRUE
  cat("Parallel backend registered with", n_cores, "cores\n")
} else {
  cat("Running sequentially (doParallel not available or single core)\n")
}


# ── Parameters ───────────────────────────────────────────────────────────────
K_OUTER        <- 10L     # firm-level folds
K_INNER        <- 10L     # folds for cv.glmnet lambda tuning
MIN_LHS_BUYERS <- 5L
ALPHA          <- 0.5
SEED           <- 42L


# ── Helper: rank-based AUC ──────────────────────────────────────────────────
compute_auc <- function(y_bin, score) {
  y_bin <- as.integer(y_bin)
  n1 <- sum(y_bin == 1L)
  n0 <- sum(y_bin == 0L)
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  r <- rank(score, ties.method = "average")
  (sum(r[y_bin == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}


# =============================================================================
# STEP 1: Load data and prepare LHS panel
# =============================================================================
cat("Loading B2B selected sample...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading training sample...\n")
load(file.path(PROC_DATA, "training_sample.RData"))

lhs <- training_sample %>%
  filter(year >= 2005) %>%
  mutate(
    y = emissions,
    log_revenue = log(pmax(revenue, 1e-12))
  ) %>%
  select(vat, year, y, log_revenue, nace2d, euets) %>%
  arrange(vat, year)
rm(training_sample)

lhs$emit <- as.integer(lhs$y > 0)

cat("LHS panel:", nrow(lhs), "firm-years,",
    length(unique(lhs$vat)), "unique firms\n")
cat("  Emitter firm-years:", sum(lhs$emit), "\n\n")


# =============================================================================
# STEP 2: Assign firms to primary sector
# =============================================================================
firm_sector <- lhs %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

lhs <- lhs %>% left_join(firm_sector, by = "vat")


# =============================================================================
# STEP 3: Stratified firm-level fold assignment
# =============================================================================
cat("Assigning firms to", K_OUTER, "folds (stratified by sector)...\n")

set.seed(SEED)

# Within each sector, randomly assign firms to folds
firmfoldcv_fold_map_asinh <- firm_sector %>%
  group_by(primary_nace2d) %>%
  mutate(
    n_in_sector = n(),
    fold_k = sample(rep(1:K_OUTER, length.out = n()))
  ) %>%
  ungroup() %>%
  select(vat, primary_nace2d, fold_k)

# Merge onto lhs
lhs <- lhs %>%
  left_join(firmfoldcv_fold_map_asinh %>% select(vat, fold_k), by = "vat")

# Diagnostic: fold balance
fold_summary <- lhs %>%
  group_by(fold_k) %>%
  summarise(
    n_sectors   = n_distinct(primary_nace2d),
    n_firms     = n_distinct(vat),
    n_firmyears = n(),
    n_emitter_fy = sum(emit),
    .groups = "drop"
  )

cat("\n-- Fold balance --\n")
cat(sprintf("%-6s  %8s  %8s  %10s  %10s\n",
            "Fold", "Sectors", "Firms", "FirmYears", "EmitFY"))
cat(strrep("-", 48), "\n")
for (i in seq_len(nrow(fold_summary))) {
  r <- fold_summary[i, ]
  cat(sprintf("%-6d  %8d  %8d  %10d  %10d\n",
              r$fold_k, r$n_sectors, r$n_firms, r$n_firmyears, r$n_emitter_fy))
}
cat("\n")


# =============================================================================
# STEP 4: Precompute B2B for LHS buyers
# =============================================================================
cat("Filtering B2B to LHS buyers, years >= 2005...\n")
b2b_lhs <- b2b %>%
  filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)

cat("B2B transactions for LHS buyers:", nrow(b2b_lhs), "\n\n")


# =============================================================================
# STEP 5: Sector-year emission totals
# =============================================================================
syt_asinh <- lhs %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")


# =============================================================================
# STEP 6: Pre-assign inner CV folds
# =============================================================================
set.seed(SEED + 1L)  # different seed from outer folds
all_unique_firms <- unique(lhs$vat)
inner_fold_map <- sample(rep(1:K_INNER, length.out = length(all_unique_firms)))
names(inner_fold_map) <- all_unique_firms


# =============================================================================
# STEP 7: Helper function
# =============================================================================
extract_suppliers <- function(fit, n_ctrl, eligible_sellers_k, s = "lambda.min") {
  co <- coef(fit, s = s)
  supplier_idx <- (n_ctrl + 2):length(co)
  data.frame(
    vat_i_ano = eligible_sellers_k,
    coef      = as.numeric(co[supplier_idx]),
    stringsAsFactors = FALSE
  ) %>%
    filter(coef != 0) %>%
    arrange(desc(abs(coef)))
}


# =============================================================================
# STEP 8: Main firm-fold CV loop
# =============================================================================
cat("=== FIRM-FOLD CV (asinh LHS): K=", K_OUTER, " folds x elastic net ===\n\n", sep = "")

proxy_pieces     <- list()
proxy_pieces_all <- list()
diag_rows        <- list()
supplier_lists   <- list()
supplier_coefs   <- list()

for (k in seq_len(K_OUTER)) {
  cat(sprintf("\n== FOLD %d / %d ==\n", k, K_OUTER))
  t0_fold <- Sys.time()

  # ── Subset ──────────────────────────────────────────────────────────────
  train_lhs <- lhs[lhs$fold_k != k, ]
  test_lhs  <- lhs[lhs$fold_k == k, ]

  cat("Training:", nrow(train_lhs), "FY,", n_distinct(train_lhs$vat), "firms,",
      sum(train_lhs$emit), "emitter FY\n")
  cat("Test:    ", nrow(test_lhs), "FY,", n_distinct(test_lhs$vat), "firms,",
      sum(test_lhs$emit), "emitter FY\n")

  # Report sector overlap
  train_sectors <- unique(train_lhs$primary_nace2d)
  test_sectors  <- unique(test_lhs$primary_nace2d)
  cat("Test sectors in training:", sum(test_sectors %in% train_sectors),
      "/", length(test_sectors), "\n")

  # ── Eligible sellers ────────────────────────────────────────────────────
  b2b_train_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% train_lhs$vat, ]

  seller_counts_k <- b2b_train_k %>%
    distinct(vat_i_ano, vat_j_ano) %>%
    count(vat_i_ano, name = "n_lhs_buyers")

  eligible_sellers_k <- seller_counts_k %>%
    filter(n_lhs_buyers >= MIN_LHS_BUYERS) %>%
    pull(vat_i_ano) %>%
    sort()

  cat("Eligible sellers:", length(eligible_sellers_k), "\n")

  seller_map_k <- data.frame(
    vat_i_ano = eligible_sellers_k,
    col_idx   = seq_along(eligible_sellers_k),
    stringsAsFactors = FALSE
  )

  # ── Design matrix ───────────────────────────────────────────────────────
  train_lhs$row_idx <- seq_len(nrow(train_lhs))

  b2b_agg_k <- b2b_train_k %>%
    filter(vat_i_ano %in% eligible_sellers_k) %>%
    group_by(vat_i_ano, vat_j_ano, year) %>%
    summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
    filter(sales != 0) %>%
    inner_join(train_lhs %>% select(vat, year, row_idx),
               by = c("vat_j_ano" = "vat", "year" = "year")) %>%
    inner_join(seller_map_k, by = "vat_i_ano")

  n_rows_k <- nrow(train_lhs)
  n_cols_k <- length(eligible_sellers_k)

  X_asinh_k <- sparseMatrix(
    i = b2b_agg_k$row_idx, j = b2b_agg_k$col_idx,
    x = asinh(b2b_agg_k$sales), dims = c(n_rows_k, n_cols_k)
  )
  colnames(X_asinh_k) <- eligible_sellers_k
  rm(b2b_agg_k, b2b_train_k)

  # Controls: log_revenue + year FE + sector FE
  year_dummies_k <- model.matrix(~ factor(year), data = train_lhs)[, -1, drop = FALSE]
  colnames(year_dummies_k) <- paste0("yr_", sort(unique(train_lhs$year))[-1])

  train_sectors_k <- sort(unique(train_lhs$nace2d))
  sector_dummies_k <- model.matrix(~ factor(nace2d, levels = train_sectors_k),
                                   data = train_lhs)[, -1, drop = FALSE]
  colnames(sector_dummies_k) <- paste0("sec_", train_sectors_k[-1])

  X_controls_k <- cbind(log_revenue = train_lhs$log_revenue,
                         year_dummies_k, sector_dummies_k)
  n_controls_k <- ncol(X_controls_k)

  X_full_k <- cbind(Matrix(X_controls_k, sparse = TRUE), X_asinh_k)
  pf_k <- c(rep(0, n_controls_k), rep(1, n_cols_k))

  # Drop zero-variance columns
  col_means <- colMeans(X_full_k)
  col_sq_means <- colMeans(X_full_k^2)
  col_vars <- col_sq_means - col_means^2
  zero_var <- which(col_vars == 0)

  if (length(zero_var) > 0) {
    X_full_k <- X_full_k[, -zero_var]
    pf_k <- pf_k[-zero_var]
    n_controls_k <- sum(pf_k == 0)
    eligible_sellers_k_enet <- colnames(X_full_k)[which(pf_k == 1)]
  } else {
    eligible_sellers_k_enet <- eligible_sellers_k
  }

  rm(X_asinh_k, X_controls_k, year_dummies_k, sector_dummies_k)

  cat("Design matrix:", nrow(X_full_k), "x", ncol(X_full_k), "\n")

  # ── cv.glmnet with asinh(y) on LHS ──────────────────────────────────────
  inner_foldid_k <- unname(inner_fold_map[train_lhs$vat])

  t0_enet <- Sys.time()
  fit_k <- cv.glmnet(
    x = X_full_k, y = asinh(train_lhs$y),
    family = "gaussian", alpha = ALPHA,
    penalty.factor = pf_k, foldid = inner_foldid_k,
    standardize = TRUE,
    parallel = USE_PARALLEL
  )
  enet_time <- round(difftime(Sys.time(), t0_enet, units = "mins"), 1)

  lambda_min_k <- fit_k$lambda.min
  cv_rmse_k <- sqrt(min(fit_k$cvm))

  cat("  lambda.min:", signif(lambda_min_k, 4),
      "| CV RMSE (asinh scale):", round(cv_rmse_k, 2),
      "| time:", enet_time, "min\n")

  rm(X_full_k)

  # ── Extract suppliers ───────────────────────────────────────────────────
  coef_lookup_k <- extract_suppliers(fit_k, n_controls_k,
                                      eligible_sellers_k_enet, "lambda.min")
  coef_pos_k <- coef_lookup_k %>% filter(coef > 0)

  n_selected_pos_k <- nrow(coef_pos_k)
  n_selected_neg_k <- sum(coef_lookup_k$coef < 0)

  cat("  Selected suppliers: pos=", n_selected_pos_k,
      ", neg=", n_selected_neg_k, "\n")

  rm(fit_k)

  # Store supplier lists for overlap analysis
  supplier_lists[[k]] <- coef_pos_k$vat_i_ano
  supplier_coefs[[k]] <- coef_pos_k

  # ── Build proxy for held-out firms ──────────────────────────────────────
  heldout_vats_k <- unique(test_lhs$vat)

  # Positive-only proxy
  if (n_selected_pos_k > 0) {
    b2b_heldout_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]
    proxy_k <- b2b_heldout_k %>%
      inner_join(coef_pos_k, by = "vat_i_ano") %>%
      group_by(vat_j_ano, year) %>%
      summarise(firmfoldcv_proxy_asinh = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                .groups = "drop") %>%
      rename(vat = vat_j_ano)
    rm(b2b_heldout_k)
  } else {
    proxy_k <- data.frame(vat = character(0), year = integer(0),
                          firmfoldcv_proxy_asinh = numeric(0))
  }

  # All-coefficient proxy
  if (nrow(coef_lookup_k) > 0) {
    b2b_heldout_all_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]
    proxy_all_k <- b2b_heldout_all_k %>%
      inner_join(coef_lookup_k, by = "vat_i_ano") %>%
      group_by(vat_j_ano, year) %>%
      summarise(firmfoldcv_proxy_all_asinh = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                .groups = "drop") %>%
      rename(vat = vat_j_ano)
    rm(b2b_heldout_all_k)
  } else {
    proxy_all_k <- data.frame(vat = character(0), year = integer(0),
                              firmfoldcv_proxy_all_asinh = numeric(0))
  }

  # Merge
  test_proxy_k <- test_lhs %>%
    select(vat, year, y, emit) %>%
    left_join(proxy_k, by = c("vat", "year")) %>%
    left_join(proxy_all_k, by = c("vat", "year")) %>%
    mutate(
      firmfoldcv_proxy_asinh     = coalesce(firmfoldcv_proxy_asinh, 0),
      firmfoldcv_proxy_all_asinh = coalesce(firmfoldcv_proxy_all_asinh, 0)
    )

  proxy_pieces[[k]]     <- test_proxy_k %>% select(vat, year, firmfoldcv_proxy_asinh)
  proxy_pieces_all[[k]] <- test_proxy_k %>% select(vat, year, firmfoldcv_proxy_all_asinh)

  # ── Diagnostics (computed against original y, not asinh(y)) ─────────────
  fold_time <- round(difftime(Sys.time(), t0_fold, units = "mins"), 1)

  # Coefficient distribution (positive only)
  if (n_selected_pos_k > 0) {
    coef_pos_mean_k   <- mean(coef_pos_k$coef)
    coef_pos_median_k <- median(coef_pos_k$coef)
    coef_pos_sd_k     <- if (n_selected_pos_k > 1) sd(coef_pos_k$coef) else NA_real_
  } else {
    coef_pos_mean_k <- coef_pos_median_k <- coef_pos_sd_k <- NA_real_
  }

  # Proxy coverage
  n_test_fy <- nrow(test_proxy_k)
  n_test_emitter_fy <- sum(test_proxy_k$emit)
  share_test_proxy_gt0 <- if (n_test_fy > 0) mean(test_proxy_k$firmfoldcv_proxy_asinh > 0) else NA_real_
  share_test_emitter_proxy_gt0 <- if (n_test_emitter_fy > 0) {
    mean(test_proxy_k$firmfoldcv_proxy_asinh[test_proxy_k$emit == 1L] > 0)
  } else {
    NA_real_
  }

  # Proxy quality on held-out fold (against original y)
  cor_proxy_y_k <- if (sum(test_proxy_k$y > 0 & test_proxy_k$firmfoldcv_proxy_asinh > 0) > 5) {
    cor(test_proxy_k$y, test_proxy_k$firmfoldcv_proxy_asinh, use = "complete.obs")
  } else {
    NA_real_
  }

  auc_emit_k <- compute_auc(test_proxy_k$emit, test_proxy_k$firmfoldcv_proxy_asinh)

  cat("  Proxy > 0:", sum(test_proxy_k$firmfoldcv_proxy_asinh > 0), "/", n_test_fy, "\n")
  cat("  AUC:", round(auc_emit_k, 3), "\n")

  diag_rows[[k]] <- data.frame(
    fold_k                        = k,
    n_train_firms                 = n_distinct(train_lhs$vat),
    n_test_firms                  = n_distinct(test_lhs$vat),
    n_train_firmyears             = nrow(train_lhs),
    n_test_firmyears              = n_test_fy,
    n_train_emitter_fy            = sum(train_lhs$emit),
    n_test_emitter_fy             = n_test_emitter_fy,
    n_train_sectors               = length(train_sectors),
    n_test_sectors                = length(test_sectors),
    n_test_sectors_in_train       = sum(test_sectors %in% train_sectors),
    n_eligible_sellers            = length(eligible_sellers_k),
    n_selected_pos                = n_selected_pos_k,
    n_selected_neg                = n_selected_neg_k,
    lambda_min                    = lambda_min_k,
    cv_rmse                       = cv_rmse_k,
    coef_pos_mean                 = coef_pos_mean_k,
    coef_pos_median               = coef_pos_median_k,
    coef_pos_sd                   = coef_pos_sd_k,
    share_test_firms_proxy_gt0    = share_test_proxy_gt0,
    share_test_emitters_proxy_gt0 = share_test_emitter_proxy_gt0,
    cor_proxy_y                   = cor_proxy_y_k,
    auc_emit                      = auc_emit_k,
    runtime_min                   = as.numeric(fold_time),
    stringsAsFactors              = FALSE
  )

  cat(sprintf("  Done (%.1f min)\n", fold_time))

  rm(train_lhs, test_lhs, coef_lookup_k, coef_pos_k, proxy_k, proxy_all_k,
     test_proxy_k, eligible_sellers_k, eligible_sellers_k_enet,
     seller_map_k, seller_counts_k, pf_k, inner_foldid_k, heldout_vats_k,
     lambda_min_k, cv_rmse_k, n_selected_pos_k, n_selected_neg_k)
  gc()
}


# =============================================================================
# STEP 9: Assemble output
# =============================================================================
cat("\n=== Assembling output ===\n")

all_proxies     <- bind_rows(proxy_pieces)
all_proxies_all <- bind_rows(proxy_pieces_all)

firmfoldcv_proxy_panel_asinh <- lhs %>%
  left_join(all_proxies, by = c("vat", "year")) %>%
  left_join(all_proxies_all, by = c("vat", "year")) %>%
  mutate(
    firmfoldcv_proxy_asinh     = coalesce(firmfoldcv_proxy_asinh, 0),
    firmfoldcv_proxy_all_asinh = coalesce(firmfoldcv_proxy_all_asinh, 0)
  ) %>%
  select(vat, year, nace2d, y, emit, log_revenue, euets,
         primary_nace2d, fold_k, firmfoldcv_proxy_asinh, firmfoldcv_proxy_all_asinh)

firmfoldcv_fold_diagnostics_asinh <- bind_rows(diag_rows)

cat("Firm-fold CV proxy panel (asinh LHS):", nrow(firmfoldcv_proxy_panel_asinh), "rows\n")
cat("  firmfoldcv_proxy_asinh > 0:", sum(firmfoldcv_proxy_panel_asinh$firmfoldcv_proxy_asinh > 0),
    sprintf("(%.1f%%)\n", 100 * mean(firmfoldcv_proxy_panel_asinh$firmfoldcv_proxy_asinh > 0)))


# =============================================================================
# STEP 10: Supplier overlap analysis
# =============================================================================
cat("\n-- Supplier overlap analysis --\n")

all_supplier_sets <- supplier_lists[sapply(supplier_lists, length) > 0]
n_folds_with_suppliers <- length(all_supplier_sets)

if (n_folds_with_suppliers > 1) {
  supplier_union <- Reduce(union, all_supplier_sets)
  supplier_intersection <- Reduce(intersect, all_supplier_sets)
} else if (n_folds_with_suppliers == 1) {
  supplier_union <- all_supplier_sets[[1]]
  supplier_intersection <- all_supplier_sets[[1]]
} else {
  supplier_union <- character(0)
  supplier_intersection <- character(0)
}

cat("Suppliers in all folds (intersection):", length(supplier_intersection), "\n")
cat("Suppliers in any fold (union):", length(supplier_union), "\n")

# Pairwise Jaccard matrix
jaccard_matrix <- matrix(NA_real_, nrow = K_OUTER, ncol = K_OUTER,
                          dimnames = list(paste0("fold_", 1:K_OUTER),
                                          paste0("fold_", 1:K_OUTER)))
for (i in seq_len(K_OUTER)) {
  for (j in seq_len(K_OUTER)) {
    si <- supplier_lists[[i]]
    sj <- supplier_lists[[j]]
    union_ij <- length(union(si, sj))
    if (union_ij > 0) {
      jaccard_matrix[i, j] <- length(intersect(si, sj)) / union_ij
    } else {
      jaccard_matrix[i, j] <- NA_real_
    }
  }
}

# Top-10 overlap (by |coef|, averaged across all fold pairs)
top10_overlaps <- c()
for (i in seq_len(K_OUTER - 1)) {
  for (j in (i + 1):K_OUTER) {
    top_i <- head(supplier_coefs[[i]]$vat_i_ano, 10)
    top_j <- head(supplier_coefs[[j]]$vat_i_ano, 10)
    if (length(top_i) > 0 && length(top_j) > 0) {
      top10_overlaps <- c(top10_overlaps,
                          length(intersect(top_i, top_j)) / length(union(top_i, top_j)))
    }
  }
}
top10_jaccard_avg <- if (length(top10_overlaps) > 0) mean(top10_overlaps) else NA_real_

cat("Average top-10 Jaccard across fold pairs:", round(top10_jaccard_avg, 3), "\n")

firmfoldcv_supplier_overlap_asinh <- list(
  n_in_all_folds    = length(supplier_intersection),
  n_in_any_fold     = length(supplier_union),
  jaccard_matrix    = jaccard_matrix,
  top10_jaccard_avg = top10_jaccard_avg
)


# =============================================================================
# STEP 11: Print diagnostics
# =============================================================================
cat("\n-- Fold diagnostics --\n")
print(firmfoldcv_fold_diagnostics_asinh[, c("fold_k", "n_test_firms",
                                       "n_test_sectors_in_train",
                                       "n_selected_pos", "cv_rmse",
                                       "share_test_firms_proxy_gt0",
                                       "cor_proxy_y", "auc_emit")])


# =============================================================================
# STEP 12: Save
# =============================================================================
OUT_PATH <- file.path(PROC_DATA, "firmfoldcv_proxy_asinh.RData")
save(firmfoldcv_proxy_panel_asinh, firmfoldcv_fold_map_asinh,
     firmfoldcv_fold_diagnostics_asinh, firmfoldcv_supplier_overlap_asinh, syt_asinh,
     file = OUT_PATH)

cat("\n==============================================\n")
cat("Saved to:", OUT_PATH, "\n")
cat("  firmfoldcv_proxy_panel_asinh:       ", nrow(firmfoldcv_proxy_panel_asinh), "rows\n")
cat("  firmfoldcv_fold_map_asinh:          ", nrow(firmfoldcv_fold_map_asinh), "rows\n")
cat("  firmfoldcv_fold_diagnostics_asinh:  ", nrow(firmfoldcv_fold_diagnostics_asinh), "rows\n")
cat("  firmfoldcv_supplier_overlap_asinh:   list with", length(firmfoldcv_supplier_overlap_asinh), "elements\n")
cat("  syt_asinh:                          ", nrow(syt_asinh), "rows\n")
cat("==============================================\n")

# Clean up parallel backend
if (USE_PARALLEL) {
  doParallel::stopImplicitCluster()
  cat("Parallel backend stopped.\n")
}
