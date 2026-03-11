###############################################################################
# analysis/nested_cv/build_fold_specific_proxy.R
#
# PURPOSE
#   Nested cross-validation with elastic net proxy construction inside the CV
#   loop. For each of K_OUTER=5 sector-grouped folds:
#     1. Exclude held-out sectors from the elastic net training set
#     2. Re-run the elastic net on the remaining sectors
#     3. Extract supplier coefficients
#     4. Build the proxy for held-out firms using those coefficients
#
#   This produces leakage-free proxy values: each firm's proxy is computed
#   from an elastic net that never saw that firm's (or sector's) emissions.
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#
# OUTPUT
#   {PROC_DATA}/fold_specific_proxy.RData
#     Contains: fs_proxy_panel, sector_fold_map, fold_diagnostics,
#               fs_supplier_overlap, syt
#     fs_proxy_panel has two proxy columns:
#       fold_specific_proxy      — uses only positive EN coefficients
#       fold_specific_proxy_all  — uses all non-zero EN coefficients (pos + neg)
#
# RUNS ON: RMD (requires full B2B data)
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
library(Matrix)
library(glmnet)


# ── Parameters ───────────────────────────────────────────────────────────────
K_OUTER        <- 5L      # sector folds
K_INNER        <- 10L     # firm-grouped folds for cv.glmnet lambda tuning
MIN_LHS_BUYERS <- 5L
ALPHA          <- 0.5
SEED           <- 42L


# =============================================================================
# STEP 1: Load data and prepare LHS panel
# =============================================================================
cat("Loading B2B selected sample...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading LOOCV training sample...\n")
load(file.path(PROC_DATA, "loocv_training_sample.RData"))

lhs <- loocv_training_sample %>%
  filter(year >= 2005) %>%
  mutate(
    y = emissions,
    log_revenue = log(pmax(revenue, 1e-12))
  ) %>%
  select(vat, year, y, log_revenue, nace2d, euets) %>%
  arrange(vat, year)
rm(loocv_training_sample)

lhs$emit <- as.integer(lhs$y > 0)

n_na <- sum(is.na(lhs$y))
if (n_na > 0) {
  warning("Found ", n_na, " NA emissions in LHS panel — check upstream preprocessing.")
}
cat("LHS panel:", nrow(lhs), "firm-years,",
    length(unique(lhs$vat)), "unique firms\n")
cat("  Emitter firm-years:", sum(lhs$emit), "\n")
cat("  Remaining emissions NAs:", n_na, "\n\n")


# =============================================================================
# STEP 2: Assign firms to primary sector
# =============================================================================
cat("Assigning firms to primary NACE 2-digit sector...\n")

firm_sector <- lhs %>%
  group_by(vat) %>%
  summarise(primary_nace2d = names(which.max(table(nace2d))), .groups = "drop")

lhs <- lhs %>%
  left_join(firm_sector, by = "vat")

cat("Firms assigned:", n_distinct(lhs$vat), "\n")
cat("Sectors:", n_distinct(lhs$primary_nace2d), "\n\n")


# =============================================================================
# STEP 3: Assign sectors to K=5 folds (stratified by emitter firm-years)
# =============================================================================
cat("Assigning sectors to", K_OUTER, "folds...\n")

# Count emitter firm-years per primary sector
sector_emitter_fy <- lhs %>%
  filter(emit == 1) %>%
  count(primary_nace2d, name = "n_emitter_fy") %>%
  arrange(desc(n_emitter_fy))

# Include sectors with 0 emitters (NACE 19, 24 — confirmed zeros)
all_sectors <- sort(unique(lhs$primary_nace2d))
sector_emitter_fy <- data.frame(
  nace2d = all_sectors,
  stringsAsFactors = FALSE
) %>%
  left_join(sector_emitter_fy, by = c("nace2d" = "primary_nace2d")) %>%
  mutate(n_emitter_fy = coalesce(n_emitter_fy, 0L)) %>%
  arrange(desc(n_emitter_fy))

# Snake-order round-robin assignment
n_sec <- nrow(sector_emitter_fy)
fold_assignment <- integer(n_sec)
for (i in seq_len(n_sec)) {
  cycle_pos <- ((i - 1) %% (2 * K_OUTER))
  if (cycle_pos < K_OUTER) {
    fold_assignment[i] <- cycle_pos + 1L
  } else {
    fold_assignment[i] <- 2L * K_OUTER - cycle_pos
  }
}

sector_fold_map <- data.frame(
  nace2d = sector_emitter_fy$nace2d,
  fold_k = fold_assignment,
  n_emitter_fy = sector_emitter_fy$n_emitter_fy,
  stringsAsFactors = FALSE
)

# HARD CONSTRAINT: NACE 19 and 24 must be in different folds
fold_19 <- sector_fold_map$fold_k[sector_fold_map$nace2d == "19"]
fold_24 <- sector_fold_map$fold_k[sector_fold_map$nace2d == "24"]

if (length(fold_19) > 0 && length(fold_24) > 0 && fold_19 == fold_24) {
  cat("WARNING: NACE 19 and 24 assigned to same fold (", fold_19,
      "). Swapping NACE 24...\n")

  # Find best swap partner: a sector in a different fold with closest
  # emitter firm-year count to NACE 24
  fy_24 <- sector_fold_map$n_emitter_fy[sector_fold_map$nace2d == "24"]
  candidates <- sector_fold_map %>%
    filter(fold_k != fold_19, nace2d != "19") %>%
    mutate(fy_diff = abs(n_emitter_fy - fy_24)) %>%
    arrange(fy_diff)

  swap_nace <- candidates$nace2d[1]
  swap_fold <- candidates$fold_k[1]

  cat("  Swapping NACE 24 (fold ", fold_19, ") with NACE ", swap_nace,
      " (fold ", swap_fold, ")\n", sep = "")

  sector_fold_map$fold_k[sector_fold_map$nace2d == "24"]       <- swap_fold
  sector_fold_map$fold_k[sector_fold_map$nace2d == swap_nace]  <- fold_19
}

# Merge fold_k onto lhs via primary_nace2d
lhs <- lhs %>%
  left_join(sector_fold_map %>% select(nace2d, fold_k),
            by = c("primary_nace2d" = "nace2d"))

# Diagnostic: print fold assignment
cat("\n── Sector fold assignment ──\n")
cat(sprintf("%-6s  %6s  %6s\n", "NACE", "Fold", "EmitFY"))
cat(strrep("-", 22), "\n")
for (i in seq_len(nrow(sector_fold_map))) {
  r <- sector_fold_map[i, ]
  cat(sprintf("%-6s  %6d  %6d\n", r$nace2d, r$fold_k, r$n_emitter_fy))
}

fold_summary <- lhs %>%
  group_by(fold_k) %>%
  summarise(
    n_sectors  = n_distinct(primary_nace2d),
    n_firms    = n_distinct(vat),
    n_firmyears = n(),
    n_emitter_fy = sum(emit),
    .groups = "drop"
  )

cat("\n── Fold balance ──\n")
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
rm(b2b)  # free memory

cat("B2B transactions for LHS buyers:", nrow(b2b_lhs), "\n\n")


# =============================================================================
# STEP 5: Sector-year emission totals (for downstream calibration)
# =============================================================================
syt <- lhs %>%
  group_by(nace2d, year) %>%
  summarise(E_total = sum(y, na.rm = TRUE), n_full = n(), .groups = "drop")

cat("Sector-year cells:", nrow(syt), "\n")
cat("Cells with E_total > 0:", sum(syt$E_total > 0), "\n\n")


# =============================================================================
# STEP 6: Helper functions
# =============================================================================

# Rank-based AUC (same as build_loso_proxy.R)
compute_auc <- function(y_bin, score) {
  y_bin <- as.integer(y_bin)
  n1 <- sum(y_bin == 1L)
  n0 <- sum(y_bin == 0L)
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  r <- rank(score, ties.method = "average")
  (sum(r[y_bin == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# Extract supplier coefficients from cv.glmnet fit
# (adapted from run_elastic_net.R:51-64)
extract_suppliers <- function(fit, n_ctrl, eligible_sellers_k, s = "lambda.min") {
  co <- coef(fit, s = s)
  supplier_idx <- (n_ctrl + 2):length(co)
  vals <- co[supplier_idx]
  data.frame(
    vat_i_ano = eligible_sellers_k,
    coef      = as.numeric(vals),
    stringsAsFactors = FALSE
  ) %>%
    filter(coef != 0) %>%
    arrange(desc(abs(coef)))
}


# =============================================================================
# STEP 7: Pre-assign inner CV folds for cv.glmnet lambda tuning
# =============================================================================
# Each unique firm gets an inner fold assignment. Within each outer fold,
# we subset to training firms and use their pre-assigned inner folds.
set.seed(SEED)
all_unique_firms <- unique(lhs$vat)
inner_fold_map <- sample(rep(1:K_INNER, length.out = length(all_unique_firms)))
names(inner_fold_map) <- all_unique_firms

cat("Inner fold assignment: K_INNER =", K_INNER, "across",
    length(all_unique_firms), "unique firms\n\n")


# =============================================================================
# STEP 8: Main fold loop
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  NESTED CV: ", K_OUTER, " outer folds x elastic net (alpha=",
    ALPHA, ")\n", sep = "")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Storage for fold-specific proxies and diagnostics
proxy_pieces       <- list()
proxy_pieces_all   <- list()  # all non-zero coefs (pos + neg)
diag_rows          <- list()
supplier_lists     <- list()     # positive-coef suppliers per fold
supplier_coefs     <- list()     # positive-coef supplier data.frames per fold

for (k in seq_len(K_OUTER)) {
  cat(sprintf("\n══ OUTER FOLD %d / %d ═══════════════════════════════════════\n", k, K_OUTER))
  t0_fold <- Sys.time()

  held_out_sectors <- sector_fold_map$nace2d[sector_fold_map$fold_k == k]
  cat("Held-out sectors:", paste(held_out_sectors, collapse = ", "), "\n")

  # ── 8a. Subset LHS to training firms ──────────────────────────────────
  train_lhs <- lhs[lhs$fold_k != k, ]
  test_lhs  <- lhs[lhs$fold_k == k, ]

  cat("Training:", nrow(train_lhs), "firm-years,",
      n_distinct(train_lhs$vat), "firms,",
      sum(train_lhs$emit), "emitter FY\n")
  cat("Test:    ", nrow(test_lhs), "firm-years,",
      n_distinct(test_lhs$vat), "firms,",
      sum(test_lhs$emit), "emitter FY\n")

  # ── 8b. Identify eligible sellers from training buyers ────────────────
  cat("Identifying eligible sellers...\n")
  b2b_train_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% train_lhs$vat, ]

  seller_counts_k <- b2b_train_k %>%
    distinct(vat_i_ano, vat_j_ano) %>%
    count(vat_i_ano, name = "n_lhs_buyers")

  eligible_sellers_k <- seller_counts_k %>%
    filter(n_lhs_buyers >= MIN_LHS_BUYERS) %>%
    pull(vat_i_ano) %>%
    sort()

  cat("Eligible sellers (>=", MIN_LHS_BUYERS, "training buyers):",
      length(eligible_sellers_k), "\n")

  seller_map_k <- data.frame(
    vat_i_ano = eligible_sellers_k,
    col_idx   = seq_along(eligible_sellers_k),
    stringsAsFactors = FALSE
  )

  # ── 8c. Build sparse design matrix (pooled, asinh) ───────────────────
  cat("Building sparse design matrix...\n")

  # Add row indices to training LHS
  train_lhs$row_idx <- seq_len(nrow(train_lhs))

  # Aggregate B2B to (seller, buyer, year) for training buyers
  b2b_agg_k <- b2b_train_k %>%
    filter(vat_i_ano %in% eligible_sellers_k) %>%
    group_by(vat_i_ano, vat_j_ano, year) %>%
    summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
    filter(sales != 0)

  # Map to matrix indices
  b2b_agg_k <- b2b_agg_k %>%
    inner_join(train_lhs %>% select(vat, year, row_idx),
               by = c("vat_j_ano" = "vat", "year" = "year")) %>%
    inner_join(seller_map_k, by = "vat_i_ano")

  n_rows_k <- nrow(train_lhs)
  n_cols_k <- length(eligible_sellers_k)

  # asinh(sales) sparse matrix
  X_asinh_k <- sparseMatrix(
    i    = b2b_agg_k$row_idx,
    j    = b2b_agg_k$col_idx,
    x    = asinh(b2b_agg_k$sales),
    dims = c(n_rows_k, n_cols_k)
  )
  colnames(X_asinh_k) <- eligible_sellers_k

  rm(b2b_agg_k, b2b_train_k)

  cat("Supplier matrix:", n_rows_k, "x", n_cols_k, "\n")

  # Control variables: log_revenue + year dummies + sector dummies
  year_dummies_k <- model.matrix(~ factor(year), data = train_lhs)[, -1, drop = FALSE]
  colnames(year_dummies_k) <- paste0("yr_", sort(unique(train_lhs$year))[-1])

  # Sector dummies — only for sectors in training set
  train_sectors <- sort(unique(train_lhs$nace2d))
  sector_dummies_k <- model.matrix(~ factor(nace2d, levels = train_sectors),
                                   data = train_lhs)[, -1, drop = FALSE]
  colnames(sector_dummies_k) <- paste0("sec_", train_sectors[-1])

  X_controls_k <- cbind(log_revenue = train_lhs$log_revenue,
                         year_dummies_k,
                         sector_dummies_k)
  n_controls_k <- ncol(X_controls_k)

  cat("Controls:", n_controls_k, "columns (1 log_revenue +",
      ncol(year_dummies_k), "year +", ncol(sector_dummies_k), "sector)\n")

  # Combine into full design matrix
  X_controls_sparse_k <- Matrix(X_controls_k, sparse = TRUE)
  X_full_k <- cbind(X_controls_sparse_k, X_asinh_k)
  pf_k <- c(rep(0, n_controls_k), rep(1, n_cols_k))

  # Drop zero-variance columns
  col_var <- diff(X_full_k@p)  # nnz per column for sparse matrix; 0 nnz => 0 var
  # For dense controls, check more carefully
  # Simple approach: compute variance for each column
  col_means <- colMeans(X_full_k)
  col_sq_means <- colMeans(X_full_k^2)
  col_vars <- col_sq_means - col_means^2
  zero_var <- which(col_vars == 0)

  if (length(zero_var) > 0) {
    cat("Dropping", length(zero_var), "zero-variance columns\n")
    X_full_k <- X_full_k[, -zero_var]
    pf_k <- pf_k[-zero_var]
    # Update n_controls_k if any control columns were dropped
    n_controls_k <- sum(pf_k == 0)
    # Update eligible_sellers_k to match remaining supplier columns
    supplier_col_indices <- which(pf_k == 1)
    eligible_sellers_k_enet <- colnames(X_full_k)[supplier_col_indices]
  } else {
    eligible_sellers_k_enet <- eligible_sellers_k
  }

  rm(X_asinh_k, X_controls_k, X_controls_sparse_k,
     year_dummies_k, sector_dummies_k)

  cat("Full design matrix:", nrow(X_full_k), "x", ncol(X_full_k),
      "(", n_controls_k, "controls +", sum(pf_k == 1), "suppliers)\n")

  # ── 8d. Run cv.glmnet ────────────────────────────────────────────────
  cat("Running cv.glmnet (alpha =", ALPHA, ")...\n")

  # Inner fold assignment: subset pre-assigned inner folds to training firms
  train_firms_k <- unique(train_lhs$vat)
  inner_foldid_k <- unname(inner_fold_map[train_lhs$vat])

  t0_enet <- Sys.time()
  fit_k <- cv.glmnet(
    x              = X_full_k,
    y              = train_lhs$y,
    family         = "gaussian",
    alpha          = ALPHA,
    penalty.factor = pf_k,
    foldid         = inner_foldid_k,
    standardize    = TRUE
  )
  enet_time <- round(difftime(Sys.time(), t0_enet, units = "mins"), 1)

  # Store fit diagnostics before cleanup
  lambda_min_k <- fit_k$lambda.min
  cv_rmse_k    <- sqrt(min(fit_k$cvm))

  cat("  cv.glmnet time:", enet_time, "min\n")
  cat("  lambda.min:", signif(lambda_min_k, 4), "\n")
  cat("  CV RMSE (lambda.min):", round(cv_rmse_k, 2), "\n")

  rm(X_full_k)

  # ── 8e. Extract supplier coefficients ────────────────────────────────
  coef_lookup_k <- extract_suppliers(fit_k, n_controls_k,
                                      eligible_sellers_k_enet, "lambda.min")
  coef_pos_k <- coef_lookup_k %>% filter(coef > 0)

  n_selected_pos_k <- nrow(coef_pos_k)
  n_selected_neg_k <- sum(coef_lookup_k$coef < 0)

  cat("  Non-zero suppliers:", nrow(coef_lookup_k),
      "| positive:", n_selected_pos_k,
      "| negative:", n_selected_neg_k, "\n")

  rm(fit_k)

  # ── 8f. Build proxy for held-out firms ───────────────────────────────
  cat("Building proxy for held-out firms...\n")

  heldout_vats_k <- unique(test_lhs$vat)

  if (nrow(coef_pos_k) > 0) {
    b2b_heldout_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]

    proxy_k <- b2b_heldout_k %>%
      inner_join(coef_pos_k, by = "vat_i_ano") %>%
      group_by(vat_j_ano, year) %>%
      summarise(fold_specific_proxy = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                .groups = "drop") %>%
      rename(vat = vat_j_ano)

    rm(b2b_heldout_k)
  } else {
    cat("  WARNING: No positive-coefficient suppliers in fold", k, "\n")
    proxy_k <- data.frame(vat = character(0), year = integer(0),
                          fold_specific_proxy = numeric(0),
                          stringsAsFactors = FALSE)
  }

  # ── 8f-bis. Build proxy using ALL non-zero coefficients (pos + neg) ──
  if (nrow(coef_lookup_k) > 0) {
    b2b_heldout_all_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]

    proxy_all_k <- b2b_heldout_all_k %>%
      inner_join(coef_lookup_k, by = "vat_i_ano") %>%
      group_by(vat_j_ano, year) %>%
      summarise(fold_specific_proxy_all = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                .groups = "drop") %>%
      rename(vat = vat_j_ano)

    rm(b2b_heldout_all_k)
  } else {
    proxy_all_k <- data.frame(vat = character(0), year = integer(0),
                              fold_specific_proxy_all = numeric(0),
                              stringsAsFactors = FALSE)
  }

  # Merge onto test_lhs, coalesce missing to 0
  test_proxy_k <- test_lhs %>%
    select(vat, year, y, emit) %>%
    left_join(proxy_k, by = c("vat", "year")) %>%
    left_join(proxy_all_k, by = c("vat", "year")) %>%
    mutate(
      fold_specific_proxy     = coalesce(fold_specific_proxy, 0),
      fold_specific_proxy_all = coalesce(fold_specific_proxy_all, 0)
    )

  proxy_pieces[[k]]     <- test_proxy_k %>% select(vat, year, fold_specific_proxy)
  proxy_pieces_all[[k]] <- test_proxy_k %>% select(vat, year, fold_specific_proxy_all)

  cat("  Held-out firms with proxy > 0 (pos-only):", sum(test_proxy_k$fold_specific_proxy > 0),
      "/", nrow(test_proxy_k), "\n")
  cat("  Held-out firms with proxy_all != 0:", sum(test_proxy_k$fold_specific_proxy_all != 0),
      "/", nrow(test_proxy_k), "\n")

  # ── 8g. Store diagnostics ────────────────────────────────────────────
  fold_time <- round(difftime(Sys.time(), t0_fold, units = "mins"), 1)

  # Store selected supplier list for overlap analysis (before cleanup)
  supplier_lists[[k]] <- coef_pos_k$vat_i_ano
  supplier_coefs[[k]] <- coef_pos_k

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
  share_test_proxy_gt0 <- if (n_test_fy > 0) mean(test_proxy_k$fold_specific_proxy > 0) else NA_real_
  share_test_emitter_proxy_gt0 <- if (n_test_emitter_fy > 0) {
    mean(test_proxy_k$fold_specific_proxy[test_proxy_k$emit == 1L] > 0)
  } else {
    NA_real_
  }

  # Proxy quality on held-out fold
  cor_proxy_y_k <- if (sum(test_proxy_k$y > 0 & test_proxy_k$fold_specific_proxy > 0) > 5) {
    cor(test_proxy_k$y, test_proxy_k$fold_specific_proxy, use = "complete.obs")
  } else {
    NA_real_
  }

  auc_emit_k <- compute_auc(test_proxy_k$emit, test_proxy_k$fold_specific_proxy)

  cat("  Proxy > 0:", sum(test_proxy_k$fold_specific_proxy > 0), "/", n_test_fy, "\n")
  cat("  AUC:", round(auc_emit_k, 3), "\n")

  diag_rows[[k]] <- data.frame(
    fold_k                        = k,
    n_train_firms                 = n_distinct(train_lhs$vat),
    n_test_firms                  = n_distinct(test_lhs$vat),
    n_train_firmyears             = nrow(train_lhs),
    n_test_firmyears              = n_test_fy,
    n_train_emitter_fy            = sum(train_lhs$emit),
    n_test_emitter_fy             = n_test_emitter_fy,
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

  cat(sprintf("Fold %d complete (%.1f min)\n", k, fold_time))

  # Clean up fold-specific objects
  rm(train_lhs, test_lhs, coef_lookup_k, coef_pos_k, proxy_k, proxy_all_k,
     test_proxy_k, eligible_sellers_k, eligible_sellers_k_enet,
     seller_map_k, seller_counts_k, pf_k, inner_foldid_k,
     train_firms_k, heldout_vats_k,
     lambda_min_k, cv_rmse_k, n_selected_pos_k, n_selected_neg_k,
     coef_pos_mean_k, coef_pos_median_k, coef_pos_sd_k,
     n_test_fy, n_test_emitter_fy, share_test_proxy_gt0,
     share_test_emitter_proxy_gt0, cor_proxy_y_k, auc_emit_k)
  gc()
}


# =============================================================================
# STEP 9: Assemble output
# =============================================================================
cat("\n═══ Assembling output ═══\n")

# Combine all fold-specific proxies
all_proxies     <- bind_rows(proxy_pieces)
all_proxies_all <- bind_rows(proxy_pieces_all)

# Merge onto full lhs panel
fs_proxy_panel <- lhs %>%
  left_join(all_proxies, by = c("vat", "year")) %>%
  left_join(all_proxies_all, by = c("vat", "year")) %>%
  mutate(
    fold_specific_proxy     = coalesce(fold_specific_proxy, 0),
    fold_specific_proxy_all = coalesce(fold_specific_proxy_all, 0)
  )

cat("Nested CV proxy assembled:", nrow(fs_proxy_panel), "rows\n")
cat("  fold_specific_proxy > 0 (pos-only):", sum(fs_proxy_panel$fold_specific_proxy > 0),
    sprintf("(%.1f%%)\n", 100 * mean(fs_proxy_panel$fold_specific_proxy > 0)))
cat("  fold_specific_proxy_all != 0 (all coefs):", sum(fs_proxy_panel$fold_specific_proxy_all != 0),
    sprintf("(%.1f%%)\n", 100 * mean(fs_proxy_panel$fold_specific_proxy_all != 0)))

# Fold diagnostics
fold_diagnostics <- bind_rows(diag_rows)

cat("\n── Fold diagnostics ──\n")
print(fold_diagnostics)

# Supplier overlap across folds (Jaccard index)
cat("\n── Supplier overlap (Jaccard index) ──\n")

# Pairwise Jaccard matrix
jaccard_matrix <- matrix(NA_real_, nrow = K_OUTER, ncol = K_OUTER,
                          dimnames = list(paste0("Fold", 1:K_OUTER),
                                         paste0("Fold", 1:K_OUTER)))
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

# Print Jaccard matrix
cat(sprintf("%-12s", ""))
for (j in seq_len(K_OUTER)) cat(sprintf("  Fold %d", j))
cat("\n")
for (i in seq_len(K_OUTER)) {
  cat(sprintf("  Fold %d    ", i))
  for (j in seq_len(K_OUTER)) {
    cat(sprintf("  %5.3f", jaccard_matrix[i, j]))
  }
  cat("\n")
}

cat("\nSuppliers selected per fold:",
    paste(sapply(supplier_lists, length), collapse = ", "), "\n")

# Suppliers in all folds vs any fold
all_folds_suppliers <- Reduce(intersect, supplier_lists)
any_fold_suppliers  <- Reduce(union, supplier_lists)
cat("Suppliers in ALL folds:", length(all_folds_suppliers), "\n")
cat("Suppliers in ANY fold:", length(any_fold_suppliers), "\n")

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

# Save as named list (parallel to loso_supplier_overlap)
fs_supplier_overlap <- list(
  n_in_all_folds    = length(all_folds_suppliers),
  n_in_any_fold     = length(any_fold_suppliers),
  jaccard_matrix    = jaccard_matrix,
  top10_jaccard_avg = top10_jaccard_avg
)


# =============================================================================
# STEP 10: Save
# =============================================================================
# Select output columns
fs_proxy_panel <- fs_proxy_panel %>%
  select(vat, year, nace2d, y, emit, log_revenue, euets,
         primary_nace2d, fold_k, fold_specific_proxy, fold_specific_proxy_all)

OUT_PATH <- file.path(PROC_DATA, "fold_specific_proxy.RData")

save(fs_proxy_panel, sector_fold_map, fold_diagnostics,
     fs_supplier_overlap, syt,
     file = OUT_PATH)

cat("\n══════════════════════════════════════════════\n")
cat("Saved to:", OUT_PATH, "\n")
cat("  fs_proxy_panel:        ", nrow(fs_proxy_panel), "rows x",
    ncol(fs_proxy_panel), "cols\n")
cat("  sector_fold_map:       ", nrow(sector_fold_map), "rows\n")
cat("  fold_diagnostics:      ", nrow(fold_diagnostics), "rows\n")
cat("  fs_supplier_overlap:    list with", length(fs_supplier_overlap), "elements\n")
cat("  syt:                   ", nrow(syt), "rows\n")
cat("══════════════════════════════════════════════\n")
