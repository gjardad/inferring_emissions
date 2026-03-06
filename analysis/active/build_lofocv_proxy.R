###############################################################################
# analysis/active/build_lofocv_proxy.R
#
# PURPOSE
#   Leave-firms-out cross-validation (K=10, stratified by sector) for the
#   elastic net proxy. For each of K folds:
#     1. Hold out a random subset of firms (stratified across sectors)
#     2. Re-run the elastic net on the remaining firms
#     3. Build the proxy for held-out firms using those coefficients
#
#   Unlike LOSOCV, firms from the same sector can be in both training and
#   test sets. This means the EN can learn sector-specific suppliers from
#   same-sector peers, which should improve proxy quality.
#
#   Fold assignment is stratified by primary_nace2d: within each sector,
#   firms are randomly split across folds proportionally. This ensures
#   every fold's test set contains firms from multiple sectors.
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#
# OUTPUT
#   {PROC_DATA}/lofocv_proxy.RData
#     Contains: lofocv_proxy_panel, lofocv_fold_map, lofocv_fold_diagnostics, syt
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


# ── Parameters ───────────────────────────────────────────────────────────────
K_OUTER        <- 10L     # firm-level folds
K_INNER        <- 10L     # folds for cv.glmnet lambda tuning
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
lofocv_fold_map <- firm_sector %>%
  group_by(primary_nace2d) %>%
  mutate(
    n_in_sector = n(),
    fold_k = sample(rep(1:K_OUTER, length.out = n()))
  ) %>%
  ungroup() %>%
  select(vat, primary_nace2d, fold_k)

# Merge onto lhs
lhs <- lhs %>%
  left_join(lofocv_fold_map %>% select(vat, fold_k), by = "vat")

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
rm(b2b)

cat("B2B transactions for LHS buyers:", nrow(b2b_lhs), "\n\n")


# =============================================================================
# STEP 5: Sector-year emission totals
# =============================================================================
syt <- lhs %>%
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
# STEP 8: Main LOFOCV loop
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  LEAVE-FIRMS-OUT CV: K=", K_OUTER, " folds x elastic net\n", sep = "")
cat("═══════════════════════════════════════════════════════════════\n\n")

proxy_pieces     <- list()
proxy_pieces_all <- list()
diag_rows        <- list()

for (k in seq_len(K_OUTER)) {
  cat(sprintf("\n══ FOLD %d / %d ══\n", k, K_OUTER))
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

  # ── cv.glmnet ───────────────────────────────────────────────────────────
  inner_foldid_k <- unname(inner_fold_map[train_lhs$vat])

  t0_enet <- Sys.time()
  fit_k <- cv.glmnet(
    x = X_full_k, y = train_lhs$y,
    family = "gaussian", alpha = ALPHA,
    penalty.factor = pf_k, foldid = inner_foldid_k,
    standardize = TRUE
  )
  enet_time <- round(difftime(Sys.time(), t0_enet, units = "mins"), 1)

  lambda_min_k <- fit_k$lambda.min
  cv_rmse_k <- sqrt(min(fit_k$cvm))

  cat("  lambda.min:", signif(lambda_min_k, 4),
      "| CV RMSE:", round(cv_rmse_k, 2),
      "| time:", enet_time, "min\n")

  rm(X_full_k)

  # ── Extract suppliers ───────────────────────────────────────────────────
  coef_lookup_k <- extract_suppliers(fit_k, n_controls_k,
                                      eligible_sellers_k_enet, "lambda.min")
  coef_pos_k <- coef_lookup_k %>% filter(coef > 0)

  cat("  Selected suppliers: pos=", nrow(coef_pos_k),
      ", neg=", sum(coef_lookup_k$coef < 0), "\n")

  rm(fit_k)

  # ── Build proxy for held-out firms ──────────────────────────────────────
  heldout_vats_k <- unique(test_lhs$vat)

  # Positive-only proxy
  if (nrow(coef_pos_k) > 0) {
    b2b_heldout_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]
    proxy_k <- b2b_heldout_k %>%
      inner_join(coef_pos_k, by = "vat_i_ano") %>%
      group_by(vat_j_ano, year) %>%
      summarise(lofocv_proxy = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                .groups = "drop") %>%
      rename(vat = vat_j_ano)
    rm(b2b_heldout_k)
  } else {
    proxy_k <- data.frame(vat = character(0), year = integer(0),
                          lofocv_proxy = numeric(0))
  }

  # All-coefficient proxy
  if (nrow(coef_lookup_k) > 0) {
    b2b_heldout_all_k <- b2b_lhs[b2b_lhs$vat_j_ano %in% heldout_vats_k, ]
    proxy_all_k <- b2b_heldout_all_k %>%
      inner_join(coef_lookup_k, by = "vat_i_ano") %>%
      group_by(vat_j_ano, year) %>%
      summarise(lofocv_proxy_all = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
                .groups = "drop") %>%
      rename(vat = vat_j_ano)
    rm(b2b_heldout_all_k)
  } else {
    proxy_all_k <- data.frame(vat = character(0), year = integer(0),
                              lofocv_proxy_all = numeric(0))
  }

  # Merge
  test_proxy_k <- test_lhs %>%
    select(vat, year) %>%
    left_join(proxy_k, by = c("vat", "year")) %>%
    left_join(proxy_all_k, by = c("vat", "year")) %>%
    mutate(
      lofocv_proxy     = coalesce(lofocv_proxy, 0),
      lofocv_proxy_all = coalesce(lofocv_proxy_all, 0)
    )

  proxy_pieces[[k]]     <- test_proxy_k %>% select(vat, year, lofocv_proxy)
  proxy_pieces_all[[k]] <- test_proxy_k %>% select(vat, year, lofocv_proxy_all)

  cat("  Proxy > 0:", sum(test_proxy_k$lofocv_proxy > 0), "/",
      nrow(test_proxy_k), "\n")

  # ── Diagnostics ─────────────────────────────────────────────────────────
  fold_time <- round(difftime(Sys.time(), t0_fold, units = "mins"), 1)

  diag_rows[[k]] <- data.frame(
    fold_k              = k,
    n_train_firms       = n_distinct(train_lhs$vat),
    n_test_firms        = n_distinct(test_lhs$vat),
    n_train_firmyears   = nrow(train_lhs),
    n_test_firmyears    = nrow(test_lhs),
    n_train_emitter_fy  = sum(train_lhs$emit),
    n_test_emitter_fy   = sum(test_lhs$emit),
    n_train_sectors     = length(train_sectors),
    n_test_sectors      = length(test_sectors),
    n_test_sectors_in_train = sum(test_sectors %in% train_sectors),
    n_eligible_sellers  = length(eligible_sellers_k),
    n_selected_pos      = nrow(coef_pos_k),
    n_selected_neg      = sum(coef_lookup_k$coef < 0),
    lambda_min          = lambda_min_k,
    cv_rmse             = cv_rmse_k,
    runtime_min         = as.numeric(fold_time),
    stringsAsFactors    = FALSE
  )

  cat(sprintf("  Done (%.1f min)\n", fold_time))

  rm(train_lhs, test_lhs, coef_lookup_k, coef_pos_k, proxy_k, proxy_all_k,
     test_proxy_k, eligible_sellers_k, eligible_sellers_k_enet,
     seller_map_k, seller_counts_k, pf_k, inner_foldid_k, heldout_vats_k,
     lambda_min_k, cv_rmse_k)
  gc()
}


# =============================================================================
# STEP 9: Assemble output
# =============================================================================
cat("\n═══ Assembling output ═══\n")

all_proxies     <- bind_rows(proxy_pieces)
all_proxies_all <- bind_rows(proxy_pieces_all)

lofocv_proxy_panel <- lhs %>%
  left_join(all_proxies, by = c("vat", "year")) %>%
  left_join(all_proxies_all, by = c("vat", "year")) %>%
  mutate(
    lofocv_proxy     = coalesce(lofocv_proxy, 0),
    lofocv_proxy_all = coalesce(lofocv_proxy_all, 0)
  ) %>%
  select(vat, year, nace2d, y, emit, log_revenue, euets,
         primary_nace2d, fold_k, lofocv_proxy, lofocv_proxy_all)

lofocv_fold_diagnostics <- bind_rows(diag_rows)

cat("LOFOCV proxy panel:", nrow(lofocv_proxy_panel), "rows\n")
cat("  lofocv_proxy > 0:", sum(lofocv_proxy_panel$lofocv_proxy > 0),
    sprintf("(%.1f%%)\n", 100 * mean(lofocv_proxy_panel$lofocv_proxy > 0)))

# Print fold diagnostics
cat("\n── Fold diagnostics ──\n")
print(lofocv_fold_diagnostics[, c("fold_k", "n_train_firms", "n_test_firms",
                                   "n_test_sectors_in_train",
                                   "n_selected_pos", "cv_rmse")])

# =============================================================================
# STEP 10: Save
# =============================================================================
OUT_PATH <- file.path(PROC_DATA, "lofocv_proxy.RData")
save(lofocv_proxy_panel, lofocv_fold_map, lofocv_fold_diagnostics, syt,
     file = OUT_PATH)

cat("\n══════════════════════════════════════════════\n")
cat("Saved to:", OUT_PATH, "\n")
cat("  lofocv_proxy_panel:       ", nrow(lofocv_proxy_panel), "rows\n")
cat("  lofocv_fold_map:          ", nrow(lofocv_fold_map), "rows\n")
cat("  lofocv_fold_diagnostics:  ", nrow(lofocv_fold_diagnostics), "rows\n")
cat("  syt:                      ", nrow(syt), "rows\n")
cat("══════════════════════════════════════════════\n")
