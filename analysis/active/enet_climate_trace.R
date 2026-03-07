###############################################################################
# analysis/active/enet_climate_trace.R
#
# PURPOSE
#   Use Climate TRACE (CT) facility-level emissions as the LHS to train
#   the elastic net that identifies fuel suppliers. This tests whether
#   satellite-derived emissions can substitute for EUTL verified emissions
#   in the supplier-identification step.
#
#   Design:
#     Training set: firms matched to both EUTL and CT (CT emissions as LHS)
#     Test set:     remaining EUTL firms (not matched to CT)
#     Proxy:        coefficient-weighted asinh(B2B purchases) from CT-trained EN
#     Evaluation:   compare CT-trained proxy against EUTL verified emissions
#                   on the test set (firms the CT-trained EN never saw)
#
#   The elastic net specification mirrors build_fold_specific_proxy.R:
#     - Unpenalized: log(revenue), year FE, sector FE
#     - Penalized:   asinh(bilateral sales) for each eligible supplier
#     - alpha = 0.5, lambda tuned via 10-fold CV grouped by firm
#
# INPUTS
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#   {RAW_DATA}/Climate TRACE/BEL/             (CT source-level CSVs)
#   {RAW_DATA}/EUTL/Oct_2024_version/         (installation.csv, compliance.csv)
#   {PROC_DATA}/EUTL_Belgium.RData            (installation-to-VAT mapping)
#
# OUTPUTS
#   {PROC_DATA}/enet_climate_trace_results.RData
#     Contains:
#       ct_proxy_panel      — test-set firms with CT-trained proxy values
#       ct_train_panel      — training-set firms (CT-matched) with fitted values
#       ct_coef_lookup      — all non-zero suppliers with coefficients
#       ct_coef_pos         — positive-coef suppliers only
#       ct_diagnostics      — single-row diagnostics (same format as fold diags)
#       ct_eutl_match       — EUTL-to-CT matching table
#       ct_fold_assignment  — which firms are train vs test
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
K_INNER        <- 10L
MIN_LHS_BUYERS <- 5L
ALPHA          <- 0.5
SEED           <- 42L
GEO_TIGHT_KM   <- 1.0
GEO_LOOSE_KM   <- 5.0
NAME_SIM_THRESH <- 0.25


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
# STEP 1: Load Climate TRACE Belgium data
# =============================================================================
cat("== STEP 1: Loading Climate TRACE data ==\n")

ct_dir <- file.path(RAW_DATA, "Climate TRACE", "BEL", "DATA")
ct_files <- list.files(ct_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
cat("Found", length(ct_files), "CT CSV files\n")

ct_raw <- lapply(ct_files, function(f) {
  tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
})
ct_raw <- bind_rows(ct_raw[!sapply(ct_raw, is.null)])
cat("CT raw rows:", nrow(ct_raw), "\n")

# Aggregate to source-year level (annual CO2 emissions)
ct <- ct_raw %>%
  filter(gas == "co2") %>%
  group_by(source_id, source_name, lat, lon, year) %>%
  summarise(ct_emissions = sum(emissions_quantity, na.rm = TRUE), .groups = "drop") %>%
  filter(ct_emissions > 0)

cat("CT source-years (CO2 > 0):", nrow(ct), "\n")
cat("CT unique sources:", n_distinct(ct$source_id), "\n")
cat("CT year range:", range(ct$year), "\n\n")


# =============================================================================
# STEP 2: Load EUTL installations and match to Climate TRACE
# =============================================================================
cat("== STEP 2: Matching EUTL installations to Climate TRACE ==\n")

# Load EUTL installation data
inst <- read.csv(file.path(RAW_DATA, "EUTL", "Oct_2024_version", "installation.csv"),
                 stringsAsFactors = FALSE)
be_inst <- inst %>%
  filter(country_id == "BE",
         isAircraftOperator != "True",
         isMaritimeOperator != "True") %>%
  mutate(
    lat_eutl = ifelse(!is.na(latitudeGoogle), latitudeGoogle, latitudeEutl),
    lon_eutl = ifelse(!is.na(longitudeGoogle), longitudeGoogle, longitudeEutl)
  ) %>%
  filter(!is.na(lat_eutl), !is.na(lon_eutl))

cat("EUTL Belgian installations:", nrow(be_inst), "\n")

# Haversine distance in km
haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat/2)^2 + cos(lat1 * pi/180) * cos(lat2 * pi/180) * sin(dlon/2)^2
  2 * R * asin(sqrt(a))
}

# Bigram similarity for name confirmation
bigram_jaccard <- function(a, b) {
  make_bigrams <- function(s) {
    s <- tolower(gsub("[^a-z0-9]", "", s))
    if (nchar(s) < 2) return(character(0))
    substring(s, 1:(nchar(s)-1), 2:nchar(s))
  }
  bg_a <- make_bigrams(a)
  bg_b <- make_bigrams(b)
  if (length(bg_a) == 0 || length(bg_b) == 0) return(0)
  length(intersect(bg_a, bg_b)) / length(union(bg_a, bg_b))
}

# Match each CT source to nearest EUTL installation
ct_sources <- ct %>%
  distinct(source_id, source_name, lat, lon)

matches <- list()
for (i in seq_len(nrow(ct_sources))) {
  s <- ct_sources[i, ]
  dists <- haversine_km(s$lat, s$lon, be_inst$lat_eutl, be_inst$lon_eutl)
  min_dist <- min(dists)
  best_idx <- which.min(dists)

  if (min_dist <= GEO_TIGHT_KM) {
    match_type <- "tight_geo"
  } else if (min_dist <= GEO_LOOSE_KM) {
    sim <- bigram_jaccard(s$source_name, be_inst$name[best_idx])
    if (sim >= NAME_SIM_THRESH) {
      match_type <- "name_confirmed"
    } else {
      next
    }
  } else {
    next
  }

  matches[[length(matches) + 1]] <- data.frame(
    source_id = s$source_id,
    ct_name = s$source_name,
    installation_id = be_inst$id[best_idx],
    eutl_name = be_inst$name[best_idx],
    dist_km = min_dist,
    match_type = match_type,
    stringsAsFactors = FALSE
  )
}

ct_eutl_match <- bind_rows(matches)
cat("CT sources matched to EUTL:", nrow(ct_eutl_match), "out of",
    nrow(ct_sources), "\n")

# Load EUTL-to-VAT mapping
load(file.path(PROC_DATA, "EUTL_Belgium.RData"))
eutl_vat_cols <- names(eutl_belgium)
cat("EUTL_Belgium columns:", paste(eutl_vat_cols, collapse = ", "), "\n")

if ("installation_id" %in% eutl_vat_cols && "vat" %in% eutl_vat_cols) {
  eutl_vat <- eutl_belgium %>% select(installation_id, vat)
} else if ("id" %in% eutl_vat_cols && "vat_ano" %in% eutl_vat_cols) {
  eutl_vat <- eutl_belgium %>% select(installation_id = id, vat = vat_ano)
} else {
  cat("WARNING: Could not identify installation_id and vat columns.\n")
  cat("Available columns:", paste(eutl_vat_cols, collapse = ", "), "\n")
  cat("Trying first two columns as installation_id and vat...\n")
  eutl_vat <- eutl_belgium[, 1:2]
  names(eutl_vat) <- c("installation_id", "vat")
}

# Map matched CT sources to VATs
ct_eutl_match <- ct_eutl_match %>%
  left_join(eutl_vat, by = "installation_id")

cat("CT sources matched to VAT:", sum(!is.na(ct_eutl_match$vat)), "\n\n")


# =============================================================================
# STEP 3: Load training sample and identify train/test split
# =============================================================================
cat("== STEP 3: Building train/test split ==\n")

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

# Firms matched to CT (training set for CT-EN)
ct_vats <- unique(ct_eutl_match$vat[!is.na(ct_eutl_match$vat)])
lhs$ct_matched <- lhs$vat %in% ct_vats

cat("LHS panel:", nrow(lhs), "firm-years,", n_distinct(lhs$vat), "firms\n")
cat("CT-matched firms:", n_distinct(lhs$vat[lhs$ct_matched]), "\n")
cat("CT-matched firm-years:", sum(lhs$ct_matched), "\n")
cat("Test firms (not in CT):", n_distinct(lhs$vat[!lhs$ct_matched]), "\n")
cat("Test firm-years:", sum(!lhs$ct_matched), "\n\n")

# Build CT emissions panel at firm-year level
ct_firm_year <- ct %>%
  inner_join(ct_eutl_match %>% filter(!is.na(vat)) %>%
               select(source_id, installation_id, vat),
             by = "source_id") %>%
  group_by(vat, year) %>%
  summarise(ct_emissions = sum(ct_emissions, na.rm = TRUE), .groups = "drop")

# Merge CT emissions onto training set
train_lhs <- lhs %>%
  filter(ct_matched) %>%
  left_join(ct_firm_year, by = c("vat", "year"))

# For years where CT has no data, drop those obs
train_lhs <- train_lhs %>% filter(!is.na(ct_emissions), ct_emissions > 0)

cat("Training sample (CT-matched, with CT emissions):",
    nrow(train_lhs), "firm-years,", n_distinct(train_lhs$vat), "firms\n")
cat("CT year coverage in training:", paste(sort(unique(train_lhs$year)), collapse = ", "), "\n\n")


# =============================================================================
# STEP 4: Load B2B and build design matrix
# =============================================================================
cat("== STEP 4: Building design matrix ==\n")

t0_total <- Sys.time()

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

b2b_lhs <- b2b %>% filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)

# Eligible sellers: trade with >= MIN_LHS_BUYERS training firms
b2b_train <- b2b_lhs %>% filter(vat_j_ano %in% train_lhs$vat)

seller_counts <- b2b_train %>%
  distinct(vat_i_ano, vat_j_ano) %>%
  count(vat_i_ano, name = "n_lhs_buyers")

eligible_sellers <- seller_counts %>%
  filter(n_lhs_buyers >= MIN_LHS_BUYERS) %>%
  pull(vat_i_ano) %>%
  sort()

cat("Eligible sellers (>=", MIN_LHS_BUYERS, "training buyers):",
    length(eligible_sellers), "\n")

seller_map <- data.frame(
  vat_i_ano = eligible_sellers,
  col_idx   = seq_along(eligible_sellers),
  stringsAsFactors = FALSE
)

# Build sparse design matrix for training set
train_lhs$row_idx <- seq_len(nrow(train_lhs))

b2b_agg <- b2b_train %>%
  filter(vat_i_ano %in% eligible_sellers) %>%
  group_by(vat_i_ano, vat_j_ano, year) %>%
  summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
  filter(sales != 0) %>%
  inner_join(train_lhs %>% select(vat, year, row_idx),
             by = c("vat_j_ano" = "vat", "year" = "year")) %>%
  inner_join(seller_map, by = "vat_i_ano")

n_rows <- nrow(train_lhs)
n_cols <- length(eligible_sellers)

X_asinh <- sparseMatrix(
  i    = b2b_agg$row_idx,
  j    = b2b_agg$col_idx,
  x    = asinh(b2b_agg$sales),
  dims = c(n_rows, n_cols)
)
colnames(X_asinh) <- eligible_sellers

rm(b2b_agg, b2b_train)

# Control variables
year_dummies <- model.matrix(~ factor(year), data = train_lhs)[, -1, drop = FALSE]
colnames(year_dummies) <- paste0("yr_", sort(unique(train_lhs$year))[-1])

train_sectors <- sort(unique(train_lhs$nace2d))
sector_dummies <- model.matrix(~ factor(nace2d, levels = train_sectors),
                               data = train_lhs)[, -1, drop = FALSE]
colnames(sector_dummies) <- paste0("sec_", train_sectors[-1])

X_controls <- cbind(log_revenue = train_lhs$log_revenue,
                     year_dummies, sector_dummies)
n_controls <- ncol(X_controls)

cat("Controls:", n_controls, "columns (1 log_revenue +",
    ncol(year_dummies), "year +", ncol(sector_dummies), "sector)\n")

# Combine
X_full <- cbind(Matrix(X_controls, sparse = TRUE), X_asinh)
pf <- c(rep(0, n_controls), rep(1, n_cols))

# Drop zero-variance columns
col_means <- colMeans(X_full)
col_sq_means <- colMeans(X_full^2)
col_vars <- col_sq_means - col_means^2
zero_var <- which(col_vars == 0)

if (length(zero_var) > 0) {
  cat("Dropping", length(zero_var), "zero-variance columns\n")
  X_full <- X_full[, -zero_var]
  pf <- pf[-zero_var]
  n_controls <- sum(pf == 0)
  eligible_sellers_enet <- colnames(X_full)[which(pf == 1)]
} else {
  eligible_sellers_enet <- eligible_sellers
}

rm(X_asinh, X_controls, year_dummies, sector_dummies)

cat("Full design matrix:", nrow(X_full), "x", ncol(X_full),
    "(", n_controls, "controls +", sum(pf == 1), "suppliers)\n\n")


# =============================================================================
# STEP 5: Run elastic net with CT emissions as LHS
# =============================================================================
cat("== STEP 5: Running elastic net (CT emissions as LHS) ==\n")

set.seed(SEED)
inner_foldid <- sample(rep(1:K_INNER, length.out = nrow(train_lhs)))

# Use CT emissions as the dependent variable
y_ct <- train_lhs$ct_emissions

cat("LHS (CT emissions) summary:\n")
print(summary(y_ct))
cat("\n")

t0 <- Sys.time()
fit_ct <- cv.glmnet(
  x              = X_full,
  y              = y_ct,
  family         = "gaussian",
  alpha          = ALPHA,
  penalty.factor = pf,
  foldid         = inner_foldid,
  standardize    = TRUE,
  parallel       = USE_PARALLEL
)
enet_time <- round(difftime(Sys.time(), t0, units = "mins"), 1)

lambda_min_ct <- fit_ct$lambda.min
cv_rmse_ct    <- sqrt(min(fit_ct$cvm))

cat("cv.glmnet time:", enet_time, "min\n")
cat("lambda.min:", signif(lambda_min_ct, 4), "\n")
cat("CV RMSE:", round(cv_rmse_ct, 2), "\n")

# Extract supplier coefficients
co <- coef(fit_ct, s = "lambda.min")
supplier_idx <- (n_controls + 2):length(co)
ct_coef_lookup <- data.frame(
  vat_i_ano = eligible_sellers_enet,
  coef      = as.numeric(co[supplier_idx]),
  stringsAsFactors = FALSE
) %>%
  filter(coef != 0) %>%
  arrange(desc(abs(coef)))

ct_coef_pos <- ct_coef_lookup %>% filter(coef > 0)

n_selected_pos <- nrow(ct_coef_pos)
n_selected_neg <- sum(ct_coef_lookup$coef < 0)

cat("\nNon-zero suppliers:", nrow(ct_coef_lookup),
    "| positive:", n_selected_pos,
    "| negative:", n_selected_neg, "\n\n")

rm(X_full, fit_ct)


# =============================================================================
# STEP 6: Build proxy for test set (non-CT EUTL firms)
# =============================================================================
cat("== STEP 6: Building CT-trained proxy for test set ==\n")

test_lhs <- lhs %>% filter(!ct_matched)
test_vats <- unique(test_lhs$vat)

# ── Positive-only proxy (test set) ─────────────────────────────────────────
if (n_selected_pos > 0) {
  b2b_test <- b2b_lhs[b2b_lhs$vat_j_ano %in% test_vats, ]

  ct_proxy_test <- b2b_test %>%
    inner_join(ct_coef_pos, by = "vat_i_ano") %>%
    group_by(vat_j_ano, year) %>%
    summarise(ct_proxy = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
              .groups = "drop") %>%
    rename(vat = vat_j_ano)

  rm(b2b_test)
} else {
  warning("No positive-coefficient suppliers from CT-trained EN")
  ct_proxy_test <- data.frame(vat = character(0), year = integer(0),
                               ct_proxy = numeric(0))
}

# ── All-coefficient proxy (test set) ───────────────────────────────────────
if (nrow(ct_coef_lookup) > 0) {
  b2b_test_all <- b2b_lhs[b2b_lhs$vat_j_ano %in% test_vats, ]

  ct_proxy_test_all <- b2b_test_all %>%
    inner_join(ct_coef_lookup, by = "vat_i_ano") %>%
    group_by(vat_j_ano, year) %>%
    summarise(ct_proxy_all = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
              .groups = "drop") %>%
    rename(vat = vat_j_ano)

  rm(b2b_test_all)
} else {
  ct_proxy_test_all <- data.frame(vat = character(0), year = integer(0),
                                   ct_proxy_all = numeric(0))
}

# ── Positive-only proxy (train set, for diagnostics) ───────────────────────
if (n_selected_pos > 0) {
  b2b_train_all <- b2b_lhs[b2b_lhs$vat_j_ano %in% train_lhs$vat, ]

  ct_proxy_train <- b2b_train_all %>%
    inner_join(ct_coef_pos, by = "vat_i_ano") %>%
    group_by(vat_j_ano, year) %>%
    summarise(ct_proxy = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
              .groups = "drop") %>%
    rename(vat = vat_j_ano)

  rm(b2b_train_all)
} else {
  ct_proxy_train <- data.frame(vat = character(0), year = integer(0),
                                ct_proxy = numeric(0))
}

# ── All-coefficient proxy (train set) ──────────────────────────────────────
if (nrow(ct_coef_lookup) > 0) {
  b2b_train_all2 <- b2b_lhs[b2b_lhs$vat_j_ano %in% train_lhs$vat, ]

  ct_proxy_train_all <- b2b_train_all2 %>%
    inner_join(ct_coef_lookup, by = "vat_i_ano") %>%
    group_by(vat_j_ano, year) %>%
    summarise(ct_proxy_all = sum(coef * asinh(corr_sales_ij), na.rm = TRUE),
              .groups = "drop") %>%
    rename(vat = vat_j_ano)

  rm(b2b_train_all2)
} else {
  ct_proxy_train_all <- data.frame(vat = character(0), year = integer(0),
                                    ct_proxy_all = numeric(0))
}

rm(b2b_lhs)

# Merge onto panels
ct_proxy_panel <- test_lhs %>%
  left_join(ct_proxy_test, by = c("vat", "year")) %>%
  left_join(ct_proxy_test_all, by = c("vat", "year")) %>%
  mutate(
    ct_proxy     = coalesce(ct_proxy, 0),
    ct_proxy_all = coalesce(ct_proxy_all, 0)
  )

ct_train_panel <- train_lhs %>%
  left_join(ct_proxy_train, by = c("vat", "year")) %>%
  left_join(ct_proxy_train_all, by = c("vat", "year")) %>%
  mutate(
    ct_proxy     = coalesce(ct_proxy, 0),
    ct_proxy_all = coalesce(ct_proxy_all, 0)
  )

cat("Test panel:", nrow(ct_proxy_panel), "firm-years,",
    n_distinct(ct_proxy_panel$vat), "firms\n")
cat("  with ct_proxy > 0:", sum(ct_proxy_panel$ct_proxy > 0), "\n")
cat("Train panel:", nrow(ct_train_panel), "firm-years,",
    n_distinct(ct_train_panel$vat), "firms\n")
cat("  with ct_proxy > 0:", sum(ct_train_panel$ct_proxy > 0), "\n\n")


# =============================================================================
# STEP 7: Diagnostics
# =============================================================================
cat("== STEP 7: Diagnostics ==\n")

total_time <- round(difftime(Sys.time(), t0_total, units = "mins"), 1)

# Coefficient distribution (positive only)
if (n_selected_pos > 0) {
  coef_pos_mean   <- mean(ct_coef_pos$coef)
  coef_pos_median <- median(ct_coef_pos$coef)
  coef_pos_sd     <- if (n_selected_pos > 1) sd(ct_coef_pos$coef) else NA_real_
} else {
  coef_pos_mean <- coef_pos_median <- coef_pos_sd <- NA_real_
}

# Proxy coverage (test set)
n_test_fy <- nrow(ct_proxy_panel)
n_test_emitter_fy <- sum(ct_proxy_panel$emit)
share_test_proxy_gt0 <- if (n_test_fy > 0) mean(ct_proxy_panel$ct_proxy > 0) else NA_real_
share_test_emitter_proxy_gt0 <- if (n_test_emitter_fy > 0) {
  mean(ct_proxy_panel$ct_proxy[ct_proxy_panel$emit == 1L] > 0)
} else {
  NA_real_
}

# Proxy quality on test set (against EUTL verified emissions)
cor_proxy_y <- if (sum(ct_proxy_panel$y > 0 & ct_proxy_panel$ct_proxy > 0) > 5) {
  cor(ct_proxy_panel$y, ct_proxy_panel$ct_proxy, use = "complete.obs")
} else {
  NA_real_
}

auc_emit <- compute_auc(ct_proxy_panel$emit, ct_proxy_panel$ct_proxy)

ct_diagnostics <- data.frame(
  n_train_firms                 = n_distinct(train_lhs$vat),
  n_test_firms                  = n_distinct(test_lhs$vat),
  n_train_firmyears             = nrow(train_lhs),
  n_test_firmyears              = n_test_fy,
  n_train_emitter_fy            = sum(train_lhs$emit),
  n_test_emitter_fy             = n_test_emitter_fy,
  n_eligible_sellers            = length(eligible_sellers),
  n_selected_pos                = n_selected_pos,
  n_selected_neg                = n_selected_neg,
  lambda_min                    = lambda_min_ct,
  cv_rmse                       = cv_rmse_ct,
  coef_pos_mean                 = coef_pos_mean,
  coef_pos_median               = coef_pos_median,
  coef_pos_sd                   = coef_pos_sd,
  share_test_firms_proxy_gt0    = share_test_proxy_gt0,
  share_test_emitters_proxy_gt0 = share_test_emitter_proxy_gt0,
  cor_proxy_y                   = cor_proxy_y,
  auc_emit                      = auc_emit,
  runtime_min                   = as.numeric(total_time),
  stringsAsFactors              = FALSE
)

# Print diagnostics
cat("\n-- CT diagnostics --\n")
print(t(ct_diagnostics))

# Also report correlations
eval_sub <- ct_proxy_panel %>% filter(y > 0, ct_proxy > 0)
if (nrow(eval_sub) > 5) {
  cat("\nTest firms with y > 0 and ct_proxy > 0:", nrow(eval_sub), "\n")
  cat(sprintf("Pearson(log y, log ct_proxy):  %.3f\n",
              cor(log(eval_sub$y), log(eval_sub$ct_proxy))))
  cat(sprintf("Spearman(y, ct_proxy):         %.3f\n",
              cor(eval_sub$y, eval_sub$ct_proxy, method = "spearman")))
} else {
  cat("Too few test observations with both y > 0 and ct_proxy > 0:", nrow(eval_sub), "\n")
}

# In-sample fit
eval_train <- ct_train_panel %>% filter(ct_emissions > 0, ct_proxy > 0)
if (nrow(eval_train) > 5) {
  cat("\nIn-sample (train) diagnostics:\n")
  cat(sprintf("Pearson(log ct_emissions, log ct_proxy): %.3f\n",
              cor(log(eval_train$ct_emissions), log(eval_train$ct_proxy))))
  cat(sprintf("Spearman(ct_emissions, ct_proxy):        %.3f\n",
              cor(eval_train$ct_emissions, eval_train$ct_proxy, method = "spearman")))
  eval_train_eutl <- eval_train %>% filter(y > 0)
  if (nrow(eval_train_eutl) > 5) {
    cat(sprintf("Pearson(log EUTL_y, log ct_proxy):        %.3f\n",
                cor(log(eval_train_eutl$y), log(eval_train_eutl$ct_proxy))))
    cat(sprintf("Spearman(EUTL_y, ct_proxy):               %.3f\n",
                cor(eval_train_eutl$y, eval_train_eutl$ct_proxy, method = "spearman")))
  }
}


# =============================================================================
# STEP 8: Save
# =============================================================================
ct_fold_assignment <- lhs %>%
  distinct(vat) %>%
  mutate(ct_split = ifelse(vat %in% ct_vats, "train", "test"))

OUT_PATH <- file.path(PROC_DATA, "enet_climate_trace_results.RData")
save(ct_proxy_panel, ct_train_panel, ct_coef_lookup, ct_coef_pos,
     ct_diagnostics, ct_eutl_match, ct_fold_assignment,
     file = OUT_PATH)

cat("\n==============================================\n")
cat("Saved to:", OUT_PATH, "\n")
cat("  ct_proxy_panel:", nrow(ct_proxy_panel), "rows (test set)\n")
cat("  ct_train_panel:", nrow(ct_train_panel), "rows (train set)\n")
cat("  ct_coef_lookup:", nrow(ct_coef_lookup), "rows (all non-zero suppliers)\n")
cat("  ct_coef_pos:   ", nrow(ct_coef_pos), "rows (positive coefs)\n")
cat("  ct_diagnostics: 1 row\n")
cat("  ct_eutl_match: ", nrow(ct_eutl_match), "rows\n")
cat("  ct_fold_assignment:", nrow(ct_fold_assignment), "firms\n")
cat("==============================================\n")

# Clean up parallel backend
if (USE_PARALLEL) {
  doParallel::stopImplicitCluster()
  cat("Parallel backend stopped.\n")
}
