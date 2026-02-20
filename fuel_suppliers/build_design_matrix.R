###############################################################################
# fuel_suppliers/build_design_matrix.R
#
# PURPOSE
#   Build sparse design matrices for the fuel-supplier elastic net.
#   Rows    = LHS firm-years from loocv_training_sample (EU ETS + NACE 19/24)
#   Columns = eligible sellers (those supplying >= MIN_LHS_BUYERS LHS firms)
#   Values  = corrected bilateral sales (corr_sales_ij)
#
#   Produces TWO specification variants:
#     Pooled:       log_revenue + year FE + sector FE + supplier columns
#     Within-buyer: log_revenue + year FE + buyer FE  + supplier columns
#                   (sector FE dropped — collinear with buyer FE)
#
#   Also prepares:
#   - LHS vector (emissions; NAs replaced by 0 for confirmed-zero firms)
#   - Penalty factor vectors (0 for controls, 1 for supplier columns)
#   - Group k-fold assignments by firm (foldid)
#   - Two RHS versions per specification: raw sales and asinh(sales)
#
# INPUT
#   {PROC_DATA}/b2b_selected_sample.RData
#   {PROC_DATA}/loocv_training_sample.RData
#
# OUTPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
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


# ── Parameters ───────────────────────────────────────────────────────────────
MIN_LHS_BUYERS <- 1L    # TODO: set back to 5 before running on RMD (lowered to 1 for local dev run on downsampled data)
K_FOLDS        <- 10L   # number of CV folds (grouped by firm)


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading B2B selected sample...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample
rm(df_b2b_selected_sample)

cat("Loading LOOCV training sample...\n")
load(file.path(PROC_DATA, "loocv_training_sample.RData"))


# ── Prepare LHS panel ───────────────────────────────────────────────────────
lhs <- loocv_training_sample %>%
  filter(year >= 2005) %>%
  mutate(
    # Confirmed zeros for non-ETS NACE 19/24 firms (NIR says EU ETS covers
    # ~100% of fuel-combustion emissions in those sectors)
    y = ifelse(is.na(emissions), 0, emissions),
    log_revenue = log(pmax(revenue, 1e-12))
  ) %>%
  select(vat, year, y, log_revenue, nace2d, euets) %>%
  arrange(vat, year) %>%
  mutate(row_idx = row_number())

n_na <- sum(is.na(loocv_training_sample$emissions[
  loocv_training_sample$year >= 2005]))
cat("LHS panel:", nrow(lhs), "firm-years,",
    length(unique(lhs$vat)), "unique firms\n")
cat("  Emissions NAs replaced with 0:", n_na, "\n")


# ── Identify eligible sellers ────────────────────────────────────────────────
cat("\nFiltering B2B to LHS buyers, years >= 2005...\n")
b2b_lhs <- b2b %>%
  filter(vat_j_ano %in% lhs$vat, year >= 2005)
rm(b2b)  # free memory

# Count distinct LHS buyers per seller (across all years)
seller_counts <- b2b_lhs %>%
  distinct(vat_i_ano, vat_j_ano) %>%
  count(vat_i_ano, name = "n_lhs_buyers")

eligible_sellers <- seller_counts %>%
  filter(n_lhs_buyers >= MIN_LHS_BUYERS) %>%
  pull(vat_i_ano) %>%
  sort()

cat("Eligible sellers (>=", MIN_LHS_BUYERS, "LHS buyers):",
    length(eligible_sellers), "\n")

# Column index mapping
seller_map <- data.frame(
  vat_i_ano = eligible_sellers,
  col_idx   = seq_along(eligible_sellers),
  stringsAsFactors = FALSE
)


# ── Build sparse supplier matrix ────────────────────────────────────────────
cat("\nBuilding sparse design matrix...\n")

# Aggregate to (seller, buyer, year) level, drop zero-sum transactions
b2b_agg <- b2b_lhs %>%
  filter(vat_i_ano %in% eligible_sellers) %>%
  group_by(vat_i_ano, vat_j_ano, year) %>%
  summarise(sales = sum(corr_sales_ij, na.rm = TRUE), .groups = "drop") %>%
  filter(sales != 0)
rm(b2b_lhs)

# Map to matrix indices
b2b_agg <- b2b_agg %>%
  inner_join(lhs %>% select(vat, year, row_idx),
             by = c("vat_j_ano" = "vat", "year" = "year")) %>%
  inner_join(seller_map, by = "vat_i_ano")

n_rows <- nrow(lhs)
n_cols <- length(eligible_sellers)

# Raw sales version
X_raw <- sparseMatrix(
  i    = b2b_agg$row_idx,
  j    = b2b_agg$col_idx,
  x    = b2b_agg$sales,
  dims = c(n_rows, n_cols)
)

# asinh(sales) version
X_asinh <- sparseMatrix(
  i    = b2b_agg$row_idx,
  j    = b2b_agg$col_idx,
  x    = asinh(b2b_agg$sales),
  dims = c(n_rows, n_cols)
)

colnames(X_raw)   <- eligible_sellers
colnames(X_asinh) <- eligible_sellers

cat("Supplier matrix:", n_rows, "x", n_cols, "\n")
cat("Non-zero entries:", nnzero(X_raw), "\n")
cat("Sparsity:", round((1 - nnzero(X_raw) / (n_rows * n_cols)) * 100, 2),
    "% zeros\n")
rm(b2b_agg)


# ── Build control matrix ────────────────────────────────────────────────────
cat("\nBuilding control variables...\n")

# Year dummies (K-1; reference = first year; glmnet adds its own intercept)
year_dummies <- model.matrix(~ factor(year), data = lhs)[, -1, drop = FALSE]
colnames(year_dummies) <- paste0("yr_", sort(unique(lhs$year))[-1])

# Sector dummies (K-1; reference = first sector alphabetically)
sector_dummies <- model.matrix(~ factor(nace2d), data = lhs)[, -1, drop = FALSE]
colnames(sector_dummies) <- paste0("sec_",
                                   sort(unique(lhs$nace2d))[-1])

# Combine: log_revenue + year dummies + sector dummies
X_controls <- cbind(log_revenue = lhs$log_revenue,
                    year_dummies,
                    sector_dummies)

n_controls <- ncol(X_controls)
cat("Control variables:", n_controls, "columns\n")
cat("  (1 log_revenue +", ncol(year_dummies), "year dummies +",
    ncol(sector_dummies), "sector dummies)\n")


# ── Build buyer FE control matrix (within-buyer specification) ─────────────
# Buyer FEs absorb sector FEs (sector is time-invariant within firm), so
# this variant uses: log_revenue + year dummies + buyer dummies
cat("\nBuilding within-buyer controls (buyer FEs)...\n")

buyer_dummies <- model.matrix(~ factor(vat), data = lhs)[, -1, drop = FALSE]
colnames(buyer_dummies) <- paste0("buyer_", sort(unique(lhs$vat))[-1])

X_controls_fe <- cbind(log_revenue = lhs$log_revenue,
                       year_dummies,
                       buyer_dummies)

n_controls_fe <- ncol(X_controls_fe)
cat("Within-buyer controls:", n_controls_fe, "columns\n")
cat("  (1 log_revenue +", ncol(year_dummies), "year dummies +",
    ncol(buyer_dummies), "buyer dummies)\n")


# ── Combine into full design matrices ────────────────────────────────────────
cat("\nAssembling full design matrices...\n")

# Convert controls to sparse for efficient cbind with supplier matrix
X_controls_sparse    <- Matrix(X_controls, sparse = TRUE)
X_controls_fe_sparse <- Matrix(X_controls_fe, sparse = TRUE)

# Pooled specification
X_full_raw   <- cbind(X_controls_sparse, X_raw)
X_full_asinh <- cbind(X_controls_sparse, X_asinh)
penalty_factor <- c(rep(0, n_controls), rep(1, n_cols))

cat("Pooled design matrix:", nrow(X_full_raw), "x", ncol(X_full_raw),
    "(", n_controls, "controls +", n_cols, "supplier cols)\n")

# Within-buyer specification
X_full_raw_fe   <- cbind(X_controls_fe_sparse, X_raw)
X_full_asinh_fe <- cbind(X_controls_fe_sparse, X_asinh)
penalty_factor_fe <- c(rep(0, n_controls_fe), rep(1, n_cols))

cat("Within-buyer design matrix:", nrow(X_full_raw_fe), "x", ncol(X_full_raw_fe),
    "(", n_controls_fe, "controls +", n_cols, "supplier cols)\n")


# ── Group k-fold by firm ─────────────────────────────────────────────────────
cat("\nAssigning group k-fold IDs (K =", K_FOLDS, ")...\n")
set.seed(42)
unique_firms <- unique(lhs$vat)
firm_folds   <- sample(rep(1:K_FOLDS, length.out = length(unique_firms)))
names(firm_folds) <- unique_firms
foldid <- unname(firm_folds[lhs$vat])

cat("Fold sizes (firm-years): ",
    paste(table(foldid), collapse = ", "), "\n")
cat("Firms per fold:          ",
    paste(table(firm_folds), collapse = ", "), "\n")


# ── LHS vector ──────────────────────────────────────────────────────────────
y <- lhs$y

cat("\ny summary:\n")
cat("  n =", length(y), "\n")
cat("  zeros:", sum(y == 0), "(", round(100 * mean(y == 0), 1), "%)\n")
cat("  mean (all):", round(mean(y)), "\n")
cat("  mean (positive):", round(mean(y[y > 0])), "\n")


# ── Save ─────────────────────────────────────────────────────────────────────
OUT_PATH <- file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData")

save(
  # Pooled specification
  X_full_raw, X_full_asinh, X_controls,
  penalty_factor, n_controls,
  # Within-buyer specification
  X_full_raw_fe, X_full_asinh_fe, X_controls_fe,
  penalty_factor_fe, n_controls_fe,
  # Shared objects
  X_raw, X_asinh,
  y, lhs, foldid,
  eligible_sellers, seller_map,
  MIN_LHS_BUYERS, K_FOLDS,
  file = OUT_PATH
)

cat("\n══════════════════════════════════════════════\n")
cat("Saved to:", OUT_PATH, "\n")
cat("══════════════════════════════════════════════\n")
