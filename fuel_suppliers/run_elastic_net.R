###############################################################################
# fuel_suppliers/run_elastic_net.R
#
# PURPOSE
#   Run elastic net (cv.glmnet) to identify fuel suppliers from B2B data.
#   Four model variants: {lasso, elastic net} x {raw sales, asinh(sales)}.
#   CV uses group k-fold by firm (all years of a firm in the same fold).
#
# INPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#     (produced by fuel_suppliers/01_build_design_matrix.R)
#
# OUTPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#     - cv.glmnet fit objects for each model variant
#     - supplier_summary: tidy dataframe of identified suppliers & coefficients
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

library(glmnet)
library(Matrix)
library(dplyr)


# ── Load inputs ──────────────────────────────────────────────────────────────
cat("Loading elastic net inputs...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData"))

cat("Design matrix:", nrow(X_full_raw), "x", ncol(X_full_raw), "\n")
cat("  Controls:", n_controls, "| Supplier cols:", ncol(X_full_raw) - n_controls, "\n")
cat("  K-fold groups:", K_FOLDS, "\n\n")


# ── Helper: extract supplier coefficients ────────────────────────────────────
extract_suppliers <- function(fit, s = "lambda.min") {
  co <- coef(fit, s = s)
  # Positions: 1 = intercept, 2:(n_controls+1) = controls,
  #            (n_controls+2):end = supplier columns
  supplier_idx <- (n_controls + 2):length(co)
  vals <- co[supplier_idx]
  data.frame(
    vat_i_ano = eligible_sellers,
    coef      = as.numeric(vals),
    stringsAsFactors = FALSE
  ) %>%
    filter(coef != 0) %>%
    arrange(desc(abs(coef)))
}


# =============================================================================
#   MODEL 1: Lasso (alpha = 1), raw sales
# =============================================================================
cat("═══ Model 1: Lasso, raw sales ═══\n")
t0 <- Sys.time()

fit_lasso_raw <- cv.glmnet(
  x              = X_full_raw,
  y              = y,
  family         = "gaussian",
  alpha          = 1,
  penalty.factor = penalty_factor,
  foldid         = foldid,
  standardize    = TRUE
)

cat("  Time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("  lambda.min:", signif(fit_lasso_raw$lambda.min, 4), "\n")
cat("  lambda.1se:", signif(fit_lasso_raw$lambda.1se, 4), "\n")

s1_min <- extract_suppliers(fit_lasso_raw, "lambda.min")
s1_1se <- extract_suppliers(fit_lasso_raw, "lambda.1se")
cat("  Non-zero suppliers (lambda.min):", nrow(s1_min),
    "| positive:", sum(s1_min$coef > 0),
    "| negative:", sum(s1_min$coef < 0), "\n")
cat("  Non-zero suppliers (lambda.1se):", nrow(s1_1se),
    "| positive:", sum(s1_1se$coef > 0),
    "| negative:", sum(s1_1se$coef < 0), "\n\n")


# =============================================================================
#   MODEL 2: Lasso (alpha = 1), asinh(sales)
# =============================================================================
cat("═══ Model 2: Lasso, asinh sales ═══\n")
t0 <- Sys.time()

fit_lasso_asinh <- cv.glmnet(
  x              = X_full_asinh,
  y              = y,
  family         = "gaussian",
  alpha          = 1,
  penalty.factor = penalty_factor,
  foldid         = foldid,
  standardize    = TRUE
)

cat("  Time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("  lambda.min:", signif(fit_lasso_asinh$lambda.min, 4), "\n")
cat("  lambda.1se:", signif(fit_lasso_asinh$lambda.1se, 4), "\n")

s2_min <- extract_suppliers(fit_lasso_asinh, "lambda.min")
s2_1se <- extract_suppliers(fit_lasso_asinh, "lambda.1se")
cat("  Non-zero suppliers (lambda.min):", nrow(s2_min),
    "| positive:", sum(s2_min$coef > 0),
    "| negative:", sum(s2_min$coef < 0), "\n")
cat("  Non-zero suppliers (lambda.1se):", nrow(s2_1se),
    "| positive:", sum(s2_1se$coef > 0),
    "| negative:", sum(s2_1se$coef < 0), "\n\n")


# =============================================================================
#   MODEL 3: Elastic net (alpha = 0.5), raw sales
# =============================================================================
cat("═══ Model 3: Elastic net (alpha=0.5), raw sales ═══\n")
t0 <- Sys.time()

fit_enet_raw <- cv.glmnet(
  x              = X_full_raw,
  y              = y,
  family         = "gaussian",
  alpha          = 0.5,
  penalty.factor = penalty_factor,
  foldid         = foldid,
  standardize    = TRUE
)

cat("  Time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("  lambda.min:", signif(fit_enet_raw$lambda.min, 4), "\n")
cat("  lambda.1se:", signif(fit_enet_raw$lambda.1se, 4), "\n")

s3_min <- extract_suppliers(fit_enet_raw, "lambda.min")
s3_1se <- extract_suppliers(fit_enet_raw, "lambda.1se")
cat("  Non-zero suppliers (lambda.min):", nrow(s3_min),
    "| positive:", sum(s3_min$coef > 0),
    "| negative:", sum(s3_min$coef < 0), "\n")
cat("  Non-zero suppliers (lambda.1se):", nrow(s3_1se),
    "| positive:", sum(s3_1se$coef > 0),
    "| negative:", sum(s3_1se$coef < 0), "\n\n")


# =============================================================================
#   MODEL 4: Elastic net (alpha = 0.5), asinh(sales)
# =============================================================================
cat("═══ Model 4: Elastic net (alpha=0.5), asinh sales ═══\n")
t0 <- Sys.time()

fit_enet_asinh <- cv.glmnet(
  x              = X_full_asinh,
  y              = y,
  family         = "gaussian",
  alpha          = 0.5,
  penalty.factor = penalty_factor,
  foldid         = foldid,
  standardize    = TRUE
)

cat("  Time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("  lambda.min:", signif(fit_enet_asinh$lambda.min, 4), "\n")
cat("  lambda.1se:", signif(fit_enet_asinh$lambda.1se, 4), "\n")

s4_min <- extract_suppliers(fit_enet_asinh, "lambda.min")
s4_1se <- extract_suppliers(fit_enet_asinh, "lambda.1se")
cat("  Non-zero suppliers (lambda.min):", nrow(s4_min),
    "| positive:", sum(s4_min$coef > 0),
    "| negative:", sum(s4_min$coef < 0), "\n")
cat("  Non-zero suppliers (lambda.1se):", nrow(s4_1se),
    "| positive:", sum(s4_1se$coef > 0),
    "| negative:", sum(s4_1se$coef < 0), "\n\n")


# =============================================================================
#   SUMMARY
# =============================================================================
cat("═══ Summary across models ═══\n")

# Collect all identified suppliers into one tidy table
supplier_summary <- bind_rows(
  s1_min %>% mutate(model = "lasso_raw",   lambda = "min"),
  s1_1se %>% mutate(model = "lasso_raw",   lambda = "1se"),
  s2_min %>% mutate(model = "lasso_asinh", lambda = "min"),
  s2_1se %>% mutate(model = "lasso_asinh", lambda = "1se"),
  s3_min %>% mutate(model = "enet_raw",    lambda = "min"),
  s3_1se %>% mutate(model = "enet_raw",    lambda = "1se"),
  s4_min %>% mutate(model = "enet_asinh",  lambda = "min"),
  s4_1se %>% mutate(model = "enet_asinh",  lambda = "1se")
)

# How many models (at lambda.min) select each supplier?
robustness <- supplier_summary %>%
  filter(lambda == "min", coef > 0) %>%
  count(vat_i_ano, name = "n_models") %>%
  arrange(desc(n_models))

cat("Suppliers with positive coef in all 4 models (lambda.min):",
    sum(robustness$n_models == 4), "\n")
cat("Suppliers with positive coef in >= 3 models (lambda.min):",
    sum(robustness$n_models >= 3), "\n")
cat("Suppliers with positive coef in >= 2 models (lambda.min):",
    sum(robustness$n_models >= 2), "\n")
cat("Suppliers with positive coef in >= 1 model  (lambda.min):",
    sum(robustness$n_models >= 1), "\n")


# =============================================================================
#   CV PERFORMANCE
# =============================================================================
cat("\n═══ CV performance (MSE at lambda.min) ═══\n")

cv_summary <- data.frame(
  model      = c("lasso_raw", "lasso_asinh", "enet_raw", "enet_asinh"),
  cvm_min    = c(min(fit_lasso_raw$cvm),   min(fit_lasso_asinh$cvm),
                 min(fit_enet_raw$cvm),     min(fit_enet_asinh$cvm)),
  lambda_min = c(fit_lasso_raw$lambda.min,  fit_lasso_asinh$lambda.min,
                 fit_enet_raw$lambda.min,    fit_enet_asinh$lambda.min)
)
cv_summary$rmse_min <- sqrt(cv_summary$cvm_min)
print(cv_summary)


# ── Save ─────────────────────────────────────────────────────────────────────
OUT_PATH <- file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData")

save(
  fit_lasso_raw, fit_lasso_asinh,
  fit_enet_raw, fit_enet_asinh,
  supplier_summary, robustness, cv_summary,
  file = OUT_PATH
)

cat("\n══════════════════════════════════════════════\n")
cat("All models saved to:", OUT_PATH, "\n")
cat("══════════════════════════════════════════════\n")
