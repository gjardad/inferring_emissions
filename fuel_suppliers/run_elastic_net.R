###############################################################################
# fuel_suppliers/run_elastic_net.R
#
# PURPOSE
#   Run elastic net (cv.glmnet) to identify fuel suppliers from B2B data.
#   Two specifications: pooled (sector FE) and within-buyer (buyer FE).
#   Four model variants per specification:
#     {lasso, elastic net} x {raw sales, asinh(sales)}.
#   CV uses group k-fold by firm (all years of a firm in the same fold).
#
# INPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#     (produced by fuel_suppliers/build_design_matrix.R)
#
# OUTPUT
#   {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#     - cv.glmnet fit objects for each model variant & specification
#     - supplier_summary_pooled / supplier_summary_fe: tidy dataframes
#     - robustness_pooled / robustness_fe: cross-model selection counts
#     - cv_summary: comparative CV performance table
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

cat("Pooled design matrix:       ", nrow(X_full_raw), "x", ncol(X_full_raw),
    "(", n_controls, "controls +", ncol(X_full_raw) - n_controls, "suppliers)\n")
cat("Within-buyer design matrix: ", nrow(X_full_raw_fe), "x", ncol(X_full_raw_fe),
    "(", n_controls_fe, "controls +", ncol(X_full_raw_fe) - n_controls_fe, "suppliers)\n")
cat("K-fold groups:", K_FOLDS, "\n\n")


# ── Helper: extract supplier coefficients ────────────────────────────────────
extract_suppliers <- function(fit, n_ctrl, s = "lambda.min") {
  co <- coef(fit, s = s)
  # Positions: 1 = intercept, 2:(n_ctrl+1) = controls,
  #            (n_ctrl+2):end = supplier columns
  supplier_idx <- (n_ctrl + 2):length(co)
  vals <- co[supplier_idx]
  data.frame(
    vat_i_ano = eligible_sellers,
    coef      = as.numeric(vals),
    stringsAsFactors = FALSE
  ) %>%
    filter(coef != 0) %>%
    arrange(desc(abs(coef)))
}


# ── Helper: run one specification ────────────────────────────────────────────
run_one_spec <- function(X_raw, X_asinh, pf, n_ctrl, spec_label) {

  model_grid <- list(
    list(alpha = 1,   X = X_raw,   label = "Lasso, raw sales"),
    list(alpha = 1,   X = X_asinh, label = "Lasso, asinh sales"),
    list(alpha = 0.5, X = X_raw,   label = "Elastic net (alpha=0.5), raw sales"),
    list(alpha = 0.5, X = X_asinh, label = "Elastic net (alpha=0.5), asinh sales")
  )
  model_keys <- c("lasso_raw", "lasso_asinh", "enet_raw", "enet_asinh")

  fits <- list()
  summaries <- list()

  for (k in seq_along(model_grid)) {
    m <- model_grid[[k]]
    cat(sprintf("═══ %s: %s ═══\n", spec_label, m$label))
    t0 <- Sys.time()

    fit <- cv.glmnet(
      x              = m$X,
      y              = y,
      family         = "gaussian",
      alpha          = m$alpha,
      penalty.factor = pf,
      foldid         = foldid,
      standardize    = TRUE
    )

    cat("  Time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
    cat("  lambda.min:", signif(fit$lambda.min, 4), "\n")
    cat("  lambda.1se:", signif(fit$lambda.1se, 4), "\n")

    s_min <- extract_suppliers(fit, n_ctrl, "lambda.min")
    s_1se <- extract_suppliers(fit, n_ctrl, "lambda.1se")
    cat("  Non-zero suppliers (lambda.min):", nrow(s_min),
        "| positive:", sum(s_min$coef > 0),
        "| negative:", sum(s_min$coef < 0), "\n")
    cat("  Non-zero suppliers (lambda.1se):", nrow(s_1se),
        "| positive:", sum(s_1se$coef > 0),
        "| negative:", sum(s_1se$coef < 0), "\n\n")

    fits[[model_keys[k]]] <- fit
    summaries[[model_keys[k]]] <- bind_rows(
      s_min %>% mutate(model = model_keys[k], lambda = "min"),
      s_1se %>% mutate(model = model_keys[k], lambda = "1se")
    )
  }

  supplier_summary <- bind_rows(summaries)

  robustness <- supplier_summary %>%
    filter(lambda == "min", coef > 0) %>%
    count(vat_i_ano, name = "n_models") %>%
    arrange(desc(n_models))

  cv_tbl <- data.frame(
    model      = model_keys,
    cvm_min    = sapply(fits, function(f) min(f$cvm)),
    lambda_min = sapply(fits, function(f) f$lambda.min)
  )
  cv_tbl$rmse_min <- sqrt(cv_tbl$cvm_min)

  cat(sprintf("═══ %s: Summary ═══\n", spec_label))
  cat("Suppliers with positive coef in all 4 models (lambda.min):",
      sum(robustness$n_models == 4), "\n")
  cat("Suppliers with positive coef in >= 3 models (lambda.min):",
      sum(robustness$n_models >= 3), "\n")
  cat("Suppliers with positive coef in >= 2 models (lambda.min):",
      sum(robustness$n_models >= 2), "\n")
  cat("Suppliers with positive coef in >= 1 model  (lambda.min):",
      sum(robustness$n_models >= 1), "\n\n")

  cat("CV performance (MSE at lambda.min):\n")
  print(cv_tbl)
  cat("\n")

  list(fits = fits, supplier_summary = supplier_summary,
       robustness = robustness, cv_summary = cv_tbl)
}


# =============================================================================
#   RUN POOLED SPECIFICATION (sector FE)
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# POOLED SPECIFICATION (sector FE)                           #\n",
    "##############################################################\n\n")

res_pooled <- run_one_spec(X_full_raw, X_full_asinh,
                           penalty_factor, n_controls,
                           "Pooled")


# =============================================================================
#   RUN WITHIN-BUYER SPECIFICATION (buyer FE)
# =============================================================================
cat("\n",
    "##############################################################\n",
    "# WITHIN-BUYER SPECIFICATION (buyer FE)                      #\n",
    "##############################################################\n\n")

res_fe <- run_one_spec(X_full_raw_fe, X_full_asinh_fe,
                       penalty_factor_fe, n_controls_fe,
                       "Within-buyer")


# ── Combined CV comparison ───────────────────────────────────────────────────
cv_summary <- bind_rows(
  res_pooled$cv_summary %>% mutate(spec = "pooled"),
  res_fe$cv_summary     %>% mutate(spec = "within_buyer")
)

cat("═══ CV performance comparison ═══\n")
print(cv_summary)


# ── Save ─────────────────────────────────────────────────────────────────────
OUT_PATH <- file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData")

# Unpack fit objects for direct access downstream
fit_pooled <- res_pooled$fits
fit_fe     <- res_fe$fits
supplier_summary_pooled <- res_pooled$supplier_summary
supplier_summary_fe     <- res_fe$supplier_summary
robustness_pooled       <- res_pooled$robustness
robustness_fe           <- res_fe$robustness

save(
  fit_pooled, fit_fe,
  supplier_summary_pooled, supplier_summary_fe,
  robustness_pooled, robustness_fe,
  cv_summary,
  eligible_sellers, n_controls, n_controls_fe,
  file = OUT_PATH
)

# ── Export supplier lists to OUTPUT_DIR ───────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(supplier_summary_pooled,
          file.path(OUTPUT_DIR, "fuel_suppliers_selected_pooled.csv"),
          row.names = FALSE)
write.csv(supplier_summary_fe,
          file.path(OUTPUT_DIR, "fuel_suppliers_selected_within_buyer.csv"),
          row.names = FALSE)

cat("\n══════════════════════════════════════════════\n")
cat("All models saved to:", OUT_PATH, "\n")
cat("Supplier lists exported to:", OUTPUT_DIR, "\n")
cat("══════════════════════════════════════════════\n")
