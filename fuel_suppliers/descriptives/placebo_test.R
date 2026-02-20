###############################################################################
# fuel_suppliers/descriptives/placebo_test.R
#
# PURPOSE
#   Test the elastic net signal with placebo LHS variables. If the elastic net
#   identifies a similar number/set of suppliers when predicting a placebo,
#   the original signal may be spurious.
#
#   Three placebos:
#     1) Fully shuffled emissions: permute y across all firm-years.
#        Tests: is there any real signal?
#     2) Revenue as LHS: replace emissions with log(revenue).
#        Tests: is the signal just firm size?
#     3) Shuffled within sector-year: permute y within (nace2d, year) cells.
#        Tests: is the signal firm-specific or just sector-driven?
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#
# OUTPUTS (to console + OUTPUT_DIR)
#   - enet_placebo_test.csv
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
library(glmnet)

set.seed(2024)
N_PERMS <- 50   # number of permutation draws (increase for finer p-values)


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net inputs...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData"))

cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))


# ── Reference: real selection ────────────────────────────────────────────────
# Use enet_asinh pooled (best model) as reference
real_selected <- supplier_summary_pooled %>%
  filter(lambda == "min", model == "enet_asinh", coef > 0) %>%
  pull(vat_i_ano)

cat("Real selection (enet_asinh, pooled): ", length(real_selected), " suppliers\n\n")


# ── Helper: run one elastic net and count selected suppliers ─────────────────
run_enet <- function(y_placebo, label = "") {
  fit <- cv.glmnet(
    x = X_full_asinh,
    y = y_placebo,
    family = "gaussian",
    alpha = 0.5,
    penalty.factor = penalty_factor,
    foldid = foldid,
    type.measure = "mse"
  )

  coefs <- as.matrix(coef(fit, s = "lambda.min"))[-1, , drop = FALSE]
  supplier_idx <- (n_controls + 1):length(penalty_factor)
  supplier_coefs <- coefs[supplier_idx, 1]
  names(supplier_coefs) <- eligible_sellers

  selected <- names(supplier_coefs[supplier_coefs > 0])
  negative <- names(supplier_coefs[supplier_coefs < 0])

  list(
    selected = selected,
    negative = negative,
    n_positive = length(selected),
    n_negative = length(negative),
    overlap_with_real = length(intersect(selected, real_selected)),
    jaccard_with_real = {
      inter <- length(intersect(selected, real_selected))
      union_size <- length(union(selected, real_selected))
      if (union_size == 0) 0 else inter / union_size
    }
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# PLACEBO 1: Fully shuffled emissions
# ══════════════════════════════════════════════════════════════════════════════
cat(strrep("=", 60), "\n")
cat("  PLACEBO 1: Fully shuffled emissions (", N_PERMS, " permutations)\n", sep = "")
cat(strrep("=", 60), "\n\n")

perm_results <- data.frame(
  perm = integer(N_PERMS),
  n_positive = integer(N_PERMS),
  n_negative = integer(N_PERMS),
  overlap_with_real = integer(N_PERMS),
  jaccard_with_real = numeric(N_PERMS)
)

t0 <- Sys.time()
for (i in seq_len(N_PERMS)) {
  y_shuffled <- sample(y)  # full permutation across all firm-years
  res <- run_enet(y_shuffled)

  perm_results$perm[i]              <- i
  perm_results$n_positive[i]        <- res$n_positive
  perm_results$n_negative[i]        <- res$n_negative
  perm_results$overlap_with_real[i] <- res$overlap_with_real
  perm_results$jaccard_with_real[i] <- res$jaccard_with_real

  if (i %% 10 == 0 || i == 1) {
    elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
    cat(sprintf("  Perm %3d/%d: %4d positive, %4d negative (%.1f min elapsed)\n",
                i, N_PERMS, res$n_positive, res$n_negative, elapsed))
  }
}

cat("\nResults (fully shuffled):\n")
cat("  Real selection:                  ", length(real_selected), " suppliers\n")
cat("  Shuffled mean positive selected: ", round(mean(perm_results$n_positive), 1), "\n")
cat("  Shuffled median positive:        ", median(perm_results$n_positive), "\n")
cat("  Shuffled max positive:           ", max(perm_results$n_positive), "\n")
cat("  Shuffled mean overlap with real: ", round(mean(perm_results$overlap_with_real), 1), "\n")
cat("  p-value (perm >= real):          ",
    round(mean(perm_results$n_positive >= length(real_selected)), 3), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# PLACEBO 2: Revenue as LHS
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 60), "\n")
cat("  PLACEBO 2: Revenue as LHS\n")
cat(strrep("=", 60), "\n\n")

# lhs$log_revenue is already in the inputs
y_revenue <- lhs$log_revenue

cat("Running elastic net with revenue as LHS...\n")
t0 <- Sys.time()
rev_result <- run_enet(y_revenue, "revenue")
elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
cat(sprintf("  Done (%.1fs)\n", elapsed))

cat("\nResults (revenue as LHS):\n")
cat("  Suppliers selected (positive):   ", rev_result$n_positive, "\n")
cat("  Suppliers selected (negative):   ", rev_result$n_negative, "\n")
cat("  Overlap with real (emissions):   ", rev_result$overlap_with_real,
    sprintf("(%.1f%% of real)\n", 100 * rev_result$overlap_with_real / length(real_selected)))
cat("  Jaccard with real:               ", round(rev_result$jaccard_with_real, 3), "\n")

# Which real suppliers are NOT selected by revenue?
real_not_revenue <- setdiff(real_selected, rev_result$selected)
cat("  Real suppliers NOT in revenue:   ", length(real_not_revenue),
    sprintf("(%.1f%% of real)\n", 100 * length(real_not_revenue) / length(real_selected)))


# ══════════════════════════════════════════════════════════════════════════════
# PLACEBO 3: Shuffled within sector-year
# ══════════════════════════════════════════════════════════════════════════════
cat("\n", strrep("=", 60), "\n")
cat("  PLACEBO 3: Shuffled within sector-year (", N_PERMS, " permutations)\n", sep = "")
cat(strrep("=", 60), "\n\n")

# Build sector-year groups from lhs
sector_year_groups <- paste0(lhs$nace2d, "_", lhs$year)

perm_within_results <- data.frame(
  perm = integer(N_PERMS),
  n_positive = integer(N_PERMS),
  n_negative = integer(N_PERMS),
  overlap_with_real = integer(N_PERMS),
  jaccard_with_real = numeric(N_PERMS)
)

t0 <- Sys.time()
for (i in seq_len(N_PERMS)) {
  # Shuffle y within sector-year cells
  y_within <- y
  for (grp in unique(sector_year_groups)) {
    idx <- which(sector_year_groups == grp)
    y_within[idx] <- sample(y_within[idx])
  }

  res <- run_enet(y_within)

  perm_within_results$perm[i]              <- i
  perm_within_results$n_positive[i]        <- res$n_positive
  perm_within_results$n_negative[i]        <- res$n_negative
  perm_within_results$overlap_with_real[i] <- res$overlap_with_real
  perm_within_results$jaccard_with_real[i] <- res$jaccard_with_real

  if (i %% 10 == 0 || i == 1) {
    elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
    cat(sprintf("  Perm %3d/%d: %4d positive, %4d negative (%.1f min elapsed)\n",
                i, N_PERMS, res$n_positive, res$n_negative, elapsed))
  }
}

cat("\nResults (shuffled within sector-year):\n")
cat("  Real selection:                  ", length(real_selected), " suppliers\n")
cat("  Shuffled mean positive selected: ", round(mean(perm_within_results$n_positive), 1), "\n")
cat("  Shuffled median positive:        ", median(perm_within_results$n_positive), "\n")
cat("  Shuffled max positive:           ", max(perm_within_results$n_positive), "\n")
cat("  Shuffled mean overlap with real: ", round(mean(perm_within_results$overlap_with_real), 1), "\n")
cat("  p-value (perm >= real):          ",
    round(mean(perm_within_results$n_positive >= length(real_selected)), 3), "\n")


# ══════════════════════════════════════════════════════════════════════════════
# SUMMARY
# ══════════════════════════════════════════════════════════════════════════════
cat("\n\n", strrep("=", 60), "\n")
cat("  SUMMARY\n")
cat(strrep("=", 60), "\n\n")

summary_df <- data.frame(
  test = c("Real (emissions)", "Placebo: shuffled",
           "Placebo: revenue", "Placebo: shuffled within sector-year"),
  n_positive = c(
    length(real_selected),
    round(mean(perm_results$n_positive), 1),
    rev_result$n_positive,
    round(mean(perm_within_results$n_positive), 1)
  ),
  overlap_with_real = c(
    length(real_selected),
    round(mean(perm_results$overlap_with_real), 1),
    rev_result$overlap_with_real,
    round(mean(perm_within_results$overlap_with_real), 1)
  ),
  jaccard_with_real = c(
    1.000,
    round(mean(perm_results$jaccard_with_real), 3),
    round(rev_result$jaccard_with_real, 3),
    round(mean(perm_within_results$jaccard_with_real), 3)
  ),
  stringsAsFactors = FALSE
)

print(summary_df, row.names = FALSE)

cat("\nInterpretation:\n")
cat("  - Shuffled emissions should select ~0 suppliers. If it does, the real\n")
cat("    signal is not an artifact of the data structure.\n")
cat("  - Revenue tests whether the signal is about firm size. High overlap\n")
cat("    means size confounding; low overlap means the signal is fuel-specific.\n")
cat("  - Shuffled within sector-year tests whether the signal is firm-specific\n")
cat("    or just driven by sector composition. ~0 suppliers means firm-level signal.\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write.csv(summary_df,
          file.path(OUTPUT_DIR, "enet_placebo_test.csv"),
          row.names = FALSE)

# Also save detailed permutation results
write.csv(perm_results,
          file.path(OUTPUT_DIR, "enet_placebo_shuffled_detail.csv"),
          row.names = FALSE)
write.csv(perm_within_results,
          file.path(OUTPUT_DIR, "enet_placebo_within_sy_detail.csv"),
          row.names = FALSE)

cat("\nSaved to:", OUTPUT_DIR, "\n")
