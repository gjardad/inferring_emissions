###############################################################################
# fuel_suppliers/descriptives/supplier_selection_stability.R
#
# PURPOSE
#   Assess how stable the elastic net supplier selection is to perturbations
#   in the sample. Uses a jackknife approach: for each of the K=10 folds,
#   drop that fold and rerun elastic net on the remaining 90% of the data.
#   Then compare the set of selected suppliers across jackknife runs and
#   against the full-data selection.
#
#   If the Jaccard similarity across runs is high, the selection is stable.
#   If low, the specific firms in the training sample strongly influence
#   which suppliers get selected.
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_inputs.RData
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#
# OUTPUTS (to console + OUTPUT_DIR)
#   - enet_supplier_stability.csv
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


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net inputs...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_inputs.RData"))

cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))


# ── Full-data selection (pooled, enet_asinh, lambda.min) ─────────────────────
# Use enet_asinh as reference (best CV performance in pooled spec)
full_selected <- supplier_summary_pooled %>%
  filter(lambda == "min", model == "enet_asinh", coef > 0) %>%
  pull(vat_i_ano)

cat("Full-data selection (enet_asinh, pooled): ", length(full_selected), " suppliers\n\n")


# ── Jackknife: drop each fold and rerun ──────────────────────────────────────
K <- max(foldid)
cat("Running jackknife over", K, "folds...\n\n")

jackknife_sets <- vector("list", K)
jackknife_n    <- integer(K)

for (k in seq_len(K)) {
  t0 <- Sys.time()

  # Rows to keep (drop fold k)
  keep <- which(foldid != k)

  X_k <- X_full_asinh[keep, , drop = FALSE]
  y_k <- y[keep]

  # Create new fold IDs for internal CV (the remaining folds become 1..K-1)
  old_folds <- foldid[keep]
  fold_levels <- sort(unique(old_folds))
  new_foldid <- match(old_folds, fold_levels)

  fit_k <- cv.glmnet(
    x = X_k,
    y = y_k,
    family = "gaussian",
    alpha = 0.5,           # elastic net
    penalty.factor = penalty_factor,
    foldid = new_foldid,
    type.measure = "mse"
  )

  # Extract supplier coefficients at lambda.min
  coefs_k <- as.matrix(coef(fit_k, s = "lambda.min"))[-1, , drop = FALSE]
  supplier_idx <- (n_controls + 1):length(penalty_factor)
  supplier_coefs_k <- coefs_k[supplier_idx, 1]
  names(supplier_coefs_k) <- eligible_sellers

  selected_k <- names(supplier_coefs_k[supplier_coefs_k > 0])
  jackknife_sets[[k]] <- selected_k
  jackknife_n[k] <- length(selected_k)

  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  cat(sprintf("  Fold %2d dropped: %4d suppliers selected (%.1fs)\n",
              k, length(selected_k), elapsed))
}


# ── Jaccard similarity ──────────────────────────────────────────────────────
jaccard <- function(a, b) {
  inter <- length(intersect(a, b))
  union_size <- length(union(a, b))
  if (union_size == 0) return(1)
  inter / union_size
}

# Pairwise Jaccard between jackknife runs
pair_jaccards <- numeric(0)
for (i in 1:(K - 1)) {
  for (j in (i + 1):K) {
    pair_jaccards <- c(pair_jaccards, jaccard(jackknife_sets[[i]], jackknife_sets[[j]]))
  }
}

# Jaccard between each jackknife run and full-data selection
full_jaccards <- sapply(jackknife_sets, function(s) jaccard(s, full_selected))


# ── Results ──────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 60), "\n")
cat("  Supplier selection stability (jackknife)\n")
cat(strrep("=", 60), "\n\n")

cat("Full-data selection: ", length(full_selected), " suppliers\n\n")

cat("Jackknife runs (suppliers selected per run):\n")
cat("  Mean:   ", round(mean(jackknife_n), 1), "\n")
cat("  SD:     ", round(sd(jackknife_n), 1), "\n")
cat("  Min:    ", min(jackknife_n), "\n")
cat("  Max:    ", max(jackknife_n), "\n\n")

cat("Pairwise Jaccard similarity (between jackknife runs):\n")
cat("  Mean:   ", round(mean(pair_jaccards), 3), "\n")
cat("  SD:     ", round(sd(pair_jaccards), 3), "\n")
cat("  Min:    ", round(min(pair_jaccards), 3), "\n")
cat("  Max:    ", round(max(pair_jaccards), 3), "\n\n")

cat("Jaccard with full-data selection:\n")
cat("  Mean:   ", round(mean(full_jaccards), 3), "\n")
cat("  SD:     ", round(sd(full_jaccards), 3), "\n")
cat("  Min:    ", round(min(full_jaccards), 3), "\n")
cat("  Max:    ", round(max(full_jaccards), 3), "\n\n")

# How many suppliers appear in ALL jackknife runs?
all_suppliers <- unique(unlist(jackknife_sets))
appearance_count <- sapply(all_suppliers, function(s) {
  sum(sapply(jackknife_sets, function(set) s %in% set))
})

stable_all   <- sum(appearance_count == K)
stable_most  <- sum(appearance_count >= K - 1)  # appear in >= 9/10
stable_half  <- sum(appearance_count >= K / 2)   # appear in >= 5/10

cat("Supplier stability across", K, "jackknife runs:\n")
cat("  Appear in all", K, "runs:    ", stable_all, "\n")
cat("  Appear in >=", K - 1, "runs:     ", stable_most, "\n")
cat("  Appear in >=", K / 2, "runs:      ", stable_half, "\n")
cat("  Appear in only 1 run:    ", sum(appearance_count == 1), "\n")
cat("  Total unique suppliers:  ", length(all_suppliers), "\n")


# ── Interpretation guide ─────────────────────────────────────────────────────
cat("\nInterpretation:\n")
cat("  Jaccard > 0.7:  Strong stability — the selection is robust to removing 10% of firms.\n")
cat("  Jaccard 0.4-0.7: Moderate stability — a core set is stable, but the fringe changes.\n")
cat("  Jaccard < 0.4:  Weak stability — the selection is sensitive to the specific sample.\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

stability_df <- data.frame(
  vat_i_ano = all_suppliers,
  n_jackknife_appearances = appearance_count[all_suppliers],
  in_full_data = all_suppliers %in% full_selected,
  stringsAsFactors = FALSE
) %>%
  arrange(desc(n_jackknife_appearances))

write.csv(stability_df,
          file.path(OUTPUT_DIR, "enet_supplier_stability.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "enet_supplier_stability.csv"), "\n")
