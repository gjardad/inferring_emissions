###############################################################################
# fuel_suppliers/00_data_dimensions.R
#
# PURPOSE
#   Compute key data dimensions for:
#   (a) the fuel-supplier elastic net (sections 1-3)
#   (b) future Leontief inverse calculations (section 4)
#   Run on the RMD where the full (non-downsampled) B2B data lives.
#
# OUTPUT
#   Saves results incrementally to {INT_DATA}/fuel_suppliers_dimensions.rds
#   Each section is saved as a named element in the list. On re-run, sections
#   with existing results are skipped (delete the .rds to force a full re-run).
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

#library(haven)
library(dplyr)
library(igraph)

OUT_PATH <- file.path(INT_DATA, "fuel_suppliers_dimensions.rds")

# Load existing results if any; otherwise start fresh
if (file.exists(OUT_PATH)) {
  dims <- readRDS(OUT_PATH)
  cat("Loaded existing results from", OUT_PATH, "\n")
  cat("Sections already done:", paste(names(dims), collapse = ", "), "\n\n")
} else {
  dims <- list()
}

# Helper: save dims to disk after each section
save_dims <- function() saveRDS(dims, OUT_PATH)


# ── Load data (always needed) ───────────────────────────────────────────────

cat("Loading B2B data...\n")
load(file.path(PROC_DATA, "b2b_selected_sample.RData"))
b2b <- df_b2b_selected_sample

cat("Loading LOOCV training sample...\n")
load(file.path(PROC_DATA, "loocv_training_sample.RData"))

# LHS firms: the unique firm IDs in the training sample (EU ETS + NACE 19/24)
lhs_firms <- unique(loocv_training_sample$vat)


# =============================================================================
#   1. FULL B2B DIMENSIONS
# =============================================================================

if (is.null(dims[["b2b_full"]])) {
  cat("\n── Section 1: Full B2B dimensions ──\n")

  dims[["b2b_full"]] <- list(
    total_rows      = nrow(b2b),
    unique_sellers  = length(unique(b2b$vat_i_ano)),
    unique_buyers   = length(unique(b2b$vat_j_ano)),
    year_min        = min(b2b$year),
    year_max        = max(b2b$year),
    rows_per_year   = as.list(table(b2b$year))
  )
  save_dims()

  cat("Total rows:        ", dims$b2b_full$total_rows, "\n")
  cat("Unique sellers:    ", dims$b2b_full$unique_sellers, "\n")
  cat("Unique buyers:     ", dims$b2b_full$unique_buyers, "\n")
  cat("Year range:        ", dims$b2b_full$year_min, "-", dims$b2b_full$year_max, "\n")
  cat("Saved.\n")
} else {
  cat("\n── Section 1: SKIPPED (already saved) ──\n")
}


# =============================================================================
#   2. SELLERS SUPPLYING TO LHS FIRMS (pre-filtering thresholds)
# =============================================================================

if (is.null(dims[["lhs_sellers"]])) {
  cat("\n── Section 2: Sellers supplying to LHS firms ──\n")

  # Check ID overlap
  b2b_buyers <- unique(b2b$vat_j_ano)
  id_overlap <- sum(lhs_firms %in% b2b_buyers)
  cat("LHS firms (LOOCV sample):   ", length(lhs_firms), "\n")
  cat("LHS firms found in B2B buyers:", id_overlap, "\n")

  # If overlap is zero, try vat_ano crosswalk
  id_used <- "vat"
  if (id_overlap == 0) {
    cat("\n*** WARNING: No overlap between LHS vat and B2B vat_j_ano.\n")
    if (file.exists(file.path(PROC_DATA,
                              "annual_accounts_selected_sample_key_variables.RData"))) {
      load(file.path(PROC_DATA,
                     "annual_accounts_selected_sample_key_variables.RData"))
      if ("vat_ano" %in% names(df_annual_accounts_selected_sample_key_variables)) {
        cat("    Retrying with vat_ano...\n")
        aa <- df_annual_accounts_selected_sample_key_variables %>%
          select(vat, vat_ano) %>%
          distinct()
        lhs_firms_ano <- aa$vat_ano[aa$vat %in% lhs_firms]
        id_overlap <- sum(lhs_firms_ano %in% b2b_buyers)
        cat("    LHS vat_ano found in B2B:", id_overlap, "\n")
        lhs_firms <- lhs_firms_ano
        id_used <- "vat_ano"
      }
    }
  }

  # Filter B2B to LHS buyers
  b2b_lhs <- b2b %>% filter(vat_j_ano %in% lhs_firms)

  # Count distinct LHS buyers per seller (across all years)
  seller_lhs_counts <- b2b_lhs %>%
    distinct(vat_i_ano, vat_j_ano) %>%
    count(vat_i_ano, name = "n_lhs_buyers")

  thresholds <- c(1, 2, 3, 5, 10, 20)
  sellers_by_threshold <- setNames(
    sapply(thresholds, function(k) sum(seller_lhs_counts$n_lhs_buyers >= k)),
    paste0("ge_", thresholds)
  )

  dims[["lhs_sellers"]] <- list(
    id_field_used       = id_used,
    n_lhs_firms         = length(lhs_firms),
    id_overlap          = id_overlap,
    b2b_rows_lhs        = nrow(b2b_lhs),
    sellers_by_threshold = as.list(sellers_by_threshold)
  )
  save_dims()

  cat("\nB2B rows involving LHS buyers:", nrow(b2b_lhs), "\n")
  cat("Sellers by minimum LHS buyers served:\n")
  for (k in thresholds) {
    cat(sprintf("  >= %2d: %6d sellers\n", k, sellers_by_threshold[paste0("ge_", k)]))
  }
  cat("Saved.\n")
} else {
  cat("\n── Section 2: SKIPPED (already saved) ──\n")
  # Rebuild b2b_lhs and seller_lhs_counts (needed by section 3)
  if (dims$lhs_sellers$id_field_used == "vat_ano") {
    load(file.path(PROC_DATA,
                   "annual_accounts_selected_sample_key_variables.RData"))
    aa <- df_annual_accounts_selected_sample_key_variables %>%
      select(vat, vat_ano) %>% distinct()
    lhs_firms <- aa$vat_ano[aa$vat %in% lhs_firms]
  }
  b2b_lhs <- b2b %>% filter(vat_j_ano %in% lhs_firms)
  seller_lhs_counts <- b2b_lhs %>%
    distinct(vat_i_ano, vat_j_ano) %>%
    count(vat_i_ano, name = "n_lhs_buyers")
}


# =============================================================================
#   3. DESIGN MATRIX SPARSITY
# =============================================================================

if (is.null(dims[["design_matrix"]])) {
  cat("\n── Section 3: Design matrix sparsity ──\n")

  threshold <- 3
  eligible_sellers <- seller_lhs_counts %>%
    filter(n_lhs_buyers >= threshold) %>%
    pull(vat_i_ano)

  b2b_matrix <- b2b_lhs %>%
    filter(vat_i_ano %in% eligible_sellers, year >= 2005)

  lhs_firmyears <- loocv_training_sample %>%
    distinct(vat, year) %>%
    nrow()

  nonzero <- b2b_matrix %>%
    distinct(vat_j_ano, year, vat_i_ano) %>%
    nrow()

  total_cells <- lhs_firmyears * length(eligible_sellers)
  sparsity <- 1 - nonzero / total_cells

  dims[["design_matrix"]] <- list(
    threshold        = threshold,
    n_eligible       = length(eligible_sellers),
    lhs_firmyears    = lhs_firmyears,
    nonzero_entries  = nonzero,
    total_cells      = total_cells,
    sparsity_pct     = round(sparsity * 100, 4)
  )
  save_dims()

  cat("Threshold: >=", threshold, "LHS buyers\n")
  cat("Eligible sellers (cols):", length(eligible_sellers), "\n")
  cat("LHS firm-years (rows):  ", lhs_firmyears, "\n")
  cat("Non-zero entries:       ", nonzero, "\n")
  cat(sprintf("Sparsity: %.4f%% zeros\n", sparsity * 100))
  cat("Saved.\n")
} else {
  cat("\n── Section 3: SKIPPED (already saved) ──\n")
}


# =============================================================================
#   4. LEONTIEF INVERSE DIMENSIONS (full B2B network, per year)
# =============================================================================

if (is.null(dims[["leontief"]])) {
  cat("\n── Section 4: Leontief inverse dimensions ──\n")

  years <- sort(unique(b2b$year[b2b$year >= 2005]))

  # 4a. Per-year network dimensions
  cat("Computing per-year network stats...\n")
  all_firms_by_year <- b2b %>%
    filter(year >= 2005) %>%
    group_by(year) %>%
    summarise(
      n_sellers    = n_distinct(vat_i_ano),
      n_buyers     = n_distinct(vat_j_ano),
      n_firms      = n_distinct(c(vat_i_ano, vat_j_ano)),
      n_both_sides = length(intersect(unique(vat_i_ano), unique(vat_j_ano))),
      n_edges      = n(),
      .groups = "drop"
    ) %>%
    mutate(
      density        = n_edges / (n_firms * (n_firms - 1)),
      n_only_seller  = n_sellers - n_both_sides,
      n_only_buyer   = n_buyers - n_both_sides
    )

  cat("  Done. N ranges from", min(all_firms_by_year$n_firms),
      "to", max(all_firms_by_year$n_firms), "\n")

  # 4b. Max degree per year
  cat("Computing max degree per year...\n")
  degree_stats <- list()
  for (yr in years) {
    b2b_yr <- b2b %>% filter(year == yr)
    out_deg <- b2b_yr %>% count(vat_i_ano) %>% pull(n)
    in_deg  <- b2b_yr %>% count(vat_j_ano) %>% pull(n)
    degree_stats[[as.character(yr)]] <- list(
      max_out_degree = max(out_deg),
      max_in_degree  = max(in_deg)
    )
    cat(sprintf("  %d: max out = %d, max in = %d\n",
                yr, max(out_deg), max(in_deg)))
  }

  # 4c. Connected components per year
  cat("Computing connected components per year...\n")
  component_stats <- list()
  for (yr in years) {
    b2b_yr <- b2b %>% filter(year == yr)
    g <- graph_from_data_frame(
      b2b_yr %>% select(vat_i_ano, vat_j_ano),
      directed = TRUE
    )
    comp <- components(g, mode = "weak")
    sizes <- sort(comp$csize, decreasing = TRUE)
    component_stats[[as.character(yr)]] <- list(
      n_components = comp$no,
      largest      = sizes[1],
      second       = ifelse(length(sizes) >= 2, sizes[2], 0),
      third        = ifelse(length(sizes) >= 3, sizes[3], 0)
    )
    cat(sprintf("  %d: %d components (largest: %d)\n",
                yr, comp$no, sizes[1]))
  }

  dims[["leontief"]] <- list(
    per_year_network = as.data.frame(all_firms_by_year),
    degree_stats     = degree_stats,
    component_stats  = component_stats
  )
  save_dims()
  cat("Saved.\n")
} else {
  cat("\n── Section 4: SKIPPED (already saved) ──\n")
}


cat("\n══════════════════════════════════════════════\n")
cat("All sections complete. Results saved to:\n")
cat(OUT_PATH, "\n")
cat("══════════════════════════════════════════════\n")
