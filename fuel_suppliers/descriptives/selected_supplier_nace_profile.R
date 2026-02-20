###############################################################################
# fuel_suppliers/descriptives/selected_supplier_nace_profile.R
#
# PURPOSE
#   Profile the NACE sectors of elastic-net-selected suppliers. Separate
#   suppliers into those with positive vs negative mean coefficients and show
#   the top NACE 4-digit codes in each group.
#
#   Positive-coef suppliers: firms whose sales to buyers are positively
#     correlated with buyer emissions (likely fuel suppliers).
#   Negative-coef suppliers: firms whose sales are negatively correlated
#     with emissions (possibly clean substitutes, services, etc.).
#
# INPUTS
#   - {INT_DATA}/fuel_suppliers_elastic_net_results.RData
#   - {PROC_DATA}/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS (to console + OUTPUT_DIR)
#   - enet_supplier_nace_profile.csv
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


# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading elastic net results...\n")
load(file.path(INT_DATA, "fuel_suppliers_elastic_net_results.RData"))

cat("Loading annual accounts...\n")
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))


# ── Build supplier-level summary ─────────────────────────────────────────────
# For each supplier, compute mean coefficient across the 4 pooled models at
# lambda.min. Suppliers not selected in a given model get coef = 0 for that
# model when computing the mean.

all_models <- c("lasso_raw", "lasso_asinh", "enet_raw", "enet_asinh")

supplier_coefs <- supplier_summary_pooled %>%
  filter(lambda == "min") %>%
  select(vat_i_ano, model, coef)

# Expand to full grid (supplier x model), fill missing with 0
supplier_grid <- tidyr::expand_grid(
  vat_i_ano = unique(supplier_coefs$vat_i_ano),
  model = all_models
) %>%
  left_join(supplier_coefs, by = c("vat_i_ano", "model")) %>%
  mutate(coef = coalesce(coef, 0))

supplier_mean <- supplier_grid %>%
  group_by(vat_i_ano) %>%
  summarise(
    mean_coef   = mean(coef),
    n_models    = sum(coef != 0),
    n_positive  = sum(coef > 0),
    n_negative  = sum(coef < 0),
    .groups = "drop"
  ) %>%
  mutate(sign_group = if_else(mean_coef > 0, "positive", "negative"))

cat("\nSuppliers by mean coefficient sign:\n")
cat("  Positive: ", sum(supplier_mean$sign_group == "positive"), "\n")
cat("  Negative: ", sum(supplier_mean$sign_group == "negative"), "\n")


# ── Match suppliers to NACE codes ────────────────────────────────────────────
# Use the modal NACE 5-digit code for each firm across all years
nace_lookup <- df_annual_accounts_selected_sample_key_variables %>%
  filter(!is.na(nace5d)) %>%
  group_by(vat, nace5d) %>%
  summarise(n_years = n(), .groups = "drop") %>%
  group_by(vat) %>%
  slice_max(n_years, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(nace4d = substr(nace5d, 1, 4),
         nace2d = substr(nace5d, 1, 2)) %>%
  select(vat, nace5d, nace4d, nace2d)

supplier_nace <- supplier_mean %>%
  left_join(nace_lookup, by = c("vat_i_ano" = "vat"))

n_matched   <- sum(!is.na(supplier_nace$nace4d))
n_unmatched <- sum(is.na(supplier_nace$nace4d))
cat("\nNACE match rate: ", n_matched, " matched, ",
    n_unmatched, " unmatched (",
    round(100 * n_unmatched / nrow(supplier_nace), 1), "% missing)\n", sep = "")


# ── Top NACE codes by group ─────────────────────────────────────────────────
print_top_nace <- function(df, group_label, top_n = 10, concentration_n = 5) {
  cat(sprintf("\n%s\n  %s (N = %d suppliers)\n%s\n",
              strrep("=", 60), group_label, nrow(df), strrep("=", 60)))

  df_with_nace <- df %>% filter(!is.na(nace4d))
  if (nrow(df_with_nace) == 0) {
    cat("  No NACE codes available.\n")
    return(invisible(NULL))
  }

  n_unique_nace <- n_distinct(df_with_nace$nace4d)

  top <- df_with_nace %>%
    count(nace4d, nace2d, sort = TRUE) %>%
    head(top_n) %>%
    mutate(
      share = round(100 * n / nrow(df_with_nace), 1),
      cum_share = round(100 * cumsum(n) / nrow(df_with_nace), 1)
    )

  cat(sprintf("\n  Total unique NACE 4d codes: %d\n", n_unique_nace))
  cat(sprintf("  Top %d NACE 4d codes:\n\n", top_n))
  cat(sprintf("  %-8s  %-8s  %6s  %7s  %9s\n",
              "NACE4d", "NACE2d", "Count", "Share%", "Cum.Share%"))
  cat("  ", strrep("-", 50), "\n")
  for (i in seq_len(nrow(top))) {
    cat(sprintf("  %-8s  %-8s  %6d  %6.1f%%  %8.1f%%\n",
                top$nace4d[i], top$nace2d[i], top$n[i],
                top$share[i], top$cum_share[i]))
  }

  top5_share <- top %>% head(concentration_n) %>% summarise(s = sum(n)) %>% pull(s)
  cat(sprintf("\n  Top %d NACE codes cover %d / %d suppliers (%.1f%%)\n",
              concentration_n, top5_share, nrow(df_with_nace),
              100 * top5_share / nrow(df_with_nace)))
  cat(sprintf("  Top %d represent %d / %d unique NACE codes (%.1f%% of NACE variety)\n",
              concentration_n, min(concentration_n, n_unique_nace), n_unique_nace,
              100 * min(concentration_n, n_unique_nace) / n_unique_nace))

  invisible(top)
}

top_pos <- print_top_nace(
  supplier_nace %>% filter(sign_group == "positive"),
  "POSITIVE coefficient suppliers (fuel-linked)"
)

top_neg <- print_top_nace(
  supplier_nace %>% filter(sign_group == "negative"),
  "NEGATIVE coefficient suppliers"
)


# ── Also show NACE 2-digit summary ──────────────────────────────────────────
cat("\n\n")
cat(strrep("=", 60), "\n")
cat("  NACE 2-digit summary (positive-coef suppliers)\n")
cat(strrep("=", 60), "\n\n")

nace2d_summary <- supplier_nace %>%
  filter(sign_group == "positive", !is.na(nace2d)) %>%
  count(nace2d, sort = TRUE) %>%
  mutate(share = round(100 * n / sum(n), 1))

print(as.data.frame(nace2d_summary), row.names = FALSE)


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

out <- supplier_nace %>%
  select(vat_i_ano, sign_group, mean_coef, n_models, n_positive, n_negative,
         nace5d, nace4d, nace2d) %>%
  arrange(sign_group, desc(abs(mean_coef)))

write.csv(out,
          file.path(OUTPUT_DIR, "enet_supplier_nace_profile.csv"),
          row.names = FALSE)
cat("\nSaved to:", file.path(OUTPUT_DIR, "enet_supplier_nace_profile.csv"), "\n")
