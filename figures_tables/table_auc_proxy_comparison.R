###############################################################################
# figures_tables/table_auc_proxy_comparison.R
#
# PURPOSE
#   Compute AUC for the EN-based and Tabachova (NACE-based) fuel-supply proxies
#   as classifiers of EU ETS emitter status, within sectors 17/18, 19, and 24.
#   These are the only sectors in the training sample with both confirmed
#   emitters (EU ETS) and confirmed non-emitters (non-ETS firms in 19/24;
#   non-ETS firms in 17/18 assumed non-emitters).
#
#   Proxies compared:
#     EN:  fold_specific_proxy_all_asinh  (weighted sum of asinh(purchases))
#     Tab: proxy_tabachova                (sum of sales from NACE fuel suppliers)
#          -- asinh-transformed here to match the scale used in fig_proxy_density
#
#   AUC is invariant to monotone transformations, so asinh vs levels does not
#   affect the EN results. For Tab, we report AUC on both raw levels and
#   asinh-transformed values to confirm invariance.
#
# INPUT
#   {PROC_DATA}/firm_year_panel_with_proxies.RData
#
# OUTPUT
#   Console: AUC table by sector x proxy
#   {OUTPUT_DIR}/table_auc_proxy_comparison.csv
#
# RUNS ON: local 1
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_") {
  REPO_DIR <- tryCatch(
    dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
    error = function(e) normalizePath(getwd(), winslash = "/")
  )
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

# ── Load data ────────────────────────────────────────────────────────────────
cat("Loading firm-year panel with proxies...\n")
load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))

panel <- training_sample %>%
  mutate(nace2d = as.character(nace2d))
rm(training_sample)


# ── Helper: AUC via trapezoidal rule ─────────────────────────────────────────
compute_auc <- function(score, label) {
  label <- as.integer(label)
  complete <- !is.na(score) & !is.na(label)
  score <- score[complete]
  label <- label[complete]

  n_pos <- sum(label == 1)
  n_neg <- sum(label == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)

  ord <- order(score, decreasing = TRUE)
  score <- score[ord]
  label <- label[ord]

  thresholds <- sort(unique(score), decreasing = TRUE)
  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    pred_pos  <- score >= thresholds[i]
    tpr[i] <- sum(pred_pos & label == 1) / n_pos
    fpr[i] <- sum(pred_pos & label == 0) / n_neg
  }

  fpr <- c(0, fpr, 1)
  tpr <- c(0, tpr, 1)
  sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
}


# ── Restrict to sectors with known emitter/non-emitter status ────────────────
df <- panel %>%
  filter(nace2d %in% c("17", "18", "19", "24")) %>%
  mutate(
    sector = ifelse(nace2d %in% c("17", "18"), "17/18", nace2d),
    emitter = as.integer(euets == 1)
  )

cat(sprintf("\nObs by sector:\n"))
print(df %>% count(sector, emitter))


# ── Compute AUC by sector x proxy ───────────────────────────────────────────
sectors <- c("19", "24", "17/18", "pooled 19+24", "pooled all")

results <- list()

for (sec in sectors) {
  sub <- switch(sec,
    "pooled 19+24" = df %>% filter(sector %in% c("19", "24")),
    "pooled all"   = df,
    df %>% filter(sector == sec)
  )

  auc_en  <- compute_auc(sub$fold_specific_proxy_all_asinh, sub$emitter)
  auc_tab <- compute_auc(sub$proxy_tabachova,               sub$emitter)
  # Confirm monotone-invariance: AUC should be identical with asinh(tab)
  auc_tab_asinh <- compute_auc(asinh(pmax(sub$proxy_tabachova, 0)), sub$emitter)

  results[[sec]] <- data.frame(
    sector         = sec,
    n_obs          = nrow(sub),
    n_emitters     = sum(sub$emitter == 1),
    n_nonemitters  = sum(sub$emitter == 0),
    auc_en         = round(auc_en,         3),
    auc_tab        = round(auc_tab,         3),
    auc_tab_asinh  = round(auc_tab_asinh,   3),
    stringsAsFactors = FALSE
  )
}

results_df <- bind_rows(results)


# ── Print ────────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  AUC: EN vs Tabachova proxy as emitter classifier\n")
cat("══════════════════════════════════════════════════════════════════\n\n")
print(results_df, row.names = FALSE)
cat("\nNote: auc_tab and auc_tab_asinh should be identical (monotone invariance).\n")
cat("      EN proxy = fold_specific_proxy_all_asinh (positive + negative EN coefs).\n")
cat("      Tab proxy = proxy_tabachova (sum of sales from NACE fuel suppliers).\n")


# ── Save ─────────────────────────────────────────────────────────────────────
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, "table_auc_proxy_comparison.csv")
write.csv(results_df, out_path, row.names = FALSE)
cat("\nSaved:", out_path, "\n")
