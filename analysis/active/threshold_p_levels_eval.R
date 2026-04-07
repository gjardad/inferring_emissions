# threshold_p_levels_eval.R
#
# Purpose: take the p_i thresholds tuned in threshold_p_mixed_sectors.R
# (scheme a, leave-one-sector-out across paper / refining / metals) and use
# them to build *level* predictions for each held-out sector. Evaluate the
# resulting predictions with calc_metrics, matching the metrics reported in
# table_main_with_mixed_crf.R panel B.
#
# Pipeline per held-out sector g:
#   1. tau_g = average of best-J thresholds on the OTHER two sectors (scheme a,
#      computed exactly as in threshold_p_mixed_sectors.R).
#   2. Classify firm-year i as predicted-emitter iff p_i >= tau_g.
#   3. For each (sector g, year t) cell:
#        E_gt = sum of true y in the cell  (oracle sector-year total)
#        Distribute E_gt proportionally to proxy_mean among the firm-years
#        classified as emitters in the cell, taking pmax(proxy_mean, 0).
#        - If no firm-year is classified as emitter, all predictions in the
#          cell are 0 (true emitters get FN of magnitude y_i).
#        - If classified emitters have all-zero proxy_mean, distribute E_gt
#          uniformly among them.
#        Firm-years not classified as emitters get yhat = 0.
#   4. Pool the held-out predictions across the three sectors and compute
#      calc_metrics on the pooled vector.
#
# Notes
#   - Eval subset and redistribution cells now match panel B exactly:
#       eval set       = firm-years with nace2d in {17,18,17/18,19,24,25}
#       redistribution = primary_crf_group x year cells (firm-level CRF group)
#     primary_crf_group is constant across years for a given firm; it is
#     determined by the firm's modal NACE 2d via crf_group_map (see
#     repeated_cv_proxy_panel$primary_crf_group).
#   - For threshold-tuning purposes, sectors are still {paper, refining,
#     metals} based on the firm-year nace2d. The threshold-tuning step
#     (LOSO over the three sectors) is unchanged.
#   - This is an "oracle" sector-year-total evaluation: the true totals are
#     used as the calibration target. It isolates the contribution of
#     (extensive-margin classification + within-cell ranking by proxy_mean)
#     from any error in the totals themselves.

suppressPackageStartupMessages({
  library(dplyr)
})

source("paths.R")
source("utils/calc_metrics.R")

in_path  <- file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData")
out_dir  <- file.path("analysis", "active", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

load(in_path)
stopifnot(nrow(proxy_matrix) == nrow(firmyear_index))

# --- Step 1: per firm-year p_i and proxy_mean -------------------------------
pos_mat   <- proxy_matrix > 0
n_obs     <- rowSums(!is.na(proxy_matrix))
n_pos     <- rowSums(pos_mat, na.rm = TRUE)
p_i       <- n_pos / n_obs
proxy_avg <- rowMeans(proxy_matrix, na.rm = TRUE)

panel <- firmyear_index %>%
  mutate(p = p_i, proxy_mean = proxy_avg) %>%
  inner_join(
    repeated_cv_proxy_panel %>%
      select(vat, year, nace2d, primary_crf_group,
             y, emit, log_revenue, euets),
    by = c("vat", "year")
  )

# --- Step 2: panel-B mixed-sector subset ------------------------------------
# Eval subset = firm-years with nace2d in MIXED_CODES (matches panel B's
# idx_mixed). The "sector" label here is only used for threshold tuning
# (LOSO over paper/refining/metals); the redistribution cell key below is
# the panel-B (primary_crf_group, year), not this label.
mixed <- panel %>%
  mutate(nace2d = ifelse(nace2d %in% c("17", "18"), "17/18", nace2d)) %>%
  filter(nace2d %in% c("17/18", "19", "24", "25")) %>%
  mutate(sector = case_when(
    nace2d == "17/18"        ~ "paper",
    nace2d == "19"           ~ "refining",
    nace2d %in% c("24", "25") ~ "metals"
  ))

mixed_sectors <- c("paper", "refining", "metals")

# --- Step 3: best-J threshold on a single training sector -------------------
best_threshold <- function(s, y) {
  ord <- order(s, decreasing = TRUE)
  s_o <- s[ord]; y_o <- y[ord]
  P <- sum(y == 1); N <- sum(y == 0)
  if (P == 0 || N == 0) return(NA_real_)
  tp_cum <- cumsum(y_o == 1); fp_cum <- cumsum(y_o == 0)
  j <- tp_cum / P - fp_cum / N
  last_at_value <- !duplicated(s_o, fromLast = TRUE)
  s_eval <- s_o[last_at_value]; j_eval <- j[last_at_value]
  s_eval[which.max(j_eval)]
}

# --- Step 4: build held-out level predictions for each LOSO fold ------------
# Distribute the sector-year total proportionally to pmax(proxy_mean, 0)
# among classified emitters.
distribute_cell <- function(y_cell, proxy_mean_cell, classified_cell) {
  yhat <- rep(0, length(y_cell))
  E_target <- sum(y_cell)
  if (E_target <= 0) return(yhat)
  emit_idx <- which(classified_cell)
  if (length(emit_idx) == 0) return(yhat)            # all FN
  w <- pmax(proxy_mean_cell[emit_idx], 0)
  if (sum(w) > 0) {
    yhat[emit_idx] <- E_target * w / sum(w)
  } else {
    yhat[emit_idx] <- E_target / length(emit_idx)    # fallback
  }
  yhat
}

# Step 4a: tune one threshold per mixed sector via LOSO
tau_by_sector <- sapply(mixed_sectors, function(g) {
  train_sectors <- setdiff(mixed_sectors, g)
  taus <- sapply(train_sectors, function(g_tr) {
    sub <- mixed %>% filter(sector == g_tr)
    best_threshold(sub$p, sub$emit)
  })
  cat(sprintf("Held-out %s: tau from %s=%.3f, %s=%.3f -> tau_avg=%.4f\n",
              g, train_sectors[1], taus[1], train_sectors[2], taus[2],
              mean(taus, na.rm = TRUE)))
  mean(taus, na.rm = TRUE)
})
names(tau_by_sector) <- mixed_sectors

# Step 4b: assign each firm-year its sector's threshold and classify.
mixed <- mixed %>%
  mutate(
    tau_used        = tau_by_sector[sector],
    classified_emit = p >= tau_used
  )

# Step 4c: redistribute within (primary_crf_group, year) cells across the
# entire mixed subset, in one pass. This matches panel B's cell key.
mixed <- mixed %>%
  group_by(primary_crf_group, year) %>%
  mutate(yhat = distribute_cell(y, proxy_mean, classified_emit)) %>%
  ungroup()

# --- Step 5: compute metrics ------------------------------------------------
# Pooled across the three held-out sectors.
m_pooled <- calc_metrics(
  y      = mixed$y,
  yhat   = mixed$yhat,
  fp_threshold = 0,
  nace2d = mixed$nace2d,
  year   = mixed$year
)

# Per held-out sector.
per_sector <- lapply(mixed_sectors, function(g) {
  sub <- mixed %>% filter(sector == g)
  m <- calc_metrics(sub$y, sub$yhat, fp_threshold = 0,
                    nace2d = sub$nace2d, year = sub$year)
  data.frame(
    sector       = g,
    n            = nrow(sub),
    n_emit       = sum(sub$emit),
    n_classified = sum(sub$classified_emit),
    rmse_kt      = m$rmse / 1e3,
    median_apd   = m$median_apd,
    pearson      = m$pearson,
    spearman     = m$spearman,
    tpr          = m$tpr_emitters,
    fpr          = m$fpr_nonemitters,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

cat("\n=== Per held-out sector (proxy_mean redistribution within cell) ===\n")
print(per_sector %>%
        mutate(across(c(rmse_kt, median_apd, pearson, spearman, tpr, fpr),
                      \(x) round(x, 3))))

cat("\n=== Pooled across the three held-out sectors ===\n")
cat(sprintf("  n             : %d\n", m_pooled$n))
cat(sprintf("  RMSE (kt)     : %.2f\n", m_pooled$rmse / 1e3))
cat(sprintf("  median APD    : %.3f\n", m_pooled$median_apd))
cat(sprintf("  Pearson rho   : %.3f\n", m_pooled$pearson))
cat(sprintf("  Spearman rho  : %.3f\n", m_pooled$spearman))
cat(sprintf("  TPR emitters  : %.3f\n", m_pooled$tpr_emitters))
cat(sprintf("  FPR non-emit  : %.3f\n", m_pooled$fpr_nonemitters))
cat(sprintf("  emitter mass captured : %.3f\n", m_pooled$emitter_mass_captured))

saveRDS(list(
  per_sector = per_sector,
  pooled     = m_pooled,
  predictions = mixed %>%
    select(vat, year, sector, nace2d, y, emit, p, proxy_mean,
           tau_used, classified_emit, yhat)
), file.path(out_dir, "threshold_p_levels_eval_results.rds"))

cat("\nWrote ", file.path(out_dir, "threshold_p_levels_eval_results.rds"),
    "\n", sep = "")
