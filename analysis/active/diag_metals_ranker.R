# Does proxy_mean rank TRUE emitters above false positives WITHIN
# (metals, year) cells, among the firms my pipeline classified as positive?
#
# If yes, the metals problem is "too many FP in the cell" — fixable by a
# stricter threshold. If no, the ranker itself is bad in metals — no
# threshold change will fix the levels accuracy.

suppressPackageStartupMessages(library(dplyr))

res  <- readRDS("analysis/active/output/threshold_p_levels_eval_results.rds")
pred <- res$predictions

# Restrict to metals classified positives
mp <- pred %>%
  filter(sector == "metals", classified_emit) %>%
  mutate(true_emit = (y > 0))

cat("Metals classified positives:\n")
cat(sprintf("  total           : %d\n", nrow(mp)))
cat(sprintf("  true emitters   : %d\n", sum(mp$true_emit)))
cat(sprintf("  false positives : %d\n", sum(!mp$true_emit)))

# Within-cell AUC: for each (primary_crf_group, year) cell, share of
# (true_emitter, false_positive) pairs in which the true emitter has the
# higher proxy_mean. AUC = 0.5 means proxy_mean is uninformative within
# the cell; AUC = 1 means perfect ranking.
cell_auc <- mp %>%
  group_by(year) %>%
  group_modify(~{
    df <- .x
    te <- df$proxy_mean[df$true_emit]
    fp <- df$proxy_mean[!df$true_emit]
    if (length(te) == 0 || length(fp) == 0) {
      tibble(n = nrow(df), n_te = length(te), n_fp = length(fp), auc = NA_real_)
    } else {
      pairs <- length(te) * length(fp)
      wins  <- sum(outer(te, fp, ">"))
      ties  <- sum(outer(te, fp, "=="))
      tibble(n = nrow(df), n_te = length(te), n_fp = length(fp),
             auc = (wins + 0.5 * ties) / pairs)
    }
  }) %>% ungroup()

cat("\nWithin-(crf,year)-cell AUC of proxy_mean among classified positives:\n")
cat(sprintf("  cells with both TE and FP : %d\n",
            sum(!is.na(cell_auc$auc))))
cat(sprintf("  mean AUC (unweighted)     : %.3f\n",
            mean(cell_auc$auc, na.rm = TRUE)))
cat(sprintf("  median AUC                : %.3f\n",
            median(cell_auc$auc, na.rm = TRUE)))
cat("  AUC distribution:\n")
print(quantile(cell_auc$auc,
               c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE))

# Pooled AUC across all metals classified positives (ignoring cell
# structure): tells you if proxy_mean ranks emitters at the level of
# the metals subset overall.
te_all <- mp$proxy_mean[mp$true_emit]
fp_all <- mp$proxy_mean[!mp$true_emit]
pairs  <- length(te_all) * length(fp_all)
wins   <- sum(outer(te_all, fp_all, ">"))
ties   <- sum(outer(te_all, fp_all, "=="))
auc_pool <- (wins + 0.5 * ties) / pairs
cat(sprintf("\nPooled AUC across all metals classified positives: %.3f\n",
            auc_pool))

# Within-cell rank of true emitters: of the classified positives in a
# cell, what is the average percentile rank of the true emitters by
# proxy_mean (1 = highest in the cell)?
mp_ranked <- mp %>%
  group_by(year) %>%
  mutate(
    rank_pm     = rank(-proxy_mean, ties.method = "average"),
    pct_top     = 1 - (rank_pm - 0.5) / n()  # 1 = top of cell
  ) %>% ungroup()

cat("\nAmong classified-positive metals firm-years, percentile-from-top of true emitters:\n")
print(summary(mp_ranked$pct_top[mp_ranked$true_emit]))
cat("Among classified-positive metals firm-years, percentile-from-top of false positives:\n")
print(summary(mp_ranked$pct_top[!mp_ranked$true_emit]))

# Sanity check: relate within-cell rank to APD outcome.
mp_ranked <- mp_ranked %>%
  mutate(apd = ifelse(true_emit, abs(y - res$predictions$yhat[
    match(paste(vat, year), paste(res$predictions$vat, res$predictions$year))
  ]) / y, NA_real_))
