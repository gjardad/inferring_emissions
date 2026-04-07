# threshold_p_mixed_sectors.R
#
# Purpose: tune a threshold on the selection frequency p_i and evaluate it
# out-of-sector, using leave-one-sector-out across the three "mixed" sectors
# defined in panel B of table_main_with_mixed_crf.R / table_zero_emitters_crf_cv:
#
#   - paper     NACE 2d 17 and 18 (merged into "17/18")
#   - refining  NACE 2d 19
#   - metals    NACE 2d 24 and 25
#
# Each firm-year is assigned to its sector via its `nace2d` field on that
# year (with 17 and 18 collapsed to "17/18"), exactly as the paper does
# (panel$nace2d %in% MIXED_CODES). A firm whose nace2d changes across years
# can therefore contribute to different sectors in different years.
#
# For each predictor and each held-out sector g:
#   - On the other two sectors, find the threshold maximizing TPR - FPR
#     (Youden's J) using a pooled ROC across the firm-years in that training
#     sector. Compute one threshold per training sector, then average them.
#   - Apply the averaged threshold to the held-out sector and compute pooled
#     TPR / FPR / J.
#
# Predictors evaluated (four):
#   - p_a       : selection frequency p_i, used directly  (scheme a)
#   - p_b       : within-(sector x year) mid-rank percentile of p_i  (b)
#   - lrev_a    : log_revenue, used directly  (size baseline, scheme a)
#   - lrev_b    : within-(sector x year) mid-rank percentile of log_revenue (b)
#
# Cells for scheme (b) are sector x year, where "sector" is one of
# {"paper", "refining", "metals"}. For paper, NACE 17 and 18 firms in the
# same year share one cell; for metals, NACE 24 and 25 firms share one cell.
#
# Tiny-cell handling under scheme (b):
#   - All sector-year cells are kept.
#   - Percentiles are mid-ranks: pct_i = (rank_i - 0.5) / n_cell, with ties
#     averaged. A 2-firm cell yields {0.25, 0.75}.
#   - As a robustness check, the same evaluation is repeated on the subset
#     of firm-years in cells with n_cell >= 5.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

source("paths.R")

in_path  <- file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData")
out_dir  <- file.path("analysis", "active", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

load(in_path)
# brings in: proxy_matrix, firmyear_index, repeated_cv_proxy_panel,
#            crf_group_map, syt, ...

stopifnot(nrow(proxy_matrix) == nrow(firmyear_index))

# --- Step 1: build per firm-year selection frequency p_i --------------------
pos_mat <- proxy_matrix > 0
n_obs   <- rowSums(!is.na(proxy_matrix))
n_pos   <- rowSums(pos_mat, na.rm = TRUE)
p_i     <- n_pos / n_obs

panel <- firmyear_index %>%
  mutate(p = p_i) %>%
  inner_join(
    repeated_cv_proxy_panel %>%
      select(vat, year, nace2d, y, emit, log_revenue, euets),
    by = c("vat", "year")
  )

# --- Step 2: build the mixed-sector subset, exactly as panel B does ---------
# (1) restrict to firm-years with nace2d in {17, 18, 19, 24, 25}
#     (nace2d already includes "17/18" if a firm-year had been merged earlier;
#      we collapse 17 and 18 to "17/18" here as well to be safe)
# (2) map nace2d -> {paper, refining, metals}
mixed <- panel %>%
  mutate(nace2d = ifelse(nace2d %in% c("17", "18"), "17/18", nace2d)) %>%
  filter(nace2d %in% c("17/18", "19", "24", "25")) %>%
  mutate(sector = case_when(
    nace2d == "17/18"        ~ "paper",
    nace2d == "19"           ~ "refining",
    nace2d %in% c("24", "25") ~ "metals"
  ))

mixed_sectors <- c("paper", "refining", "metals")
stopifnot(all(unique(mixed$sector) %in% mixed_sectors))

cat("Mixed sectors (matching paper's table_zero_emitters):\n")
print(mixed_sectors)

cat("\nFirm-year counts by sector:\n")
print(mixed %>%
        group_by(sector) %>%
        summarise(n = n(), n_emit = sum(emit), n_nonemit = sum(emit == 0),
                  .groups = "drop"))

cat(sprintf("\nTotal mixed-sector firm-years: %d  (emit=1: %d, emit=0: %d)\n",
            nrow(mixed), sum(mixed$emit), sum(mixed$emit == 0)))

# --- Step 3: within-cell mid-rank percentile helper -------------------------
# Cells = sector x year. pct_i = (rank_i - 0.5) / n_cell, ties get average rank.
add_within_cell_pct <- function(df, var, new_var) {
  df %>%
    group_by(sector, year) %>%
    mutate(
      !!new_var := (rank(.data[[var]], ties.method = "average") - 0.5) / n()
    ) %>%
    ungroup()
}

mixed <- mixed %>%
  add_within_cell_pct("p",           "p_pct") %>%
  add_within_cell_pct("log_revenue", "lrev_pct")

# Cell size, for the robustness restriction.
mixed <- mixed %>%
  group_by(sector, year) %>%
  mutate(n_cell = n()) %>%
  ungroup()

# --- Step 4: pooled-ROC threshold + TPR/FPR ---------------------------------
# For a given vector of scores `s` and binary labels `y`, find the threshold
# tau that maximizes TPR(s >= tau) - FPR(s >= tau).
best_threshold <- function(s, y) {
  ord <- order(s, decreasing = TRUE)
  s_o <- s[ord]; y_o <- y[ord]
  P <- sum(y == 1); N <- sum(y == 0)
  if (P == 0 || N == 0) {
    return(list(tau = NA_real_, tpr = NA_real_,
                fpr = NA_real_, J = NA_real_))
  }
  tp_cum <- cumsum(y_o == 1)
  fp_cum <- cumsum(y_o == 0)
  tpr <- tp_cum / P
  fpr <- fp_cum / N
  j   <- tpr - fpr
  # Collapse ties on s_o so we evaluate one J per distinct threshold value.
  last_at_value <- !duplicated(s_o, fromLast = TRUE)
  j_eval <- j[last_at_value]
  s_eval <- s_o[last_at_value]
  i_star <- which.max(j_eval)
  tau_star <- s_eval[i_star]
  pred <- s >= tau_star
  list(
    tau = tau_star,
    tpr = sum(pred & y == 1) / P,
    fpr = sum(pred & y == 0) / N,
    J   = sum(pred & y == 1) / P - sum(pred & y == 0) / N
  )
}

apply_threshold <- function(s, y, tau) {
  P <- sum(y == 1); N <- sum(y == 0)
  if (P == 0 || N == 0 || is.na(tau)) {
    return(list(tau = tau, tpr = NA_real_, fpr = NA_real_,
                J = NA_real_, n = length(y), P = P, N = N))
  }
  pred <- s >= tau
  list(
    tau = tau,
    tpr = sum(pred & y == 1) / P,
    fpr = sum(pred & y == 0) / N,
    J   = sum(pred & y == 1) / P - sum(pred & y == 0) / N,
    n   = length(y), P = P, N = N
  )
}

# --- Step 5: leave-one-sector-out evaluation --------------------------------
predictors <- list(
  p_a    = "p",
  p_b    = "p_pct",
  lrev_a = "log_revenue",
  lrev_b = "lrev_pct"
)

run_loso <- function(df, label) {
  rows <- list()
  for (g_held in mixed_sectors) {
    train <- df %>% filter(sector != g_held)
    test  <- df %>% filter(sector == g_held)
    train_sectors <- setdiff(mixed_sectors, g_held)

    for (pname in names(predictors)) {
      svar <- predictors[[pname]]

      # Per-training-sector threshold, then average.
      taus <- sapply(train_sectors, function(g_tr) {
        sub <- train %>% filter(sector == g_tr)
        best_threshold(sub[[svar]], sub$emit)$tau
      })
      tau_avg <- mean(taus, na.rm = TRUE)

      # Held-out evaluation.
      held <- apply_threshold(test[[svar]], test$emit, tau_avg)
      rows[[length(rows) + 1]] <- data.frame(
        subset      = label,
        held_out    = g_held,
        predictor   = pname,
        tau_train_1 = taus[1], tau_train_2 = taus[2],
        tau_avg     = tau_avg,
        tpr_held    = held$tpr, fpr_held = held$fpr,
        J_held      = held$J,
        n_held      = held$n, P_held = held$P, N_held = held$N,
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows)
}

results_full <- run_loso(mixed, "all_cells")
results_n5   <- run_loso(mixed %>% filter(n_cell >= 5), "n_cell_ge_5")

results <- bind_rows(results_full, results_n5)

cat("\n=== Held-out TPR / FPR / J by predictor and held-out sector ===\n")
print(results %>%
        select(subset, held_out, predictor,
               tau_avg, tpr_held, fpr_held, J_held, n_held, P_held, N_held) %>%
        mutate(across(c(tau_avg, tpr_held, fpr_held, J_held), \(x) round(x, 3))))

cat("\n=== Held-out J averaged over the 3 LOSO folds ===\n")
print(results %>%
        group_by(subset, predictor) %>%
        summarise(
          mean_J   = mean(J_held,   na.rm = TRUE),
          mean_TPR = mean(tpr_held, na.rm = TRUE),
          mean_FPR = mean(fpr_held, na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        mutate(across(starts_with("mean"), \(x) round(x, 3))))

saveRDS(results, file.path(out_dir, "threshold_p_mixed_sectors_results.rds"))
cat("\nWrote ", file.path(out_dir, "threshold_p_mixed_sectors_results.rds"),
    "\n", sep = "")
