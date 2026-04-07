# threshold_p_levels_eval_all.R
#
# Two extensions to threshold_p_levels_eval.R:
#
#   (1) Try GPA, GLO, and GEV as the redistribution shape (alongside the
#       proportional baseline), all fit on the same reference distribution
#       (true emitters in non-mixed CRF groups, log y demeaned by
#       primary_crf_group and year).
#
#   (2) For each redistribution shape, run TWO versions:
#         - "classified": use the LOSO-tuned p_i threshold to decide which
#           firm-years receive any share. This is the actual pipeline.
#         - "oracle"    : use the TRUE emitter label (y > 0) as the mask.
#           This is a diagnostic — it isolates how much of the levels-accuracy
#           failure is due to FP among classified positives versus due to the
#           shape of proxy_mean not matching the shape of y.
#
# Reports pooled and per-sector RMSE / median APD / Pearson / Spearman / TPR
# / FPR for all 8 (4 shapes x 2 masks) variants.

suppressPackageStartupMessages({
  library(dplyr)
  library(lmom)
})

source("paths.R")
source("utils/calc_metrics.R")

in_path  <- file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData")
out_dir  <- file.path("analysis", "active", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

load(in_path)
stopifnot(nrow(proxy_matrix) == nrow(firmyear_index))

# --- p_i and proxy_mean -----------------------------------------------------
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

mixed <- panel %>%
  mutate(nace2d = ifelse(nace2d %in% c("17", "18"), "17/18", nace2d)) %>%
  filter(nace2d %in% c("17/18", "19", "24", "25")) %>%
  mutate(sector = case_when(
    nace2d == "17/18"        ~ "paper",
    nace2d == "19"           ~ "refining",
    nace2d %in% c("24", "25") ~ "metals"
  ))

mixed_sectors <- c("paper", "refining", "metals")

# --- Reference distribution (built once, no mixed-sector leakage) -----------
ref_df <- panel %>%
  filter(y > 0,
         !primary_crf_group %in% c("paper", "refining", "metals"),
         !is.na(primary_crf_group)) %>%
  mutate(log_y = log(y)) %>%
  group_by(year)              %>% mutate(tilde = log_y - mean(log_y)) %>% ungroup() %>%
  group_by(primary_crf_group) %>% mutate(d     = tilde - mean(tilde)) %>% ungroup()

ref_d   <- sort(ref_df$d)
lmoms   <- samlmu(ref_d, nmom = 4)
gpa_par <- tryCatch(pelgpa(lmoms[1:3]), error = function(e) NULL)
glo_par <- tryCatch(pelglo(lmoms[1:3]), error = function(e) NULL)
gev_par <- tryCatch(pelgev(lmoms[1:3]), error = function(e) NULL)

cat(sprintf("Reference distribution: %d emitter firm-years\n", nrow(ref_df)))
cat("L-moments:\n"); print(lmoms)
cat("GPA params:\n"); print(gpa_par)
cat("GLO params:\n"); print(glo_par)
cat("GEV params:\n"); print(gev_par)

# --- Threshold tuning -------------------------------------------------------
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

tau_by_sector <- sapply(mixed_sectors, function(g) {
  train_sectors <- setdiff(mixed_sectors, g)
  taus <- sapply(train_sectors, function(g_tr) {
    best_threshold(mixed$p[mixed$sector == g_tr],
                   mixed$emit[mixed$sector == g_tr])
  })
  mean(taus, na.rm = TRUE)
})
names(tau_by_sector) <- mixed_sectors

mixed <- mixed %>%
  mutate(
    tau_used        = tau_by_sector[sector],
    classified_emit = p >= tau_used
  )

# --- Generic redistribution -------------------------------------------------
# `mask`: logical vector of which firms receive any share
# `shape`: "prop", "gpa", "glo", or "gev"
distribute <- function(y_cell, ranking_cell, mask_cell, shape, params = NULL) {
  yhat <- rep(0, length(y_cell))
  E_target <- sum(y_cell)
  if (E_target <= 0) return(yhat)
  emit_idx <- which(mask_cell)
  n_emit <- length(emit_idx)
  if (n_emit == 0) return(yhat)

  ranking <- ranking_cell[emit_idx]

  prop_fallback <- function() {
    w <- pmax(ranking, 0)
    if (sum(w) > 0) yhat[emit_idx] <<- E_target * w / sum(w)
    else            yhat[emit_idx] <<- E_target / n_emit
  }

  if (shape == "prop" || n_emit < 2 || is.null(params)) {
    prop_fallback(); return(yhat)
  }

  ranks <- rank(ranking, ties.method = "average")
  p_cell <- (ranks - 0.5) / n_emit

  qfun <- switch(shape, gpa = quagpa, glo = quaglo, gev = quagev)
  w_i <- tryCatch(qfun(p_cell, params),
                  error = function(e) rep(NA_real_, n_emit))
  if (any(is.na(w_i)) || any(!is.finite(w_i))) {
    prop_fallback(); return(yhat)
  }
  exp_w <- exp(w_i - max(w_i))
  yhat[emit_idx] <- E_target * exp_w / sum(exp_w)
  yhat
}

# --- Run all 8 variants -----------------------------------------------------
shape_specs <- list(
  prop = NULL, gpa = gpa_par, glo = glo_par, gev = gev_par
)
mask_specs <- list(
  classified = function(d) d$classified_emit,
  oracle     = function(d) d$y > 0
)

results <- list()

for (mask_name in names(mask_specs)) {
  for (shape_name in names(shape_specs)) {
    df <- mixed
    df$mask <- mask_specs[[mask_name]](df)
    df <- df %>%
      group_by(primary_crf_group, year) %>%
      mutate(yhat = distribute(y, proxy_mean, mask,
                               shape_name, shape_specs[[shape_name]])) %>%
      ungroup()

    m <- calc_metrics(df$y, df$yhat, fp_threshold = 0,
                      nace2d = df$nace2d, year = df$year)
    pooled_row <- data.frame(
      mask       = mask_name,
      shape      = shape_name,
      scope      = "pooled",
      n          = nrow(df),
      rmse_kt    = m$rmse / 1e3,
      median_apd = m$median_apd,
      pearson    = m$pearson,
      spearman   = m$spearman,
      tpr        = m$tpr_emitters,
      fpr        = m$fpr_nonemitters
    )

    sec_rows <- lapply(mixed_sectors, function(g) {
      sub <- df %>% filter(sector == g)
      ms  <- calc_metrics(sub$y, sub$yhat, fp_threshold = 0,
                          nace2d = sub$nace2d, year = sub$year)
      data.frame(
        mask = mask_name, shape = shape_name, scope = g,
        n = nrow(sub),
        rmse_kt = ms$rmse / 1e3, median_apd = ms$median_apd,
        pearson = ms$pearson, spearman = ms$spearman,
        tpr = ms$tpr_emitters, fpr = ms$fpr_nonemitters
      )
    }) %>% bind_rows()

    results[[length(results) + 1]] <- bind_rows(pooled_row, sec_rows)
  }
}

results <- bind_rows(results)

cat("\n=== Pooled across the three mixed sectors ===\n")
print(results %>% filter(scope == "pooled") %>%
        select(mask, shape, rmse_kt, median_apd, pearson, spearman, tpr, fpr) %>%
        mutate(across(c(rmse_kt, median_apd, pearson, spearman, tpr, fpr),
                      \(x) round(x, 3))))

cat("\n=== Per held-out sector ===\n")
print(results %>% filter(scope != "pooled") %>%
        select(mask, shape, scope, rmse_kt, median_apd, pearson, spearman) %>%
        arrange(scope, mask, shape) %>%
        mutate(across(c(rmse_kt, median_apd, pearson, spearman),
                      \(x) round(x, 3))))

saveRDS(results, file.path(out_dir, "threshold_p_levels_eval_all_results.rds"))
