# threshold_p_levels_eval_gpa.R
#
# Same pipeline as threshold_p_levels_eval.R, but with GPA redistribution
# instead of proportional. Used to check whether the Pearson/RMSE/MAPD gap
# vs panel B's "EN (prop)" is due to over-concentration under proportional
# redistribution among classified emitters.
#
# Pipeline:
#   1. Threshold on p_i tuned via LOSO over the three mixed sectors
#      (paper / refining / metals), one tau per held-out sector, applied
#      to firm-years based on which sector they belong to.
#   2. Reference distribution for GPA fit ONCE from true emitters in the
#      training panel that are NOT in any mixed sector (CRF groups other
#      than paper / refining / metals). Demean log y by primary_crf_group
#      and year, then fit GPA via L-moments (matches panel B's
#      build_reference_dist_crf logic, but pooled rather than per-fold).
#   3. For each (primary_crf_group, year) cell intersected with the mixed
#      eval subset:
#        - E_target = sum(y) over the cell  (oracle, matches panel B)
#        - Among classified emitters, rank by proxy_mean -> percentiles
#          -> GPA quantiles -> softmax weights
#        - If <2 classified emitters or GPA params unavailable, fall back
#          to proportional redistribution by pmax(proxy_mean, 0).

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
# Use emitters whose primary_crf_group is NOT one of the mixed groups.
ref_df <- panel %>%
  filter(y > 0,
         !primary_crf_group %in% c("paper", "refining", "metals"),
         !is.na(primary_crf_group))

cat(sprintf("Reference distribution: %d emitter firm-years from %d non-mixed CRF groups\n",
            nrow(ref_df), length(unique(ref_df$primary_crf_group))))

ref_df <- ref_df %>%
  mutate(log_y = log(y)) %>%
  group_by(year)              %>% mutate(tilde = log_y - mean(log_y)) %>% ungroup() %>%
  group_by(primary_crf_group) %>% mutate(d     = tilde - mean(tilde)) %>% ungroup()

ref_d <- sort(ref_df$d)
gpa_params <- tryCatch(pelgpa(samlmu(ref_d, nmom = 3)), error = function(e) NULL)
if (is.null(gpa_params)) stop("GPA fit failed on reference distribution")
cat("GPA params: "); print(gpa_params)

# --- Threshold tuning (same as before) --------------------------------------
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
    sub <- mixed %>% filter(sector == g_tr)
    best_threshold(sub$p, sub$emit)
  })
  cat(sprintf("Held-out %s: tau from %s=%.3f, %s=%.3f -> tau_avg=%.4f\n",
              g, train_sectors[1], taus[1], train_sectors[2], taus[2],
              mean(taus, na.rm = TRUE)))
  mean(taus, na.rm = TRUE)
})
names(tau_by_sector) <- mixed_sectors

mixed <- mixed %>%
  mutate(
    tau_used        = tau_by_sector[sector],
    classified_emit = p >= tau_used
  )

# --- GPA redistribution within (primary_crf_group, year) cells --------------
distribute_cell_gpa <- function(y_cell, proxy_mean_cell, classified_cell,
                                gpa_params) {
  yhat <- rep(0, length(y_cell))
  E_target <- sum(y_cell)
  if (E_target <= 0) return(yhat)
  emit_idx <- which(classified_cell)
  n_emit <- length(emit_idx)
  if (n_emit == 0) return(yhat)

  ranking <- proxy_mean_cell[emit_idx]

  prop_fallback <- function() {
    w <- pmax(ranking, 0)
    if (sum(w) > 0) {
      yhat[emit_idx] <<- E_target * w / sum(w)
    } else {
      yhat[emit_idx] <<- E_target / n_emit
    }
  }

  if (n_emit < 2 || is.null(gpa_params)) { prop_fallback(); return(yhat) }

  ranks <- rank(ranking, ties.method = "average")
  p_i_cell <- (ranks - 0.5) / n_emit
  w_i <- tryCatch(quagpa(p_i_cell, gpa_params),
                  error = function(e) rep(NA_real_, n_emit))
  if (any(is.na(w_i)) || any(!is.finite(w_i))) { prop_fallback(); return(yhat) }

  exp_w <- exp(w_i - max(w_i))
  yhat[emit_idx] <- E_target * exp_w / sum(exp_w)
  yhat
}

mixed <- mixed %>%
  group_by(primary_crf_group, year) %>%
  mutate(yhat = distribute_cell_gpa(y, proxy_mean, classified_emit,
                                    gpa_params)) %>%
  ungroup()

# --- Metrics ----------------------------------------------------------------
m_pooled <- calc_metrics(
  y = mixed$y, yhat = mixed$yhat, fp_threshold = 0,
  nace2d = mixed$nace2d, year = mixed$year
)

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

cat("\n=== Per held-out sector (GPA redistribution) ===\n")
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

saveRDS(list(per_sector = per_sector, pooled = m_pooled,
             gpa_params = gpa_params),
        file.path(out_dir, "threshold_p_levels_eval_gpa_results.rds"))
