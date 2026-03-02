###############################################################################
# fuel_suppliers/figures_tables/plot_pareto_threshold.R
#
# PURPOSE
#   Pareto frontier analysis of the hurdle threshold. For each threshold
#   in the sweep, three objectives are evaluated:
#     1. nRMSE  (minimize)
#     2. FPR    (minimize)
#     3. rho_pooled (maximize, i.e. minimize -rho_pooled)
#
#   A threshold is Pareto-optimal if no other threshold weakly dominates it
#   on all three objectives with at least one strict improvement.
#
# INPUTS
#   {OUTPUT_DIR}/threshold_sweep_lofocv.csv
#   {OUTPUT_DIR}/threshold_sweep_losocv.csv  (optional)
#
# OUTPUTS
#   {OUTPUT_DIR}/pareto_threshold_pooled.pdf
#   {OUTPUT_DIR}/pareto_threshold_weighted.pdf
#   {OUTPUT_DIR}/pareto_thresholds.csv
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))

library(ggplot2)
library(dplyr)
library(gridExtra)


# ── Load data ────────────────────────────────────────────────────────────────
lofocv_path <- file.path(OUTPUT_DIR, "threshold_sweep_lofocv.csv")
if (!file.exists(lofocv_path)) stop("Threshold sweep not found: ", lofocv_path)

sweep <- read.csv(lofocv_path, stringsAsFactors = FALSE)
cat("Loaded LOFOCV sweep:", nrow(sweep), "rows\n")

# Optionally load LOSOCV sweep
losocv_path <- file.path(OUTPUT_DIR, "threshold_sweep_losocv.csv")
has_losocv <- file.exists(losocv_path)
if (has_losocv) {
  losocv_sweep <- read.csv(losocv_path, stringsAsFactors = FALSE)
  cat("Loaded LOSOCV sweep:", nrow(losocv_sweep), "rows\n")
}


# ── Pareto identification ────────────────────────────────────────────────────
# A point i is Pareto-dominated if there exists a point j such that:
#   nRMSE_j <= nRMSE_i AND FPR_j <= FPR_i AND rho_j >= rho_i
#   with at least one strict inequality.
find_pareto <- function(df) {
  n <- nrow(df)
  dominated <- logical(n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) next
      weakly_better <- (df$nRMSE[j] <= df$nRMSE[i]) &
                       (df$fpr_nonemitters[j] <= df$fpr_nonemitters[i]) &
                       (df$rho_pooled[j] >= df$rho_pooled[i])
      strictly_better <- (df$nRMSE[j] < df$nRMSE[i]) |
                         (df$fpr_nonemitters[j] < df$fpr_nonemitters[i]) |
                         (df$rho_pooled[j] > df$rho_pooled[i])
      if (weakly_better && strictly_better) {
        dominated[i] <- TRUE
        break
      }
    }
  }
  !dominated
}


# ── Plot function ────────────────────────────────────────────────────────────
make_pareto_plot <- function(df, proxy_label) {
  # Remove rows with NA in key metrics
  df <- df[!is.na(df$nRMSE) & !is.na(df$fpr_nonemitters) &
           !is.na(df$rho_pooled), ]

  # Identify Pareto frontier
  df$pareto <- find_pareto(df)

  # RMSE-optimal threshold
  rmse_opt_thr <- df$threshold[which.min(df$nRMSE)]

  # Colors
  col_main   <- "gray30"
  col_pareto <- "#2166AC"
  col_rmse   <- "#D94040"
  col_tpr    <- "gray60"

  base_theme <- theme_minimal(base_size = 11) +
    theme(
      panel.grid       = element_blank(),
      panel.background = element_blank(),
      plot.background  = element_blank(),
      axis.line        = element_line(color = "black", linewidth = 0.3),
      axis.ticks       = element_line(color = "black", linewidth = 0.3),
      axis.title.x     = element_text(margin = margin(t = 8)),
      axis.title.y     = element_text(margin = margin(r = 8)),
      plot.title       = element_text(size = 11, face = "bold", hjust = 0.5)
    )

  # Panel A: nRMSE
  p1 <- ggplot(df, aes(x = threshold, y = nRMSE)) +
    geom_line(color = col_main, linewidth = 0.6) +
    geom_point(data = df[df$pareto, ], color = col_pareto, size = 2) +
    geom_vline(xintercept = rmse_opt_thr, linetype = "dashed",
               color = col_rmse, linewidth = 0.5) +
    labs(x = "Threshold", y = "nRMSE", title = "Level accuracy") +
    base_theme

  # Panel B: FPR and TPR
  p2 <- ggplot(df) +
    geom_line(aes(x = threshold, y = fpr_nonemitters), color = col_main,
              linewidth = 0.6) +
    geom_line(aes(x = threshold, y = tpr_emitters), color = col_tpr,
              linewidth = 0.6, linetype = "dashed") +
    geom_point(data = df[df$pareto, ],
               aes(x = threshold, y = fpr_nonemitters),
               color = col_pareto, size = 2) +
    geom_vline(xintercept = rmse_opt_thr, linetype = "dashed",
               color = col_rmse, linewidth = 0.5) +
    annotate("text", x = max(df$threshold) * 0.95, y = 0.95,
             label = "TPR", color = col_tpr, size = 3.5, hjust = 1) +
    annotate("text", x = max(df$threshold) * 0.95, y = 0.05,
             label = "FPR", color = col_main, size = 3.5, hjust = 1) +
    labs(x = "Threshold", y = "Rate", title = "Discrimination") +
    scale_y_continuous(limits = c(0, 1)) +
    base_theme

  # Panel C: pooled within-sector rho
  p3 <- ggplot(df, aes(x = threshold, y = rho_pooled)) +
    geom_line(color = col_main, linewidth = 0.6) +
    geom_point(data = df[df$pareto, ], color = col_pareto, size = 2) +
    geom_vline(xintercept = rmse_opt_thr, linetype = "dashed",
               color = col_rmse, linewidth = 0.5) +
    labs(x = "Threshold",
         y = expression(rho[pooled]),
         title = "Within-sector ranking") +
    base_theme

  grid.arrange(p1, p2, p3, nrow = 1)
}


# ── Generate plots for each proxy ────────────────────────────────────────────
proxy_map <- list(
  pooled = list(
    lofocv_model = "hurdle_proxy_pooled_ind_base",
    label        = "Unweighted proxy"
  ),
  weighted = list(
    lofocv_model = "hurdle_proxy_weighted_ind_base",
    label        = "Coefficient-weighted proxy"
  )
)

pareto_results <- list()

for (proxy_key in names(proxy_map)) {
  pm <- proxy_map[[proxy_key]]

  df <- sweep[sweep$model == pm$lofocv_model, ]
  if (nrow(df) == 0) {
    cat(sprintf("WARNING: No sweep data for %s. Skipping.\n", pm$lofocv_model))
    next
  }

  cat(sprintf("\n── %s (%d thresholds) ──\n", pm$label, nrow(df)))

  # Identify Pareto set
  df_clean <- df[!is.na(df$nRMSE) & !is.na(df$fpr_nonemitters) &
                 !is.na(df$rho_pooled), ]
  df_clean$pareto <- find_pareto(df_clean)

  pareto_pts <- df_clean[df_clean$pareto, ]
  pareto_pts$proxy <- proxy_key
  pareto_results[[proxy_key]] <- pareto_pts

  cat(sprintf("  Pareto-optimal thresholds: %s\n",
              paste(sprintf("%.2f", pareto_pts$threshold), collapse = ", ")))
  cat(sprintf("  RMSE-optimal threshold: %.2f\n",
              df_clean$threshold[which.min(df_clean$nRMSE)]))

  # Print Pareto set
  cat(sprintf("  %-6s  %6s  %6s  %6s  %10s\n",
              "thr", "nRMSE", "FPR", "TPR", "rho_pooled"))
  for (i in seq_len(nrow(pareto_pts))) {
    r <- pareto_pts[i, ]
    cat(sprintf("  %-6.2f  %6.3f  %6.3f  %6.3f  %10.3f\n",
                r$threshold, r$nRMSE, r$fpr_nonemitters,
                r$tpr_emitters, r$rho_pooled))
  }

  # Save plot
  out_pdf <- file.path(OUTPUT_DIR, paste0("pareto_threshold_", proxy_key, ".pdf"))

  pdf(out_pdf, width = 12, height = 4)
  make_pareto_plot(df, pm$label)
  dev.off()
  cat(sprintf("  Saved: %s\n", out_pdf))
}

# ── Save Pareto thresholds CSV ───────────────────────────────────────────────
if (length(pareto_results) > 0) {
  pareto_all <- bind_rows(pareto_results)
  pareto_path <- file.path(OUTPUT_DIR, "pareto_thresholds.csv")
  write.csv(pareto_all, pareto_path, row.names = FALSE)
  cat(sprintf("\nPareto thresholds saved to: %s\n", pareto_path))
}
