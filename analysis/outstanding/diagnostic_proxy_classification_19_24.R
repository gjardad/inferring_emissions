###############################################################################
# analysis/model_selection/losocv/diagnostic_proxy_classification_19_24.R
#
# PURPOSE
#   Within sectors 19 and 24 (where we observe both emitters and confirmed
#   non-emitters), test how well proxy_weighted and proxy_weighted/revenue
#   discriminate emitters from non-emitters.
#
#   Produces:
#     - ROC curves (proxy vs GAM classifier)
#     - AUC comparison
#     - Youden-optimal thresholds with TPR/FPR
#     - Density plots of proxy by emitter status
#
# INPUT
#   {PROC_DATA}/training_sample.RData
#   {OUTPUT_DIR}/model_selection/losocv/classifier_phat_gam_enriched.csv
#
# OUTPUT
#   {OUTPUT_DIR}/model_selection/losocv/diagnostic_proxy_roc_19_24.pdf
#   {OUTPUT_DIR}/model_selection/losocv/diagnostic_proxy_density_19_24.pdf
#   Console output with AUC and Youden thresholds
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
library(ggplot2)
library(tidyr)

OUT_DIR <- file.path(OUTPUT_DIR, "model_selection", "losocv")


# ── Load data ────────────────────────────────────────────────────────────────
load(file.path(PROC_DATA, "training_sample.RData"))
panel <- training_sample
rm(training_sample)

# Load GAM phat for comparison
gam_phat <- read.csv(file.path(OUT_DIR, "classifier_phat_gam_enriched.csv"))

panel <- panel %>%
  left_join(gam_phat %>% select(vat, year, phat), by = c("vat", "year"))


# ── Restrict to sectors 19 and 24 ───────────────────────────────────────────
df <- panel %>%
  filter(nace2d %in% c("19", "24")) %>%
  mutate(
    emitter = as.integer(emit == 1),
    # Revenue: use turnover_VAT, fall back to v_0000070
    revenue = ifelse(!is.na(turnover_VAT) & turnover_VAT > 0, turnover_VAT,
                     ifelse(!is.na(v_0000070) & v_0000070 > 0, v_0000070, NA)),
    proxy_intensity = ifelse(!is.na(revenue) & revenue > 0,
                             proxy_weighted / revenue, NA)
  )

cat(sprintf("Sectors 19/24: %d obs (%d emitters, %d non-emitters)\n",
            nrow(df), sum(df$emitter == 1), sum(df$emitter == 0)))
cat(sprintf("  Sector 19: %d obs (%d emit, %d non-emit)\n",
            sum(df$nace2d == "19"),
            sum(df$nace2d == "19" & df$emitter == 1),
            sum(df$nace2d == "19" & df$emitter == 0)))
cat(sprintf("  Sector 24: %d obs (%d emit, %d non-emit)\n",
            sum(df$nace2d == "24"),
            sum(df$nace2d == "24" & df$emitter == 1),
            sum(df$nace2d == "24" & df$emitter == 0)))
cat(sprintf("  Revenue available: %d / %d\n",
            sum(!is.na(df$revenue)), nrow(df)))


# ── Helper: compute ROC curve ────────────────────────────────────────────────
compute_roc <- function(score, label) {
  # Sort by score descending
  ord <- order(score, decreasing = TRUE)
  score <- score[ord]
  label <- label[ord]

  n_pos <- sum(label == 1)
  n_neg <- sum(label == 0)

  # Unique thresholds
  thresholds <- sort(unique(score), decreasing = TRUE)

  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    pred_pos <- score >= thresholds[i]
    tpr[i] <- sum(pred_pos & label == 1) / n_pos
    fpr[i] <- sum(pred_pos & label == 0) / n_neg
  }

  # Add (0,0) and (1,1) endpoints
  fpr <- c(0, fpr, 1)
  tpr <- c(0, tpr, 1)
  thresholds <- c(Inf, thresholds, -Inf)

  # AUC via trapezoidal rule
  auc <- sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)

  # Youden's J
  j <- tpr - fpr
  best_idx <- which.max(j)

  list(
    fpr = fpr, tpr = tpr, thresholds = thresholds,
    auc = auc,
    youden_j = j[best_idx],
    youden_threshold = thresholds[best_idx],
    youden_tpr = tpr[best_idx],
    youden_fpr = fpr[best_idx]
  )
}


# ── Compute ROC for each score × sector ─────────────────────────────────────
results <- list()

for (sec in c("19", "24", "pooled")) {
  if (sec == "pooled") {
    sub <- df
  } else {
    sub <- df %>% filter(nace2d == sec)
  }

  # (a) Raw proxy_weighted
  roc_raw <- compute_roc(sub$proxy_weighted, sub$emitter)
  results[[paste0(sec, "_raw")]] <- roc_raw

  # (b) Proxy intensity (proxy_weighted / revenue)
  ok_int <- !is.na(sub$proxy_intensity)
  if (sum(ok_int) > 10) {
    roc_int <- compute_roc(sub$proxy_intensity[ok_int], sub$emitter[ok_int])
    results[[paste0(sec, "_intensity")]] <- roc_int
  }

  # (c) GAM phat (benchmark)
  ok_phat <- !is.na(sub$phat)
  if (sum(ok_phat) > 10) {
    roc_gam <- compute_roc(sub$phat[ok_phat], sub$emitter[ok_phat])
    results[[paste0(sec, "_gam")]] <- roc_gam
  }
}


# ── Print summary ────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════════\n")
cat("  ROC Analysis: Proxy as Classifier in Sectors 19/24\n")
cat("══════════════════════════════════════════════════════════════\n\n")

for (nm in names(results)) {
  r <- results[[nm]]
  cat(sprintf("%-20s  AUC = %.3f  |  Youden: J = %.3f  TPR = %.3f  FPR = %.3f  threshold = %.4f\n",
              nm, r$auc, r$youden_j, r$youden_tpr, r$youden_fpr, r$youden_threshold))
}


# ── Plot 1: ROC curves ──────────────────────────────────────────────────────
roc_plot_data <- list()
for (nm in names(results)) {
  r <- results[[nm]]
  parts <- strsplit(nm, "_")[[1]]
  sector <- parts[1]
  score_type <- paste(parts[-1], collapse = "_")
  roc_plot_data[[nm]] <- data.frame(
    fpr = r$fpr, tpr = r$tpr,
    sector = ifelse(sector == "pooled", "Pooled (19+24)", paste("Sector", sector)),
    score = score_type,
    stringsAsFactors = FALSE
  )
}
roc_df <- bind_rows(roc_plot_data)

# Nicer labels
roc_df$score <- rub <- ifelse(roc_df$score == "raw", "proxy_weighted",
                        ifelse(roc_df$score == "intensity", "proxy / revenue",
                        ifelse(roc_df$score == "gam", "GAM (phat)", roc_df$score)))

p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, colour = score)) +
  geom_line(linewidth = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  facet_wrap(~ sector) +
  coord_equal() +
  labs(
    x = "FPR", y = "TPR", colour = "Score",
    title = "ROC: Proxy as emitter classifier (sectors 19 & 24)",
    subtitle = "Can the raw proxy separate emitters from non-emitters?"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_DIR, "diagnostic_proxy_roc_19_24.pdf"), p_roc, width = 12, height = 5)
cat(sprintf("\nSaved: %s\n", file.path(OUT_DIR, "diagnostic_proxy_roc_19_24.pdf")))


# ── Plot 2: Density of proxy by emitter status ──────────────────────────────
dens_df <- df %>%
  mutate(
    emitter_label = ifelse(emitter == 1, "Emitter", "Non-emitter"),
    sector_label = paste("Sector", nace2d),
    asinh_proxy = asinh(proxy_weighted)
  )

p_dens_raw <- ggplot(dens_df, aes(x = asinh_proxy, fill = emitter_label)) +
  geom_density(alpha = 0.5, colour = NA) +
  facet_wrap(~ sector_label, scales = "free_y") +
  scale_fill_manual(values = c("Emitter" = "#2166AC", "Non-emitter" = "#B2182B")) +
  labs(
    x = "asinh(proxy_weighted)", y = "Density", fill = NULL,
    title = "Proxy distribution by emitter status (sectors 19 & 24)",
    subtitle = "Raw proxy_weighted"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# Intensity version
dens_df_int <- df %>%
  filter(!is.na(proxy_intensity)) %>%
  mutate(
    emitter_label = ifelse(emitter == 1, "Emitter", "Non-emitter"),
    sector_label = paste("Sector", nace2d),
    asinh_intensity = asinh(proxy_intensity)
  )

p_dens_int <- ggplot(dens_df_int, aes(x = asinh_intensity, fill = emitter_label)) +
  geom_density(alpha = 0.5, colour = NA) +
  facet_wrap(~ sector_label, scales = "free_y") +
  scale_fill_manual(values = c("Emitter" = "#2166AC", "Non-emitter" = "#B2182B")) +
  labs(
    x = "asinh(proxy_weighted / revenue)", y = "Density", fill = NULL,
    title = "Proxy intensity distribution by emitter status (sectors 19 & 24)",
    subtitle = "proxy_weighted / revenue"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

# Combine into one PDF
pdf(file.path(OUT_DIR, "diagnostic_proxy_density_19_24.pdf"), width = 10, height = 5)
print(p_dens_raw)
print(p_dens_int)
dev.off()
cat(sprintf("Saved: %s\n", file.path(OUT_DIR, "diagnostic_proxy_density_19_24.pdf")))
