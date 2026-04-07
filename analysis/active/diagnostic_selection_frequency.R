# diagnostic_selection_frequency.R
#
# Purpose: check whether collapsing the 200 CRF-group CV repeats into per
# firm-year summary statistics (selection frequency p_i and conditional mean
# m_i) would carry information beyond the single-fit answer.
#
# For each firm-year i with 200 proxy draws proxy_i_r:
#   p_i = mean(proxy_i_r > 0)                  # selection frequency
#   m_i = mean(proxy_i_r | proxy_i_r > 0)      # conditional mean intensity
#
# The key check is the distribution of p_i across firm-years. If it piles up
# at 0 and 1, the 200 repeats are effectively redundant and the proposed
# (p_i, m_i) reframing buys nothing. If there is meaningful interior mass,
# the proposal is worth building out.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

source("paths.R")

in_path  <- file.path(PROC_DATA, "repeated_cv_proxy_crf_asinh.RData")
out_dir  <- file.path("analysis", "active", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

load(in_path)  # brings in proxy_matrix, firmyear_index, repeated_cv_proxy_panel, ...

stopifnot(nrow(proxy_matrix) == nrow(firmyear_index))
M <- ncol(proxy_matrix)
cat(sprintf("Loaded proxy_matrix: %d firm-years x %d repeats\n",
            nrow(proxy_matrix), M))

# --- compute p_i and m_i ----------------------------------------------------
# Treat NA repeats (firm-year not in any test fold for that repeat) as missing,
# not as zero, so p_i is over the repeats where the firm-year was actually held out.

pos_mat   <- proxy_matrix > 0
n_obs     <- rowSums(!is.na(proxy_matrix))
n_pos     <- rowSums(pos_mat, na.rm = TRUE)
p_i       <- n_pos / n_obs

sum_pos   <- rowSums(proxy_matrix * pos_mat, na.rm = TRUE)
m_i       <- ifelse(n_pos > 0, sum_pos / n_pos, NA_real_)

summary_df <- firmyear_index %>%
  mutate(
    n_repeats_observed = n_obs,
    n_repeats_positive = n_pos,
    p                  = p_i,
    m_cond_pos         = m_i
  ) %>%
  left_join(
    repeated_cv_proxy_panel %>% select(vat, year, y, emit, nace2d, euets),
    by = c("vat", "year")
  )

# --- core diagnostic: distribution of p_i -----------------------------------
cat("\nDistribution of p_i across firm-years:\n")
print(summary(summary_df$p))

bins <- c(-1e-9, 0, 0.05, 0.2, 0.4, 0.6, 0.8, 0.95, 1 - 1e-9, 1 + 1e-9)
labs <- c("=0", "(0,0.05]", "(0.05,0.2]", "(0.2,0.4]", "(0.4,0.6]",
          "(0.6,0.8]", "(0.8,0.95]", "(0.95,1)", "=1")
tab <- summary_df %>%
  mutate(bin = cut(p, breaks = bins, labels = labs, include.lowest = TRUE)) %>%
  count(bin) %>%
  mutate(share = n / sum(n))
cat("\nBinned p_i (the interior bins are what matter):\n")
print(tab)

interior_share <- mean(summary_df$p > 0.05 & summary_df$p < 0.95, na.rm = TRUE)
cat(sprintf("\nShare of firm-years with 0.05 < p_i < 0.95: %.3f\n", interior_share))
cat("  (rule of thumb: < 0.05 -> proposal is dead; > 0.15 -> worth pursuing)\n")

# Same split by emit, since the interesting question is whether p_i discriminates.
cat("\nDistribution of p_i within emit == 1 vs emit == 0:\n")
print(summary_df %>%
        group_by(emit) %>%
        summarise(
          n         = n(),
          mean_p    = mean(p, na.rm = TRUE),
          median_p  = median(p, na.rm = TRUE),
          share_eq0 = mean(p == 0, na.rm = TRUE),
          share_eq1 = mean(p == 1, na.rm = TRUE),
          share_int = mean(p > 0.05 & p < 0.95, na.rm = TRUE)
        ))

# --- plots ------------------------------------------------------------------
p_hist <- ggplot(summary_df, aes(x = p)) +
  geom_histogram(binwidth = 0.02, boundary = 0) +
  labs(x = "p_i  (share of 200 repeats with proxy > 0)",
       y = "firm-years",
       title = "Distribution of selection frequency p_i across firm-years") +
  theme_minimal()

p_hist_by_emit <- ggplot(summary_df %>% filter(!is.na(emit)),
                         aes(x = p)) +
  geom_histogram(binwidth = 0.02, boundary = 0) +
  facet_wrap(~ emit, scales = "free_y",
             labeller = as_labeller(c(`0` = "emit = 0", `1` = "emit = 1"))) +
  labs(x = "p_i", y = "firm-years",
       title = "p_i by true emit status") +
  theme_minimal()

ggsave(file.path(out_dir, "diag_selfreq_hist.png"),
       p_hist, width = 6, height = 4, dpi = 150)
ggsave(file.path(out_dir, "diag_selfreq_hist_by_emit.png"),
       p_hist_by_emit, width = 8, height = 4, dpi = 150)

saveRDS(summary_df, file.path(out_dir, "diag_selfreq_summary.rds"))
cat("\nWrote:\n",
    " - ", file.path(out_dir, "diag_selfreq_hist.png"), "\n",
    " - ", file.path(out_dir, "diag_selfreq_hist_by_emit.png"), "\n",
    " - ", file.path(out_dir, "diag_selfreq_summary.rds"), "\n", sep = "")
