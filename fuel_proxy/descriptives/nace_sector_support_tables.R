###############################################################################
# fuel_proxy/descriptives/nace_sector_support_tables.R
#
# PURPOSE
#   Assess sector coverage of the training sample (EU ETS firms) relative to
#   the deployment population (all firms), at both NACE 5-digit and 2-digit
#   levels. Motivates the choice of partial pooling for sector effects.
#
#   Three diagnostics:
#     1) Sector coverage: how many NACE sectors in deployment data are
#        represented in the training sample?
#     2) Same at NACE 2-digit level.
#     3) Distribution of number of firms per sector in training sample
#        (histograms saved to OUTPUT_DIR).
#
# INPUTS
#   - PROC_DATA/firm_year_belgian_euets.RData
#   - PROC_DATA/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS
#   - OUTPUT_DIR/sector_coverage.csv
#   - OUTPUT_DIR/histogram_firms_per_nace5d.pdf
#   - OUTPUT_DIR/histogram_firms_per_nace2d.pdf
###############################################################################

# ====================
# Define paths -------
# ====================

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
library(ggplot2)

# ==================
# Load data --------
# ==================

load(file.path(PROC_DATA, "firm_year_belgian_euets.RData"))
load(file.path(PROC_DATA, "annual_accounts_selected_sample_key_variables.RData"))

# Training sample: EU ETS firms
train <- firm_year_belgian_euets %>%
  filter(!is.na(emissions)) %>%
  mutate(nace2d = substr(nace5d, 1, 2))

# Deployment population: all firms
deploy <- df_annual_accounts_selected_sample_key_variables %>%
  mutate(nace2d = substr(nace5d, 1, 2))


# =====================================================================
# 1) SECTOR COVERAGE: training vs deployment
# =====================================================================

# NACE 5-digit
deploy_5d <- n_distinct(deploy$nace5d, na.rm = TRUE)
train_5d  <- n_distinct(train$nace5d, na.rm = TRUE)
covered_5d <- length(intersect(
  unique(na.omit(deploy$nace5d)),
  unique(na.omit(train$nace5d))
))

# NACE 2-digit
deploy_2d <- n_distinct(deploy$nace2d, na.rm = TRUE)
train_2d  <- n_distinct(train$nace2d, na.rm = TRUE)
covered_2d <- length(intersect(
  unique(na.omit(deploy$nace2d)),
  unique(na.omit(train$nace2d))
))

coverage_table <- tibble(
  level = c("NACE 5-digit", "NACE 2-digit"),
  sectors_in_deployment = c(deploy_5d, deploy_2d),
  sectors_in_training   = c(train_5d, train_2d),
  sectors_covered       = c(covered_5d, covered_2d),
  coverage_pct          = round(c(covered_5d / deploy_5d,
                                  covered_2d / deploy_2d) * 100, 1)
)

cat("\n=== SECTOR COVERAGE: TRAINING vs DEPLOYMENT ===\n")
print(as.data.frame(coverage_table), row.names = FALSE)


# =====================================================================
# 2) FIRMS PER SECTOR IN TRAINING SAMPLE
# =====================================================================

# NACE 5-digit
firms_per_5d <- train %>%
  group_by(nace5d) %>%
  summarise(n_firms = n_distinct(vat), .groups = "drop")

cat("\n=== FIRMS PER NACE 5-DIGIT SECTOR (training sample) ===\n")
summary(firms_per_5d$n_firms)

# NACE 2-digit
firms_per_2d <- train %>%
  group_by(nace2d) %>%
  summarise(n_firms = n_distinct(vat), .groups = "drop")

cat("\n=== FIRMS PER NACE 2-DIGIT SECTOR (training sample) ===\n")
summary(firms_per_2d$n_firms)


# =====================================================================
# 3) HISTOGRAMS
# =====================================================================

p_5d <- ggplot(firms_per_5d, aes(x = n_firms)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Training sample: firms per NACE 5-digit sector",
    x = "Number of distinct firms",
    y = "Number of sectors"
  ) +
  theme_minimal()

p_2d <- ggplot(firms_per_2d, aes(x = n_firms)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(
    title = "Training sample: firms per NACE 2-digit sector",
    x = "Number of distinct firms",
    y = "Number of sectors"
  ) +
  theme_minimal()


# =====================================================================
# Save outputs
# =====================================================================

write.csv(coverage_table,
          file.path(OUTPUT_DIR, "sector_coverage.csv"),
          row.names = FALSE)

ggsave(file.path(OUTPUT_DIR, "histogram_firms_per_nace5d.pdf"),
       p_5d, width = 7, height = 4.5)

ggsave(file.path(OUTPUT_DIR, "histogram_firms_per_nace2d.pdf"),
       p_2d, width = 7, height = 4.5)

cat("\nSaved to", OUTPUT_DIR, "\n")
