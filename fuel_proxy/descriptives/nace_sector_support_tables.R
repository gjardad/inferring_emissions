###############################################################################
# fuel_proxy/descriptives/nace_sector_support_tables.R
#
# PURPOSE
#   Assess sector coverage of the training sample (EU ETS firms) relative to
#   the deployment population (all firms), at both NACE 5-digit and 2-digit
#   levels. Motivates the choice of partial pooling for sector effects.
#
#   Two diagnostics combined into a single table:
#     1) Distribution of firms per NACE sector in the training sample
#        (binned: < 3, 3-10, > 10).
#     2) Share of NACE sectors in deployment data that are represented
#        in the training sample (sector coverage).
#
# INPUTS
#   - PROC_DATA/firm_year_belgian_euets.RData
#   - PROC_DATA/annual_accounts_selected_sample_key_variables.RData
#
# OUTPUTS
#   - OUTPUT_DIR/sector_support.tex
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
library(knitr)
library(kableExtra)

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
# 3) COMBINED TABLE: firms per sector and sector coverage
# =====================================================================

bins_2d <- c(
  sum(firms_per_2d$n_firms < 3),
  sum(firms_per_2d$n_firms >= 3 & firms_per_2d$n_firms <= 10),
  sum(firms_per_2d$n_firms > 10)
)

bins_5d <- c(
  sum(firms_per_5d$n_firms < 3),
  sum(firms_per_5d$n_firms >= 3 & firms_per_5d$n_firms <= 10),
  sum(firms_per_5d$n_firms > 10)
)

combined_table <- data.frame(
  nace_level   = c("2-digit codes", "5-digit codes"),
  lt3          = c(bins_2d[1], bins_5d[1]),
  mid          = c(bins_2d[2], bins_5d[2]),
  gt10         = c(bins_2d[3], bins_5d[3]),
  in_training  = c(covered_2d, covered_5d),
  coverage_pct = c(round(covered_2d / deploy_2d * 100, 1),
                   round(covered_5d / deploy_5d * 100, 1))
)

cat("\n=== COMBINED SECTOR SUPPORT TABLE ===\n")
print(combined_table, row.names = FALSE)


# =====================================================================
# Save output
# =====================================================================

writeLines(
  combined_table %>%
    kable(format = "latex",
          col.names = c("NACE level", "$< 3$", "$3$--$10$", "$> 10$",
                        "In training", "Coverage (\\%)"),
          booktabs = TRUE, escape = FALSE,
          align = c("l", rep("c", 5))) %>%
    kable_styling(latex_options = "hold_position") %>%
    add_header_above(c(" " = 1, "Firms per cell" = 3, "Sector coverage" = 2)),
  file.path(OUTPUT_DIR, "sector_support.tex")
)

cat("\nSaved to", OUTPUT_DIR, "\n")
