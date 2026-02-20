###############################################################################
# fuel_proxy/descriptives/total_supply_vs_imports_fossil_fuels.R
#
# PURPOSE
#   Show that Belgium's fossil fuel supply is overwhelmingly imported.
#   Scatter plot: each point is a fuel-year observation with
#     x = Imports (TJ)
#     y = Domestic production + Imports (TJ)
#   Points near the 45-degree line indicate negligible domestic production.
#
# INPUTS
#   - RAW_DATA/Eurostat/supply_imports_energy_balance.csv
#     (created by preprocess/download_supply_imports_from_energy_balance.R)
#
# OUTPUTS (to OUTPUT_DIR)
#   - total_supply_vs_imports.png
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
library(tidyr)
library(ggplot2)

# ==================
# Load data --------
# ==================

df_raw <- read.csv(file.path(RAW_DATA, "Eurostat",
                              "supply_imports_energy_balance.csv"),
                    stringsAsFactors = FALSE)

# =====================================================================
# Filter to fossil fuels (exclude non-fossil, umbrella, and aggregate)
# =====================================================================

exclude_siec <- c(
  "E7000",                    # Electricity
  "H8000",                    # Heat
  "FE",                       # Final energy
  "C0000X0350-0370",          # Coal umbrella
  "C0350-0370",               # Coal sub-umbrella
  "P1000",                    # Peat umbrella
  "O4000XBIO",                # Oil umbrella excl. biofuels
  "R5110-5150_W6000RI",       # Renewables + waste umbrella
  "R5210B",                   # Biogasoline blended
  "R5210P",                   # Biogasoline pure
  "R5220B",                   # Biodiesel blended
  "R5220P",                   # Biodiesel pure
  "R5230P",                   # Bio jet kerosene pure
  "R5320B",                   # Blended biogases (not in data but excluded per user)
  "R5290",                    # Other liquid biofuels
  "R5300",                    # Biogases
  "TOTAL",                    # Grand total
  "S2000",                    # Nuclear
  "N900H"                     # Non-renewable waste heat
)

# Also exclude all renewables (RA*, R5*, W6*) and non-renewable waste
exclude_prefixes <- c("RA", "R5", "W6", "BIOE")

df <- df_raw %>%
  filter(!siec %in% exclude_siec,
         !grepl(paste0("^(", paste(exclude_prefixes, collapse = "|"), ")"), siec))


# =====================================================================
# Pivot to wide: one row per siec-year with PPRD and IMP columns
# =====================================================================

df_wide <- df %>%
  select(siec, year, nrg_bal, value_tj) %>%
  pivot_wider(names_from = nrg_bal, values_from = value_tj, values_fill = 0) %>%
  mutate(
    PPRD    = replace(PPRD, is.na(PPRD), 0),
    IMP     = replace(IMP, is.na(IMP), 0),
    imports = IMP,
    supply  = PPRD + IMP
  ) %>%
  filter(imports > 0 | supply > 0)

cat("\n=== SUPPLY vs IMPORTS: FOSSIL FUELS ===\n")
cat("Fuel-year observations:", nrow(df_wide), "\n")
cat("Unique SIEC codes:", n_distinct(df_wide$siec), "\n\n")

# =====================================================================
# Plot
# =====================================================================

max_val <- max(c(df_wide$imports, df_wide$supply), na.rm = TRUE)

p <- ggplot(df_wide, aes(x = imports, y = supply)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dotted", color = "black", linewidth = 0.5) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  labs(x = "Imports", y = "Domestic production + Imports") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, NA)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, NA)) +
  theme(
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.grid        = element_blank(),
    axis.ticks        = element_blank(),
    axis.line         = element_line(color = "black", linewidth = 0.3),
    axis.title.x      = element_text(size = 14, margin = margin(t = 15)),
    axis.title.y      = element_text(size = 14, margin = margin(r = 15)),
    axis.text         = element_blank()
  )

ggsave(file.path(OUTPUT_DIR, "total_supply_vs_imports.png"),
       p, width = 7, height = 6, dpi = 300)

cat("Saved to", file.path(OUTPUT_DIR, "total_supply_vs_imports.png"), "\n")
