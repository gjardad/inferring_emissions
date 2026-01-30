###############################################################################
# 05_analysis/04_compare_fuel_share_emitters_nonemitters_C19_C24.R
#
# PURPOSE
#   Plot: compare the distribution of fuel input cost share between EU ETS and non-EU ETS firms in NACE 2-digit sectors 19 and 24.
#
# INPUTS
#   - data/processed/fuel_input_cost_share.RData
#
# OUTPUTS
#   - Two density plots (shown in session; ggsave line is commented).
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/05_analysis/04_compare_fuel_share_emitters_nonemitters_C19_C24.R
###############################################################################

#### HEADER -------

## This code creates plot that compares histogram offuel input cost share
# across emitters and non-emitters in sectors C19, C24

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

library(dplyr)
library(ggplot2)
library(scales)

# =========================
# Load data ---------------
# =========================

load(paste0(proc_data, "/fuel_input_cost_share.RData"))

# =========================
# Generate plot -----------
# =========================

# Distribution of fuel cost share by sector
df_plot <- fuel_input_cost_share %>%
  filter(year >= 2005) %>% 
  mutate(nace2d = substr(nace5d, 1, 2)) %>% 
  filter(nace2d %in% c("19", "24")) %>% 
  mutate(
    group = if_else(euets == 1, "EUETS", "Non-EUETS"),
    group = factor(group, levels = c("EUETS", "Non-EUETS"))
  )

plot_sector_kde <- function(sector_code, out_file, adjust = 1.1) {
  
  df_s <- df_plot %>% filter(nace2d == sector_code)
  
  p <- ggplot(
    df_s,
    aes(x = fuel_share_costs, color = group, linetype = group)
  ) +
    geom_density(
      linewidth = 1.05,
      adjust = adjust,
      key_glyph = "path"  
    ) +
    
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, NA)
    ) +
    
    scale_color_manual(
      values = c("EUETS" = "black", "Non-EUETS" = "grey45")
    ) +
    scale_linetype_manual(
      values = c("EUETS" = "solid", "Non-EUETS" = "dashed")
    ) +
    
    labs(
      x = "Fuel importer cost share",
      y = "Density",
      color = NULL,
      linetype = NULL
    ) +
    
    theme_classic(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      
      axis.title.x = element_text(size = 15, margin = margin(t = 12)),
      axis.title.y = element_text(size = 15, margin = margin(r = 12)),
      
      axis.text = element_text(size = 12),
      
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.key = element_blank(),
      legend.key.width = unit(3.5, "lines")
    )
  
  #ggsave(out_file, p, width = 7.5, height = 4.8, dpi = 300)
  p
}

# Generate and save
p_c19 <- plot_sector_kde("19", "kde_fuel_share_costs_C19.png")
p_c24 <- plot_sector_kde("24", "kde_fuel_share_costs_C24.png")

p_c19
p_c24
