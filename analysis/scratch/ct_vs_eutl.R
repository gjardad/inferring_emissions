###############################################################################
# analysis/scratch/ct_vs_eutl.R
#
# Quick comparison: Climate TRACE estimates vs EUTL verified emissions
# at the installation level (matched sample only).
#
# RUNS ON: local 1
###############################################################################

REPO_DIR <- "c:/Users/jota_/Documents/inferring_emissions"
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)

# Load CT results (contains ct_train_panel with y and ct_emissions)
load(file.path(PROC_DATA, "enet_climate_trace_results.RData"))

cat("CT train panel:", nrow(ct_train_panel), "firm-years\n")
cat("Columns:", paste(names(ct_train_panel), collapse = ", "), "\n\n")

# ct_train_panel has y (EUTL verified) and ct_emissions (Climate TRACE)
ok <- is.finite(ct_train_panel$y) & is.finite(ct_train_panel$ct_emissions)
y    <- ct_train_panel$y[ok]
yhat <- ct_train_panel$ct_emissions[ok]

cat("Matched firm-years with both:", sum(ok), "\n")
cat("  y > 0:", sum(y > 0), "\n")
cat("  ct_emissions > 0:", sum(yhat > 0), "\n\n")

# nRMSE (normalized by sd(y))
rmse <- sqrt(mean((y - yhat)^2))
nrmse_sd <- rmse / sd(y)

# Spearman rank correlation
rho <- cor(y, yhat, method = "spearman", use = "complete.obs")

cat(sprintf("nRMSE (sd):          %.3f\n", nrmse_sd))
cat(sprintf("Spearman rho:        %.3f\n", rho))
cat(sprintf("RMSE:                %.0f\n", rmse))
cat(sprintf("Mean y:              %.0f\n", mean(y)))
cat(sprintf("SD y:                %.0f\n", sd(y)))
cat(sprintf("Mean CT:             %.0f\n", mean(yhat)))
cat(sprintf("Median CT/EUTL:      %.2f\n", median(yhat[y > 0] / y[y > 0])))
