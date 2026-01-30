####################
# File: create_folders.R
# Purpose: Creates the folder structure for the new LOOCV pipeline
####################

rm(list = ls())

if (tolower(Sys.info()[["user"]]) == "jardang") {
  folder <- "X:/Documents/JARDANG"
} else {
  stop("Set `folder` for this user.")
}

code <- file.path(folder, "carbon_policy_networks", "code")
root <- file.path(code, "loocv_pipeline")

dirs <- c(
  "00_config",
  
  "01_preprocess",
  
  "02_proxies",
  "02_proxies/cache",
  
  "03_models",
  "03_models/benchmark",
  "03_models/extensive",
  "03_models/extensive/cache",
  "03_models/intensive",
  "03_models/intensive/cache",
  "03_models/calibration",
  
  # optional: heterogeneous revenue betas as a model variant bucket
  "03_models/heterogeneous_betas",
  "03_models/heterogeneous_betas/cache",
  
  "04_loocv",
  "04_loocv/results",
  "04_loocv/results/lofo_firm",
  "04_loocv/results/loso_sector",
  "04_loocv/results/diagnostics",
  
  "05_analysis",
  "05_analysis/figures",
  
  "06_utils",
  
  # optional: a place for runnable entrypoints / orchestration scripts
  "00_setup"
)

full_paths <- file.path(root, dirs)

for (d in full_paths) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created: ", d)
  } else {
    message("Exists:  ", d)
  }
}

# Add empty README if missing
readme_path <- file.path(root, "README.md")
if (!file.exists(readme_path)) {
  writeLines(c("# loocv_pipeline", "", "Pipeline for preprocessing, proxies, models, CV, and analysis."), readme_path)
  message("Created: ", readme_path)
}
