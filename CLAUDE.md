# Inferring Emissions

## What this project is
PhD thesis chapter comparing regression models for predicting firm-year-level GHG
emissions out-of-sample in Belgium. The core exercise is Leave-One-Firm-Out Cross
Validation (LOOCV) across different model specifications and fuel-consumption proxies.

## Key idea
Firm-level fuel consumption is unobserved. Since nearly all fuel supply in Belgium
comes from imports, we identify fuel importers in customs data, then trace downstream
purchases via business-to-business (B2B) transaction data to construct firm-level fuel
consumption proxies. Different proxy variants apply different tweaks to this pipeline.

## Data sources (all Belgium, not stored in this repo)
- **B2B transactions**: near-universe of firms, 2002-2022
- **EU ETS emissions**: regulated firms, 2005-2022
- **Customs (CN8)**: firm-product-year imports/exports, 2002-2022
- **Eurostat NCVs**: fuel-level net calorific values (SIEC codes)
- **IPCC 2006 emission factors**: fuel-level (IPCC classification)
- **Energy balances**: fuels used in GHG activities by sector
- **Belgian NIR annexes (2024-2025)**: aggregate EU ETS coverage shares by sector

## Language & runtime
- All code is **R**
- Shared config in `config/paths.R` (sets BASE_DIR, DATA_RAW, DATA_INT, etc.)
  and `config/load_packages.R`
- Data lives on a network drive (`X:/Documents/JARDANG/carbon_policy_networks/data/`)

## Folder structure
config/          Shared paths and package loading
preprocess/      Numbered scripts (00-32) that build intermediate & processed datasets
proxies/         Defines the proxy grid and builds fuel-consumption proxy variants
loocv/           LOOCV pipeline: model definitions, execution, parallel runners
descriptives/    Summary stats, diagnostics, tables, and figures (outside LOOCV)
utils/           Reusable helper functions (metrics, subsample builders, etc.)
paper/           LaTeX source for the paper (git submodule)


## Conventions
- Preprocessing scripts are numbered to indicate execution order
- Descriptive scripts are also numbered
- Helper functions live in `utils/` and are sourced where needed
- `config/paths.R` must be sourced at the top of every script to set path constants
