# Inferring Emissions

## What this project is
PhD thesis chapter on predicting firm-year-level GHG emissions for firms not
regulated by the EU ETS in Belgium. Emissions are observed only for EU ETS firms;
the goal is to predict them for the rest of the economy. Two alternative approaches
are explored — both exploit B2B transaction data and the fact that nearly all fuel
supply in Belgium comes from imports, but they differ in how they use that information.

## Two approaches (`fuel_proxy/` vs `fuel_suppliers/`)

### `fuel_proxy/` — Fuel-consumption proxies
Identify fuel importers from customs (CN8) data, then trace their downstream sales
through B2B transactions to construct firm-level fuel-consumption proxies. Different
proxy variants apply different tweaks to this pipeline. Proxies enter as covariates
in PPML and hurdle regression models evaluated via Leave-One-Firm-Out CV (LOOCV).

### `fuel_suppliers/` — Fuel-supplier identification
Rather than building proxies from pre-identified importers, identify which firms
are fuel suppliers directly from the data. The logic: firms whose B2B sales to
downstream buyers are positively and consistently correlated with those buyers'
emissions are likely selling fuel. This uses penalised regression (elastic net)
where each candidate supplier gets its own coefficient; the sparsity penalty
shrinks non-fuel-suppliers to zero. Ground truth on the LHS comes from EU ETS
emissions plus confirmed zeros for non-ETS firms in NACE 19 and 24 (where NIR
data shows EU ETS covers ~100% of fuel-combustion emissions).

Both approaches share common inputs (B2B data, customs data on fuel importers,
EU ETS emissions) but are ultimately substitutes — one will be chosen to produce
the final emission predictions.

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
- Shared config in `paths.R` (root; sets DATA_DIR, REPO_DIR, derived paths)
- Data lives on a network drive (`X:/Documents/JARDANG/data/`)

## Folder structure
paths.R              Shared path constants (must be sourced at top of every script)
preprocess/          Numbered scripts (00-32) that build intermediate & processed datasets
fuel_proxy/          Everything for the fuel-consumption-proxy exercise
  proxies/           Defines the proxy grid and builds fuel-consumption proxy variants
  models/            LOOCV pipeline: model definitions, execution, parallel runners
  descriptives/      Summary stats, diagnostics, tables, and figures
  utils/             Reusable helper functions (metrics, subsample builders, etc.)
fuel_suppliers/      Everything for the fuel-supplier identification exercise (TBD)
  models/
  descriptives/
  utils/
paper/               LaTeX source for the paper (git submodule)


## Regression models (fuel_proxy)

All models are evaluated via Leave-One-Firm-Out CV (LOFOCV): hold out one
firm (all its years), train on the rest, predict the held-out firm. After
prediction, raw estimates are calibrated to known sector-year emission totals
(deployment-style proportional rescaling with equal-split fallback).

All estimation uses `mgcv::gam(..., method = "REML")`.

### PPML (single-equation Poisson)

Benchmark (no proxy):
  E[y_it] = exp( β·log(revenue_it) + year FE + sector effect )

Proxy-augmented:
  E[y_it] = exp( β·log(revenue_it) + θ₁·I(proxy_it > 0) + θ₂·asinh(proxy_it)
                 + year FE + sector effect )

Sector effect variants:
  - Fixed effects: sector as a factor (with reference-level fallback for
    unseen test sectors)
  - Random effects: `s(sector, bs = "re")` (partial pooling via mgcv)

Three PPML specifications are run:
  1. No proxy, sector FE
  2. No proxy, sector RE
  3. With proxy (loop over all proxy variants), sector RE

### Hurdle model (two-part)

Allows different proxies for the two steps and searches over hard thresholds.

**Step 1 — extensive margin (logit):**
  Pr(y_it > 0) = logit⁻¹( β·log(revenue) + θ₁·I(proxy_ext > 0)
                           + θ₂·asinh(proxy_ext) + year FE + sector RE )
  Produces phat (out-of-fold predicted probability of being an emitter).

**Step 2 — intensive margin (Poisson, emitters only):**
  E[y_it | y_it > 0] = exp( β·log(revenue) + θ₁·I(proxy_int > 0)
                             + θ₂·asinh(proxy_int) + year FE + sector RE )
  Trained only on firms with emissions > 0. Produces muhat (predicted level).

**Combined prediction:**
  ŷ_it = I(phat_it > threshold) × muhat_it

The pipeline precomputes step 1 and step 2 independently for every proxy,
then evaluates only the top-K (proxy_ext, threshold) pairs × top-K proxy_int
combinations to find the best triple by RMSE.

## Work environment
This project is developed across two desktops (never simultaneously):
- **Remote desktop (RMD)**: user `jardang` — paths start with `X:/Documents/JARDANG/...`
- **Local desktop**: user `jota_` — paths start with `C:/Users/jota_/...`

When running data analysis or scripts, the user will specify which desktop they
are working from so that the correct paths are used.

### Local desktop data
The local desktop holds a **downsampled, self-consistent mini-version** of the full
pipeline. All processed and intermediate datasets (b2b_selected_sample, loocv_training_sample,
etc.) were built from the same downsampled raw B2B data, so everything is internally
consistent — just smaller than the real thing (e.g. ~281 LHS firms vs 640, ~1,439
candidate sellers vs 25K+). Scripts developed locally should be validated end-to-end
on the downsampled data first, then run on the RMD with full data.

Any parameters tuned for the local dev run (e.g. `MIN_LHS_BUYERS` lowered from 5 to 1
in `fuel_suppliers/build_design_matrix.R`) must be restored before running on the RMD.
These are always marked with a `# TODO: ... before running on RMD` comment.

## Conventions
- Preprocessing scripts are numbered to indicate execution order
- Descriptive scripts are also numbered
- Helper functions live in `fuel_proxy/utils/` or `fuel_suppliers/utils/` and are sourced where needed
- `paths.R` (at repo root) must be sourced at the top of every script to set path constants
