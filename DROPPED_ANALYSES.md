# Dropped Analyses

Analyses explored during the project that are **not** part of the final results. The final analysis is in `analysis/nested_cv/models_with_fold_specific_proxy.R` (weighted proxy, positive coefficients only).

---

## 1. Fossil Fuel Consumption Proxy from Customs Data

**Idea:** Since Belgium imports almost all fossil fuels, build a firm-year proxy for fossil fuel consumption based on the amount each firm purchases from firms identified via Customs data as fossil fuel importers (CN8 Chapter 27 codes).

**Scripts:** `preprocess/amount_consumed_from_fuel_importers.R`, `preprocess/build_cn8_fossil_fuel_list.R`, `preprocess/fuel_imported_by_firm_year.R`, `figures_tables/table_cn8_fossil_fuel_codes.R`, `figures_tables/table_cn8_validation.R`, `figures_tables/cn8_random_baseline.R`

**Why dropped:** Out-of-sample prediction gains were modest. The B2B-based elastic net proxy (which identifies fuel suppliers statistically rather than via product codes) performs better.

**Note:** Customs data may still appear in an appendix for validation of the elastic net supplier selection (see `analysis/validate_against_cn8.R`).

---

## 2. Hurdle Model with Poisson Intensive Margin (muhat-based)

**Idea:** The original hurdle specification used a Poisson GAM as the intensive margin: `yhat = I(phat > tau) * muhat`, where `muhat` is the Poisson-predicted emission level conditional on being classified as an emitter.

**Why dropped:** Replaced by the hybrid approach, which uses the raw fuel-supply proxy as the ranking signal (`yhat = I(phat > tau) * proxy_weighted`), followed by `calibrate_with_cap`. The Poisson GAM was smoothing away ranking information the proxy naturally contains. The hybrid improved within-sector ranking (rho_s from 0.414 to 0.652) and nRMSE (0.193 to 0.140) with no meaningful loss in FPR or TPR.

---

## 3. Pooled (Unweighted) Fuel-Supply Proxy

**Idea:** An earlier proxy variant that summed raw transaction amounts from all elastic-net-selected suppliers, without weighting by EN coefficients.

**Scripts:** `analysis/rho_by_sector_pooled_vs_weighted.R`, `analysis/compare_pooled_vs_within.R`

**Why dropped:** The coefficient-weighted proxy is more intuitive (higher-weight suppliers contribute more to the proxy) and performs slightly better. All final results use the weighted proxy only.

---

## 4. All-Coefficient Fuel-Supply Proxy (Including Negative EN Coefficients)

**Idea:** Use all non-zero elastic net coefficients (positive and negative) when constructing the fuel-supply proxy, rather than restricting to positive coefficients only.

**Scripts:** `analysis/nested_cv/models_with_fold_specific_proxy.R` (Row 5a comparison), `fold_specific_proxy_all` in `fold_specific_proxy.RData`

**Why dropped:** The positive-only proxy outperforms across most metrics. The all-coefficient variant degrades rho_s (0.326 vs 0.484) and TPR (0.840 vs 0.915). Negative coefficients likely reflect suppressor effects from collinear suppliers that do not transfer to non-ETS deployment. See `THOUGHTS.md` for the full discussion.

---

## 5. Within-Buyer Fixed Effects Elastic Net

**Idea:** Run the elastic net for fuel-supplier identification with buyer fixed effects (demeaning within-firm) instead of pooled sector fixed effects, to check whether the pooled model captures genuine fuel signal or just cross-sectional firm-size variation.

**Scripts:** `analysis/run_elastic_net.R` (within-buyer specification), `analysis/compare_pooled_vs_within.R`

**Why dropped:** Diagnostic analysis only. The pooled specification was retained for the final proxy because the within-buyer FE approach has weaker statistical power (few within-firm time-series observations per buyer) and the pooled model was validated against Customs data.

---

## 6. Soft Hurdle (Continuous phat Weighting)

**Idea:** Replace the hard threshold `I(phat > tau)` with continuous `phat` weighting: `yhat = phat * muhat`. This preserves continuous ranking information from the extensive margin classifier.

**Scripts:** `analysis/soft_hurdle_experiment.R`

**Why dropped:** Explored as a diagnostic. The hard threshold + calibration approach was retained.

---

## 7. Alternative Classifier Specifications for the Extensive Margin

**Idea:** Test whether XGBoost, random forest, or enriched GAM models (with additional covariates: capital, FTE, capital intensity, proxy x revenue interactions) outperform the baseline GAM for emitter vs. non-emitter classification.

**Scripts:** `analysis/fit_extensive_margin.R`

**Why dropped:** The baseline GAM is already strong (FPR=3.9%, TPR=98.1%). XGBoost edges it marginally but relies on sector identity for 70% of its classification gain — a feature that does not transfer to deployment (unseen sectors). Adding covariates does not meaningfully improve over the baseline. After `calibrate_with_cap`, all sector-aware models converge to nRMSE ~0.141-0.145.

---

## 8. Comprehensive Feature Group and Architecture Grid Search

**Idea:** Systematic comparison of 9 feature groups x 2 proxy variants x 2 architectures (hurdle vs hybrid), totaling 32 specifications.

**Scripts:** `analysis/alternative_specs.R`

**Why dropped:** The grid search confirmed that the hybrid architecture with the baseline feature set dominates. Additional covariates (capital, FTE, capital intensity, smooth terms, sector-specific slopes) do not meaningfully improve predictions.

---

## 9. Legacy Fuel Proxy Pipeline

**Scripts:** Everything in `fuel_proxy_legacy/` (35+ files covering descriptives, models, proxies, and utilities).

**Why dropped:** This was the original proxy-construction pipeline, replaced by the current `preprocess/` + `analysis/nested_cv/` approach. The legacy pipeline used a different cross-validation structure, different proxy definitions, and a different model architecture (including PPML). Do not modify, reference, or source from this folder.

---

## 10. Energy Balances, Emission Factors, and Fuel Concordances

**Idea:** Use energy balance data (SIEC classifications), IPCC emission factors, net calorific values, and CN-IPCC-SIEC concordances to build a bottom-up emission estimate from trade data.

**Scripts:** Various in `preprocess/` (e.g., `build_emission_factors_ipcc.R`, `build_ipcc_ncv_year.R`, `build_ncv_by_fuel_year.R`, `build_hs_cn_to_ipcc_mapping.R`, `build_siec_to_ipcc_mapping.R`, `build_hs_to_siec_map.R`, `download_ncv_from_energy_balance.R`, `download_fuel_use_from_energy_balance.R`, `download_supply_imports_from_energy_balance.R`)

**Why dropped:** Not part of the current analysis. The statistical (elastic net) approach to supplier identification outperformed the engineering approach.

---

## Not Dropped — Supporting Analyses

The following scripts are **not** part of the main results table but remain relevant as diagnostics, validation, or planned extensions:

- `analysis/validate_against_cn8.R` — validates EN-selected suppliers against Customs data (appendix material)
- `analysis/rho_star_test.R` — formalizes the argument that negative within-sector rho values are sampling noise
- `analysis/within_firm_emission_dynamics.R` — variance decomposition (between vs within-firm), provides context for the paper
- `analysis/diagnostic_rho_by_threshold.R` — threshold sensitivity diagnostic
- `analysis/nested_cv/alpha_sensitivity.R` — EN hyperparameter sensitivity (planned, not yet run systematically)
- `analysis/nested_cv/enet_financials_calibrated.R` — EN-on-financials benchmark (Row 2 variant)
- `analysis/nested_cv/ensemble_proxy_enet.R` — ensemble of proxy and EN (exploratory)
- `analysis/nested_cv/power_transform_allocation.R` — power transformation for allocation (exploratory)
- `analysis/model_selection/losocv/` — full model selection table scripts (01-07, plus figures)
