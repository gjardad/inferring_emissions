## Communication Guidelines: refer to the user as Jota

## Project Overview: The aim of the project is to build and evaluate the out-of-sample performance of a prediction model for firm-year-level emissions of greenhouse gases for the universe of firms in Belgium between 2005-2022.

The model should be evaluated with respect to its

1. prediction accuracy in levels
2. ability to discriminate between emitters and non-emitters, within and across sectors
3. ability to correctly rank firms within NACE 2-digit sectors with respect to their emissions

Getting small emitters right is a central goal. In Belgium, large emitters are already observed through the EU ETS, whose regulation threshold is strongly correlated with firm size. The prediction model's value-added is therefore primarily for the non-ETS firms — which tend to be smaller emitters.

### Research Question: to what extent does business-to-business transaction data help us infer emissions at the firm-year-level?

### Data Sources

All data at the firm-level contain an unique firm-level identifier which is an anonymized VAT code. The code makes it possible to merge data sets.

NBB data sets (located in DATA_DIR/raw/NBB):

	Customs data: firm-product-year-level data on imports and exports for the universe of goods imported into and exported from Belgium between 2000 and 2022. Goods are 	identified by CN 8-digit product codes and firms are identified by anonymized VAT codes.

	B2B: buyer-supplier-year-level data on transactions between any two given private firms in Belgium between 2002 and 2022. It only contains the total nominal euros any   	two given firms transact, not which products were transacted. Buyers and suppliers are identified by their anonymized VAT code. The raw data is B2B\_ANO.dta

	Annual accounts: a large set of firm-year-level characteristics. It includes, among others, revenue, wage bill, capital, and nace 5-digit sectors. The raw data is 	called Annual\_Accounts\_MASTER\_ANO.dta

	PRODCOM: firm-product-year data on quantities and prices for each 8-digit code product produced by any given firm that is part of the survey sample. It only covers 	manufacturing goods (NACE codes between 07 and 33), for a sample of around 200k firms.

	EUTL\_Belgium: for each installation covered by the EUTL, it provides unique firm identifiers (BvD id and the corresponding anonymized VAT).

EUTL data sets (located in DATA_DIR/raw/EUTL/Oct_2024):

	account: for each installation, it informs account unique id and corresponding BvD id.

	compliance: for each installation-year regulated by the EUETS, it contains amount of emission permits allocated to the installation and verified emissions on that year. 	Installation are identified by unique installation\_id.

	installation: for each installation regulated by the EUETS, it contains installation\_id, geographic location (lat/lon), NACE sector id (2 or 4 digit), among others.

NIR data (located in DATA_DIR/raw/NIR):

	BEL-CRTs: for each year between 1990 and 2023, it informs GHG emissions by CRF category. The data is split into multiple .xlsx, one for each year.

	Annex XII: data on total GHG emissions and share of it regulated by the EUETS by CRF category. It consists of two .xslx files, one for 2022 (published in 2024) and one 	for 2023 (published in 2025).

CRF-to-NACE crosswalk (located in DATA_DIR/raw/Correspondences_and_dictionaries): maps CRF categories to NACE Rev. 2 sectors. Combined with BEL-CRTs, this enables sector-level calibration targets for deployment to non-EUETS firms.

Processed data (located in DATA_DIR/processed):

	training_sample.RData: firm-year panel used for cross-validation. Contains EU ETS firms (with verified emissions) and non-ETS firms from NACE 19/24 (with emissions set to 0), merged with annual accounts variables, B2B fuel-supply proxies, fold assignments, and sector-year emission totals. Available on local 1.

**Downsampled data on local 1.** The following processed files on local 1 are **downsampled** versions of the full data (which is available only on RMD). The same applies to the corresponding raw .dta files in DATA_DIR/raw/NBB/.

- `annual_accounts_selected_sample.RData` (and `_key_variables` and `_more_selected_sample` variants)
- `b2b_selected_sample.RData`
- `df_b2b.RData`
- `df_trade.RData`
- `df_national_accounts_with_5digits.RData`
- `firms_in_selected_sample.RData`

The full training sample (`training_sample.RData`), EUTL data (`firm_year_belgian_euets.RData`, `firm_year_emissions.RData`, `installation_year_emissions.RData`), and all mapping/crosswalk files are NOT downsampled and are available in full on local 1.

Feel free to suggest any additional data sets you may think can help.

### Hardware setup:

**I use three desktops in this project: local 1, local 2, and RMD (remote desktop).
Access to the full NBB data is restricted to RMD through a VPN connection. RMD doesn't have access to the web browser, but it is connected to GitHub. I can only use the VPN connection through local 2. Local 2 has regular access to the web browser. Local 1 is my personal desktop and it is where I have Claude code and cursor downloaded.
When copying files from RMD to local 1, I first need to copy them to local 2, then from local 2 to the cloud (Dropbox/Claude), then from the Claude to local 1.
In local 1 I have available a downsampled version of the full NBB data sets as well as the full training sample. I built the training sample in RMD and copied it to local 2.
Any script that only requires `training_sample.RData` (e.g., CV scripts, alternative specs, rho comparisons) can be run locally on local 1. RMD is only needed for scripts that access the raw NBB data (e.g., preprocessing, proxy construction).**

**Data sync log.** No known discrepancies.

## Guidelines for specifications

**Design before results.** During estimation and analysis:

- Do NOT express concern or excitement about point estimates
- Do NOT interpret results as "good" or "bad" until the design is intentional
- Focus entirely on whether the specification is ideal for deployment

**Concern of distribution shift is real.** Training data consists of all the firms regulated by the EU ETS (for which we observe their yearly emissions) plus non-EUETS firms from sectors with NACE 2-digit codes 19 and 24. We confidently assume emissions are 0 in the latter group, since EU ETS covers nearly 100% of the emissions from fuel combustion in these two sectors. Deployment data consists of all the non-EUETS firms from all other sectors. A lot of sectors that are present in deployment are not present in EU ETS, and vice-versa. It is not clear if conditional on observables distribution of emissions across firms is the same in the two data. Any suggestion in framing or modelling that alleviates this concern is welcome.

**We are only modelling emissions from the combustion of fossil fuels** since EU ETS covers virtually all emissions from industrial processes.

**Domain knowledge matters**. Institutional aspects of the Belgium economy that can help justify modelling choices are welcome. For example, one important features of the Belgium economy is that across all years and fossil fuels, fossil fuels are almost exclusively imported and not domestically produced.

**The main innovation of the paper is the use of B2B transactions data** so the set of specifications and the choice of benchmarks should be such that they highlight any prediction improvements from us using this data.

### Purpose of the project and audience

This is the first chapter of my PhD thesis. It is supposed to be a standalone paper. The audience are economics academics experts on environmental economics and familiar with, although not necessarily experts on, regularization techniques and the institutional details of the EU ETS.

## Dropped Analyses

- **Fossil fuel consumption proxy from Customs**: In Belgium, fossil fuels are almost exclusively imported. For this reason, we attempted to model emissions by building a firm-year-level proxy for fossil fuel consumption that consisted on the amount purchased from firms identified from Customs data as fossil fuel importers. The out-of-sample prediction gains from this exercise were modest.

- **Hurdle model with Poisson intensive margin (muhat-based)**: The original hurdle specification used a Poisson GAM intensive margin to predict emission levels among predicted emitters (`yhat = I(phat > tau) * muhat`). This was replaced by the hybrid approach, which uses the raw fuel-supply proxy as the ranking signal instead (`yhat = I(phat > tau) * proxy_weighted`), followed by `calibrate_with_cap`. The hybrid dramatically improved within-sector ranking (rho_s from 0.414 to 0.652 on the training sample) while also improving nRMSE (0.193 to 0.140), with no meaningful loss in FPR or TPR. The Poisson GAM was smoothing away ranking information the proxy naturally contains; the hybrid lets calibration allocate sector-year totals proportionally to the raw proxy signal.

## Current Status

**Writing the paper.** Results are ready. We are now drafting the paper in `paper/winter26_version/`. The `dec25_version/` is obsolete — do not reference, modify, or draw content from it.

**Weighted proxy only.** Going forward, all tables and figures use only the coefficient-weighted fuel-supply proxy (not the unweighted/pooled variant). The weighted proxy is more intuitive and performs slightly better.

**Legacy code.** The `fuel_proxy_legacy/` folder contains earlier proxy-construction scripts that are no longer used. Do not modify, reference, or source anything from this folder. All active proxy construction is handled in `preprocess/`.

**Excluded from the paper.** The following are not part of the current analysis and should not appear in the text: Customs-based fuel proxy variants, energy balances / SIEC fuel classifications, emission factors and NCVs, fuel concordances (CN-IPCC-SIEC), PRODCOM data. Customs data may appear in an appendix for validation only.

**Elastic net hyperparameters not yet tuned.** No sensitivity analysis has been done on the elastic net hyperparameters used to construct the fuel-supply proxy. Currently, `run_elastic_net.R` tests only `alpha = 1.0` (lasso) and `alpha = 0.5` (elastic net), and the weighted proxy uses `alpha = 0.5` with `asinh(sales)` and `lambda.min`. A systematic grid search over alpha values and comparison between `lambda.min` and `lambda.1se` has not been performed. This requires running on RMD (full B2B data).

**Next step (2026-03-04): re-run `build_fold_specific_proxy.R` on RMD.** This will (1) update variable/object/file names from the old `proxy_nested`/`nested_cv_proxy` to `fold_specific_proxy`/`fs_proxy_panel`, and (2) produce `fold_specific_proxy_all`, a second proxy variant that includes all non-zero EN coefficients (positive and negative). The current proxy (`fold_specific_proxy`) only uses positive coefficients; negative coefficients carry ranking information that may improve within-sector discrimination. After running on RMD, copy `fold_specific_proxy.RData` to local 1 and re-run `models_with_fold_specific_proxy.R`.

**TODO: Rows 1–2 restricted to sectors 19/24.** Re-run revenue-proportional and EN-on-financials using only firms in NACE 19 and 24. The hypothesis is that revenue tracks emissions well within EU ETS firms (which are large and within-sector homogeneous), flattering Row 1 relative to Row 2 in the full-sample table. Restricting to 19/24 — where there is genuine extensive-margin variation and smaller firms — should give a fairer comparison.

**TODO: Negative rho pooling test.** To argue that negative within-sector rho values are due to small N rather than model failure: pool all sectors with negative rho into one pseudo-sector. Split the sample into (a) all firms from sectors with positive rho and (b) firms from the negative-rho sectors. Use group (a) to identify relevant suppliers and build the proxy, then predict emissions for group (b) and compute rho treating the pooled negative-rho group as a single sector. The larger effective N should yield a non-negative rho, supporting the claim that the negatives are sampling noise from thin sectors.

**TODO: Re-run downstream analyses with new fold-specific proxy.** After `fold_specific_proxy.RData` is updated on RMD (with new names + `fold_specific_proxy_all`), copy to local 1 and re-run: `models_with_fold_specific_proxy.R`, model selection scripts, and any figures/tables that depend on the proxy.

**Extensive margin classification experiment (2026-03-03).** `analysis/fit_extensive_margin.R` tests 7 classifiers for the extensive margin (emitter vs non-emitter): GAM proxy-only, GAM baseline, GAM enriched, GAM interaction (proxy × revenue), XGBoost full, XGBoost no-sector, and random forest. Results (LOFOCV on local 1): the current GAM baseline is already strong (FPR=3.9%, TPR=98.1%); XGBoost full edges it marginally (FPR=3.5%, TPR=98.4%). The critical finding is that XGBoost feature importance shows sector identity (`nace2d_int`) accounts for 70% of classification gain, while the fuel-supply proxy contributes only ~2%. When sector is dropped, XGBoost degrades to FPR=13.5%. This confirms the distribution shift concern: in-sample classification is driven by sector, not the proxy, but at deployment to unseen sectors the proxy becomes the marginal signal. Adding capital, FTE, capital intensity, or proxy × revenue interactions does not meaningfully improve over the baseline. After `calibrate_with_cap`, all sector-aware models converge to nRMSE ≈ 0.141–0.145.

## Model Selection Table

Scripts in `analysis/model_selection/losocv/` (01–07) and `analysis/model_selection/lofocv/`. Section structure and table design documented in `paper/winter26_version/section/model_selection.tex`.

## Referee 2 Correspondence

This project uses the Referee 2 audit protocol. There are no correspondences with referee 2 yet.

**Current Status:** [Not yet audited]

**Critical Rule:** Referee 2 NEVER modifies author code. It only reads, runs, and creates its own replication scripts in `code/replication/`. Only the author (you) modifies your own code in response to referee concerns.

**Important:** Referee reports do NOT belong in this CLAUDE.md file. They are standalone documents in the correspondence directory. This section only tracks status.

## Variable naming: fold-specific proxy

The fold-specific (leakage-free) fuel-supply proxy variable is called `fold_specific_proxy` in all local code. The R object holding the panel is `fs_proxy_panel`, saved to `fold_specific_proxy.RData`. The version on RMD still uses the old names: variable `proxy_nested`, object `nested_cv_proxy`, file `nested_cv_proxy.RData`. If re-running `build_fold_specific_proxy.R` on RMD, the output will use the updated names.

**TODO:** Re-run `build_fold_specific_proxy.R` on RMD to produce `fold_specific_proxy.RData` with the new names, then copy to local 1. Until then, `models_with_fold_specific_proxy.R` has a compatibility shim that loads the old `nested_cv_proxy.RData` and renames on the fly.

## Notes for Claude

Only change code after I explicitly tell you to do so. In particular, if I express an idea for code or analysis in the format of a question, answer it before making any changes to code. The exception is if I pose a question of the sort of "can you make X changes to code Y?".

Before committing changes, make sure changes have been tested locally. If they haven't, ask user if I'd like to test them locally before committing.
