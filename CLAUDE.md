## Communication Guidelines: refer to the user as Jota

## Project Overview: The aim of the project is to build and evaluate the out-of-sample performance of a prediction model for firm-year-level emissions of greenhouse gases for the universe of firms in Belgium between 2005-2022.

### Research Question: to what extent does business-to-business transaction data help us infer emissions at the firm-year-level?

## Guidelines for evaluating models

The model should be first evaluated with respect to whether it satisfies internal validity: no information from the test set leaks into training. This includes any model parameters, hyperparameters, or variable selection — if a choice is data-dependent (e.g., tuning a threshold, selecting a weight, choosing a transformation parameter), it must be cross-fitted: tuned on the training folds and applied to the held-out fold. Evaluating a data-dependent choice on the same data used to select it is meaningless.

It should also be evaluated with respect to its external validity: it should generalize beyond the training distribution. External validity is impossible to guarantee with certainty in our context, so the goal here is to mitigate any obvious concerns.

**Concern of distribution shift is real.** Training data consists of all the firms regulated by the EU ETS (for which we observe their yearly emissions) plus non-EUETS firms from sectors with NACE 2-digit codes 17, 18, 19, and 24. We confidently assume emissions are 0 in the latter group, since EU ETS covers nearly 100% of the emissions from fuel combustion in these two sectors. Deployment data consists of all the non-EUETS firms from all other sectors. A lot of sectors that are present in deployment are not present in EU ETS, and vice-versa. It is not clear if conditional on observables distribution of emissions across firms is the same in the two data. Any suggestion in framing or modelling that alleviates this concern is welcome.

If it satisfies internal validity and there's no obvious concerns with respect to external validity, the performance of the model should be evaluated with respect to

1. prediction accuracy in levels
2. ability to discriminate between emitters and non-emitters, within and across sectors
3. ability to correctly rank firms within NACE 2-digit sectors with respect to their emissions

Getting small emitters right is a central goal. In Belgium, large emitters are already observed through the EU ETS, whose regulation threshold is strongly correlated with firm size. The prediction model's value-added is therefore primarily for the non-ETS firms — which tend to be smaller emitters.

**The main innovation of the paper is the use of B2B transactions data** so the set of specifications and the choice of benchmarks should be such that they highlight any prediction improvements from us using this data.

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

	fold_specific_proxy.RData: fold-specific (leakage-free) fuel-supply proxy panel. Contains `fs_proxy_panel` (5,876 × 11: vat, year, nace2d, y, emit, log_revenue, euets, primary_nace2d, fold_k, fold_specific_proxy, fold_specific_proxy_all), `fold_diagnostics` (per-fold EN summary), `sector_fold_map` (NACE 2-digit to fold assignment), and `syt` (sector-year emission totals). `fold_specific_proxy` uses only positive EN coefficients; `fold_specific_proxy_all` includes all non-zero coefficients (positive and negative). Built by `build_fold_specific_proxy.R` on RMD. Available on local 1.

**Downsampled data on local 1.** The following processed files on local 1 are **downsampled** versions of the full data (which is available only on RMD). The same applies to the corresponding raw .dta files in DATA_DIR/raw/NBB/.

- `annual_accounts_selected_sample.RData` (and `_key_variables` and `_more_selected_sample` variants)
- `b2b_selected_sample.RData`
- `df_b2b.RData`
- `df_trade.RData`
- `df_national_accounts_with_5digits.RData`
- `firms_in_selected_sample.RData`

The full training sample (`training_sample.RData` and `ffirm_year_panel_with_proxies.RData`) are NOT downsampled and are available in full on local 1.

### Hardware setup:

**I use three desktops in this project: local 1, local 2, and RMD (remote desktop).
Access to the full NBB data is restricted to RMD through a VPN connection. RMD doesn't have access to the web browser, but it is connected to GitHub. I can only use the VPN connection through local 2. Local 2 has regular access to the web browser. Local 1 is my personal desktop and it is where I have Claude code and cursor downloaded.
When copying files from RMD to local 1, I first need to copy them to local 2, then from local 2 to the cloud (Dropbox/Claude), then from the Claude to local 1.
In local 1 I have available a downsampled version of the full NBB data sets as well as the full training sample. I built the training sample in RMD and copied it to local 2.
Any script that only requires `training_sample.RData` (e.g., CV scripts, alternative specs, rho comparisons) can be run locally on local 1. RMD is only needed for scripts that access the raw NBB data (e.g., preprocessing, proxy construction).**

**Data sync log.** No known discrepancies.

### Purpose of the project and audience

This is the first chapter of my PhD thesis. It is supposed to be a standalone paper. The audience are economics academics experts on environmental economics and familiar with, although not necessarily experts on, regularization techniques and the institutional details of the EU ETS.

## Dropped Analyses

See `DROPPED_ANALYSES.md` for the full catalog of explored-and-dropped approaches.

## TO-DO List

1. **Main results tables.** Classify as emitters the top X% of firms within each sector-year (for a grid of X values) and redistribute the (CRF group, year) emission total among classified emitters using the CVed GLO. Report OOS performance (levels APD, rank correlation, extensive-margin metrics) for three ranking predictors side by side: (i) B2B proxy (proxy_mean), (ii) revenue, (iii) NACE-based fuel-supply proxy. This is the core comparison showing whether B2B data adds value over simpler benchmarks.

2. **Auxiliary tables.**
   - Compare GLO against other distributional assumptions (GPA, etc.).
   - Split of emitters/non-emitters by CRF mixed-sector (currently reported by NACE 2-digit).
   - Share of emissions regulated by EU ETS by CRF category (currently table 12, reported by NACE 2-digit).
   - L-moments table making the case that GLO is the best distributional choice.

3. **Logistic-regression threshold.** Report the logistic regression of the emitter dummy on p_i + log(1 + proxy_mean) as a descriptive result, but do not use it as the classification threshold. Classification uses the top-X% rule from item 1 instead.

4. **Re-write section 4.**

5. **Report IMJV validation results.** Present the IMJV external validation in the paper: 97% detection rate conditional on B2B presence but median APD of 0.97 at the intensive margin; revenue dominates the proxy at the extensive margin (top-X% cutoff comparison); IMJV-vs-EUTL baseline APD of 0.04 confirms the imputation error is real, not measurement noise. Discuss the coverage gap (14 of 31 non-ETS firms absent from B2B) and the distribution shift concern (proxy trained on heavy industry, validated on plastics/food/waste). Frame honestly as a negative result for the B2B proxy's external validity.

6. **Sanity check: ETS firms with zero B2B purchases.** On RMD, check whether any EU ETS firm-years in the training sample have zero B2B purchases (i.e., the firm's anonymized VAT from `EUTL_Belgium.dta` does not appear as a buyer in `b2b_selected_sample.RData` for that year). This would indicate that M&A-driven VAT reassignments could cause a firm's EUTL emissions to be recorded under one VAT while its B2B transactions are under another, which would introduce noise in the EN training. If prevalent, assess the magnitude (share of ETS firm-years affected, share of total ETS emissions they represent).

## Writing Notes

- **EN supplier selection introduces noise.** The EN selects suppliers from 53 NACE 2-digit sectors, many of which are clearly not fuel suppliers (e.g., management consultancy, NACE 70). The likely mechanism is that within a sector-year, larger emitters are also larger firms that purchase more services generally — so purchases from consultancies correlate with emissions not because they supply fuel but because they correlate with firm size. The paper should acknowledge this: the EN identifies a mix of genuine fuel supply relationships and spurious size-correlated purchasing patterns. This is a limitation of the data-driven approach relative to NACE-based classification, which at least selects on economic substance — though at the cost of missing intermediaries and multi-activity firms.

## Current Status

Drafting the paper in `paper/winter26_version/`. The `dec25_version/` is obsolete. All tables and figures use the coefficient-weighted fuel-supply proxy only. Fold-specific proxy was updated on 2026-03-05 (all-coefficient variant underperforms positive-only; see `THOUGHTS.md`).

### Tables and figures to generate

| # | Item | Section | Script | Where | Blocker |
|---|------|---------|--------|-------|---------|
| 16 | Classifier battery (GAM, XGBoost, RF) | appendix | `fit_extensive_margin.R` (re-run with fold-specific proxy) | local 1 | stale results, needs re-run |
| 17 | EN hyperparameter sensitivity (alpha grid) | appendix | `alpha_sensitivity.R` | RMD | not yet run |

### Pending pipeline: proxy R² and alternative CV schemes

| Step | Task | Script | Where | Status |
|------|------|--------|-------|--------|
| C1 | **Climate TRACE EN** — train EN with CT emissions as LHS; build proxy for non-CT EUTL firms. | `analysis/active/enet_climate_trace.R` | RMD | **done** |
| C2 | Copy `enet_climate_trace_results.RData` from RMD → local 1. | — | local 2 → cloud → local 1 | **done** |
| C3 | Evaluate CT-trained proxy locally against EUTL verified emissions on test set. | needs writing | local 1 | blocked on C2 |

## Referee 2 Correspondence

This project uses the Referee 2 audit protocol. There are no correspondences with referee 2 yet.

**Current Status:** [Not yet audited]

**Critical Rule:** Referee 2 NEVER modifies author code. It only reads, runs, and creates its own replication scripts in `code/replication/`. Only the author (you) modifies your own code in response to referee concerns.

**Important:** Referee reports do NOT belong in this CLAUDE.md file. They are standalone documents in the correspondence directory. This section only tracks status.

## Notes for Claude

Only change code after I explicitly tell you to do so. In particular, if I express an idea for code or analysis in the format of a question, answer it before making any changes to code. The exception is if I pose a question of the sort of "can you make X changes to code Y?".

Before committing changes, make sure changes have been tested locally. If they haven't, ask user if I'd like to test them locally before committing.
