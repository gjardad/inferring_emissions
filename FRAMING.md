# Framing the Paper

## Research question

How much can business-to-business transaction data tell us about which firms emit CO2 and how much?

## Contribution

We show that B2B data contains a meaningful signal for firm-level emissions — purchases from fuel suppliers explain about 20% of the log-variance in emissions across firms — and use this to produce the first firm-level emission estimates for the universe of Belgian firms. The signal is robust across extraction methods (data-driven EN and simple NACE lookup), and the rigorous CV design we develop provides an honest assessment of what can and cannot be predicted out of sample.

---

## What problem does this paper solve?

We estimate firm-level CO2 emissions from fossil fuel combustion for the universe of private firms in Belgium, using business-to-business transaction data. The core economic logic is simple: firms that buy more fuel emit more. B2B data reveals who buys from whom, so it can be used to infer fuel purchases — and therefore emissions — at the firm level.

## Positioning relative to satellite-based emissions monitoring

### The state of the art for independent installation-level emissions

**For methane:** Satellite-based direct detection is approaching true installation-level capability. GHGSat (~25m resolution) can detect and quantify CH4 plumes from individual facilities. MethaneSAT (~100–400m, launched March 2024) covers entire basins. TROPOMI/Sentinel-5P (~5.5 km) catches only super-emitters.

**For CO2:** There is no reliable independent installation-level satellite measurement yet. The atmospheric CO2 background is ~420 ppm, making it much harder to attribute marginal enhancements to individual facilities. What exists:
- OCO-2/OCO-3 (NASA): detects CO2 enhancements over very large sources (>10 MtCO2/yr) but with narrow swath and infrequent revisit.
- CO2M/Copernicus (ESA, expected ~2026–2027): first planned operational satellite for installation-level CO2 plume attribution (~4 Mt/yr threshold). Not launched yet.
- Climate TRACE: satellite-derived *activity* signals (thermal anomaly for steel/cement, water vapor plumes for power plants, VIIRS for flaring) combined with emission factors. Estimates production, not emissions directly — but is the best operational system at scale. For sectors without satellite signal (chemicals, aluminum, petrochemicals, heat plants, coal mining), CT uses capacity × utilization × emission factor — the same approach national inventories have used for decades.

### Where our B2B approach fits: the long tail below satellite detection thresholds

Satellites and registries (EUTL, EPA GHGRP) solve the *large point source* problem well. The EU ETS covers ~40% of EU emissions from ~10,000 installations. Climate TRACE tracks ~128 sources in Belgium. The aggregate tonnage from smaller emitters below these thresholds is modest relative to national totals.

**But for three policy-relevant applications, the long tail matters:**

1. **Carbon taxation extending below ETS thresholds.** CBAM and ETS2 (covering buildings and transport, starting 2027) signal an expansion of carbon pricing. Firm-level emission estimates become necessary for any extension to smaller industrial emitters.

2. **Supply chain due diligence (Scope 3).** Under CSRD, firms must estimate upstream/downstream emissions from *all* suppliers. Sector-level averages are the current default; firm-level estimates from B2B data are strictly more informative.

3. **Within-sector ranking, even when sector totals are small.** A firm emitting 500 tCO2/yr is invisible to any satellite but may be in the top decile of its sector among non-ETS firms. Knowing that ranking has allocative value for targeted policy (audits, subsidies, efficiency programs).

**Our contribution operates at a fundamentally different margin from satellites:**
- The **extensive margin**: which non-ETS firms emit at all.
- **Within-sector ranking** among the long tail of smaller emitters that no current or planned remote sensing technology can individually resolve.
- **Universal firm coverage**: B2B data covers all private firms in Belgium, not just the ~10,000 with EUTL installations or the ~128 that CT tracks.

The firm-level information content — who emits, how they rank, how their emission exposure propagates through the supply chain — is new and cannot be obtained from satellite data at any resolution.

## The elastic net vs. the NACE-code lookup (Tabachova benchmark)

### The conceptual point

The economic logic of our approach is simple: identify fuel suppliers, then sum each firm's purchases from them. In principle, fuel suppliers can be identified directly from industry classification codes (NACE 46.71 — wholesale of fuels, 19.20 — refining, 35.21–35.23 — gas, 47.30 — retail fuel). Tabachova et al. (2025) do exactly this for Hungary and allocate national emission totals proportionally to fuel purchases.

Our elastic net solves the same identification problem statistically rather than by lookup. This is necessary in our setting because VAT codes are anonymized — we cannot look up "is this firm a fuel distributor?" But it raises the question: **does the EN add value over the simple NACE lookup?**

### Empirical comparison (2026-03-07)

Among emitters only (y > 0, N = 3,095):

| Proxy | R² (levels) | R² (log-log) | Spearman ρ |
|-------|-------------|--------------|------------|
| Tabachova (NACE lookup, no parameters) | 0.10 | 0.19 | 0.42 |
| LOSOCV K=5 (sector-fold EN) | 0.31 | 0.17 | 0.45 |
| LOSO (29 sector folds) | 0.21 | 0.14 | 0.43 |
| Firm-fold CV K=10 | 0.47 | 0.22 | 0.52 |
| Full-sample EN (in-sample upper bound) | 0.97 | 0.39 | 0.59 |

### Interpretation

**The NACE lookup is a strong baseline for ranking.** In log-log R² and Spearman correlation, Tabachova is competitive with or better than the strictest CV schemes (LOSOCV, LOSO). This is a zero-parameter method that requires no training data whatsoever.

**The EN adds value primarily in two ways:**

1. **Levels prediction.** The EN learns supplier-specific *weights* — how much each euro of purchases from supplier X predicts emissions — not just supplier identities. This matters for distinguishing a 100 kt emitter from a 10 kt emitter. Firm-fold CV R² in levels (0.47) is 5× the Tabachova R² (0.10).

2. **Intensive margin ranking under favorable CV.** When within-sector firm overlap is available (firm-fold CV), the EN Spearman rises to 0.52 vs. Tabachova's 0.42. But under strict sector-holdout CV (LOSOCV, LOSO), the EN barely matches Tabachova.

**The gap between full-sample EN (0.39 log-log) and Tabachova (0.19) shows the EN identifies fuel suppliers that NACE codes miss** — firms whose primary activity isn't fuel distribution but who sell fuel as a secondary business. The CV proxies can't fully exploit this because supplier overlap across folds is incomplete (the known cross-sector non-overlap problem).

### Framing for the paper

The EN is solving an **identification problem**, not a conceptual one. The economic logic — firms that buy more fuel emit more — is simple and known in advance. The ML is needed only because fuel supplier identities are latent in our anonymized data. In a setting where supplier identities are known (e.g., a national statistical office with non-anonymized VATs), the EN step could be replaced by a direct lookup, and the proxy would likely be *better*.

**This is a strength, not a weakness.** A policymaker reading the paper should come away thinking: "we could implement this even more easily with our non-anonymized data." The paper demonstrates that B2B transaction data contains a strong signal for emissions. Whether that signal is extracted via EN or NACE lookup is an implementation detail — the key insight is that **who you buy from predicts how much you emit**.

The honest presentation shows both the NACE lookup (simple, transparent, zero-parameter) and the EN (data-driven, learns weights, requires training data) side by side. The EN's main value-added is in levels prediction and in contexts where fuel supplier identities are not known ex ante.

---

## Paper Skeleton

### 1. Introduction (intro.tex — to write)

**Opening:** Firm-level emissions matter for policy (carbon pricing, supply-chain disclosure, targeted regulation), but are unobserved for the vast majority of firms. Registries (EU ETS) cover ~10,000 large installations; satellites resolve only mega-emitters. The long tail — hundreds of thousands of smaller firms — is invisible.

**Research question:** How much can business-to-business transaction data tell us about which firms emit CO2 and how much?

**Core logic:** Firms that buy more fuel emit more. B2B data reveals who buys from whom. If we can identify fuel suppliers, we can infer fuel purchases — and therefore emissions — at the firm level.

**Preview of results:**
- The B2B fuel-supply proxy explains ~20% of the log-variance in emissions across firms (among emitters).
- The signal is robust: a zero-parameter NACE-code lookup (Tabachova et al. 2025) and a data-driven elastic net produce comparable ranking performance, confirming that the information lives in the transaction data, not the extraction method.
- A rank-and-calibrate architecture — classify who emits, rank them by proxy, calibrate to national/sector totals — outperforms standard regression approaches that use the same data.
- We produce the first firm-level emission estimates for the universe of Belgian private firms.

**Positioning:**
- vs. satellite monitoring: complementary margin (extensive margin + within-sector ranking for the long tail)
- vs. EEIO / revenue-proportional: B2B data distinguishes fuel purchasers from non-purchasers; revenue cannot
- vs. Trucost/Bloomberg-style regressions on financials: the proxy adds information orthogonal to firm size
- vs. Tabachova et al. (2025): same economic logic applied to Belgium; we add rigorous OOS evaluation via CV

**Roadmap:** Section 2 describes data. Section 3 constructs and validates the fuel-supply proxy. Section 4 evaluates prediction performance. Section 5 discusses model selection for deployment. Section 6 concludes.

### 2. Data (data.tex — largely written)

**2.1 Firm Characteristics** — Annual Accounts (revenue, NACE codes). [Written]

**2.2 Business-to-Business Transactions** — B2B database: buyer-supplier-year euros, no product info. [Written]

**2.3 Sample Selection** — Private non-financial sector, standard filters. [Written]
- TABLE: Sample coverage (share of GDP, wage bill, EUETS emissions) [#3 in to-do — script exists]

**2.4 Firm-Level Emissions** — EUTL verified emissions, aggregated to firm-year. [Written]

**2.5 Sector-Level Emission Totals** — NIR + Annex XII + CRF-to-NACE crosswalk. Two roles: (a) zero-emission assumption for NACE 19/24, (b) calibration targets. [Written]

### 3. A Proxy for Fuel Consumption (proxy.tex — skeleton written, prose to write)

**3.1 Setup and Motivation**
- Scope restriction: combustion emissions only (EU ETS covers virtually all process emissions)
- TABLE: NIR emissions by sector + EU ETS coverage shares [to-do #1]
- Training sample: EU ETS firms (observed emissions) + non-ETS firms in NACE 19/24 (zero emissions)
- TABLE: Sectors 19/24 summary by emitter status [to-do #2]

**3.2 Identifying Fuel Suppliers**
- Elastic net procedure: supplier columns (asinh sales), unpenalized controls (log revenue, year FE, sector FE), alpha=0.5, lambda.min
- Proxy definition: coefficient-weighted sum over selected suppliers
- ALGORITHM environment: make cross-fitting of supplier selection explicit
- Tabachova benchmark: NACE-code lookup (46.71, 19.20, 35.21-23, 47.30) — zero-parameter alternative

**3.3 Diagnostics**
- Supplier characterization: NACE breakdown, Customs validation [to-do #3]
- TABLE: OLS proxy regressions (4 columns) [to-do #4 — done]
- FIGURE: Kernel density by emitter status, sectors 19/24 [to-do #5]
- FIGURE: Binscatter levels vs within-sector ranks [to-do #6]

**3.4 Discussion**
- Proxy is a ranking signal, not a level predictor → motivates rank-and-calibrate
- Full-sample vs CV R² gap (0.41 vs 0.17): supplier non-overlap across sectors [Step A — done]
- EN vs Tabachova comparison table [to-do #18]
- Framing: B2B data contains the signal; EN vs lookup is an implementation detail

### 4. Evaluating Prediction Performance (predictive_performance.tex — skeleton written, prose to write)

**4.1 Cross-Validation Design**
- LOSOCV as primary scheme: simulates deployment to unseen sectors
- TABLE: Sector overlap training vs deployment [to-do #7]
- Firm-fold CV as complement: tests within-sector generalization
- Evaluation metrics: nRMSE, Median APD, within-sector Spearman rho, FPR/TPR

**4.2 Prediction Pipeline**
- Three-stage architecture: (a) classify emitter/non-emitter, (b) rank by proxy, (c) calibrate to aggregate
- National-aggregate calibration as purest ranking test
- Justification: zero-inflated emissions → hurdle model; proxy is ranking signal → calibration needed

**4.3 The Proxy in a Regression Framework**
- EN on ~220 annual accounts covariates vs same + proxy (unpenalized)
- TABLE: EN on financials vs EN + proxy [to-do #8 — needs RMD]
- Discussion: limited gains because regression is wrong tool for a ranking signal

**4.4 Extensive Margin Classification**
- Cross-sector proxy threshold transfer: learn from 19, apply to 24 and vice versa [to-do #9]
- TABLE: Threshold transfer results
- Classifier battery (GAM, XGBoost, RF) in appendix [to-do #16]

**4.5 Main Results**
- TABLE: 5-row build-up with national-aggregate calibration under LOSOCV [to-do #10]
  - Row 1: Revenue-proportional (EEIO benchmark)
  - Row 2: EN on financials (Trucost/Bloomberg benchmark)
  - Row 3: Proxy-proportional (value of B2B data)
  - Row 4: Proxy + hurdle (value of extensive margin)
  - Row 5: Proxy + hurdle + clipping
- Revenue benchmark restricted to sectors 19/24 [to-do #11]
- TABLE: R² decomposition (revenue, EN residual, proxy) [to-do #12]
- Key narrative: proxy's value is in ranking; rank-and-calibrate unlocks it

**4.6 Within-Sector Ranking**
- Negative-rho sectors: small-N artifact
- TABLE: rho by sector size [to-do #13]
- Pooling test for negative-rho sectors

**4.7 Gains from Sectoral Granularity**
- TABLE: Two rows compared back to main results [to-do #14, #15]
  - Row 1: Sector-year calibration targets (LOSOCV) [to-do #14]
  - Row 2: Firm-fold CV (sector identity in proxy construction) [to-do #15 — needs RMD]

### 5. Model Selection for Deployment (model_selection.tex — to write)

- Which specification to deploy? Synthesize CV results into a recommended model.
- Scripts in `analysis/model_selection/losocv/` (01-07) and `analysis/model_selection/firmfoldcv/`.
- Discussion of which CV scheme is appropriate for which deployment context:
  - Sectors with ETS presence → closer to firm-fold CV performance
  - Sectors without ETS presence → LOSOCV is the honest benchmark

### 6. Conclusion (conclusion.tex — to write)

- B2B transaction data contains a meaningful signal for firm-level emissions (~20% log-variance).
- The signal is robust across extraction methods (EN, NACE lookup).
- The rank-and-calibrate architecture is key: the proxy ranks, sector totals calibrate.
- Policy implications: any statistical office with non-anonymized B2B data could implement this.
- Limitations: distribution shift (training on ETS firms, deploying to non-ETS); proxy=0 blind spot; sector-level calibration quality varies.

### Appendix

- EU ETS coverage by CRF category (existing table)
- Fuel concordance examples (existing table)
- EN hyperparameter sensitivity (alpha grid) [to-do #17 — needs RMD]
- Classifier battery results [to-do #16]
- LOSO vs LOSOCV comparison
- Climate TRACE EN results [Steps C1-C3]
