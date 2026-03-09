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

### Quantitative accuracy of satellite-derived facility-level CO2 estimates

Even the best satellite systems produce noisy facility-level CO2 estimates, and only for the largest point sources:

**Single-overpass (instantaneous) estimates:**
- Individual overpass uncertainty ranges from 8–53% per plume (Nassar et al. 2017, *Geophysical Research Letters*; Nassar et al. 2021, *Remote Sensing of Environment*), with wind speed as the dominant error source.
- Across 50 individual overpasses of power plants worldwide, R² against reported CEMS emissions is just **0.12** (Brunner et al. 2023, *Atmospheric Chemistry and Physics*).
- PRISMA imaging spectrometer achieves average agreement within **27%** of CEMS-reported emissions per overpass; OCO-3 within **30%**, with R² = 0.32 against CEMS — but based on only 14 cases (Dumont Le Brazidec et al. 2023, *Atmospheric Chemistry and Physics*).

**Temporal averaging (multiple overpasses per plant):**
- Averaging over ~5 overpasses per plant (22 plants) improves R² from 0.12 to **0.40**; averaging over many overpasses pushes R² to **0.87** (Lin et al. 2023, *Atmospheric Chemistry and Physics*).
- Aggregate accuracy can be high — the sum of satellite estimates for 8 US power plants is within **0.8%** of reported totals (Nassar et al. 2021) — but this is aggregate, not facility-level accuracy.
- Current satellites collect few usable overpasses per plant per year: OCO-2's narrow swath (~10 km) yields roughly 2–5 cloud-free passes annually; OCO-3's Snapshot Area Mapping mode produced ~4–5 usable observations per plant over a 2-year window (Dumont Le Brazidec et al. 2023); PRISMA detects CO2 plumes in only ~50% of acquired scenes. A realistic annual facility-level R² for current-generation satellites is therefore **~0.4–0.6**, well below the asymptotic 0.87 achievable with many more repeat visits.

**Prospective (CO2M / Sentinel-7, ~2025–2026 launch):**
- The CO2M constellation (3 satellites, 250 km swath, 5-day revisit) will dramatically increase overpasses per plant, potentially pushing annual R² toward the 0.87 asymptote for large sources.
- Simulations suggest it could quantify annual emissions for 80–95% of large power plants with uncertainty ≤30%.
- Detection threshold remains high: ~5 Mt CO2/yr at 4 m/s wind; ~2.7 Mt at 2 m/s.

**Bottom line:** At annual frequency — the resolution relevant for comparison with our B2B proxy — the best current satellite CO2 estimates achieve facility-level R² of roughly **0.4–0.6** for the world's largest power plants (>3,000 MW coal). Future constellations may approach R² ~ 0.87, but the detection threshold (~3–5 Mt CO2/yr) means smaller industrial facilities — the population our B2B proxy targets — remain below satellite detection thresholds entirely.

**Satellite detectability among Belgian EU ETS firms:** In our training sample (257 unique EU ETS firms, 3,180 firm-years), the median emitter produces ~31 kt CO2/yr — two orders of magnitude below the satellite detection threshold. Only **5 firms** (1.9%) ever exceed 3 Mt/yr, and only **2 firms** (0.8%) ever exceed 5 Mt/yr. These 2–5 firms account for 28–45% of total EU ETS emissions in Belgium, but the remaining 98% of EU ETS firms — and the entire non-ETS population — are invisible to any current or planned satellite system.

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

### Empirical comparison: B2B proxy vs Climate TRACE (script: `analysis/scratch/compare_proxy_ct_matched.R`)

On an apples-to-apples comparison (same 102 firm-years matched between EUTL and Climate TRACE, all emitters), the B2B proxy outperforms CT's own satellite-derived facility-level estimates:

| Method | nRMSE | Spearman ρ | Median APD |
|--------|-------|------------|------------|
| Firm-fold EN (raw) | 0.82 | 0.58 | 0.84 |
| Sector-fold EN (raw) | 0.89 | 0.54 | 0.84 |
| Climate TRACE (direct) | 0.91 | 0.49 | 0.83 |

CT's self-reported uncertainty of ~30% is an internal confidence band, not a validated error metric. The actual median absolute percentage deviation against EUTL verified emissions is ~83%, with CT systematically overestimating (median CT/EUTL ratio ~1.3). The B2B proxy — constructed from transaction amounts alone, with no satellite data — achieves better rank correlation and comparable level accuracy on this matched sample.

## Identifying fuel suppliers: NACE codes and the elastic net

### The core insight is B2B, not the identification method

The key innovation is using B2B transaction data to measure firm-level fuel purchases. The economic logic is simple: firms that buy more fuel emit more. B2B data reveals who buys from whom and how much, so if we can identify fuel suppliers, we can construct a firm-level proxy for fuel consumption — and therefore emissions.

### The straightforward approach: NACE codes

The most natural way to identify fuel suppliers is through their industry classification. NACE codes directly flag fuel wholesalers (46.71), refineries (19.20), gas distributors (35.21–35.23), and fuel retailers (47.30). Tabachova et al. (2025) do exactly this for Hungary and allocate national emission totals proportionally to fuel purchases. This approach is transparent, requires no training data, and has zero parameters.

### Why NACE codes may not be enough

Two concerns motivate going beyond the NACE lookup:

1. **Secondary activities.** Some firms sell fuels even though their primary NACE code is in a different sector. A chemicals company that also distributes petroleum products, or a logistics firm that resells fuel, would be missed by a NACE-based identification.

2. **Missing or coarse NACE codes.** In practice, NACE codes are not always available or reliable — especially for smaller firms or in administrative datasets where classification is self-reported or outdated.

### The elastic net as a data-driven alternative

To address these concerns, we use an elastic net that treats each supplier as a potential fuel supplier and lets the data determine which ones predict emissions. The EN does not require knowing supplier identities ex ante — it learns which suppliers matter from the correlation between purchases and observed emissions in the training sample.

### Results: EN shows genuine gains, especially with within-sector overlap

Among emitters only (y > 0, N = 3,095):

| Proxy | R² (levels) | R² (log-log) | Spearman ρ |
|-------|-------------|--------------|------------|
| Tabachova (NACE lookup, no parameters) | 0.10 | 0.19 | 0.42 |
| Sector-fold CV K=5 (EN) | 0.31 | 0.17 | 0.45 |
| Firm-fold CV K=10 (EN) | 0.47 | 0.22 | 0.52 |
| Full-sample EN (in-sample upper bound) | 0.97 | 0.39 | 0.59 |

The three rows form a clean gradient of how much supplier information is available at training time: Tabachova uses none (zero parameters), sector-fold CV holds out entire sectors (limiting supplier overlap), and firm-fold CV preserves within-sector overlap. Performance increases monotonically along this gradient.

**The EN improves on NACE codes, and the gains are clearest under firm-fold CV** — where the EN has within-sector overlap to learn from. Firm-fold CV yields nearly 5× the levels R² (0.47 vs 0.10) and a 10-point gain in Spearman ρ (0.52 vs 0.42). The levels gain reflects the EN learning supplier-specific *weights* — how much each euro of purchases from a given supplier predicts emissions — rather than treating all fuel suppliers equally. The ranking gain reflects the EN identifying fuel suppliers that NACE codes miss: firms whose primary activity isn't fuel distribution but who sell fuel as a secondary business.

**Under strict sector-holdout CV, the gains are more muted.** Spearman ρ rises modestly (0.45 vs 0.42), and log-log R² is comparable (0.17 vs 0.19). This is because supplier overlap across sectors is incomplete — many suppliers selected by the EN in training folds are absent from the held-out sector. The full-sample EN (R² = 0.39 log-log vs Tabachova's 0.19) confirms that additional fuel suppliers exist beyond NACE-coded ones, but exploiting them OOS requires seeing some firms that buy from them in training.

**Bottom line:** Both approaches confirm the same underlying signal — B2B transaction data contains meaningful information about firm-level emissions. The EN genuinely improves on the NACE lookup when it has sufficient overlap to learn supplier weights and identify secondary fuel sellers. The fact that a zero-parameter NACE lookup already captures much of the ranking signal is reassuring: it means the predictive power lives in the transaction *amounts*, not in the identification method.

### Implication

**This is a strength, not a weakness.** A policymaker with non-anonymized B2B data could start with the NACE lookup and obtain a strong baseline. The EN adds value by learning weights and catching secondary fuel suppliers — gains that would likely be even larger with non-anonymized data, where supplier identities could be verified and the EN's selections interpreted directly. The paper demonstrates that B2B transaction data contains a robust signal for emissions, and that the EN extracts more of it than NACE codes alone when the data allows.

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
- National-aggregate calibration as the conservative benchmark. Sector-year calibration targets from NIR are unreliable for many CRF categories: inventory compilers allocate aggregate emissions across categories using revenue-based or similar models, so sector-level figures are themselves model outputs, not observed data. Only categories heavily covered by EU ETS have reliable sector totals (anchored by verified installation data). National-aggregate calibration avoids leaning on these uncertain sector splits.
- **The gap between national and sector-year calibration is large** (script: `analysis/scratch/compare_proxy_variants.R`). Under national calibration, firm-fold EN achieves nRMSE ~0.78. Under sector-year calibration, it drops to ~0.51. The proxy alone can rank firms within sectors but cannot allocate emissions across sectors. If one is willing to trust CRF-level emission estimates as calibration targets, prediction accuracy improves substantially — but the improvement comes from the calibration targets, not from the proxy.
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
- **The proxy's standalone value is in within-sector ranking and classification, not levels.** Spearman ρ ~0.68, within-sector ρ ~0.43 (firm-fold), FPR/TPR ~40%/92% — all calibration-free.
- Firm-fold CV dominates sector-fold CV on aggregate metrics (nRMSE 0.78 vs 0.84, Spearman ρ 0.68 vs 0.61).
- **Tabachova (NACE-based supplier ID) has catastrophic FPR (82%)** — predicts positive emissions for most non-emitters because the 6 NACE codes are too coarse. Competitive on ranking (Spearman ρ 0.69) but unusable for classification.

**4.6 Within-Sector Ranking**
- **The distribution of within-sector rho is wide and similar across all proxy approaches** (script: `analysis/scratch/compare_proxy_variants.R`). IQR from near-zero to ~0.6–0.7 across 23 sectors. Five sectors (13-Textiles, 22-Rubber/plastics, 25-Fabricated metals, 49-Land transport, 71-Architecture/engineering) have rho < 0.3 under all three methods (Tabachova, firm-fold EN, sector-fold EN). The poor-performing sectors are a feature of the underlying emission processes — emissions in these sectors aren't well traced through B2B fuel purchases — not an artifact of the proxy construction method.
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
