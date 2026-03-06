# Open Questions and Design Thoughts

## Positive-only vs all-coefficient fuel-supply proxy

**Status: undecided.**

The elastic net that identifies fuel suppliers produces both positive and negative coefficients. The current proxy (`fold_specific_proxy`) uses only positive coefficients; `fold_specific_proxy_all` includes all non-zero coefficients.

**Empirical comparison (2026-03-05, LOSOCV on local 1):**

| Variant | nRMSE | Med APD | rho_g | rho_s | FPR | TPR |
|---------|-------|---------|-------|-------|-----|-----|
| Positive-only (Row 5) | 0.543 | 0.817 | 0.696 | 0.484 | 0.453 | 0.915 |
| All coefficients (Row 5a) | 0.558 | 0.921 | 0.686 | 0.326 | 0.426 | 0.840 |

The positive-only proxy outperforms on nRMSE, rho_s (0.48 vs 0.33), and TPR (92% vs 84%). The all-coefficient variant gains marginally on FPR (0.43 vs 0.45) but loses everywhere else.

**Possible justification for positive-only:** The proxy is meant to capture fuel purchases. A firm buying more from a fuel supplier should have higher emissions, not lower — so positive coefficients have a clear economic interpretation while negative ones do not. Negative coefficients likely reflect suppressor effects from collinear suppliers: if suppliers A and B are correlated, the EN may assign A a positive weight and B a negative one to partial out shared variation. These statistical artifacts are estimated on EU ETS firms and are unlikely to transfer to non-ETS deployment, which is consistent with the rho_s degradation. Restricting to positive coefficients can be framed as sign-constrained regularization, analogous to NNLS, imposing economically motivated structure.

**Counterargument:** There is no a priori statistical reason to exclude negative coefficients — the EN selected them because they improve in-sample fit. The sign constraint is motivated by an economic prior that may be too strong. Negative coefficients could capture substitution effects (firms buying from supplier B instead of a true fuel supplier, signaling lower fuel use) rather than pure noise.

**Open question:** Is the economic interpretation strong enough to justify the constraint, or should we let the data speak? The empirical results favor positive-only, but that could be driven by overfitting of the negative coefficients in the training sample rather than by the sign constraint being structurally correct.

---

# Uncertainty in Firm-Level Emission Predictions

## Context

Firm-level predictions are used downstream for:
1. A quantitative model of the Belgian economy
2. Stylized facts about the distribution of emissions across the production network (e.g., correlation between scope 1 and upstream emissions within sectors)

## Where the uncertainty lives

Sector-year **totals** are pinned down by NIR calibration targets — essentially no uncertainty at that level. The uncertainty is in the **within-sector allocation**: which firms get how much of the sector total.

Two dimensions of within-sector allocation uncertainty:
- **Extensive margin:** which firms are classified as emitters (near-threshold firms could flip)
- **Intensive margin:** conditional on being an emitter, what rank/share does a firm get

## Approach: constrained resampling within sectors

### Baseline: perturb using CV rank displacements

In LOSOCV/LOFOCV, for each held-out EU ETS firm *i* in sector *s* year *t*, we observe both true emissions and predicted emissions. Within each sector-year, compute each firm's **rank displacement**: difference between true rank and predicted rank.

Example: if sector *s* in year *t* has 10 EU ETS firms, and firm *i* is ranked 3rd by the model but is truly 6th, its rank displacement is -3.

To generate a perturbed allocation at deployment:
1. For each non-ETS firm in a sector, draw a rank displacement from the empirical distribution of rank displacements (pooled across sectors, or sector-specific if N allows)
2. Apply these displacements to the predicted ranks
3. Re-allocate the sector-year total proportionally to the perturbed ordering
4. Recompute upstream emissions using the B2B network and the perturbed scope 1 predictions
5. Compute the stylized fact of interest
6. Repeat many times to get a distribution

**Why this rather than calibrating noise to rho_s:** Using actual rank displacements preserves features like asymmetry (model may rank top emitters better than middle ones) and sector-size dependence, without needing to estimate rho_s as an intermediate step. It also automatically reflects sampling uncertainty — sectors with few firms produce noisier rank displacement distributions, generating wider perturbations.

### Extensive margin perturbation

In addition to rank perturbations, flip firms near the classification threshold (emitter vs non-emitter) with some probability calibrated to FPR/TPR from CV. Then re-rank among predicted emitters. This captures both sources: who's in the set, and how they're ordered within it.

### Sensitivity analysis: robustness to distribution shift

CV rank displacements are estimated on EU ETS firms but applied to non-ETS firms. The deployment population differs from training — rho_s could be systematically different. This is fundamentally unknowable.

Address this transparently: show stylized facts for a range of ranking accuracies (e.g., rho = 0.3, 0.5, 0.7) by scaling the noise magnitude. If findings are robust across this range, they are credible despite distribution shift. If they flip, that is an honest finding.

**Framing for the paper:** "Our baseline uses CV residuals to calibrate ranking uncertainty. Because the deployment population differs from the training population, we also show robustness to degraded ranking accuracy."

---

# Extension: Satellite-Derived Emissions via Sentinel-5P and Industrial Geolocation

## Motivation

The method developed in this paper requires two country-level inputs: (1) B2B transaction data, and (2) ground-truth emissions for a subset of firms (here, EU ETS verified emissions) to train the elastic net that identifies fuel suppliers. Without ground-truth emissions, the model cannot be trained — there is no way to learn which suppliers are selling fossil fuels if we never observe who is burning them.

Satellite-derived measures of emissions could remove this dependency on ground-truth firm-level emissions data, potentially making the method applicable to countries or contexts where no emissions registry exists.

## Core idea

Combine two data sources to construct satellite-derived, firm-level emission proxies:

1. **E-PRTR / IED Industrial Reporting**: provides geolocations (lat/lon) of industrial installations across Europe. This pins down where specific firms operate physically.

2. **LEGO-4-AQ dataset**: provides NO2 atmospheric concentration at 1 km × 1 km resolution, derived from Sentinel-5P satellite observations. NO2 is a combustion byproduct and a strong correlate of fossil fuel use.

By matching installation geolocations to the 1 km × 1 km NO2 grid, one can assign each installation a satellite-derived NO2 concentration measure. This becomes a noisy but universally available proxy for combustion activity — no emissions registry needed.

## Implementation roadmap

**Step 1: Build data infrastructure and identify emitters.** Match E-PRTR/IED installation geolocations to the 1 km × 1 km LEGO-4-AQ NO2 grid. For each installation, extract the NO2 concentration from the corresponding grid cell (or average over neighboring cells). Link installations to firms via E-PRTR identifiers. The result is a firm-year panel of satellite-derived NO2 measures — available only for firms with geolocated installations in E-PRTR, not the full universe.

**Step 2: Validate satellite-derived emissions against EUTL.** For the subset of firms present in both EU ETS and E-PRTR, establish that the satellite-derived NO2 measure tracks verified emissions from the EUTL. This is the credibility check: if NO2 concentration at 1 km resolution does not correlate meaningfully with verified CO2 emissions, the extension fails here. Report correlations in levels, ranks, and across sectors.

**Step 3: Use satellite-derived emissions to train the elastic net.** Replace (or supplement) EU ETS verified emissions with the satellite-derived NO2 measure on the LHS of the elastic net. Since satellite data is available only for a subset of firms (those with geolocated installations in E-PRTR), this creates a natural cross-validation design: train on firms with satellite-derived measures, test on the remainder. This mimics the real deployment scenario — learning supplier identities from a noisy but broadly available signal, then predicting for firms where no direct emission measure exists.

## What this buys us

- **Removes the ground-truth bottleneck.** The method becomes applicable in any country with B2B transaction data and satellite coverage (i.e., everywhere Sentinel-5P observes, which is global).
- **Validation opportunity.** In Belgium, where we have both EU ETS verified emissions and satellite data, we can validate the satellite-derived proxy against the ground truth. This provides a credibility check before applying the method elsewhere.
- **Complementarity.** Even in countries with emissions registries, satellite data could supplement ground truth for firms/installations not covered by the registry.

## Challenges and limitations

- **NO2 ≠ CO2.** NO2 concentration is a proxy for combustion, not a direct measure of GHG emissions. The mapping from NO2 to CO2 depends on fuel type, combustion technology, and atmospheric conditions. This adds noise relative to verified emissions.
- **Spatial attribution.** A 1 km × 1 km grid cell may contain multiple installations, making it hard to attribute NO2 to a specific firm. Industrial zones with co-located plants are particularly problematic.
- **Background concentration.** NO2 from traffic, residential heating, and other non-industrial sources contaminates the signal. Differencing against nearby non-industrial cells or using temporal variation (weekday vs weekend, seasonal) could help but adds complexity.
- **Atmospheric transport.** NO2 disperses from its source, so the grid cell directly above an installation captures only a fraction of its plume. Wind patterns and atmospheric stability affect the spatial footprint.

## Empirical exploration: Climate TRACE vs EUTL for Belgium (2026-03-06)

**Climate TRACE** (climatetrace.org) provides free, publicly available facility-level CO2 emission estimates derived from satellite data + AI/ML. Covers 352M+ assets globally. Belgium data downloaded to `DATA_DIR/raw/Climate TRACE/BEL/`. Covers manufacturing, power, mineral extraction, fossil fuel operations. 128 unique sources in Belgium, with monthly data 2021–2024 and annual 2015–2024.

### Matching EUTL ↔ Climate TRACE

Matched using geolocation: tight geo match (<1 km) or name-confirmed match (1–5 km with bigram Jaccard similarity ≥ 0.25). EUTL has 483 land-based BE installations; CT has 128 sources.

**EUTL → CT:** 161/483 matched (33%). Most unmatched EUTL installations are small combustion plants (activity 20) that CT doesn't track.

**CT → EUTL:** 94/128 matched (73%). 34 unmatched CT sources are mostly food-beverage-tobacco and textiles — small facilities below the EU ETS threshold. These represent CT's incremental coverage beyond EUTL.

**Bottom line on overlap:** CT mostly tracks what EUTL already covers. The incremental value (34 non-ETS sources) is too small to meaningfully expand the training set.

### Emission comparison for matched installations

Panel: 277 installation-year obs, 96 installations, 2021–2023 overlap (after dropping missing/zero on both sides).

**Cross-sectional correlations (pooled):**

| Metric | r |
|--------|---|
| Pearson (levels) | 0.455 |
| Pearson (log) | 0.529 |
| Spearman (ranks) | 0.548 |

Stable across years (~0.52 log correlation each year).

**Within-installation correlations (time series, ≥3 years):**
- 90 installations with ≥3 years
- Median r = 0.83 (both levels and log) — CT tracks year-to-year variation well for individual installations
- Mean r = 0.54 (dragged down by ~20 installations where CT reports constant emissions, giving NA/zero-variance)
- Top matches near-perfect: power stations, cement plants, steel mills (r ≈ 1.0)

**Level comparison:**
- Median CT/EUTL ratio: 1.85 (CT systematically overestimates ~2×)
- Huge dispersion: p25 = 0.95, p75 = 9.14, mean = 40 (extreme outliers)

### Implications for the extension

1. **CT is not a viable alternative training signal for Belgium.** It covers mostly the same large emitters as EUTL, with only 34 additional non-ETS sources. The cross-sectional rank correlation of 0.55 means it would be a substantially noisier LHS variable than EUTL verified emissions.

2. **CT could serve as validation.** For the 94 matched sources, CT provides an independent check on EUTL data quality. The within-installation correlation of 0.83 is reassuring.

3. **The level bias (CT/EUTL ≈ 1.85) reinforces the ranking-not-levels finding.** Just like the B2B proxy, satellite-derived estimates are better at ranking than at levels — consistent with the paper's calibration approach.

4. **For non-EU countries without emissions registries**, CT could be the training signal despite being noisier. The 0.55 rank correlation with ground truth suggests it carries real information, just with more measurement error. Whether this is enough to identify fuel suppliers via the elastic net is an open empirical question.

5. **The raw Sentinel-5P / LEGO-4-AQ approach (building NO2 measures from scratch) may still have value** over CT's processed estimates, since it would cover all geolocated installations (not just the 128 CT tracks) and could be tailored to combustion-specific NO2 signatures. But this requires substantially more data engineering. **Update:** see the TROPOMI feasibility check below — raw ambient NO2 is not viable.

### Raw TROPOMI NO2 feasibility check (2026-03-06)

**Data:** TROPOMI-inferred annual mean surface NO2 at ~1 km resolution for Europe (2019), from Zenodo record 5484305. Downloaded to `DATA_DIR/raw/S-MESH/TROPOMI_NO2_europe_2019.nc` (314 MB). The dataset uses Sentinel-5P TROPOMI observations downscaled via XGBoost to ~1 km surface-level NO2 (ppb).

**Procedure:** Cropped the European raster to Belgium (2–7°E, 49–52°N; 440 × 330 grid cells). For each of the 483 EUTL Belgian land-based installations, extracted the NO2 value at the nearest 1 km grid cell. Merged with EUTL compliance data for 2019. Final sample: 296 installations with positive verified emissions and valid NO2 values.

**Correlations: NO2 (ppb) vs Verified CO2 Emissions (tCO2), 2019**

| Metric | r |
|--------|---|
| Pearson (levels) | 0.171 |
| Pearson (log-log) | 0.208 |
| Spearman (ranks) | 0.156 |

All correlations are negligible — far worse than Climate TRACE's ~0.55 Spearman.

**Why it fails:** The top-NO2 installations cluster in the Antwerp port/chemical zone (NO2 ≈ 15–16 ppb), regardless of individual emissions. The bottom-NO2 installations are in rural Wallonia (Bastogne, Vielsalm, Malmedy; NO2 ≈ 3–5 ppb), including a cement plant emitting 418 kt CO2. Ambient NO2 at 1 km resolution reflects **regional pollution load** (industrial density, traffic, urban background), not installation-specific combustion. Within-activity correlations are mostly near zero; the few positive ones (activity 24, 28, 36) have N ≤ 7.

**Implication:** Raw ambient NO2 concentration is not a viable proxy for installation-level CO2 emissions. Climate TRACE's processed estimates — which combine multiple satellite products with ML models calibrated to known emitters — are far superior (Spearman 0.55 vs 0.16). The value of satellite data for this extension lies in **processed, facility-attributed** estimates (like Climate TRACE), not in raw atmospheric concentration fields. Building a custom NO2-to-CO2 pipeline would require plume detection, wind-field correction, and background subtraction — essentially replicating what Climate TRACE already does.

### Strategic framing: B2B proxy vs satellite-derived emissions

Two complementary angles for the satellite extension, both valuable:

**Angle 1 (best case): satellite + B2B removes the ground-truth bottleneck.** If properly processed satellite-derived NO2 (via flux inversion — background subtraction, wind-field correction, atmospheric lifetime modeling, fuel-type emission factors) tracks installation-level CO2 well enough, it can replace EUTL verified emissions as the LHS of the elastic net. The method would then require only B2B transaction data + freely available satellite/meteorological data, making it applicable globally without any emissions registry. Script `analysis/active/enet_climate_trace.R` tests a version of this using Climate TRACE's processed estimates as the LHS.

**Angle 2 (defensive): B2B proxy matches or beats satellite.** Even if the satellite-derived measure is not great after corrections, showing that the B2B proxy achieves comparable or better ranking accuracy reinforces the paper's core message: administrative transaction data is a surprisingly powerful signal for emissions, competitive with expensive satellite-based approaches.

**Benchmark comparison (2026-03-06):**
- Climate TRACE vs EUTL (processed satellite → CO2): global Spearman ≈ 0.55 (128 BE sources)
- B2B proxy vs EUTL (K=5 LOSOCV, strictest CV): global Spearman ≈ 0.42 (2,839 firm-years)
- Raw TROPOMI NO2 vs EUTL (no processing): global Spearman ≈ 0.16 (296 installations)

The B2B proxy at 0.42 is already approaching CT's 0.55, and this is under the strictest CV scheme where entire sectors are held out. Under LOFOCV (where same-sector firms remain in training), the B2B proxy should improve — potentially reaching parity with CT. LOSO and LOFOCV scripts are ready to run on RMD (`analysis/active/build_loso_proxy.R`, `analysis/active/build_lofocv_proxy.R`).

**Note on the comparison:** these numbers are not perfectly apples-to-apples. CT operates at the installation level (128 sources), while the B2B proxy operates at the firm level (2,839 firm-years). Multi-installation firms and differences in sample composition make direct comparison imprecise. But the orders of magnitude are informative.

### What a proper NO2 → CO2 pipeline would require

Converting raw satellite NO2 concentration to installation-level CO2 emission estimates requires:

1. **High-frequency NO2 data** — daily TROPOMI L2 swaths or the S-MESH daily 1km product (not the annual mean we used). Daily resolution is essential for plume detection.
2. **Wind fields** — ERA5 reanalysis (hourly, ~30km), freely available from Copernicus. Needed for flux inversion: trace the observed NO2 plume back to its source.
3. **Atmospheric chemistry parameters** — NO2 photolysis rates, OH radical concentrations, boundary layer height. Available from ERA5 and chemical transport models.
4. **Installation geolocations** — E-PRTR/IED for Europe, or EUTL coordinates (already available).
5. **Background NO2 subtraction** — isolate the facility-specific plume from regional/urban NO2. Methods: upwind/downwind differencing, weekend/weekday contrasts, or chemical transport model baselines.
6. **Fuel-type-specific NO2/CO2 emission factors** — the ratio varies ~3× between gas and coal. E-PRTR reports fuel types for some installations but coverage is incomplete.

Items 1, 2, 3, 4 are freely available. The hard part is the atmospheric modeling expertise (flux inversion) and fuel-type information (item 6). This is essentially what Climate TRACE does with their ML pipeline. Pursuing this would require a collaborator with atmospheric science expertise or substantial self-investment in the methodology.

## Key references to investigate

- **Climate TRACE**: climatetrace.org/data. Free facility-level emission estimates, CC 4.0. Belgium data in `DATA_DIR/raw/Climate TRACE/BEL/`.
- **LEGO-4-AQ**: 1 km × 1 km NO2 maps for Europe derived from Sentinel-5P TROPOMI, produced by BIRA-IASB. Need to verify temporal coverage and access.
- **E-PRTR / IED Industrial Reporting**: European Pollutant Release and Transfer Register. Provides installation-level geolocation and reported emissions for large polluters. Could serve dual purpose: geolocation source and partial validation.
- **Sentinel-5P TROPOMI**: ESA satellite operational since late 2017. Provides daily global coverage of NO2 tropospheric columns at ~5.5 km × 3.5 km native resolution; LEGO-4-AQ downscales this to 1 km.
- **Carbon Mapper**: carbonmapper.org. Detects methane and CO2 super-emitters at facility scale. Narrower scope than CT.
- **ESSD Global Power Plant CO2 Catalogue**: High-resolution emission profiles for 16,000+ power plants (2018). essd.copernicus.org/articles/16/337/2024/.

---

## Related paper

Fava (2025), "Training and Testing with Multiple Splits: A Central Limit Theorem for Split-Sample Estimators" (arXiv:2511.04957). Provides a CLT for split-sample estimators under weak conditions. Most relevant for formally testing whether the B2B proxy significantly improves predictions (model comparison). Reading notes in `articles/split_fava_2025_multiple_splits/notes.md`.
