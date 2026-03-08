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

## Scope 1 vs network-adjusted emissions: what the comparison reveals

### Core idea

Within a NACE 2-digit sector, firms can differ in how much of the production chain they perform in-house vs. purchase from suppliers. Scope 1 emissions only capture on-site combustion, so a firm that outsources energy-intensive steps appears "clean" even though the same emissions occur at its suppliers' facilities. Network-adjusted emissions — which incorporate upstream emissions flowing through B2B links — can partially correct for this by attributing supplier-side combustion back to the purchasing firm.

Comparing within-sector rankings under scope 1 vs. network-adjusted emissions reveals the extent to which the **organizational boundary** (vertical integration vs. outsourcing) distorts emission rankings. If rank correlation is well below 1 in a sector, it means that make-or-buy decisions — not underlying production technology — are driving scope 1 heterogeneity.

### When it matters and when it doesn't

The comparison is **uninformative** in sectors where the dirty node is internal to the sector regardless of firm structure. Example: cement vs. ready-mix concrete (both NACE 23). Cement producers operate kilns (high scope 1); concrete producers buy cement (low scope 1). But the concrete producers' network-adjusted emissions trace back to the same cement kilns — so both scope 1 and network-adjusted measures identify the cement producers as the high emitters. Network adjustment doesn't change the ranking of who matters.

The comparison is **informative** in sectors where the dirty node is **outside** the sector — i.e., firms purchase carbon-intensive inputs from suppliers in other NACE sectors. In these sectors, scope 1 is uniformly low but firms vary in how much they buy from high-emission upstream suppliers. Network-adjusted emissions reveal within-sector heterogeneity that scope 1 cannot.

### Practical examples

- **Plastics manufacturing (NACE 22):** Own processes (molding, extrusion) are low-temperature and low-emission. The key input is polymer resin from petrochemical producers (NACE 20), where steam cracking of naphtha is extremely energy-intensive. Within NACE 22, firms vary in how much virgin polymer they purchase from Antwerp crackers vs. using recycled plastics. Scope 1 looks similar; network-adjusted emissions diverge.

- **Fabricated metals (NACE 25):** Firms buy steel and aluminum from primary metals producers (NACE 24) and perform cutting, welding, coating — modest energy use. Variation in how much primary metal a firm purchases from blast-furnace steelmakers shows up only in network-adjusted emissions.

- **Paper products (NACE 17):** Non-integrated firms buy pulp from energy-intensive pulp mills. Their scope 1 is low but their upstream exposure is high. Integrated firms (with on-site pulping) have high scope 1 but similar network-adjusted emissions.

### Policy relevance

Scope 1 accounting creates a perverse incentive: firms can reduce reported emissions by outsourcing dirty production steps without reducing actual emissions in the economy. If within-sector rank correlation between scope 1 and network-adjusted emissions is substantially below 1, that is direct evidence of this distortion. The B2B data is uniquely suited to detect it, since it traces purchases to specific suppliers whose emission intensity can be estimated.

---

## Other empirical analyses enabled by inferred emissions + B2B data

### 1. Within-sector heterogeneity in upstream emission exposure

Traditional IO analysis assigns every firm in a sector the same emission intensity. With firm-level B2B data + inferred emissions, we can measure how much firms *within the same sector and size class* vary in their upstream emission exposure. If the dispersion is large, it means sector-level emission factors — which is what most carbon accounting relies on — are a poor approximation of actual firm-level exposure. This is a direct empirical test of whether firm-level data matters over sector-level averages.

### 2. Concentration of emission exposure across suppliers

For each firm, compute a Herfindahl-type index over its suppliers weighted by supplier emission intensity. Do firms spread their sourcing across many low-emission suppliers, or concentrate purchases with a few high-emission ones? This has direct implications for transition risk — firms with concentrated dirty-supplier exposure are more vulnerable to carbon pricing pass-through. The B2B data provides the transaction-level resolution needed to compute this; aggregate IO tables cannot.

### 3. Emission propagation depth

With multi-hop B2B links (supplier's suppliers), ask: how many steps upstream does most of the embodied emission come from? If 90% is captured at depth 1 (direct suppliers), then scope 2/3 approximations based on direct suppliers are adequate. If significant emission mass appears at depth 2 or 3, it means simple upstream accounting misses a lot. This is an empirical question that hasn't been answered at the firm level with actual transaction data — existing estimates rely on sector-level IO tables, which smooth over firm heterogeneity.

---

## Related paper

Fava (2025), "Training and Testing with Multiple Splits: A Central Limit Theorem for Split-Sample Estimators" (arXiv:2511.04957). Provides a CLT for split-sample estimators under weak conditions. Most relevant for formally testing whether the B2B proxy significantly improves predictions (model comparison). Reading notes in `articles/split_fava_2025_multiple_splits/notes.md`.

---

## Positioning relative to satellite-based emissions monitoring (2026-03-07)

### The state of the art for independent installation-level emissions

Based on reading all 10 Climate TRACE sector methodology documents:

**For methane:** Satellite-based direct detection is approaching true installation-level capability. GHGSat (~25m resolution) can detect and quantify CH4 plumes from individual facilities. MethaneSAT (~100–400m, launched March 2024) covers entire basins. TROPOMI/Sentinel-5P (~5.5 km) catches only super-emitters. The combination of these three at different scales is close to a comprehensive CH4 monitoring system for large point sources.

**For CO2:** There is no reliable independent installation-level satellite measurement yet. The atmospheric CO2 background is ~420 ppm, making it much harder to attribute marginal enhancements to individual facilities (unlike CH4, which has a lower background and stronger spectral signature). What exists:
- OCO-2/OCO-3 (NASA): detects CO2 enhancements over very large sources (>10 MtCO2/yr) but with narrow swath and infrequent revisit.
- CO2M/Copernicus (ESA, expected ~2026–2027): first planned operational satellite for installation-level CO2 plume attribution (~4 Mt/yr threshold). Not launched yet.
- Climate TRACE's proxy approach: satellite-derived *activity* signals (thermal anomaly for steel/cement, water vapor plumes for power plants, VIIRS for flaring) combined with emission factors. Estimates production, not emissions directly — but is the best operational system at scale.

For sectors without satellite signal (chemicals, pulp & paper, aluminum, petrochemicals, heat plants, coal mining), CT uses capacity × utilization × emission factor — the same approach national inventories have used for decades, applied more systematically to global asset databases.

### Where our B2B approach fits: the long tail below satellite detection thresholds

The quantitative question is whether emissions from installations below satellite detection thresholds are "small enough to ignore." The answer depends on the policy question:

**If the goal is accounting for the bulk of national/global emissions:** Yes, satellites + administrative registries (EUTL, EPA GHGRP) cover most of the total tonnage. Emissions are extremely concentrated — the EU ETS covers ~40% of EU emissions from ~10,000 installations. The "long tail" of smaller emitters is modest in aggregate.

**But for three policy-relevant applications, the long tail matters:**

1. **Carbon taxation extending below ETS thresholds.** EU policy is moving in this direction: CBAM and ETS2 (covering buildings and transport, starting 2027) signal an expansion of carbon pricing to previously uncovered sectors. Firm-level emission estimates become necessary for any extension to smaller industrial emitters.

2. **Supply chain due diligence (Scope 3).** Under CSRD (Corporate Sustainability Reporting Directive), firms must estimate upstream/downstream emissions from *all* suppliers — not just the large ones that satellites or registries cover. A bank or manufacturer needs firm-level carbon exposure for its entire supplier base. Sector-level averages are the current default, but firm-level estimates from B2B data are strictly more informative.

3. **Within-sector ranking, even when sector totals are small.** A firm in NACE 23 (non-metallic minerals) emitting 500 tCO2/yr is invisible to any satellite but may be in the top decile of its sector among non-ETS firms. Knowing that ranking has allocative value for targeted policy (audits, subsidies, efficiency programs).

### Framing for the paper

Satellites and registries solve the *large point source* problem well — and will solve it even better once CO2M launches. Our contribution operates at a fundamentally different margin:
- The **extensive margin**: which non-ETS firms emit at all (classification).
- **Within-sector ranking** among the long tail of smaller emitters that no current or planned remote sensing technology can individually resolve.
- **Universal firm coverage**: B2B data covers all private firms in Belgium, not just the ~10,000 with EUTL installations or the ~128 that CT tracks.

The aggregate tonnage we predict for the non-ETS sector may be modest relative to national totals. But the *firm-level information content* — who emits, how they rank, how their emission exposure propagates through the supply chain — is new and cannot be obtained from satellite data at any resolution.
