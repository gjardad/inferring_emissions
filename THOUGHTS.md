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

## Key references to investigate

- **LEGO-4-AQ**: 1 km × 1 km NO2 maps for Europe derived from Sentinel-5P TROPOMI, produced by BIRA-IASB. Need to verify temporal coverage and access.
- **E-PRTR / IED Industrial Reporting**: European Pollutant Release and Transfer Register. Provides installation-level geolocation and reported emissions for large polluters. Could serve dual purpose: geolocation source and partial validation.
- **Sentinel-5P TROPOMI**: ESA satellite operational since late 2017. Provides daily global coverage of NO2 tropospheric columns at ~5.5 km × 3.5 km native resolution; LEGO-4-AQ downscales this to 1 km.

---

## Related paper

Fava (2025), "Training and Testing with Multiple Splits: A Central Limit Theorem for Split-Sample Estimators" (arXiv:2511.04957). Provides a CLT for split-sample estimators under weak conditions. Most relevant for formally testing whether the B2B proxy significantly improves predictions (model comparison). Reading notes in `articles/split_fava_2025_multiple_splits/notes.md`.
