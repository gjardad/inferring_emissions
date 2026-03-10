# Literature Review: Inferring Firm-Level Emissions

This document organizes the relevant literature into three strands and clarifies
how our paper relates to each.

---

## Strand 1: Applied Micro with Direct Emission Data

**What they do.** These papers have direct access to firm- or plant-level emission
data — either from mandatory reporting (EU ETS verified emissions, US GHGRP/CEMS)
or from detailed energy surveys where researchers apply emission factors to observed
fuel consumption. The emission data is an *input* to their analysis (estimating
causal effects of regulation, decomposing pollution trends, etc.), not the main
contribution.

**Why it matters for us.** Strand 1 establishes the economic primitive our proxy
exploits: emissions from fossil fuel combustion are a direct function of fuel
inputs. This is the intellectual justification for why B2B purchases from fuel
suppliers should predict emissions. Strand 1 also sets the "gold standard" for
firm-level emission data — but this standard is only achievable where detailed
fuel consumption micro-data or mandatory emission reporting exists, and typically
only for a subset of firms (large, regulated, surveyed).

**Relationship to our paper.** We cite strand 1 to:
1. Acknowledge the gold standard and its limitations in coverage.
2. Motivate *why* our fuel-supply proxy should be informative — it is an indirect
   measure of exactly what these papers observe directly.

### Key references

- **Shapiro & Walker (2018)** "Why is Pollution from US Manufacturing Declining?"
  *AER* 108(12), 3814-54.
  - Data: National Emissions Inventory (NEI) for US manufacturing plants.
  - Decomposes the 60% decline in manufacturing pollution 1990-2008 into
    regulation, productivity, and trade channels.

- **Colmer, Martin, Muuls & Wagner (2025)** "Does Pricing Carbon Mitigate Climate
  Change? Firm-Level Evidence from the European Union Emissions Trading System."
  *Review of Economic Studies* 92(3), 1625-1660.
  - Data: EU ETS verified emissions + Belgian administrative data (NBB).
  - Finds EU ETS reduced regulated manufacturing emissions by 14-16% with no
    detectable contraction in economic activity.
  - Especially relevant: Muuls is at the NBB; uses the same Belgian institutional
    setting.

- **Lyubich, Shapiro & Walker (2018)** "Regulating Mismeasured Pollution:
  Implications of Firm Heterogeneity for Environmental Policy."
  *AEA Papers and Proceedings* 108, 136-42.
  - Data: Census of Manufactures + MECS (compute CO2 from energy inputs).
  - Documents enormous within-industry heterogeneity in CO2 productivity.

- **Marin & Vona (2021)** "The Impact of Energy Prices on Socioeconomic and
  Environmental Performance: Evidence from French Manufacturing Establishments,
  1997-2015." *European Economic Review* 135.
  - Data: French manufacturing energy use data (compute emissions from fuel
    consumption).
  - Finds 10% energy price increase reduces CO2 intensity by ~6%.

- **Curtis (2018)** "Who Loses Under Power Plant Cap-and-Trade Programs?" *JPE*.
  - Data: CEMS continuous emission monitoring.

- **Fowlie, Reguant & Ryan (2016)** "Market-Based Emissions Regulation and
  Industry Dynamics." *JPE* 124(1).
  - Data: US cement plant emissions.

- **Dechezlepretre, Nachtigall & Venmans (2023)** "The Joint Impact of the EU
  ETS on Carbon Emissions and Economic Performance." *JEEA*.
  - Data: EU ETS verified emissions.

- **Martin, Muuls & Wagner (2016)** "The Impact of the EU ETS on Regulated Firms:
  What Is the Evidence after Ten Years?" *REEP* 10(1), 129-148.
  - Review of EU ETS evidence. Same author group as Colmer et al.

---

## Strand 2: Emission Imputation from Financial Data

**What they do.** These papers predict or impute firm-level emissions for firms
that do not report, using financial statement variables (revenue, assets,
employees, sector codes). This includes both academic ML exercises and commercial
provider methodologies (Bloomberg, Trucost/S&P, MSCI, ISS).

**Why it matters for us.** Strand 2 defines the problem we address and provides
the motivation. Its main limitation — forcefully demonstrated by Aswani et al.
(2024) — is that financial variables end up largely proxying for firm size rather
than capturing actual emission variation. Our paper introduces a fundamentally
different predictor (B2B fuel purchases) that reflects actual purchasing behavior.

**Relationship to our paper.** We cite strand 2 to:
1. Motivate the prediction problem (many firms don't report emissions).
2. Highlight the limitations of using only financial variables.
3. Position our B2B-based approach as offering a qualitatively different signal.

Our calibrated PPML benchmark (revenue + sector RE, calibrated to sector-year
totals) approximates the commercial provider approach, so we do not need a
separate "commercial provider" specification.

### Key references — Academic

- **Chen, Lioui & Scaillet (2025)** "Green Silence: Double Machine Learning
  Carbon Emissions Under Sample Selection Bias." Swiss Finance Institute Research
  Paper No. 25-66.
  - Method: Heckman + kernel group lasso + DML to correct for strategic
    non-disclosure.
  - Key finding: Voluntary disclosure creates selection bias; standard ML
    imputation systematically understates high emitters.
  - Relevant to us: our training sample (EU ETS firms) is also a selected
    sample, though selection is regulatory (capacity threshold), not strategic.

- **Nguyen, Diaz-Rainey & Kuruppuarachchi (2021)** "Predicting Corporate Carbon
  Footprints for Climate Finance Risk Analyses: A Machine Learning Approach."
  *Energy Economics* 95.
  - Method: Meta-Elastic Net combining 6 base learners.
  - Data: 13,435 observations from 2,289 global listed companies.
  - Result: Up to 30% MAE improvement over linear models.

- **Han, Gopal, Ouyang & Key (2021)** "Estimation of Corporate Greenhouse Gas
  Emissions via Machine Learning." arXiv:2109.04318 (ICML Workshop).
  - Affiliation: Bloomberg LP.
  - Method: Gradient boosted trees on ~800 financial/operational fields.
  - This is the academic description of the Bloomberg BESG model.

- **Assael, Heurtebize, Carlier & Soupe (2023)** "Greenhouse Gases Emissions:
  Estimating Corporate Non-Reported Emissions Using Interpretable Machine
  Learning." *Sustainability* 15(4), 3391.
  - Affiliation: BNP Paribas.
  - Method: ML + Shapley values for interpretability.

- **Serafeim & Velez Caicedo (2022)** "Machine Learning Models for Prediction of
  Scope 3 Carbon Emissions." Harvard Business School WP No. 22-080.
  - Method: AdaBoost for 15 Scope 3 categories.

- **Goldhammer, Busse & Busch (2017)** "Estimating Corporate Carbon Footprints
  with Externally Available Data." *J. Industrial Ecology* 21(5), 1165-1179.
  - Early paper in the strand; regression-based.

- **Barthe, Choquet & Jourde (2025)** "Estimating Corporate Carbon Emissions
  Using Artificial Intelligence." Banque de France Eco Notepad No. 421.
  - Method: Random forest. Out-of-sample correlation 0.78.

### Key references — Commercial providers

- **Trucost / S&P Global**: Environmentally Extended Input-Output (EEIO) model.
  Sector-level emission intensities (from ~500 NAICS industries) x firm revenue.
- **Bloomberg BESG**: Gradient boosted trees (see Han et al. 2021 above).
- **MSCI**: Tiered hierarchy (firm-specific intensity > industry ratio > industry
  average).

### Key references — Critical evaluations

- **Aswani, Raghunandan & Rajgopal (2024)** "Are Carbon Emissions Associated with
  Stock Returns?" *Review of Finance* 28(1), 75-106.
  - Key finding: Bolton & Kacperczyk's "carbon premium" is driven entirely by
    vendor-estimated (not disclosed) emissions. Estimated Scope 1 has 0.73
    correlation with sales — estimates largely capture firm size.
  - **This is the sharpest motivation for our approach.**

- **Busch, Johnson & Pioch (2022)** "Corporate Carbon Performance Data: Quo
  Vadis?" *J. Industrial Ecology* 26(1), 350-363.
  - Reported data more consistent across providers than estimated data.

- **Kalesnik, Wilkens & Zink (2022)** "Green Data or Greenwashing?" *Journal of
  Portfolio Management*.
  - Estimated emissions 2.4x less effective than reported data at identifying
    worst emitters.

---

## Strand 3: B2B Transaction Data for Emission Estimation

**What they do.** A recent line of work from the Complexity Science Hub Vienna
uses Hungarian VAT-based B2B transaction data — structurally identical to the
Belgian NBB B2B data — to estimate firm-level emissions or energy consumption
from fossil fuel purchases. Three papers from this group are relevant, in
chronological order of publication.

**Why it matters for us.** This is the closest methodological precedent. It
validates the core idea that B2B transaction data contains emission-relevant
information. At the same time, the methodological differences between their
approach and ours are substantial and constitute our value-added.

**Relationship to our paper.** We cite strand 3 to:
1. Establish precedent for using B2B/VAT data for emission estimation.
2. Discuss methodological differences in detail (see below).
3. Include a "Hungarian benchmark" specification that replicates their approach
   in our data for transparent comparison.

### Key references

#### Stangl, Borsos, Diem, Reisch & Thurner (2024)

"Firm-Level Supply Chains to Minimize Unemployment and Economic Losses in
Rapid Decarbonization Scenarios." *Nature Sustainability* 7, 581-589.

**Goal:** Design decarbonization strategies that minimize unemployment and
economic output losses by accounting for supply-chain contagion. Compares four
strategies for hypothetically shutting down ETS firms; the "smart strategy"
achieves 20% emission reduction with only 2% job and output losses.

**Emission estimation:** None for non-ETS firms. Uses only EU ETS verified
emissions for 119 Hungarian firms matched to the production network (122 found
in EUTL, 3 unmatched). The paper explicitly states that "the emissions of the
firms outside the Hungarian ETS were not known." This is precisely the gap that
Tabachova et al. (2025) and our paper aim to fill.

#### Tabachova, Diem, Stangl, Borsos & Thurner (2025)

"Combined Climate Stress Testing of Supply-Chain Networks and the Financial
System with Nation-Wide Firm-Level Emission Estimates." arXiv:2503.10644 /
INET Oxford WP 2025-04.

**Goal:** Climate stress testing of the Hungarian financial system under carbon
pricing (ETS I and upcoming ETS II), accounting for supply-chain contagion.

**Emission estimation: the closest paper to ours.** Estimates CO2 for all
410,523 Hungarian VAT-paying firms. The methodology has three steps:

*Step 1: Fuel supplier identification.* Pre-selected by NACE 4-digit code.
Six codes, split into gas and oil:
- Gas: D35.2.1 (Manufacture of gas, 5 firms), D35.2.2 (Distribution of
  gaseous fuels through mains, 11 firms), D35.2.3 (Trade of gas through mains,
  31 firms). Total: 47 gas distributors.
- Oil: C19.2.0 (Manufacture of refined petroleum products, 5 firms), G46.7.1
  (Wholesale of solid, liquid, gaseous fuels, 159 firms), G47.3.0 (Retail sale
  of automotive fuel, 565 firms). Total: 729 oil distributors.
- Total: 776 fuel suppliers. Selection is NOT data-driven.

*Step 2: Proportional allocation at national level.* For each firm i:

  E_i = (s_i^{in,gas} / s^{out,gas}) × E^{gas} + (s_i^{in,oil} / s^{out,oil}) × E^{oil}

Where s_i^{in,gas} = total purchases from gas suppliers, s^{out,gas} = total
output of gas sector (excluding intra-sector trade), and E^{gas} = 12.5 Mt,
E^{oil} = 13.7 Mt are national totals from the Global Carbon Project. Key
adjustments: subtract ~1/3 of gas for residential heating, subtract private car
oil (73.6% of oil consumption attributed to commercial sector), exclude financial
sector (NACE K). There is no regression model — the relationship between
purchases and emissions is assumed to be purely proportional.

*Step 3: Aggregation.* 185,783 firms (45%) receive positive emission estimates,
totaling 19 Mt. Remaining 55% buy neither oil nor gas → zero emissions.

**Validation:** 106 ETS firms compared (out of 119 total). Log-log correlation:
0.61. **Linear correlation: only 0.22.** Total estimated for these 106 firms:
6 Mt vs. 16 Mt reported — captures only **37.5%** of actual emissions. The gap
arises because estimates miss electricity-related emissions, chemical process
emissions, waste handling, and foreign fuel purchases. No cross-validation, no
ranking metrics, no FPR/TPR.

**Key assumptions and limitations** (from their S7):
1. Uniform price: all fuel purchases treated as homogeneous in price per unit.
   Small consumers pay more per unit → their emissions are overestimated.
2. Oil heterogeneity: gasoline, heating oil, and naphtha have different prices,
   but all oil-providing sectors are aggregated.
3. All out-links of identified fuel suppliers assumed to be fuel sales, but
   some may involve other products (no product-level information in B2B data).

#### Stangl, Borsos & Thurner (2026)

"Using Firm-Level Supply Chain Networks to Measure the Speed of the Energy
Transition." *Nature Communications* (Article in Press).

**Goal:** Measure the speed of the firm-level energy transition in Hungary using
semi-annual supply chain snapshots (2020-2024). Tracks how firms shift between
fossil and low-carbon energy sources.

**Energy estimation:** Estimates energy consumption in kWh (not CO2 emissions
directly) for 25,231 Hungarian firms. The methodology is more refined than
Tabachova et al. but addresses a different question:

*Step 1: Energy provider identification.* Pre-selected by NACE 4-digit code,
with an expanded set relative to Tabachova (now includes electricity):
- Electricity: D35.1 (generation, transmission, distribution), D35.1.1
  (production), D35.1.2 (transmission), D35.1.3 (distribution), D35.1.4 (trade).
- Gas: D35.2.1 (manufacture), D35.2.2 (distribution through mains), D35.2.3
  (trade through mains). Same as Tabachova.
- Oil: B6.1.0 (extraction of crude petroleum — new), C19.2.0 (refined
  petroleum), G47.3.0 (retail automotive fuel), G46.7.1 (wholesale fuels).
- Coal excluded (no distinct NACE code; mostly used for electricity production).
- Selection remains NOT data-driven.

*Step 2: Sample restrictions.* Much more restrictive than Tabachova:
- Require firms to purchase from BOTH gas AND electricity providers (because
  energy providers often supply both under one NACE code).
- Exclude firms classified under ANY energy-related NACE code (end-users only).
- Exclude financial sector (NACE K), H52.2.1 (land transport services), and
  EU ETS firms (201 firms).
- Final sample: 25,231 firms (vs. 410,523 in Tabachova).

*Step 3: Price-based conversion to kWh.* Instead of Tabachova's proportional
allocation of national emission totals, this paper converts monetary purchases
to kWh using EUROSTAT non-household energy prices by consumption band:
- Electricity: 7 consumption bands. Gas: 6 consumption bands.
- Assign each firm to a band based on its semi-annual expenditure level.
- Oil: weighted average of diesel (74%) and gasoline (26%) prices from EU
  Weekly Oil Bulletin. Weights from Hungary's National Detailed Energy Balance.
- Semi-annual estimates aggregated to annual.

*Step 4: Low-carbon share.* Compute L_i(t) = E_i(t) × u(t), where u(t) is
Hungary's low-carbon share of electricity from Ember. Then l_i(t) = L_i(t) /
T_i(t) measures each firm's low-carbon energy share.

**Coverage:** 25% of total final energy consumption in Hungary (2023): 16.6% of
oil, 40.0% of gas, 17.1% of electricity.

**Key limitations:**
1. Consumption band assignment is approximate — within-band price heterogeneity
   from load profiles, contractual arrangements, self-generation.
2. Oil product heterogeneity — chemical industry (naphtha) underestimated.
3. Missing behind-the-meter PV (estimated 2.3-8.8% of commercial electricity).
4. Pre-selected NACE codes — same fundamental limitation as Tabachova.

### Timeline of the Hungarian group's work

| Year | Paper | Emission/energy estimation? | Coverage |
|------|-------|---------------------------|----------|
| 2024 | Stangl et al. (*Nat Sust*) | No — ETS verified only | 119 firms |
| 2025 | Tabachova et al. (arXiv) | Yes — proportional CO2 allocation | 410,523 firms |
| 2026 | Stangl et al. (*Nat Comms*) | Yes — price-based kWh conversion | 25,231 firms |

Note: Tabachova et al. estimates CO2 emissions (our direct comparator); Stangl
(2026) estimates energy consumption in kWh (different target variable). For our
Hungarian benchmark, Tabachova's proportional allocation method is the relevant
comparison.

### Methodological differences: our paper vs. strand 3

| Dimension | Hungarian approach | Our approach |
|-----------|-------------------|--------------|
| Fuel supplier ID | Pre-selected NACE codes (6 in Tabachova, ~13 in Stangl 2026) | Data-driven (elastic net on B2B sales to known fuel suppliers) |
| Prediction model | None — proportional allocation (Tabachova) or price conversion (Stangl 2026) | Trained hurdle + PPML with learned nonlinearities and interactions |
| Extensive margin | All fuel buyers get positive emissions (Tabachova); require both gas+electricity purchases (Stangl 2026) | Hurdle model explicitly classifies emitters vs. non-emitters |
| Calibration | National totals, single step (Tabachova); no calibration (Stangl 2026) | Sector-year totals, iterative proportional fitting with cap |
| Validation | Single log-log correlation of 0.61, linear correlation 0.22 (Tabachova); no emission validation (Stangl 2026) | Full CV battery: nRMSE, APD, Spearman rho, within-sector rho, FPR/TPR |
| Out-of-sample design | None (contemporaneous allocation) | LOFOCV + LOSOCV (genuine out-of-sample, including unseen sectors) |
| Coverage | All VAT-paying firms (Tabachova) or subset of 25K firms (Stangl 2026) | All non-ETS firms across all sectors |

### Related: Belgian B2B data and sample selection references

- **Dhyne, Magerman & Rubinova (2015)** NBB WP No. 288. Introduces the Belgian
  B2B dataset.
- **Duprez, Dhyne & Komatsu (2023)** NBB WP No. 444. Updated dataset 2002-2021.
- **Dhyne, Kikkawa, Mogstad & Tintelnot (2020)** "Trade and Domestic Production
  Networks." *Review of Economic Studies* 88(2), 643-668.
  - Uses the Belgian B2B + Annual Accounts data. Establishes the standard sample
    selection criteria we follow: private non-financial sector, positive labor
    costs, ≥1 FTE, positive output. Removes self-employed and foreign firms
    without local economic activity.
- **Dhyne, Kikkawa, Mogstad & Tintelnot (2023)** "Measuring the Share of Imports
  in Final Consumption." *AEA Papers and Proceedings* 113, 81-86.
  - Documents a revision of the B2B dataset. Our sample numbers slightly differ
    from Dhyne et al. (2020) due to this revision.
- **De Loecker, Fuss & Van Biesebroeck (2014)** "International Competition and
  Firm Performance: Evidence from Belgium." NBB WP No. 269.
  - Uses Annual Accounts with the same sample selection criteria as Dhyne et al.
    (2020). We cite both as precedent for our sample restrictions.
- **Bernard, Dhyne, Magerman, Manova & Moxnes (2022)** "The Origins of Firm
  Heterogeneity: A Production Network Approach." *JPE* 130(7), 1765-1804.
- **Pichler, Diem, ..., Magerman, ..., Thurner (2023)** "Building an Alliance to
  Map Global Supply Networks." *Science* 382(6668), 270-272. Links the Belgian
  and Hungarian B2B data communities.

---

## Paper Framing: B2B Data as a Growing Resource

### The expansion of e-invoicing

The data infrastructure underlying our approach — firm-to-firm transaction records
collected by tax authorities — is expanding rapidly worldwide. Mandatory electronic
invoicing (e-invoicing) systems generate exactly the kind of B2B transaction data
we use, as a byproduct of tax compliance.

**Latin America** has led the way. Brazil mandated e-invoicing in 2008 (Nota Fiscal
Eletrônica), Mexico introduced CFDI in 2011, and Chile has required e-invoicing for
all businesses since 2018. Argentina, Colombia, Ecuador, Peru, and Uruguay all have
operational systems.

**Asia** followed: India introduced mandatory e-invoicing under GST in 2020 (with
progressively lower turnover thresholds), South Korea has had mandatory e-invoicing
for large firms since 2011, and Vietnam mandated it for all transactions in 2022.
Malaysia and Pakistan have recent mandates as well.

**Europe** is now catching up. The EU's VAT in the Digital Age (ViDA) package,
adopted in March 2025, will make structured e-invoicing mandatory for
intra-Community B2B transactions by July 2030 and removes the need for prior EU
authorization for domestic B2B e-invoicing mandates. Several member states are
moving ahead of the EU-wide deadline: Italy has had mandatory B2B e-invoicing since
2019, Belgium will require all VAT-registered businesses to use the Peppol network
for domestic B2B invoices, Poland's KSeF platform begins mandatory operation in
2026, and Croatia, Greece, France, Germany, and Spain have all set 2026-2028
deadlines. Over 90 countries now have e-invoicing mandates in place or in
implementation.

### The landscape of firm-level energy consumption surveys

The gold standard for estimating firm-level emissions — observing fuel consumption
by type at the establishment level — requires dedicated energy surveys. A small
number of countries have maintained such surveys for decades:

- **United States**: Manufacturing Energy Consumption Survey (MECS), conducted
  quadrennially since 1985 by the EIA. Covers ~15,000 establishments, representing
  97-98% of manufacturing payroll. Reports consumption by fuel type.
- **France**: Enquête Annuelle sur la Consommation d'Énergie dans l'Industrie
  (EACEI), annual. Reports quantities consumed by energy type, costs, and breakdown
  by use for manufacturing establishments.
- **Germany**: Erhebung über die Energieverwendung, annual, covering ~47,000
  establishments in manufacturing and mining with at least 20 employees.
- **Canada**: Industrial Consumption of Energy Survey (ICE), annual, sampling
  ~5,000 manufacturing establishments. Reports consumption by fuel type.
- **Japan**: Survey on Energy Consumption Structure (since 1980), covering ~1,600
  establishments in energy-intensive industries.
- **China**: NBS industrial survey covering energy consumption for large and medium
  enterprises (~3,000 firms/year), complemented by the Top-1,000 Energy-Consuming
  Enterprises program.

At the EU level, Regulation (EC) 1099/2008 (amended in 2019) made detailed
industry energy consumption reporting mandatory from reference year 2020, with
harmonized methodology across member states. However, this reporting is at the
aggregate sector level — the underlying establishment-level microdata, where it
exists, remains with national statistical offices and is not uniformly accessible
to researchers.

These surveys share important limitations. They typically cover only manufacturing
and mining (not services, transport, or agriculture). They impose size thresholds
that exclude small establishments. And they require substantial statistical
infrastructure to design, administer, and maintain — which is why the set of
countries with such surveys has remained small and largely stable over time.

### What this means for emission estimation

The practical landscape is therefore one where a growing number of countries hold
comprehensive B2B transaction records (through e-invoicing), while only a handful
maintain the firm-level energy surveys that would allow direct emission estimation.
B2B data and energy surveys are not substitutes — the latter provides direct
observation of fuel quantities by type, which no amount of transaction data can
replicate. But in the many settings where energy surveys do not exist or do not
cover the full firm population, the question we address becomes practically
relevant: how much can be learned about firm-level emissions from transaction
records that governments already collect?

### Our contribution

We document both the gains and the limitations of using B2B transaction data for
emission inference. The fuel-supply proxy substantially improves prediction accuracy
over financial-variable-only approaches, particularly for within-sector firm
ranking and extensive-margin classification. At the same time, the approach has
clear shortcomings — including sectors where within-sector ranking breaks down —
that energy surveys with direct fuel consumption data would not face. We do not
claim B2B data substitutes for dedicated energy surveys; rather, we show what can
and what cannot be extracted from transaction networks in settings where such
surveys do not exist.

---

## Climate TRACE Methodology

Climate TRACE is a global coalition that provides independent, facility-level
greenhouse gas emissions estimates using satellite data and other remote sensing
inputs. All data is freely available (CC-BY 4.0) at climatetrace.org. This section
documents their methodology by sector, based on their published methodology papers.

The emissions described here are bottom-up engineering estimates — they do not use
econometric models, financial data, or transaction data. The general approach is:
identify facilities from asset databases, estimate production activity (often via
satellite signals), and apply route- or technology-specific emissions factors to
convert production into CO2 estimates.

**Relevance to our project:** We use Climate TRACE facility-level emissions as an
alternative training signal in an exploratory exercise (step C in the pipeline):
train an elastic net on CT emissions for facilities where CT provides estimates,
then predict emissions for non-CT firms. The methodology documentation helps us
understand the measurement error in these labels — CT estimates are noisy, and the
noise structure varies by sector and production technology.

### Iron & Steel Manufacturing

**Source:** Crane, V. and Ebri, G. (2025). *Manufacturing and Industrial Processes
sector - Iron & Steel Manufacturing Emissions*. TransitionZero, UK, Climate TRACE
Emissions Inventory. Version November 2025.

**Scope:** Monthly CO2 emissions for 890 operating steel plants in 81 countries,
from January 2021 onward. Covers direct emissions only (CO2); CH4 and N2O are not
modeled.

**Method — two-track production estimation:**

1. *Satellite-monitored (BF/BOF plants).* Blast furnace / basic oxygen furnace
   plants operate at >1,200°C. Satellites (Sentinel-2 at 10-60m resolution,
   Landsat-8/9 at 30m) capture thermal infrared imagery of known plant locations.
   A Thermal Anomaly Index (TAI) — the ratio of the difference between two SWIR
   bands to a NIR band — identifies "hotspots" (pixels dramatically hotter than
   surroundings) corresponding to active blast furnaces, coke ovens, sinter plants,
   and BOFs. Hotspot intensity is calibrated against monthly country-level production
   data (from the World Steel Association) to derive capacity factors, which are
   multiplied by plant capacity to estimate monthly production. The TAI formulas:
   - Sentinel-2A/B: TAI = (B12 − B11) / B8a
   - Landsat-8/9: TAI = (B7 − B6) / B5

   The two satellite collections are harmonized via NASA band-pass adjustments.
   Partial images (<80% coverage) and cloudy images (>20% clouds) are excluded.
   All processing is done in Google Earth Engine.

2. *Capacity-based (DRI-EAF and EAF plants).* Electric arc furnaces do not
   produce thermal signatures detectable from space. For these plants, production
   is estimated by applying a regional average utilization rate (from historical
   national production and aggregated capacity) to each plant's known capacity.

**Emissions factors (Table 1 from the paper):**

| Route   | Direct EF (tCO2/t-steel) | Electricity Use (MWh/t-steel) | Process EF (tCO2/t-steel) |
|---------|--------------------------|-------------------------------|---------------------------|
| BF/BOF  | 1.9                      | 0.83                          | 1.71                      |
| DRI-EAF | 1.0                      | 0.87                          | 0.62                      |
| EAF     | 0.08                     | 0.87                          | 0.08                      |

For EAF plants, the effective emission factor is a weighted average over feedstock
composition (scrap, DRI, pig iron), using GEM data on each facility's feedstock
mix. Indirect emissions use regional grid intensity from Ember. Multi-route plants
use capacity-weighted emission factors.

**Coverage:** Asset-level estimates account for 83% (3.0 Gt-CO2) of total sector
emissions (2024 data). Satellite-monitored facilities represent ~41% of sources but
~87% of total asset-level emissions.

**Data inputs:**
- Steel facility inventory: Global Iron and Steel Tracker (GIST) from Global
  Energy Monitor — 1,205 facilities, 3.5 billion tonnes capacity, 89 countries.
  334 BF/BOF plants have hand-drawn polygons for sub-asset identification.
- Satellite imagery: Sentinel-2A/B (since 2015/2017), Landsat-8/9 (since
  2013/2021), processed via Google Earth Engine.
- National production: World Steel Association (monthly).
- Emissions factors: IPCC (2006), WSA (2024b), IEA (2020), MPP (2024).
- Grid emissions intensity: Ember (2025).

**Reported uncertainty:**
- Production estimates: ±8–15%
- CO2 emissions factor: ±10–12% (based on IPCC)
- CO2 emissions: ±13–20%
- Total CO2e (100yr GWP): ±20%

**Limitations relevant to our project:**
1. Satellite monitoring only works for BF/BOF plants; EAF estimates are much
   cruder (national-average utilization rates applied uniformly).
2. Temporal coverage starts January 2021 — only 2021-2022 overlap with our
   training sample.
3. Uncertainty of ±13-20% means CT emissions are noisy labels for our EN
   exercise.
4. The methodology requires knowing plant locations from a pre-existing inventory
   — it does not discover emitters.
5. Belgium is not among the top steel-producing countries, but ArcelorMittal Gent
   (BF/BOF) is the major Belgian installation and should be covered.

### Cement Manufacturing

**Source:** Crane, V. and Ebri, G. (2025). *Manufacturing and Industrial Processes
sector – Cement Manufacturing Emissions*. TransitionZero, UK, Climate TRACE
Emissions Inventory. Version November 2025.

**Scope:** Monthly CO2 emissions for clinker-producing cement plants globally, from
January 2021 onward. Covers direct emissions only (CO2); CH4 and N2O not modeled.
2,241 integrated cement plants covering 138 countries. Grinding-only facilities
(~25% of global cement capacity) are excluded — their emissions are attributed to
the clinker-producing plant.

**Key difference from steel:** The majority of cement emissions are *process-related*
(limestone calcination: CaCO3 → CaO + CO2), not from fuel combustion. Calcination
accounts for ~0.42 tCO2/t-cement, while fuel combustion adds ~0.25 tCO2/t-cement.
This means our B2B fuel-supply proxy would only capture the fuel combustion
component (~40% of total cement emissions), missing the dominant process component.

**Method — same two-track structure as steel:**

1. *Satellite-monitored.* Clinkerization occurs at ~1,400°C, producing detectable
   thermal signatures. Same TAI methodology as steel (Sentinel-2 + Landsat-8/9),
   with hand-drawn polygons around rotary kilns. Hotspot intensity calibrated
   against annual country-level production data (from USGS). As of December 2024,
   85% of total direct emissions derived from satellite data. Satellite-monitored
   assets represent 93% of integrated plants and 93% of asset-level emissions.

2. *Capacity-based.* Plants without usable satellite signal (small kilns, indoor
   kilns, low signal-to-noise) use regional/global average utilization rates.

**Emissions factors — asset-specific, based on multiple characteristics:**
- Process emissions: global clinker EF of 0.507 tCO2/t-clinker (IPCC), adjusted
  by clinker-to-cement ratio which varies by cement type (95% for Portland, 71%
  for blended, 50% for blast-furnace slag cement) and by region (China reduced
  by up to 15%).
- Fuel emissions: vary by production route — dry (0.32 tCO2/t-clinker), semi-dry
  (0.39), wet (0.59). White cement requires 70% more energy than grey.
- Adjustments for: alternative fuels (35% fuel emissions reduction), CCS (70%
  capture rate on process emissions), clinker substitution with GGBS or LC3.

**Coverage:** Asset-level estimates account for 68% (1.51 Gt) of the sector's direct
emissions (2023 data).

**Reported uncertainty:**
- Production estimates: ±8–15%
- CO2 emissions factor: ±10% (based on IPCC)
- CO2 emissions: ±13–20%

**Limitations relevant to our project:**
1. Cement emissions are dominated by process emissions from calcination (~60%),
   which our fuel-supply proxy cannot capture. Only the fuel combustion component
   (~40%) relates to fossil fuel purchases.
2. Same temporal limitation: January 2021 onward.
3. Belgium has several cement plants (Holcim, CBR/Heidelberg Materials) in the
   EUTL — these should be covered by CT.

### Chemicals and Pulp & Paper

**Source:** Crane, V., Ebri, G., Galib, K.M., and Underwood, C. (2025).
*Manufacturing and Industrial Processes sector – Chemicals and Pulp (Paper)
Emissions*. TransitionZero, UK, Climate TRACE Emissions Inventory. Version
November 2025.

**Scope:** Annual CO2 emissions for four sub-sectors: ammonia (258 sources in 41
countries), methanol (140 sources in 31 countries), soda ash (61 sources in 34
countries), and pulp & paper (354 assets in 49 countries). Combined, these
produce ~2.5% of global CO2: ammonia ~350 Mt, methanol ~138 Mt, pulp & paper
~130 Mt, soda ash ~26 Mt.

**Key difference from steel and cement: NO satellite monitoring.** There is no
thermal anomaly detection for these sectors. Production is estimated entirely
through capacity-based disaggregation of national production totals.

**Method — capacity-based only:**

1. *Asset inventory.* No comprehensive global database existed, so CT developed a
   large language model to collect asset metadata from online sources, supplemented
   by SFI's Global Pulp and Paper Mill Database and Industrial Info Resources for
   residual capacities.
2. *Production estimation.* For each facility, compute its share of national
   capacity, then multiply by national production (from FAO for pulp/paper, USGS
   for ammonia/soda ash; methanol estimated from literature). Where asset
   capacities are missing, various fallback methods are used (equal distribution,
   global average utilization rates).
3. *Emissions = production × emissions factor.* Factors sourced per sub-sector:
   ammonia from Climatiq (country-specific, Tier 2), methanol from Methanol
   Institute, soda ash from US EPA, pulp & paper from Tomberlin et al. (2020).

**Uncertainty — substantially higher than steel/cement:**
- Activity uncertainty: 0.12–0.30 depending on capacity data source (LLM-derived
  capacities have uncertainty of 0.20–0.30, i.e., "very low confidence").
- Emissions factor uncertainty: 0.15 (Tier 1 global factor) to 0.10–0.12 (Tier 2
  country-specific, ammonia only).
- Combined CO2 uncertainty: propagated as σ = √(σ_activity² + σ_EF²).
- Confidence levels: >0.25 = very low, 0.15–0.25 = low, <0.15 = medium.

**Limitations relevant to our project:**
1. No satellite signal — all estimates are proportional allocation of national
   totals based on capacity shares. This means CT cannot capture within-country
   variation in utilization rates across plants.
2. Asset inventory built partly by LLM web scraping — coverage and accuracy
   of plant capacities is uncertain, especially for chemicals.
3. Belgium hosts a major chemical cluster in Antwerp (BASF, INEOS, Solvay)
   with substantial EUTL-regulated emissions. CT's capacity-based approach
   for chemicals is much cruder than the satellite-based methods for steel
   and cement — these Belgian chemical plants' CT estimates should be treated
   with caution.
4. Soda ash is particularly relevant: Solvay (headquartered in Belgium) is a
   major global producer and has developed the "e.Solvay" low-emission process.

### Aluminum Production

**Source:** Crane, V., Ebri, G., Underwood, C., and Heal, J. (2025).
*Manufacturing and Industrial Processes sector – Aluminum Production Emissions*.
TransitionZero, UK, Climate TRACE Emissions Inventory. Version November 2025.

**Scope:** Monthly CO2 emissions for primary aluminum production (smelting and
alumina refining), from January 2021 onward. 242 assets across 46 countries.
Secondary production (recycling) excluded — accounts for <2% of sector emissions.
Also produces PFC emissions (CF4, C2F6) but these are not the focus.

**Key difference from steel and cement: NO satellite monitoring.** Like chemicals,
production is estimated purely through capacity-based disaggregation of regional
production data (from the International Aluminium Institute). Smelter asset
database from Light Metal Age; alumina refinery data partly from LLM web scraping
with residual capacities from Industrial Info Resources.

**Method — capacity-based only:**
- Regional (not national) production data from IAI, disaggregated to facilities
  by their share of regional capacity: P_A = (C_A / (C_A + C_B)) × P_regional.

**Emissions components:**
- *Electrolysis process emissions:* 2.025 tCO2/t-Al (global factor from Ecofys).
- *Anode production:* 0.447 tCO2/t-Al (pre-bake process; same factor applied to
  Søderberg plants, which are ~4% of global production).
- *Alumina refining (Bayer process):* Fossil fuel combustion for heat. Regional
  energy intensity and fuel mix from IAI, fuel-specific EFs from EIA.
- *Indirect emissions (electricity for smelting):* By far the largest component.
  Regional electricity use from IAI × local grid intensity from Ember. Share of
  indirect emissions: ~80% in India, ~76% in China, ~53% in Canada (depends on
  grid carbon intensity — hydropower-heavy regions like Canada/Norway much lower).

**Coverage:** Asset-level estimates account for ~63.8% of the sector's direct
emissions (~400 MtCO2, 2024). Shortfall mainly from low refinery coverage (~50%).

**Limitations relevant to our project:**
1. No satellite signal — purely capacity-based. Assumes all assets in a region
   operate at the same utilization rate.
2. Aluminum is dominated by indirect (electricity) emissions, not fuel combustion.
   Our B2B fuel-supply proxy would miss the dominant emission source.
3. Belgium is not a significant primary aluminum producer (no smelters in the
   top-10 countries), though some alumina processing may exist.

### Petrochemical (Ethylene Steam Cracker)

**Citation:** RMI (2026). *Manufacturing and Industrial Processes sector — Petrochemical
Ethylene Steam Cracker Emissions*. Climate TRACE Emissions Inventory. Version February 2026.

**Note:** This methodology is by RMI, not TransitionZero (which covers steel, cement,
chemicals, pulp & paper, and aluminum). Different authoring team and approach.

**Scope:** Annual CO2 and CH4 emissions from ethylene steam cracker facilities worldwide,
from January 2021 onward. Covers 351 assets across 53 countries. Steam cracking is the
dominant process for producing ethylene, propylene, and other olefins — key building blocks
for plastics, packaging, and synthetic fibers.

**No satellite monitoring.** Production is estimated from capacity × utilization rate ×
regional emissions factors. No thermal or atmospheric satellite signal is used.

**Method — capacity × utilization × emissions factor:**
- Plant-level capacity from ICIS and GEM databases.
- National utilization rates from national statistics or, where unavailable, regional
  averages. Utilization rates vary 60–90% depending on market conditions.
- Emissions = capacity × utilization × regional emissions factor × flaring factor (1.075).
- The flaring factor accounts for routine flaring during cracker operation.

**Feedstock matters enormously for emissions intensity:**
- Heavier feeds (naphtha, gas oil) require more energy to crack → higher emissions.
- Lighter feeds (ethane, LPG) crack more easily → lower emissions.
- Regional feed mixes vary dramatically: US = ethane-dominant; EU = ~70% naphtha;
  China = ~87% naphtha; Middle East = mostly ethane/LPG.
- Emissions factors are computed regionally based on the weighted-average feedstock mix.

**Emissions components:**
- *Fuel combustion CO2:* Fossil fuel burned in cracker furnaces. Regional EFs from
  IEA/IPCC, adjusted for feedstock mix.
- *Process CO2:* From cracking reactions themselves.
- *CH4 emissions:* Included (unlike most other CT manufacturing methodologies).
- *Indirect emissions:* Electricity consumption × grid intensity from Ember.

**Data confidence scoring:** Uses national Corruption Perception Index (CPI) as a proxy
for statistical data quality. Countries with low CPI get lower confidence ratings,
reflecting uncertainty about reported utilization rates and capacity data.

**Coverage:** Asset-level estimates account for ~92% of total sector emissions. High
coverage because steam crackers are large, well-documented industrial assets.

**Limitations relevant to our project:**
1. No satellite signal — purely capacity-based with regional utilization rates applied
   uniformly to all plants in a country.
2. Belgium is highly relevant: the Antwerp petrochemical cluster is one of the largest
   in Europe (BASF, INEOS, TotalEnergies, ExxonMobil crackers). These facilities are
   major emitters in the EU ETS. CT estimates for Belgian crackers use the EU naphtha-
   heavy feedstock mix.
3. CH4 is modeled here (unlike steel and cement), which expands coverage beyond CO2-only.
4. The feedstock-dependence of emissions means that even knowing production volume is
   insufficient — the input mix matters. Our B2B proxy could potentially capture
   feedstock purchases (naphtha vs ethane suppliers), though we currently don't
   distinguish by product type in B2B data.

### Electricity Generation

**Citation:** Freeman, J., Kargar, A.R., Couture, H.D., Alvara, M., Christian, P., Doctor, Z.,
Jeyaratnam, J., Lewis, J., Koenig, H., Nakano, T., Davitt, A., Lewis, C., and McCormick, G.
(2026). *Power sector — Emissions from Electricity Generation*. WattTime / Pixel Scientia Labs /
GEM / Climate TRACE. Version March 2026. Companion paper: Couture et al. (2024),
*Estimating Carbon Dioxide Emissions from Power Plant Water Vapor Plumes Using Satellite
Imagery and Machine Learning*.

**Note:** This methodology is by WattTime (not TransitionZero). It is the most sophisticated
satellite-based methodology in the CT portfolio, using ML on visible-spectrum water vapor
plumes rather than thermal anomaly indices.

**Scope:** Monthly CO2, SO2, NOx, and PM2.5 emissions from combustible (thermal) power
plants globally — fossil fuel, waste, and biomass. Over 9,000 individual assets representing
96% of total power plant GHG emissions. Coverage from January 2015 to present with a
60-day lag. Biomass CO2 is tracked separately to avoid double-counting with forestry sector.

**Three methods for estimating electricity generation:**

1. **Reported electricity generation data** (new in 2025): Where available and validated,
   actual reported generation from grid operators is used directly. Sources: ENTSO-E
   (Europe), CAMPD (US), ONS (Brazil), NPP (India), NEM (Australia). Data validated
   at plant-month level for completeness, realistic values, and consistency. Higher
   confidence assigned to these estimates.

2. **Baseline method:** For plants without reported data or satellite signal. Synthesizes
   unit-level capacity, fuel, and prime mover information with EIA and Ember country-
   level generation data to compute annual fuel-specific capacity factors per country.
   Same capacity factor applied to all plants of a given fuel type in a country. Higher
   uncertainty but universally applicable.

3. **Remote Sensing + Machine Learning (RS+ML):** The distinctive CT contribution.
   Satellite imagery from Landsat 8 C2, Sentinel-2H, and PlanetScope PSScene is used
   to detect **water vapor plumes** — visible proxy signals from:
   - Exhaust stacks (smoke plumes)
   - Flue gas desulfurization (FGD) stacks (common at coal plants)
   - Natural draft cooling towers (large hyperbolic structures, mainly coal)
   - Mechanical draft cooling towers (fan-like rooftop structures, coal and gas; added 2024)

   CNN models are trained separately for each structure type, predicting both an "on/off"
   score and a capacity factor. Only ~4% of plants worldwide have detectable proxy
   structures, but these account for ~43% of non-biomass combustion CO2 (2015–2024).
   Mechanical draft towers extend coverage to gas plants but produce fainter plumes,
   requiring temperature/humidity filtering for usable images.

**Final generation estimate:** Ensemble of RS+ML proxy signal estimates (where available)
and country-/fuel-/prime-mover-specific baseline averages.

**Emissions estimation:** Plant-specific emissions factors based on fuel type, boiler type,
and pollution control technology. EFs from EMEP/EEA guidebook and EIA Electric Power
Annual. Pollution control efficiency from EPA AP-42. For non-GHG pollutants (SO2, NOx,
PM2.5), absence of pollution control information → conservative assumption (no controls).

**Monthly resolution:** Generation scaled using estimated monthly electricity demand
(synthetic demand from Mattsson et al. + TransitionZero load profiles), not simply
dividing annual by 12. Monthly data published with 60-day lag since March 2025.

**Limitations relevant to our project:**
1. The RS+ML approach detects water vapor plumes in visible-spectrum imagery — a
   fundamentally different signal from the thermal anomaly index used for steel/cement.
   It works for large coal plants with cooling towers but misses most gas plants and all
   smaller facilities.
2. For Europe (including Belgium), ENTSO-E reported generation data is now used
   directly where validated, making CT estimates quite accurate for large Belgian thermal
   plants. Belgium's electricity mix is dominated by nuclear and gas; coal has been phased
   out (last coal plant closed in 2016).
3. Power generation is NACE 35.11 — a sector well-covered by the EU ETS. Belgian gas
   plants (Engie, Luminus) are EU ETS regulated, so CT estimates overlap with our
   training data rather than filling gaps.

### Heat Plants

**Citation:** Freeman, J., Sridhar, L., and Alvara, M. (2026). *Power sector — Emissions from
Heat Plants*. WattTime / Climate TRACE. Version March 2026.

**The simplest CT methodology.** Country-level only — no asset-level estimates, no satellite
monitoring, no facility database. Purely a top-down accounting exercise.

**Scope:** CO2 emissions from district heating plants (not CHP, which is under electricity).
Heat plants combust fossil fuels to generate heat distributed via pipe networks. ~0.12% of
global emissions; significant in Central Asia and Eastern Europe. Global total: 91.4 MtCO2
(2024) from 37 countries. UNFCCC sector 1.A.1.a.iii. Coverage: January 2015 to present.

**Method — IPCC Tier 1:**
- Emissions = Emission Factor (by fuel type, t gas/TJ) × Fuel Consumption (TJ).
- Fuel consumption data from IEA World Energy Balances (2024), by fuel type and country.
- Default emission factors from IPCC Emission Factors Database (Fuel Combustion 1.A).
- Carbon oxidation factor assumed = 1 for CO2.
- Calculated at country level annually, then spatially disaggregated to county/district level
  and temporally disaggregated to monthly using separate CT post-processing methods.

**Validation:** Compared against UNFCCC-reported heat plant emissions for 14 Annex 1
countries. CT estimates ~30–34 MtCO2 vs UNFCCC ~35 MtCO2 — reasonable agreement.
Notable gap for Switzerland (CT < half of reported). Russia is conspicuously absent from
both IEA and UNFCCC heat-only data despite having significant district heating.

**Limitations relevant to our project:**
1. No asset-level data — country-level only, spatially disaggregated. Cannot be linked to
   individual firms or installations.
2. Belgium has limited district heating infrastructure compared to Nordic/Eastern European
   countries. Likely negligible for our exercise.
3. CHP plants (which are common in Belgian industry) are classified under electricity
   generation, not heat plants.

### Oil and Gas Refining

**Citation:** Wang, J., Fallurin, J., Peltier, M., Conway, T.J., and Gordon, D. (2025).
*Fossil Fuel Operations Sector — Refining Emissions*. RMI / Climate TRACE. Version
November 2025.

**Note:** By RMI (same team as petrochemical). Uses the PRELIM process model — a detailed
engineering simulation of refinery operations, not satellite-based.

**Scope:** Scope 1 (on-site) CO2, CH4, N2O, and CO2e emissions from oil refineries globally.
Covers the refining process within plant gate boundaries. Excludes transportation of crude
to the refinery and products to end-use. Oil refining alone represents >3% of global
anthropogenic emissions. Global refining capacity coverage: ~99%. Quarterly emissions
estimates at the asset level.

**Method — PRELIM process model (v1.6):**
The Petroleum Refinery Life Cycle Emissions Model (PRELIM), developed at the University
of Calgary (Bergerson, 2022), simulates emissions per barrel of crude refined. Four key
inputs determine asset-level emissions:

1. **Crude assays:** Library of 600+ crude oils with chemical properties (API gravity,
   sulfur content, etc.). Representative assays selected per refinery based on location
   and configuration. Heavier/sourer crudes → more energy-intensive processing → higher
   emissions.

2. **Refinery configuration:** Each refinery classified as hydroskimming (light crude,
   simple), medium conversion, or deep conversion (heavy crude, complex). More complex
   refineries have more process units and higher emissions per barrel. US refineries have
   8 finer configuration categories.

3. **Country-level crude slate:** Estimated from country's oil production, imports (by
   origin, from JODI/OEC), and exports. Weighted-average emissions intensity computed
   from the crude origins' assay-specific PRELIM outputs.

4. **Capacity and utilization:** Asset-level capacity from EIA (US), GEO, company/
   government websites. Utilization rates from JODI (monthly where available), Energy
   Institute (annual fallback). Chinese independent refineries ("teapots") tracked
   separately due to different utilization patterns.

**Emissions formula (Eq. 1):**
Emissions = Capacity × Capacity factor × Emissions Intensity from PRELIM

Emissions aggregated quarterly, then averaged to monthly estimates.

**Emission sources within a refinery:** Heat, steam, hydrogen production (SMR/CNR),
FCC catalyst regeneration, flaring of refinery fuel gas, subprocess emissions, support
services, managed waste releases.

**Limitations relevant to our project:**
1. No satellite monitoring — purely engineering model + capacity/utilization data.
   All variation across refineries comes from configuration, crude slate, and utilization.
2. Belgium is highly relevant: NACE 19.20 (petroleum refining) is one of our two
   zero-emission reference sectors. Belgium has major refineries — TotalEnergies Antwerp,
   ExxonMobil Antwerp — which are EU ETS regulated. CT's PRELIM-based estimates for
   these facilities can be compared against EUTL verified emissions.
3. The crude-slate methodology is interesting: Belgian refineries process a mix of
   imported crudes (Belgium produces no oil), so CT infers the crude mix from Belgium's
   import patterns via JODI/OEC data. This is analogous to our B2B approach — inferring
   emissions from input procurement patterns.

### Oil and Gas Production and Transport

**Citation:** Schmeisser, L., Chaney, C., Tecza, A., Stanger, J., Bylsma, S., DeLang, M.,
Gauthier, K., and Gordon, D. (2025). *Fossil Fuel Operations Sector — Oil and Gas Production
and Transport Emissions*. RMI / Climate TRACE. Version November 2025.

**Note:** By RMI. Uses the OPGEE model (Oil Production Greenhouse Gas Emissions Estimator),
developed by California Air Resources Board and Stanford University (Brandt et al., 2021).
This is the upstream counterpart to PRELIM (refining). Together with PRELIM and OPEM, they
form the OCI+ (Oil Climate Index plus Gas) tool.

**Scope:** CO2 and CH4 emissions from upstream oil and gas operations — drilling, extraction,
pre-processing, and transport of crude oil and natural gas to the refinery gate or point of
end-use consumption. Nearly 3,500 individual oil and gas fields globally (>99% of global
production), aggregated to ~640 grouped assets across 97 countries. Methane is a major focus:
super-emitter events (<5% of sources) can account for >50% of total emissions.

**Method — OPGEE lifecycle engineering model:**
OPGEE models emissions at the individual field level using 100+ emission source modules
across all production stages. Key emission sources:
- **Flaring:** Combustion of unwanted gas at extraction/processing sites (CO2 + CH4).
- **Venting:** Leaking of gas to depressurize sites (predominantly CH4).
- **Fugitive losses:** Equipment leaks during production, compression, transmission.
- **Super-emitter events:** Large methane leaks (≥25 kg/hr), stochastic and hard to predict.
- **On-site fuel usage:** Combustion for powering drilling rigs, vehicles, equipment.
- **Biogenic and embodied emissions:** Site preparation, materials used.

**Key model inputs:** Field age, field depth, gas-to-oil ratio, production volumes, API gravity,
gas composition, injection characteristics, production methods, transport distances.

**Satellite data integration — two types:**
1. **VIIRS nightfire flaring data** (NOAA, 750m resolution, daily): Detects gas flares at
   oil fields via shortwave/infrared bands (M10, M11 at 1.6 and 2.2 μm). Flares
   distinguished from wildfires by temperature (>1,000K) and persistence. Flaring-oil
   ratio (FOR) computed per field and fed into OPGEE. 90% of flaring occurs at upstream
   production sites.

2. **Methane satellite detections** (TROPOMI on Sentinel-5P, aircraft surveys): Used to
   adjust OPGEE's default fugitive methane loss rates for specific high-emitting regions
   (Permian Basin TX/NM, Algeria's Hassi R'Mel, Turkmenistan's Amudarya Basin, Russia).
   Calibrated against Cusworth et al. (2021) and Lauvaux et al. (2022) findings.

**Limitations relevant to our project:**
1. Belgium has no significant oil or gas production — this sector is irrelevant for
   Belgian firm-level emissions.
2. However, the methodology is interesting for its satellite-based methane detection,
   which represents a different use of remote sensing than the thermal/visible signals
   used for steel, cement, and power plants.
3. The OPGEE model is open-source and peer-reviewed, unlike some proprietary CT inputs.

### Coal Mining

**Citation:** Lewis, C., Tate, R.D., Mei, D.L., and Piscopo, A. (2025). *Fossil Fuel Operations
Sector — Coal Mining Emissions*. WattTime / Global Energy Monitor / Climate TRACE. Version
November 2025.

**Scope:** Fugitive methane (CH4) emissions from active coal mines globally. 4,363 mines,
2015–2023. UNFCCC sector 1.B.1.a (Coal Mining and Handling). Total 2023 emissions:
~1,873 MtCO2e. **Only CH4 is estimated in the 2025 release** — CO2 from on-site combustion
and emissions from abandoned mines are not yet included.

**No satellite monitoring.** Purely production × emissions factor approach using GEM's
Global Coal Mine Tracker database.

**Method:**
CH4 Emissions = CH4 Emissions Factor × Production (Eq. 4)

Where the emissions factor has two components:
1. **Methane gas content** (m³ CH4 per tonne of coal): Depends on mining depth and coal
   rank (anthracite, bituminous, subbituminous). Estimated by GEM using the approach from
   *Global methane emissions from coal mining to continue growing even with declining
   coal production*. Converted from m³ to tonnes using EPA factor (÷ 1.4703e3).

2. **Emissions factor coefficient** (multiplier = 1.65, average of 1.3–2.0 range from
   Ju et al., 2016): Accounts for fugitive CH4 released from adjacent coal seams and
   pillars beyond the mined coal itself.

**Production data:** From GEM's GCMT. Where only capacity is available, production =
capacity × capacity factor. Capacity factors from literature: China (regional, stochastic
model from Ju et al., 2019), US (state-level, EIA), rest of world (global average of
China + US).

**Coal Mine Methane (CMM) release mechanisms:**
- *Underground mines:* Dominated by vented emissions — ventilation systems (60%) and
  drainage systems (25%). Also incomplete combustion (2%), fugitive equipment leaks (5%),
  post-mining (3%), and outcrops/workings (5%).
- *Surface mines:* Dominated by fugitive emissions from outcrops/workings (75%) and
  drainage (15%).

**Limitations relevant to our project:**
1. Belgium has no active coal mines (last mine closed in 1992). Completely irrelevant
   for Belgian firm-level emissions.
2. Only CH4, not CO2. Our project focuses on CO2 from fuel combustion.
3. No satellite monitoring — the simplest CT methodology alongside heat plants.

---

## How Our Paper Fits

We bridge strands 1 and 2 using the data infrastructure of strand 3:

- **From strand 1** we take the economic logic: fuel consumption drives combustion
  emissions. Our B2B fuel-supply proxy is an indirect measure of what strand 1
  papers observe directly.

- **From strand 2** we take the prediction problem: estimate emissions for firms
  that don't report. But we replace the standard financial-variable approach
  (revenue, assets, sector) with a fundamentally more informative signal — actual
  purchasing behavior from fuel suppliers.

- **Relative to strand 3** we contribute a trained prediction model rather than
  mechanical allocation, data-driven fuel supplier identification rather than
  pre-selected NACE codes, explicit extensive-margin modeling (hurdle), rigorous
  out-of-sample evaluation, and calibration to sector-year (not national) totals.

The calibrated-benchmark comparison in the main table makes the value-added
decomposition transparent:
- PPML + calibration (nRMSE 0.670): what you get from revenue + sector + known
  totals alone (approximates the commercial provider approach, strand 2).
- Hurdle + proxy + indicator + calibration (nRMSE 0.193): what you get when you
  add B2B data. The 71% improvement is driven by the fuel-supply proxy.

A planned "Hungarian benchmark" specification will replicate the strand 3
proportional allocation approach in our data for direct comparison.

---

## How the NIR Sector-Year Emission Totals Are Constructed

We use sector-year emission totals from Belgium's National Inventory Report (NIR)
as calibration targets. This section documents how those totals are built, to
assess their reliability.

### The IPCC framework: three tiers

Under the UNFCCC, all Annex I countries submit annual greenhouse gas inventories
following IPCC Guidelines (2006, refined 2019). For fuel combustion (CRF category
1A), the IPCC defines three tiers of increasing complexity:

- **Tier 1**: Emissions = fuel consumed × default IPCC emission factor. Requires
  only aggregate fuel consumption data.
- **Tier 2**: Same structure but uses country-specific emission factors (reflecting
  national fuel characteristics, combustion technology, etc.).
- **Tier 3**: Detailed facility-level or technology-specific modeling. Uses
  plant-level data where available (e.g., EU ETS verified emissions for regulated
  installations).

Belgium uses a mix of Tier 2 and Tier 3 methods for the energy sector. For
EU ETS installations, verified emissions are used directly (Tier 3). For non-ETS
sources, the inventory relies on regional energy balances combined with
country-specific emission factors (Tier 2).

### Energy balances: the core input

The primary data source for non-ETS combustion emissions is the **regional energy
balance**. Belgium's inventory is compiled by three regional agencies:
- **Flanders**: VEKA (Vlaams Energie- en Klimaatagentschap), previously VITO
  (until 2020).
- **Wallonia**: AwAC (Agence wallonne de l'air et du climat).
- **Brussels-Capital**: Bruxelles Environnement.

Each region constructs an energy balance describing the quantities of energy
imported, produced, transformed, and consumed within the region in a given year.

### How energy balances are constructed: fuel-by-fuel data sources

The reliability of the regional energy balances varies sharply by fuel type.
This matters because the NIR converts fuel consumption to emissions using
emission factors, so the quality of the emission estimate inherits the quality
of the underlying activity data.

*Source: Belgian NIR 2024, Section 3.2.5 (pp. 74–93).*

**Electricity and natural gas — metered, reliable.**
Grid operators (Fluvius/Elia in Flanders, CWaPE in Wallonia, SIBELGA/Elia in
Brussels) report consumption per connection point, classified by NACE code.
Electricity data available since 2003, natural gas since 2005 in Flanders.
Uncertainty: ~2% (NIR Section 3.2.7.3, p. 110).

**Solid fossil fuels — firm-level surveys, reliable.**
In Flanders, solid fuel consumption is captured through individual company
reporting: IMJV (mandatory for firms >0.1 PJ primary energy), ETS annual
emission reports (from 2013), and energy audits (from 2017). The petroleum
extrapolation method described below is explicitly *not* applied to solid fuels
(NIR p. 82 mentions only petroleum products). In Wallonia (NIR p. 89):
"The solid fuels and the natural gas are listed with the annual survey" — i.e.,
the REGINE survey (~280 large firms plus ~800 additional). Uncertainty: ~5%
(NIR p. 110).

**Petroleum products — extrapolated, uncertain.**
Neither Flanders nor Wallonia has complete regional statistics for petroleum
product consumption. Both regions use an extrapolation method (NIR pp. 82, 89).
The Flemish method is described as follows (p. 82, verbatim):

> "Since we only have complete energy statistics on the Flemish level for
> electricity and natural gas and no complete regional statistics for petroleum
> products, some petroleum products of which we know they are widely used in
> Flanders like LPG (butane/propane), gas- and diesel oil, heavy fuel oil in
> certain sectors) are extrapolated on a sectoral level. This extrapolation
> method is based on the ratio of the known individual companies' electricity
> consumption versus the known total electricity consumption of the sector
> they belong to. Via this ratio, the known individual companies' consumption
> of petroleum products is scaled up to the sector level."

In formula: for a given NACE sector, let E_known be total electricity of
individually reporting firms (IMJV, ETS, energy audits), E_sector the total
sector electricity from grid operators, and P_known the petroleum consumption
of those same reporters. Then:

    P_sector = P_known × (E_sector / E_known)

The key assumption is that the petroleum-to-electricity ratio of large
reporters is representative of the entire sector, including non-reporting
(smaller) firms. This is a strong assumption: smaller firms may have very
different fuel mixes. In Wallonia, the method is described in a single
sentence (p. 89): "The petroleum products are extrapolated on the basis of
electricity consumption." Uncertainty: 2–8% depending on sector (NIR p. 110),
though this likely understates the true uncertainty given the extrapolation.

The NIR does not clarify: (a) whether the extrapolation is done fuel-by-fuel
(diesel, heavy fuel oil, LPG separately) or pooled; (b) whether the scaling
ratio is computed at the firm level or directly at the sector level; (c) the
exact set of "known individual companies" (IMJV only, ETS only, or their
union).

**EU ETS data — verified, plant-specific.**
From 2013 onward, ETS installation-level data (activity data and CO2 emissions)
are fully integrated into all three regional energy balances (NIR pp. 80, 103).
ETS firms use Tier 2–3 plant-specific emission factors with measured net
calorific values and oxidation factors. Non-ETS firms use mainly IPCC 2006
default emission factors (Tier 1), except natural gas which uses a
Belgium-specific factor from 2021.

**Non-ETS emissions are a residual.** Non-ETS fuel consumption = total sectoral
consumption (from energy balance) minus ETS installations' consumption (from
verified reports). All errors in the total energy balance therefore propagate
entirely to the non-ETS residual.

**Brussels.** Industry is a negligible share of Brussels emissions. For fuels
other than electricity and natural gas, "the final consumption in 2013 is
extrapolated taking degree days into account" (NIR p. 91).

### From energy balances to emissions

The conversion from fuel quantities to CO2 emissions is conceptually
straightforward:

  Emissions = Σ (fuel quantity_f × net calorific value_f × emission factor_f)

where the sum runs over fuel types f. Belgium uses country-specific emission
factors (Tier 2) for most fuels. For CO2 from fossil fuel combustion, the
uncertainty is relatively low because CO2 emissions depend almost entirely on
the carbon content of the fuel, which is well characterized.

### Sectoral allocation (CRF categories)

The NIR reports emissions by CRF (Common Reporting Format) category:
- **1A1**: Energy industries (power plants, refineries)
- **1A2**: Manufacturing industries and construction
- **1A3**: Transport
- **1A4**: Other sectors (commercial/institutional, residential, agriculture)

Within 1A2 (manufacturing), emissions are further disaggregated by sub-sector
(iron and steel, chemicals, food processing, etc.), using the energy balance's
sectoral breakdown plus any available facility-level data.

The CRF-to-NACE crosswalk (in our data: `DATA_DIR/raw/Correspondences_and_dictionaries`)
maps these CRF categories to NACE Rev. 2 sectors, enabling us to construct
sector-year calibration targets at the NACE 2-digit level.

### Reliability assessment: which sectors can we trust?

The reliability of NIR sectoral emission totals depends on the fuel mix of
each sector. Using CRT data (BEL-CRT-2025, Table 1.A(a) Sheet 2), we compute
the share of fossil CO2 from petroleum products for each CRF 1.A.2 subcategory,
averaged over 2013–2022:

| CRF category | Sector | Avg CO2 (kt) | Liquid% | Solid% | Gas% | Other% |
|---|---|---:|---:|---:|---:|---:|
| 1.A.2.a | Iron and steel | 1,164 | 1.9% | 2.0% | 95.8% | 0.4% |
| 1.A.2.b | Non-ferrous metals | 423 | 10.3% | 21.2% | 68.4% | 0.1% |
| 1.A.2.c | Chemicals | 3,797 | 6.4% | 0.1% | 93.2% | 0.3% |
| 1.A.2.d | Pulp, paper, print | 563 | 6.8% | 17.4% | 56.5% | 19.3% |
| 1.A.2.e | Food, beverages, tobacco | 2,330 | 5.0% | 3.9% | 91.1% | 0.0% |
| 1.A.2.f | Non-metallic minerals | 3,388 | 10.2% | 40.5% | 36.0% | 13.4% |
| 1.A.2.g | Other manufacturing | 1,975 | 43.6% | 0.8% | 54.6% | 1.0% |
| **1.A.2** | **Total manufacturing** | **13,640** | **12.2%** | **12.4%** | **71.0%** | **4.4%** |

*"Liquid%" = share from petroleum products, whose sectoral allocation relies on
electricity-ratio extrapolation. "Gas%" and "Solid%" are based on metered or
surveyed data. Script: `figures_tables/table_petroleum_dependency.R`.*

**High reliability (<10% petroleum-dependent):**
Iron and steel (1.A.2.a), chemicals (1.A.2.c), food/beverages/tobacco (1.A.2.e),
and pulp/paper/print (1.A.2.d) are overwhelmingly gas- or solid-fuel based.
Their NIR totals rest almost entirely on metered natural gas and firm-level
survey data. These sectors account for ~58% of total manufacturing CO2.

**Moderate reliability (10–25% petroleum-dependent):**
Non-ferrous metals (1.A.2.b) and non-metallic minerals (1.A.2.f). About 10% of
their emissions come from petroleum products. Non-metallic minerals also has a
large "other fossil fuels" share (waste fuels used in cement kilns), whose
measurement quality is unclear.

**Low reliability (>25% petroleum-dependent):**
"Other manufacturing" (1.A.2.g) — a residual catch-all category — derives 44%
of its fossil CO2 from liquid fuels. This is the sector where the NIR's
petroleum extrapolation assumption bites hardest. It is also where the
proportionality assumption (petroleum/electricity ratio of large firms =
that of small firms) is least defensible, given the heterogeneity of the
"other" category.

**Overall strengths (unchanged):**
- Energy balances are cross-checked for internal consistency (supply = demand +
  losses + statistical difference).
- CO2 emission factors have low uncertainty (~1–5%) because CO2 depends on
  well-characterized carbon content.
- The Reference Approach (top-down, national fuel supply) cross-checks the
  Sectoral Approach (bottom-up). Belgium reports both.
- Inventories undergo annual UNFCCC expert review.

**Bottom line for our calibration exercise:** For most named manufacturing
sectors (1.A.2.a through 1.A.2.f), the NIR emission totals are grounded in
metered gas data and firm-level surveys — they are reliable calibration targets.
The main exception is "other manufacturing" (1.A.2.g), where nearly half of
emissions come from extrapolated petroleum data. The CRF-to-NACE crosswalk
introduces additional mapping uncertainty at the 2-digit level.

### Key references

- [Belgium NIR 2024](https://klimaat.be/doc/nir-2024.pdf) — Chapter 3 (Energy)
  describes the methodology in detail.
- [Belgium NIR 2023](https://klimaat.be/doc/nir-2023-15042023-final.pdf)
- [Eurostat Energy Balance Metadata](https://ec.europa.eu/eurostat/cache/metadata/en/nrg_bal_esms.htm)
  — Describes data collection, validation, and accuracy.
- [Eurostat Energy Balance Guide](https://ec.europa.eu/eurostat/documents/38154/4956218/ENERGY-BALANCE-GUIDE.pdf)
  — Methodology for constructing energy balances.
- [2006 IPCC Guidelines, Vol. 2 (Energy)](https://www.ipcc-nggip.iges.or.jp/public/2006gl/)
  — Tier 1/2/3 methodology for fuel combustion.
- [2019 Refinement to IPCC Guidelines](https://www.ipcc.ch/report/2019-refinement-to-the-2006-ipcc-guidelines-for-national-greenhouse-gas-inventories/)
- [Statbel: Energy Statistics by Economic Sector](https://statbel.fgov.be/en/themes/energy/energy-statistics-economic-sector-and-energy-source)
