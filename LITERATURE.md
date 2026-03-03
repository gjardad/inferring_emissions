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
