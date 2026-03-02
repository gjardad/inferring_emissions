# Stangl, Borsos & Thurner (2026)
# "Using Firm-Level Supply Chain Networks to Measure the Speed of the Energy Transition"

Nature Communications (Article in Press). Authors: Complexity Science Hub Vienna / Central Bank of Hungary.

---

## 1. Research Question
How fast are individual firms transitioning from fossil to low-carbon energy, and
does the current pace align with international climate goals?

This paper is about **measuring the energy transition** — not about emission
estimation. The emission/energy estimation is a means to reconstruct firm-level
energy portfolios, which are then analyzed for transition dynamics.

## 2. Audience
Energy systems researchers, climate policy community, industrial ecology.
NOT environmental economics or applied micro.

## 3. Method — Energy Estimation

### Energy provider identification (Methods, "Identifying energy providers")
Firms categorized by NACE 4-digit codes. **Expanded** set relative to Tabachova
et al. (2025) — now includes electricity providers:

- **Electricity:** D35.1 (Electric power generation, transmission, distribution),
  D35.1.1 (Production of electricity), D35.1.2 (Transmission of electricity),
  D35.1.3 (Distribution of electricity), D35.1.4 (Trade of electricity)
- **Gas:** D35.2.1 (Manufacture of gas), D35.2.2 (Distribution of gaseous fuels
  through mains), D35.2.3 (Trade of gas through mains)
- **Oil:** B6.1.0 (Extraction of crude petroleum), C19.2.0 (Manufacture of refined
  petroleum products), G47.3.0 (Retail sale of automotive fuel), G46.7.1
  (Wholesale of solid, liquid, and gaseous fuels)

Multiple NACE codes per energy type "to account for the limitations of industry
classifications" — firms may transport and sell gas or refine and sell oil but
only be classified under one code.

**Coal excluded:** No distinct NACE code for coal distribution. Coal primarily used
for electricity production (already captured in electricity mix). Beyond that,
coal consumption limited to a small number of steel and pulp/paper firms.

### Sample construction (Methods, "Firm sample construction")
Key filtering steps:
1. Retain only firms buying from BOTH gas AND electricity providers (because
   energy providers often supply both under one NACE code).
2. **Exclude** firms classified under ANY energy-related NACE code → focus on
   end-users, not energy suppliers.
3. **Exclude** financial sector (NACE 1-digit K) — may act as energy brokers.
4. **Exclude** H52.2.1 (Service activities incidental to land transportation) —
   gas purchases likely for liquefaction, not consumption.
5. **Exclude** EU ETS firms (201 firms) — carbon credits distort energy costs.
6. Exclude firms without NACE classification or missing revenue data.
7. Exclude one outlier (gas consumption increased 3 orders of magnitude).

**Final sample: 25,231 firms** with continuous semi-annual time series on
electricity, gas, and oil consumption + revenue.

### Conversion from EUR to kWh (Methods, "Conversion of monetary inputs...")
The key innovation relative to Tabachova et al.: **price-based conversion** rather
than proportional allocation of national totals.

- **Electricity and gas:** EUROSTAT non-household energy prices by consumption band.
  7 bands for electricity, 6 for gas. Prices include all taxes and levies.
  - Assign each firm to a consumption band based on its expenditure level.
  - Semi-annual prices → semi-annual kWh estimates → aggregated to annual.
  - Firms with annual VAT reporting: average of semi-annual prices (1,000-5,000
    firms per year).

- **Oil:** Weighted average of diesel (74%) and gasoline (26%) prices from EU
  Weekly Oil Bulletin. Weights from Hungary's National Detailed Energy Balance.
  - Assumes firms consume oil primarily as diesel/gasoline.
  - Chemical industry (naphtha) may be underestimated since naphtha is cheaper.

### Coverage
- 25% of total final energy consumption in Hungary (2023)
- Breakdown: 16.6% of oil, 40.0% of gas, 17.1% of electricity
- Includes transport and residential: the 25% is of TOTAL final energy

### Government support during energy crisis
- 2022: SME support scheme covering 50% of increased electricity/gas costs
  (~10,000 firms). Implemented as reimbursement → doesn't affect observed B2B
  transactions.
- 2023: Electricity price cap for ~5,000 firms (manufacturing, accommodation,
  warehousing/transport). Applied directly to energy bills → observed prices
  still reflect actual amounts paid.

## 4. Data
- **Supply chain network:** Hungarian VAT-based B2B transactions (same as
  Tabachova et al.). Semi-annual snapshots, 2020-2024.
- **Reporting thresholds:** Pre-2018: >1M HUF cumulative. 2018-mid 2020: >100K
  HUF per transaction. Post-Q3 2020: all inter-firm invoices. Analysis starts
  2020 for this reason.
- **Energy prices:** EUROSTAT non-household electricity and gas prices by
  consumption band. EU Weekly Oil Bulletin for oil.
- **Hungary's energy balance:** National Detailed Energy Balance from MEKH.
- **Electricity mix:** Ember (annual data on clean vs fossil generation in TWh).
- **Firm characteristics:** Revenue, employment, NACE sector from company registry.
- **Code:** https://github.com/jo-stangl/using_firm-level_supply_chain_networks_to_measure_the_speed_of_the_energy_transition

## 5. Statistical Methods
- Energy estimation: mechanical (monetary purchases / energy prices = kWh).
  No regression model.
- Decarbonization trends: Robust regression (Huber loss, k=1.345) of
  low-carbon share on time, separately for each firm. Linear (Eq. 3) and
  exponential (Eq. 7) specifications.
- Transition status classification: firm transitions if both δ_i > 0 and λ_i > 0.
- Logistic regression of transition status on firm characteristics (Eq. 10),
  within NACE 1-digit sectors. Odds ratios for 10% increases (Eq. 11).
- Forecasting: linear/exponential extrapolation + Hungary's electricity mix
  decarbonization trajectory.

## 6. Findings
- Substantial within-sector heterogeneity: frontrunners and laggards coexist
  within fine-grained NACE 4-digit sectors.
- ~Half of firms increase low-carbon share, ~half decrease.
- Business-as-usual: low-carbon shares reach only 20-26% by 2050 (far from
  climate goals).
- Best-case (all firms adopt peer frontrunner rates): 70% by 2050.
- Transitioning firms: higher electricity cost shares, higher total energy use.
- Non-transitioning firms: higher fossil cost shares, higher revenue (can
  "afford" to keep paying fossil fuel costs — "lock-in effect").

## 7. Contributions
1. First firm-level measurement of energy transition speed using B2B data.
2. Shows energy transition is characterized by polarization, not convergence.
3. Identifies "lock-in effect": fossil-reliant firms less likely to switch.
4. Demonstrates frontrunner firms exist in nearly all NACE 4-digit sectors
   → transition is a coordination/incentive problem, not a technology problem.

## 8. Replication Feasibility
- Data: restricted (Central Bank of Hungary). Contact: olahzs@mnb.hu.
- Code: public GitHub repository.
- Belgian equivalent: NBB B2B data (same structure).

---

## Key Takeaways for Our Paper

### Methodological evolution from Tabachova et al.
The NACE codes for fuel supplier identification are **expanded** in this paper
relative to Tabachova et al. (2025):
- Electricity providers added (D35.1.x codes)
- Crude oil extraction added (B6.1.0)
- Same gas and oil distribution codes retained

The conversion methodology is **different** from Tabachova et al.:
- Tabachova: proportional allocation of NATIONAL emission totals
  (firm purchases / total fuel sector sales × national emissions)
- Stangl (2026): price-based conversion to kWh using EUROSTAT consumption-band
  prices. No reference to national totals.

This is a more sophisticated approach — but still mechanical (no regression),
still based on pre-selected NACE codes (not data-driven), and still assumes
all purchases from identified suppliers are energy purchases.

### Limitations relevant to our comparison
1. **Requires buying from BOTH gas AND electricity providers** → excludes firms
   using only one energy type. This is a substantial restriction (final sample
   is only 25,231 out of 410K+ firms).
2. **Consumption band assignment is approximate** — assigns bands based on
   total expenditure, but within-band price heterogeneity exists (load profiles,
   contractual arrangements, self-generation).
3. **Oil product heterogeneity** — weighted average of diesel/gasoline prices
   misses naphtha (chemical industry) and other products.
4. **Missing behind-the-meter PV** — estimated 2.3-8.8% of commercial electricity.
5. **Pre-selected NACE codes** — same fundamental limitation as Tabachova.

### For our Hungarian benchmark
This paper's approach is MORE sophisticated than Tabachova et al.'s proportional
allocation. Our Hungarian benchmark should replicate the SIMPLER Tabachova
approach (proportional allocation of sector-year totals based on raw proxy),
since that is the emission estimation method. Stangl (2026) estimates energy
consumption (kWh), not emissions directly.
