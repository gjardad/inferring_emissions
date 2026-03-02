# Tabachova, Diem, Stangl, Borsos & Thurner (2025)
# "Combined Climate Stress Testing of Supply-Chain Networks and the Financial System with Nation-Wide Firm-Level Emission Estimates"

arXiv:2503.10644 / INET Oxford WP 2025-04. Dated March 17, 2025.

---

## 1. Research Question
How do carbon pricing policies (existing ETS I and upcoming ETS II) affect the
real economy and the financial system, accounting for supply-chain contagion?

The emission estimation is a MEANS to this end — not the paper's main
contribution. They need firm-level emissions to compute firm-level carbon costs,
which feed into a stress testing model.

## 2. Audience
Central bankers, financial regulators, climate stress testing community.
NOT the environmental economics / applied micro audience.

## 3. Method — Emission Estimation

### Fuel supplier identification (Methods Step 1, p.11; Table II, p.13; Fig. S10, p.36)
- NOT data-driven. They pre-select 6 NACE 4-digit codes:
  - **Gas:** D35.2.1 (Manufacture of gas, 5 firms), D35.2.2 (Distribution of
    gaseous fuels through mains, 11 firms), D35.2.3 (Trade of gas through
    mains, 31 firms)
  - **Oil:** C19.2.0 (Manufacture of refined petroleum products, 5 firms),
    G46.7.1 (Wholesale of solid, liquid, gaseous fuels, 159 firms),
    G47.3.0 (Retail sale of automotive fuel, 565 firms)
- Total: 47 gas distributors, 729 oil distributors (776 firms total).
- D35.2.3 dominates gas (102K out-links, EUR 12.3B sales).
  G47.3.0 dominates oil (261K out-links, EUR 5.2B sales).
- **Exclusion:** G46.1.2 (Agents in sale of fuels, ores, metals, industrial
  chemicals) excluded from stress testing — 1.2 Mt estimated emissions.
  They recognize these firms are intermediaries, not consumers.

### Allocation formula (Equation 2, p.12)
Purely proportional, applied at the NATIONAL level:

  E_i = (s_i^{in,gas} / s^{out,gas}) × E^{gas} + (s_i^{in,oil} / s^{out,oil}) × E^{oil}

Where:
- s_i^{in,gas} = Σ_{j∈I_g} W_{ij} (firm i's total purchases from gas suppliers)
- s^{out,gas} = Σ_{i∈I_g} s_i^{out} (total output of gas sector, excluding intra-sector trade)
- E^{gas} = 12.5 Mt, E^{oil} = 13.7 Mt (national totals from Global Carbon Project)

Adjustments:
- Household gas: ~1/3 subtracted (residential heating)
- Private car oil: estimated and subtracted
- 73.6% of oil consumption estimated as commercial sector
- Financial firms (NACE K) manually excluded (some are energy trading firms)

**Key assumptions (from S7 Limitations, p.33):**
1. Uniform price of gas/oil — small consumers pay more per unit, so method
   may underestimate large consumers and overestimate small ones.
2. Oil is heterogeneous (gasoline, heating oil, naphtha) with different prices,
   but they aggregate transaction values across all oil-providing sectors.
3. Results "highly dependent on the number of firms and links" in the network.
4. All out-links of distributors assumed to be fuel sales, but some may involve
   other products — a limitation due to lack of product-level information.

### Coverage
- 410,523 Hungarian firms paying VAT in 2022.
- 185,783 firms (45%) get positive emission estimates, totaling 19 Mt.
- 106 ETS I firms appear in the SCN data (out of 119 total), covering 16 Mt.
- Remaining 55% of firms buy neither oil nor gas → zero emissions.

### Validation (Section S1, p.18, Figure S1)
- 106 ETS I firms compared: estimated vs. reported emissions.
- **Log-log correlation: 0.61**
- **Linear correlation: 0.22** (much worse!)
- Total reported emissions of 106 firms: 16 Mt
- Total estimated from oil+gas purchases: **6 Mt (only 37.5% of reported!)**
- 60 firms: method underestimates (dots above diagonal)
- 46 firms: method overestimates (dots below diagonal)
- 2 firms: method yields zero (don't buy oil or gas in Hungary)
- Reasons for underestimation: estimates don't capture electricity-related
  emissions, chemical reactions, waste handling, or foreign fuel purchases.
  "For the majority of firms in the supply network this mis-estimation should
  be small as very few firms have import relations."

## 4. Data
- **Supply chain network (SCN):** Hungarian VAT-based B2B transaction data.
  10,698,769 links between 410,523 firms. Total output EUR 254 bn.
  Year: 2022. Filter: only links appearing in ≥2 different quarters.
  W_ij = total purchase volume of firm j from firm i.
- **ETS I emissions:** 106 firms matched to SCN (119 total in Hungary).
- **Company registry:** Balance sheets, income statements (equity, profits).
- **Banking data:** 20 banks (CET1 EUR 13 bn), loans to 56,595 firms (EUR 24 bn).
- **National emissions:** Global Carbon Project [49] for 2022.
- **Replication code:** https://github.com/zlatataa/Supply_chain_adjusted_financial_climate_stress_testing

## 5. Statistical Methods
- Emission estimation: purely mechanical allocation (no econometrics).
- Cost pass-through: iterative proportional allocation based on market shares
  within NACE 4-digit sectors (Equations 3-4). Converges by sub-stochastic
  matrix property.
- Default identification: CPR (Carbon-to-Profit Ratio) = E_i / P_i.
  Firm defaults if P_i ≤ γ(π), where γ is carbon cost after pass-through.
- Supply chain contagion: Generalized Leontief production function (pessimistic)
  vs. Linear production function (optimistic). From [38, 42].
- Banking losses: direct (loan write-offs from defaulted firms) + indirect
  (from supply-chain-contagion-induced defaults). Equations 7-8, 17-19.

## 6. Findings
- At 45 EUR/t: direct losses 1.3% of sales, bank equity losses <1%.
- Supply chain contagion amplifies 3-40x depending on substitutability:
  - Optimistic (linear): 5.3% output loss, 2.7% equity loss at 45 EUR/t
  - Pessimistic (Leontief): >50% output loss at 30 EUR/t (systemic risk core)
- At 200 EUR/t (2-degree compatible): 3.8% direct output loss, 4.7% equity.
- CPRS-based exposure estimates substantially misestimate actual bank exposure.
- Banks exposed not just through own debtors but through debtors' supply chains.

## 7. Contributions
1. First nationwide firm-level emission estimates using B2B/VAT data for all
   firms in a country (Hungary).
2. First combined climate stress test integrating firm-level emissions,
   supply chain contagion, and banking system losses.
3. Show CPRS taxonomy inadequate for firm-level exposure assessment.
4. Quantify supply chain amplification of transition risk (3-40x).

## 8. Replication Feasibility
- Data: restricted (Central Bank of Hungary). Access possible for researchers.
- Code: public GitHub repository (see above).
- Belgian equivalent: NBB B2B data (same structure, restricted access).

---

## Key Takeaways for Our Paper

### What they do well
- Demonstrate that B2B/VAT data contains emission-relevant information.
- Cover the universe of firms (410K) — comprehensive.
- Clean illustration of the approach with Fig. S10 schematic.

### What they do poorly (= our value-added)
1. **Fuel supplier ID is ad hoc:** 6 pre-selected NACE codes. No data-driven
   selection. They acknowledge the limitation but don't address it.
   → We use elastic net on B2B sales to known fuel suppliers.

2. **No prediction model:** Purely proportional allocation. Assumes homogeneous
   relationship between purchase volumes and emissions.
   → We train a hurdle model that learns nonlinearities and interactions.

3. **National-level allocation:** Allocates national totals, not sector-level.
   → We calibrate to sector-year totals from NIR data.

4. **No extensive margin:** Every firm buying fuel gets positive emissions.
   No classification of emitters vs non-emitters.
   → Our hurdle model explicitly models the extensive margin.

5. **Validation is minimal:** Single log-log correlation of 0.61 (linear: 0.22!)
   on 106 ETS firms. Total estimated = 6 Mt vs 16 Mt reported (captures only
   37.5%). No cross-validation. No ranking metrics. No FPR/TPR.
   → Full CV battery: LOFOCV + LOSOCV with nRMSE, APD, Spearman, rho_s, FPR/TPR.

6. **Uniform price assumption:** Treats all fuel purchases as homogeneous in
   price per unit. Small firms pay more per unit → overestimated.
   → Our model lets the data determine the functional form.

7. **Captures only combustion from domestic oil/gas purchases:** Misses
   electricity, industrial processes, imports. For ETS firms, captures only
   37.5% of actual emissions.
   → We also focus on combustion only, but our model can capture firms that
   emit from fuels not bought through identified domestic suppliers (via
   revenue and sector channels in the PPML/hurdle).

### Hungarian benchmark specification
To replicate their approach in our data:
- Take raw fuel-supply proxy (weighted or pooled)
- For each sector-year cell: yhat_i = (proxy_i / Σ proxy_j) × E_total
- Firms with proxy = 0 get yhat = 0
- Compute metrics on same sample as CV results

This is more favorable than what they actually do (sector-year vs national
totals), making it a generous benchmark.

---

## Reading Progress
- [x] Pages 1-4: Abstract, Introduction, Results overview
- [x] Pages 5-8: Stress testing results, Discussion
- [x] Pages 9-12: References, Data, Methods Step 1 (emission estimation)
- [x] Pages 13-16: Table II (fuel sectors), Methods Steps 2-5, cost pass-through
- [x] Pages 17-20: SI contents, S1 (validation — Fig S1), S2 (loss tables)
- [x] Pages 33-38: S7 (limitations), S8 (descriptive stats), S9 (Fig S10 schematic), S10
