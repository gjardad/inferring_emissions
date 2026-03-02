# Stangl, Borsos, Diem, Reisch & Thurner (2024)
# "Firm-Level Supply Chains to Minimize Unemployment and Economic Losses in Rapid Decarbonization Scenarios"

Nature Sustainability 7, 581-589. Published online 15 April 2024.

---

## 1. Research Question
How can firm-level production networks inform decarbonization strategies that
minimize unemployment and economic output losses?

**This paper does NOT estimate emissions for non-ETS firms.** It uses only
ETS verified emissions for 119 Hungarian firms and studies the supply-chain
consequences of hypothetically shutting those firms down.

## 2. Audience
Climate-economic modelling community, policy designers, IAM researchers.

## 3. Method
- Reconstruct the Hungarian firm-level production network from VAT data (2019).
- Match 119 ETS firms to the network (122 found in EUTL, 3 could not be matched).
- Compute systemic relevance metrics (OW-ESRI, EW-ESRI) for each firm.
- Simulate four decarbonization strategies: cumulatively remove ETS firms from
  the network in different orderings, compute cascading job and output losses.

### Systemic relevance measures
- **OW-ESRI** (Output-Weighted ESRI): total output loss when firm i is removed.
  Accounts for cascading effects through the production network.
- **EW-ESRI** (Employment-Weighted ESRI): same but weighted by employment.
  Measures short-term worker displacement from firm closures.

### Decarbonization strategies
1. Remove largest emitters first → 20.25% CO2 reduction, 28.56% job loss
2. Remove firms with least employees first → 17.46% CO2 reduction, 28.35% job loss
3. Remove least-risky firms first (by EW-ESRI) → 20.21% CO2 reduction, 7.97% job loss
4. **Smart strategy** (by CO2/EW-ESRI ratio) → 20.19% CO2 reduction, **1.92% job loss**

The smart strategy identifies "decarbonization leverage points": firms with
high emissions but low systemic economic relevance.

## 4. Data
- **Production network:** 243,399 Hungarian firms, 1,104,141 supply links.
  VAT transaction data, year 2019. Monetary values in HUF.
- **ETS emissions:** 119 firms matched to network (122 from EUTL, 3 unmatched).
  21.72 MtCO2 in 2019 (33.7% of Hungary's 64.44 MtCO2).
  Source: EUTL via pyeutl library (ref 39).
- **Employment:** 2,333,975 jobs from firm-level dataset (65.6% of official total).
- **No firm-level emission estimation for non-ETS firms.** Emissions of firms
  outside the ETS were not known.

## 5. Statistical Methods
- ESRI shock propagation algorithm (from Diem et al., ref 28).
- Linear production functions assumed (firms use maximum available inputs).
- Deterministic simulation: cumulatively remove firms, compute cascading effects.
- No econometrics, no regression, no prediction model.

## 6. Findings
- Naive strategy (remove largest emitters) → catastrophic: 28% job loss,
  33% output loss for only 20% emission reduction.
- Smart strategy → same emission reduction with only 2% job and output loss.
- Supply chain amplification: 3-42x higher losses when accounting for
  network effects vs. direct effects only.
- 16 firms with CO2/EW-ESRI > 1,000 are prime decarbonization leverage points.

## 7. Contributions
1. Framework for network-aware decarbonization policy design.
2. EW-ESRI measure for employment-weighted systemic risk.
3. Demonstrates that ignoring supply chains leads to catastrophic outcomes.
4. Identifies decarbonization leverage points.

## 8. Replication Feasibility
- Data: restricted (Central Bank of Hungary). Contact: olahzs@mnb.hu.
- Code: https://github.com/jo-stangl/reducing_employment_and_economic_output_loss_in_rapid_decarbonization_scenarios
- pyeutl library for ETS data: https://github.com/jabrell/pyeutl

---

## Key Takeaways for Our Paper

### Relevance
This paper is the **least relevant** of the three Hungarian papers for our
emission estimation methodology. It does NOT estimate emissions for non-ETS
firms — it only uses verified ETS emissions for 119 firms and studies network
consequences of their removal.

### What we can cite it for
1. Demonstrates the **downstream application** of firm-level emission estimates:
   once you have emissions for all firms, you can do network-aware policy design.
2. Shows that **only knowing ETS emissions is insufficient** — their analysis is
   limited to 119 firms because non-ETS emissions are unknown. Our paper would
   fill this gap by providing estimates for all firms.
3. Uses the same Hungarian B2B network data infrastructure (Diem et al. 2022,
   ref 28), establishing the institutional precedent.

### Emission estimation: NOT present
- The paper explicitly states: "the emissions of the firms outside the Hungarian
  ETS were not known."
- This was published in May 2024. Tabachova et al. (2025) is the follow-up that
  DOES estimate non-ETS emissions using B2B data.
- Stangl (2026) further refines the approach to estimate energy consumption
  (not emissions directly) for ~25K firms.

### Timeline of the Hungarian group's work
1. **Diem et al. (2022)**: Quantifying firm-level systemic risk from production
   networks. (Establishes ESRI methodology.)
2. **Stangl et al. (2024, Nat Sust)**: Uses ETS emissions + network to design
   decarbonization strategies. No emission estimation for non-ETS.
3. **Tabachova et al. (2025, arXiv)**: First attempt to estimate emissions for
   ALL firms using B2B fuel purchases. Proportional allocation method.
4. **Stangl et al. (2026, Nat Comms)**: Estimates energy consumption (kWh) for
   25K firms using B2B + EUROSTAT prices. More refined but different goal
   (energy transition tracking, not emission estimation).
