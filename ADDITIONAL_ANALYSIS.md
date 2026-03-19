# Additional Analysis

## Medium-term goals

1. **Alpha hyperparameter sensitivity.** Show that the main results are robust to the choice of alpha in the elastic net (i.e., the mixing parameter between L1 and L2 penalties).

2. **Comparison with satellite-derived emission measures.** Compare our predictions against satellite-based estimates (e.g., Climate TRACE). Additionally, assess the quality of the methodology when using satellite-derived measures on the LHS in the elastic net (instead of EU ETS verified emissions).

3. **Comparison with EN on financials only.** Train an elastic net using only financial variables from annual accounts (revenue, assets, wage bill, etc.) as predictors — no B2B data. This serves as a benchmark in the spirit of the Trucost/Bloomberg literature that infers emissions from publicly available financial data.

4. **Robustness to zero-emissions imputation in NACE 17/18, 19, and 24.** For each of these sectors, take the share of sector-level emissions not covered by the EU ETS and distribute these residual emissions across non-EU ETS firms such that the resulting nRMSE (or another performance metric) for the EN model is the worst possible. This provides a worst-case bound on how sensitive our results are to the assumption that non-ETS firms in these sectors have zero combustion emissions.
