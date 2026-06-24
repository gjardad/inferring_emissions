# Identifying Assumptions for the Rank-Only Pipeline

This note states the assumptions under which the cross-validation (CV) results
are informative about out-of-sample (deployment) performance, after the
proportional-allocation levels metric (APD on `yhat ∝ proxy_mean`) is dropped.
With that step removed, the supplier-based proxy enters the pipeline **only
through ranks**:

- **Classification (extensive margin):** the rank of the proxy, via the
  selection-frequency score `p_i`.
- **Allocation (intensive margin):** the rank of the proxy among classified
  emitters → quantile position → GLO quantile function → share of the cell
  total.
- **Scale:** the sector-cell total `E_c`, taken externally from the national
  inventory (NIR), never from the model.

Notation. Within a sector-cell `c` (a `(primary_crf_group, year)` cell) and for
firm-year `i`, let `Y_i` be emissions, `x_i` the vector of supplier purchases,
`β` the elastic-net coefficient vector (estimated on training folds and applied
to held-out folds), and `P_i = x_i'β` the proxy index. Let `s ∈ {ETS, non-ETS}`
index EU-ETS treatment status. Let `R_i = rank(Y_i)` be the within-cell rank of
emissions and `q(·) = log(·)`.

---

## (i) The three-part primitive

We maintain a single structural model for the within-cell joint law of
`(Y, P)`, decomposed into three separately-interpretable objects:

`q(Y_i) = μ_{c,s} + σ_{c,s} · Z_i`,  where  `Z_i ~ GLO(θ)`,

and the dependence between emissions and the proxy is governed by a copula `C`:

`C( F_{Y|c,s}(Y_i), F_{P|c,s}(P_i) ) = C(·,·)`.

The three objects and their status:

1. **Copula `C` (dependence) — assumed common across `s`.**
   The proxy orders emissions equally well among non-ETS firms as among ETS
   firms. This is the *rank-invariance* assumption. Because the rank is a
   monotone transform of `Y`, copula invariance of `(Y, P)` is identical to
   copula invariance of `(R, P)`:
   `copula(R, P) = copula(Y, P)`.

2. **Standardized shape `θ` (GLO) — assumed common across `s`.**
   The within-cell distribution of demeaned log-emissions has GLO shape `θ`,
   with `θ` estimated on ETS / reference firms and applied to deployment
   sectors. This is the *shape-portability* assumption.

3. **Location and scale `(μ_{c,s}, σ_{c,s})` — left free across `s`.**
   The overall emission level may differ systematically between ETS and non-ETS
   firms within the same cell. Absolute scale is then pinned by the NIR cell
   total `E_c`, not assumed. This is the distribution shift we deliberately do
   **not** restrict.

Reading: we assume invariance of the two objects that can plausibly survive a
size-based selection shift (the ordering and the standardized shape), and we
let the object that we know shifts (the level) move freely, anchoring its scale
to external inventory data.

---

## (ii) Which assumption licenses which metric

Each reported CV metric transfers to deployment under exactly one of the three
objects above. Keeping this mapping explicit prevents claiming more than the
assumptions support.

| CV metric | What it measures | Assumption that licenses OOS transfer |
|---|---|---|
| **AUC** (on `p_i`) | emitter vs non-emitter discrimination | Copula `C` common across `s` (object 1) |
| **Spearman** of `(Y, yhat)` | within-cell ordering of emitters | Copula `C` common across `s` (object 1) |
| **GLO-allocation levels** (APD / RMSE on `yhat`) | within-cell shares of the cell total | Shape `θ` common (object 2) **+** copula `C` (object 1) for the ordering **+** NIR total `E_c` for scale |

Notes:

- AUC and Spearman use the proxy only through its order, so they rest on the
  copula alone. They do **not** require shape portability or any cardinal
  assumption.
- The GLO levels metric is the only one that invokes object 2. It needs the
  ordering (object 1) to place firms at quantile positions, the shape `θ`
  (object 2) to convert positions into shares, and the external total `E_c` for
  scale. It does **not** require object 3 to be common — that is the point.
- No reported metric requires the cardinal map `P → Y` to be common across `s`.
  The dropped proportional-APD metric was the one exception, which is why it is
  removed: it allocated `yhat ∝ pmax(P, 0)` and so silently required cardinal
  invariance.

The cardinal-invariance benchmark — `E[Y | X, s] = x'β` common across `s`, i.e.
`(Y, X)` independent of `s` — is strictly stronger than objects 1–2: it implies
the copula *and* the shape *and* common location/scale. We do not need it, and
because non-ETS firms are selected to be smaller emitters it is known to be
false. The pair (objects 1–2, with object 3 free) is the weaker, shift-
compatible alternative.

---

## (iii) Location/scale is unrestricted; what is testable

**Unrestricted (object 3).** `(μ_{c,s}, σ_{c,s})` may differ across ETS status.
This is deliberate: the central distribution-shift concern is that non-ETS
firms are smaller emitters, which is precisely a shift in within-cell location
(and possibly dispersion). Restricting it would re-impose the cardinal
invariance we are trying to avoid. Absolute scale is supplied by the NIR cell
total, so nothing in the pipeline asks an ETS-calibrated level map to
extrapolate to non-ETS firms.

**Testability of each object:**

- **Object 2 (shape `θ`) — partially testable.** `θ` is low-dimensional (a
  couple of L-moment ratios). We can compare the standardized shape of
  log-emissions across sectors and CRF groups *within the observed population*
  (ETS firms and the assumed-zero non-ETS sectors) to assess whether a common
  GLO shape is reasonable. This is the role of the L-moments table. It is a
  within-sample plausibility check, not a deployment-population test.
- **Object 1 (copula `C`) — partially testable within the observed population.**
  We can compare rank-dependence summaries (Spearman, AUC) of `(Y, P)` across
  observed strata — e.g. across sectors, or across firm-size bins within a
  sector — as a proxy for stability of the ordering relationship. Stability
  across observed strata is suggestive, though not dispositive, for stability
  across ETS status.
- **Object 3 (location/scale) — not tested, by design.** We do not observe
  non-ETS emissions, so the level shift is neither testable nor assumed; it is
  absorbed by demeaning and the external total.
- **Cardinal invariance — untestable in deployment.** Because non-ETS emissions
  are unobserved, any assumption that pins the cardinal `P → Y` map in the
  deployment population cannot be checked. This is a further reason to rest the
  pipeline on objects 1–2 rather than on cardinal invariance.

---

---

## Estimator choice: elastic net vs adaptive lasso

A natural objection (raised in advising) is that elastic net (EN) lacks the
**oracle property** — consistent support recovery plus efficient estimation of
the nonzero coefficients, as if the true support were known — whereas adaptive
lasso (Zou 2006) has it, and that we should therefore prefer adaptive lasso if
we "care about getting the zeros right" (which suppliers enter the index `x'β`).
The textbook fact is correct, but oracle efficiency is the wrong desideratum
for this problem, for five reasons.

1. **No true sparse support to recover.** The oracle property presumes a true
   sparse linear DGP with a well-defined support `S_0`. Our regression is a
   reduced-form proxy: suppliers do not generate emissions, fuel purchases
   correlate with fuel use, and the selected set mixes genuine fuel suppliers
   with size-correlated spurious ones. There is no structural `S_0` to recover,
   so "getting the zeros right" presupposes an object that does not exist.

2. **Oracle efficiency targets coefficient variance — irrelevant to ranking.**
   The efficiency half of the property concerns the asymptotic variance of the
   estimated nonzero `β_j`. The pipeline uses `x'β` only to order firms, and
   ranking is invariant to monotone rescalings of the index; EN's shrinkage bias
   is harmless for ordering. The property being bought is not the property the
   ranking pipeline uses.

3. **Correlated predictors — decisive.** B2B supplier purchases are highly
   correlated (firms in a sector buy from overlapping supplier sets). Adaptive
   lasso inherits lasso's behavior under collinearity: it selects one of a
   correlated group and zeros the rest, and the chosen support flips under small
   data perturbations. EN's grouping effect spreads weight across correlated
   suppliers and stabilizes the index — the reason EN was chosen. The
   irrepresentable-type condition underpinning lasso/adaptive-lasso selection
   consistency is routinely violated under strong correlation.

4. **`beta-min` cuts against small emitters.** Selection consistency requires
   nonzero coefficients bounded away from zero. Weak-but-real fuel suppliers —
   exactly the signal useful for small emitters — sit near that boundary and get
   zeroed by adaptive lasso. EN's gentler thresholding retains more of that
   signal for ordering.

5. **It does not address the binding constraint.** Oracle selection consistency
   concerns recovery of the support *in the ETS training distribution*. The
   external-validity problem is whether the mapping *transfers* to non-ETS firms
   (the copula-invariance assumption, object 1 above). The oracle property is
   silent on transfer.

Note also that the selection-frequency score `p_i` (selection across repeats) is
already a form of stability selection (Meinshausen & Bühlmann 2010) — the
principled response to selection instability under correlation, with its own
false-discovery control — which answers "which suppliers reliably matter" more
honestly than a single adaptive-lasso fit.

**Synthesis.** If selection guarantees are wanted while keeping correlated
predictors, the correct tool is the **adaptive elastic net** (Zou & Zhang 2009):
it has the oracle property in diverging-`p` settings *and* retains the grouping/
stability for correlated designs. It is a second stage on top of EN (the EN fit
supplies the initial weights), not a replacement. Under the ranking reframe,
support recovery is even less the goal, and the natural analog is regularized
*rank* estimation (elastic-net-penalized RankSVM / pairwise logistic), where the
penalty still selects suppliers but the objective orders firms.

**Recommendation.** Keep EN + stability selection (`p_i`) as primary. If the
oracle-property point is to be engaged, run adaptive elastic net as a robustness
comparison and report (a) selection stability and (b) CV ranking metrics (AUC,
Spearman) against EN; if it does not improve the ranking — likely under
collinearity — that is the demonstrated answer. Do not switch to plain adaptive
lasso: its preconditions (true sparse support, `beta-min`, irrepresentable
condition) are the ones this setting violates, and its payoff (efficient cardinal
coefficients) is not what a ranking pipeline uses.

---

## One-paragraph summary for the paper

The proxy enters the prediction pipeline only through the order it induces on
firms within a sector-year: it determines which firms are classified as
emitters and their rank among emitters, while the magnitude of within-cell
emissions is supplied by a GLO shape and the external NIR total. Accordingly,
the cross-validation results transfer to the non-ETS deployment population
under two invariance assumptions — that the rank-dependence between emissions
and the proxy (the copula) and the standardized shape of within-cell
log-emissions (the GLO parameters) are common across EU-ETS treatment status —
while the within-cell level of emissions is left free to differ and its scale
is anchored to the national inventory. We do not assume the cardinal map from
the proxy to emission levels is common across treatment status; that assumption
is known to be violated by the size-based selection into the EU ETS and is
unnecessary for any reported result.
