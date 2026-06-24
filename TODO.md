# TODO

Working task list. Research-task backlog also lives in `CLAUDE.md` (`## TO-DO List`
and the pipeline tables); this file collects tasks arising from the
identification / estimator-choice discussion. See `IDENTIFYING_ASSUMPTIONS.md`
for the supporting notes.

1. **Transportability assumption in the paper.** Add a discussion of what we
   assume is transportable between the two subpopulations (ETS vs non-ETS) and
   what the CV can teach us *given* that assumption. Base it on the three-part
   primitive in `IDENTIFYING_ASSUMPTIONS.md` (copula invariance, GLO shape
   portability, free location/scale) and the metric→assumption mapping.

2. **Test the transportability assumption on IMJV data.** Add a section that
   uses the IMJV external-validation sample to test (or stress) the
   transportability assumption — e.g. whether the proxy→emissions ordering and
   the within-cell shape hold up on non-ETS plastics/food/waste firms. See
   CLAUDE.md TODO #5 (IMJV validation) for existing results.

3. **Why elastic net.** Add a discussion in the paper of why elastic net rather
   than LASSO / adaptive LASSO / etc. Draw on the estimator-choice section of
   `IDENTIFYING_ASSUMPTIONS.md` (correlated supplier design, grouping/stability,
   reduced-form proxy, ranking objective).

4. **Oracle efficiency.** Understand what oracle efficiency is and why it is not
   as relevant for us (no true sparse support, efficiency targets coefficient
   variance not ranking, binding constraint is transfer not in-sample selection).

5. **Ranking as dependent variable.** Consider the trade-offs of explicitly
   estimating a model where the dependent variable is the ranking (learning-to-
   rank: elastic-net RankSVM / pairwise logistic). Benchmark CV AUC/Spearman
   against the conditional-mean EN; weigh the weaker-assumption argument (rests
   on copula invariance alone, drops single-index) against thin within-cell
   emitter pairs.

6. **Inference on ranks (conditional on item 5).** If we go ahead with a ranking
   model, consider adding rank-inference methods from the Chicago + Romano group
   (Mogstad–Romano–Shaikh–Wilhelm and related) to put confidence sets on the
   reported rankings. Note these do not cover regularized estimation off the
   shelf — may require valid post-regularization SEs (debiasing / sample
   splitting).

7. **CV vs deployment pipeline.** Go over the CV pipeline in the paper and
   compare it, step by step, with the deployment pipeline — confirm the two use
   the proxy the same way (rank-only), that the external total is the analog of
   the oracle cell total in CV, and document any divergences.

8. **Deployment results section.** Add a section to the paper describing the
   deployment results: how many firms are classified as emitters, sectoral
   breakdown, share of inventory totals allocated, distribution of imputed
   emissions, etc.

9. **Justify the GLO choice** (= CLAUDE.md TODO #2). Add the analysis that makes
   the case for GLO as the within-cell distributional choice: the L-moments
   table (L-skewness / L-kurtosis vs candidate distributions), and a comparison
   of GLO against alternatives (GPA, GEV, etc.) on redistribution performance.
   This is what licenses object 2 (shape portability) in
   `IDENTIFYING_ASSUMPTIONS.md`.

10. **Within-size-bin proxy performance table** (= CLAUDE.md TODO #9). Compare
    three ranking predictors — B2B proxy (`proxy_mean`), revenue, NACE-based
    proxy (`proxy_tabachova`) — on both margins, conditioning on firm size via
    revenue bins (quartiles/quintiles/deciles, LOSO cutpoints). Extensive
    margin: within sector-year-bin AUC. Intensive margin: within-bin Spearman
    among emitters. Report weighted-mean and median across bins, pooled and per
    NACE 2-digit. Documents that the proxy's extensive-margin edge over revenue
    is mostly size, but it carries genuine fuel-supply signal at the intensive
    margin. Scripts: `within_revenue_bin_auc.R`, `_within_bin_spearman_emitters.R`.

11. **Sanity check: ETS firms with zero B2B purchases** (= CLAUDE.md TODO #6).
    On RMD, check whether any EU ETS firm-years in the training sample have zero
    B2B purchases (firm's anonymized VAT from `EUTL_Belgium.dta` does not appear
    as a buyer in `b2b_selected_sample.RData` for that year). If prevalent, this
    suggests M&A-driven VAT reassignments could record a firm's EUTL emissions
    under one VAT while its B2B transactions sit under another, adding noise to
    the EN training. Assess magnitude (share of ETS firm-years affected, share
    of total ETS emissions they represent).
