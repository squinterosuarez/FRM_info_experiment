# UK flood-protection DCE — analysis pipeline

Simulate → fit MMNL → recover truth → bundle & attribute WTPs → tables/figures.
Estimation uses **apollo**. Built so real data drops in via a documented column
contract (below).

## Run
```r
# Simulated end-to-end (validation): fits the primary ITT + cate passes
Rscript run_all.R

# Real data: point at a cleaned, apollo-ready long-format choice file (.rds)
DATABASE_RDS=path/to/cleaned_choices.rds Rscript run_all.R
```
Dependencies: `apollo`, `numDeriv`, `ggplot2`. Outputs in `./outputs/`.

`run_all.R` fits **both** primary specs on the same data and writes
`wtp_delta_itt.csv` and `wtp_delta_cate.csv` (plus `recovery_wtp_*.csv` in
simulated mode). On real data the recovery tables are skipped — there is no
known truth to recover against — but the estimated WTP tables are written.

> **Not yet scripted:** the step that turns a raw **Qualtrics export → the
> column contract below**. The analysis is ready; the cleaning/reshaping is the
> one remaining piece to build before a real-data run.

## Primary specification — ITT + conditional ITT by prior-gap category
The pre-registered primary analysis has two layers, both fit by `run_all.R`:

1. **ITT (main analysis)** — the average effect of being *assigned* the
   information treatment: `mean_k = mu_k + tau_k·T`. Identified by
   randomisation; the headline, general-hypothesis test. It is attenuated
   (opposite-direction subgroups partly cancel), not null.
2. **Conditional ITTs (the mechanism)** — interact treatment with the
   respondent's **pre-treatment prior-gap category** (underestimator /
   overestimator / correct / don't-know):
   `mean_k = mu_k + (category baselines) + tau_k·T + (T × category)`.
   Because the category is fixed *before* treatment and treatment is randomised
   within each, every conditional ITT is a clean CATE. This is where the
   directional story lives: underestimators shift toward collective provision,
   overestimators the opposite, correct estimators ≈ 0 (internal placebo), and
   don't-knows pattern with underestimators (by assumption in simulation; an
   empirical test on the real data).

Two specs remain wired as **supplementary** (select via `CFG$spec_type`):
- **`pap`** — 4-cell Treatment × NoPrior decomposition (ITT / UPDATER /
  NONUPDATER). A robustness lens: does the effect concentrate among people who
  had *no prior at all*?
- **`dir`** — the gap-**size** (dose) spec using `gapUp`/`gapDown`.
  **Simulation-only:** the objective risk band is not stored alongside the
  stated prior, so gap *magnitude* is not estimable on the real data — only the
  categorical *direction* (the surprise item) is, which is what `cate` uses.

Cost is fixed; ASC fixed by default (`CFG$asc_random` — flip to deviate from
PAP-as-written). `CFG$spec_type` default is `itt`; `CFG$dgp_type` default is
`directional`.

## Bundles (four; welfare = reference)
On A1/A2/A3 only. **A2 has two levels** since the 2026-06-05 collapse:
national taxation (1) vs local taxes / beneficiaries pay (2). Welfare = the
status-quo configuration, so its WTP is 0 by definition; the others are valued
relative to welfare via generic part-worths (the ASC is not involved). A4 is
dropped (held at SQ → 0 under effects coding); cost held at SQ.

- **public**  = all / national / flat
- **welfare** = SQ config *(reference; WTP = 0)*
- **club**    = high-risk area / **local** / flat *(beneficiaries pay)*
- **private** = opt-in / local / risk-priced

Reported: each bundle vs welfare, all pairwise contrasts, the **conditional
ITTs** on the headline public-vs-private / public-vs-club contrasts, and the
**attribute-level conditional ITTs** — national-vs-local funding,
flat-vs-risk-priced cost-sharing, and effectiveness. CIs: delta method
(primary) + respondent-clustered bootstrap (robustness).

## Power asymmetry (see the Monte Carlo)
Overestimators — people who learn they are *safer* than they thought — are
intrinsically rare (most underestimate; high-risk areas are oversampled; a
High-band respondent cannot be an overestimator), ~7% of stated-prior
respondents. Their conditional ITT is the least precise cell and is read as
directional / suggestive. The **underestimator and don't-know cells carry the
hypotheses and the statistical power**. Pre-register the asymmetry:
underestimators confirmatory, overestimators lower-powered / secondary.

## Real-data column contract (apollo *long*; one row per respondent × task)
Build a data.frame, save as `.rds`, point `DATABASE_RDS` at it; sorted by
`ID, task`, with:
- `ID, task, block, treatment` (0/1)
- `noPrior` (0/1) and the prior-gap category dummies `catUnder, catOver,
  catDK` (mutually exclusive; correct = all three 0) — from the
  surprise-direction survey item
- effects-coded alternatives `A_a1e1,A_a1e2,A_a1e3,A_a2e1,A_a3e1,A_a3e2,
  A_a4e1,A_cost100` and the `B_` equivalents — obtained by merging each
  respondent's `block × task` with the **locked design** (`01_design.R` /
  `dce_design_compact.csv`) and effects-coding the levels
- `av_A, av_B, av_SQ` (=1), `choice` (1=A, 2=B, 3=SQ)

`gapUp`/`gapDown` are needed **only** for the supplementary `dir` spec; `itt`
and `cate` do not use them. **The Qualtrics → this-contract cleaning step is not
yet scripted** — it is the bridge to a real-data run.

## Still on the PAP to-do (not yet coded)
Qualtrics→database cleaning script; treatment-on-dispersion (Δσ by arm);
WTP-space robustness model; BH-FDR within families; separate no-idea channel.
