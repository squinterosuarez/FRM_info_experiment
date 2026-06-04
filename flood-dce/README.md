# UK flood-protection DCE — analysis pipeline (directional spec)

Simulate → fit MMNL → recover truth → bundle WTPs → tables/figures. Estimation
uses **apollo**. Built so real data drops in with one substitution post-ethics.

## Run
```r
source("run_all.R")
```
Dependencies: `apollo`, `numDeriv`, `ggplot2`. Outputs in `./outputs/`.

> Not executed here: this environment has no R/CRAN access, so the apollo fit
> has not been run. The data-generating side was validated in Python
> (`sim_check.py`): cell sizes, model-free choice patterns, and the WTP
> formulas all behave as intended. Treat the first R run as a debugging pass.

## Primary specification (directional information effects)
Each random coefficient's mean is shifted by what respondents *learned*:

`mean_k = mu_k + dt_k·T + up_k·(T·gapUp) + dn_k·(T·gapDown)`  (+ np_asc·T·noPrior on the ASC only)

- `gap = actual_risk − prior_belief`, from the **pre-elicited** prior and
  postcode risk; `gapUp/gapDown` are its positive/negative parts.
- **"No idea" respondents are updaters too.** They get an imputed prior at the
  scale midpoint (2.5) — "no stated belief → midpoint" — so learning low/very-low
  reads as a *downward* update and they flow through the same `gapUp/gapDown`
  and the same `up_k/dn_k` coefficients as everyone else. `noPrior` survives only
  as an ASC control (`np_asc`); since the ASC isn't in the bundle contrasts, it
  cancels out of the headline WTPs. (Anchoring at the modal stated prior instead
  would mis-fire: in simulation that mode is "Very Low," making every update look
  upward.) Robustness toggle: estimate a separate no-idea × learned-level channel
  instead of pooling — not yet coded.
- Control carries no gap terms (T=0) → it is the clean no-information baseline.
- `dt_k` = effect of being informed with a *correct* prior (no surprise).
- Primary estimands: `up_k`, `dn_k` for the conception attributes A1/A2/A3
  (test I.H1/I.H2), plus A4 and ASC (I.H3). This is the directional version of
  the PAP — it replaces the arm-level ITT `treatment × attribute` as primary,
  because I.H1–I.H3 predict upward and downward updaters move in *opposite*
  directions and an ITT average can cancel them.

Cost is fixed; ASC random by default (`CFG$asc_random` — a deviation from the
PAP-as-written, which writes ASC as fixed; flip if you want to match it).

## Bundles (four; welfare = reference)
Welfare = A1.3 + A2.1 + A3.2 = the status-quo configuration, so it is the
reference (its WTP is 0 by definition). The others are valued **relative to
welfare** via generic part-worths (the ASC is not involved). A4 is dropped
(held at SQ → 0 under effects coding); cost held at SQ. Club is set to A2.2
(national + local) so it stays distinct from private (A2.3, local only).
Reported: each bundle vs welfare, all pairwise contrasts, and the directional
estimands `Δ(contrast) = contrast(updater group) − contrast(control)` — the
one-number test of whether learning shifts the public/club/private reading.

## Known power constraint (see sim_check.py output)
Downward updaters — people who learn they are *safer* than they thought — are
intrinsically rare here: most people underestimate risk, you oversample
high-risk areas, and a High-band respondent cannot be a downward updater. The
stated-prior downward cell is ~32; folding in "no idea" people who learn
low/very-low (via the midpoint anchor) lifts the effective downward pool to
~120 — still well below the upward pool (~340), but estimable. **Pre-register
the asymmetry with upward as confirmatory and downward as lower-powered /
secondary.** The PAP power section should say this.

## Real-data column contract (apollo wide; one row per respondent × task)
`ID, task, block, treatment(0/1), gapUp, gapDown, noPrior(0/1),
updater_type, actual_rank, risk_high`, plus effects-coded alternative columns
`A_a1e1,A_a1e2,A_a1e3,A_a2e1,A_a2e2,A_a3e1,A_a3e2,A_a4e1,A_cost100` and the
`B_` equivalents, `av_A,av_B,av_SQ`(=1), `choice`(1=A,2=B,3=SQ); sorted by
ID,task. Replace the placeholder design in `01_design.R` with the locked design.

## Still on the PAP to-do (pre-registered, not yet coded)
Treatment-on-dispersion (Δσ by arm), WTP-space robustness model, BH-FDR within
families. The plan is sound; the code needs to catch up.
