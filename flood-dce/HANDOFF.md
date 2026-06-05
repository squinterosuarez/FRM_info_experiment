# Project handoff — UK flood-protection DCE analysis pipeline

Paste this into your first message to Claude in the terminal (or just say "read HANDOFF.md"). It picks up where the last session left off.

## What this is

A discrete choice experiment on UK flood-protection preferences. N ≈ 1,500 English adults via Prolific (post-ethics-approval). Two arms, randomised: **treatment** respondents see their personal flood-risk band (from postcode) before the DCE; **control** sees national flood statistics. **Both arms answer a prior risk belief BEFORE the manipulation** — this is what makes the directional analysis causal. Five attributes, 6 choice sets per respondent, 4 blocks of 6, total 24 sets. Status quo present in every set with the same fixed bundle.

## Research goal

Does adaptation read as a public, welfare, club, or private good — and does learning about your own flood risk shift that reading? The core hypothesis is directional: people who learn they're at *higher* risk than expected shift one way (toward targeted protection, against risk-priced contributions — "protect me but don't charge me"); people who learn they're at *lower* risk shift the opposite way (toward opt-in, accept risk-pricing — "I'm safe, let it be individualised").

## Analysis spec — current state

Mixed multinomial logit in **apollo**. Random parameters: ASC + 8 attribute effects (diagonal normal); cost fixed; correlated as robustness.

The mean of each random coefficient is a small regression on what the respondent *learned*:

`mean_k = mu_k + dt_k·T + up_k·(T·gapUp) + dn_k·(T·gapDown)`  (+ `np_asc·T·noPrior` on the ASC only)

- `gap = actual_risk_rank − prior_belief_rank`; `gapUp/gapDown` are positive/negative parts.
- "No idea" priors get an imputed prior at the **scale midpoint (2.5)** — defensible as "no stated belief → midpoint" — so learning low/very-low reads as a downward update. They flow through the same `gapUp/gapDown` and the same `up/dn` coefficients as everyone else. `noPrior` survives only as an ASC control, which cancels in bundle contrasts.
- Control arm carries no gap terms; it's the no-information baseline.

This replaced the previous ITT (treatment-arm × attribute) as primary, because the hypotheses predict opposite-direction effects that the ITT average can cancel.

## Bundles (4) and WTP machinery

Bundles defined on A1/A2/A3 only:
- **public** = all-households / national / flat
- **welfare** = high-risk+deprived / national / wealthier-pay  *(this is the SQ config — used as the WTP reference)*
- **club** = high-risk-area / **local** / flat  *(A2.2 — pure club good, beneficiary group pays; **primary**)*
- **private** = opt-in / **local** / risk-priced

WTPs computed via **generic part-worths** (ASC not involved); A4 dropped (held at SQ → 0 under effects coding); cost held at SQ. Reported: each non-welfare bundle vs welfare, all pairwise contrasts, and the **directional estimands** `Δ(contrast) = contrast(updater group) − contrast(control)` — the one-number tests of whether learning shifts the public/club/private reading. CIs: delta method (fast) and respondent-clustered bootstrap (the planned method; expensive, off by default).

## Sanity-check numbers from simulated data

- Treated directional pools: upward ~340, no-surprise ~300, downward ~122 (32 stated-prior downward + 90 "no idea" who learned low/very-low).
- Raw model-free patterns recover the predicted self-interest directions.
- True directional estimands per the DGP: upward Δ(public−private) WTP ≈ +£100; downward Δ(public−private) WTP ≈ −£144.

## Modelling decisions made (don't re-litigate; flag if user wants to revisit)

1. **SQ is fixed → ASC absorbs all SQ-level utility** (collinear by construction). Don't try to enter SQ levels through generic coefficients.
2. **Welfare bundle = SQ on A1/A2/A3** → it is the reference, WTP = 0 by definition; report others as moves away from welfare. Pairwise contrasts are the cleanest "public-vs-club good" test.
3. **A4 (effectiveness)** binary in A/B (levels 1 and 3 only); A4.2 is SQ-only and absorbed into the ASC.
4. **Cost fixed**, not random — keeps WTP well-defined.
5. **ASC fixed** (`CFG$asc_random = FALSE`) per PAP-as-written. We tested random-ASC (`TRUE`) earlier and it fit better (LL −8252 vs −8266) and gave better WTP recovery (98% vs 88%) — but pre-registration discipline wins. The fit cost goes into the WTP-space queue, not into relitigating the ASC choice.
6. **A2 collapsed from 3 levels to 2** (decided 2026-06-05): A2 is now **national taxation (1)** vs **local taxes (2)**; the old middle "national+local" level was dropped when the DCE language was simplified. Consequences:
   - The `club_mixed` variant (A2.2 = national+local) **no longer exists**. The single **`club`** bundle is now the local-funded club good (A2.2 = local, beneficiaries pay) and is the primary club specification. `private` is also locally funded (A2.2).
   - The pure-club-vs-mixed-club head-to-head is gone; there is no longer a funding-step contrast *within* the club good. National-vs-local is now a single one-column part-worth (A2 effects-coded to `a2e1` only → 9 params total, was 10).
   - Code: `BUNDLES`/`PAIRWISE` in `R/00_config.R`, `DIR_PAIRS`/`target_wtps` in `04_wtp.R`, and the effects coding in `01_design.R` were all updated to the 2-level A2 on 2026-06-05. **Any simulated WTP/recovery numbers produced before that date predate the collapse and must be regenerated.**
7. **No-idea anchor = 2.5** (scale midpoint) — the modal stated prior was VeryLow, which would have made every update look upward, so the midpoint is doing real work and is the defensible choice. Robustness check: separate no-idea × learned-level channel, not yet coded.

## Pre-registration items still open

- ITT → directional change in the analysis plan (user is going to discuss with student).
- Asymmetric directional spec primary; downward arm pre-registered as **lower-powered / secondary**.
- The no-idea midpoint anchor named as an assumption.
- The cost/A5 inconsistency (PAP says A1–A5 random, then cost fixed): user is updating PAP themselves; A5 was a leftover from a 6-attribute version.
- **Club bundle reported with two variants** (primary A2.3 / robustness A2.2) — PAP language needs to commit to this dual-reporting approach. Code is already wired both ways.

## Code-side TODOs (sound in PAP, not yet implemented)

- Treatment-on-dispersion (Δσ by arm).
- WTP-space robustness model.
- BH-FDR multiple-testing adjustment within families.
- Separate no-idea channel as robustness.
- Optional: half-page note for the student explaining the ITT → directional change.

## Files

Place at project root: `README.md`, `run_all.R`, `sim_check.py`. Place the seven R modules in a subfolder named `R/`:

```
project/
├── README.md
├── run_all.R
├── sim_check.py       (Python mirror of the DGP; only needed because the last session had no R access)
├── HANDOFF.md         (this file)
└── R/
    ├── 00_config.R    settings, TRUE parameters, helpers, bundles
    ├── 01_design.R    placeholder choice design (REPLACE WITH LOCKED DESIGN)
    ├── 02_simulate.R  DGP: respondents, priors, gap, choices
    ├── 03_estimate_mmnl.R   apollo MMNL (directional spec, diagonal + correlated)
    ├── 04_wtp.R       bundle / pairwise / directional WTPs, delta + clustered bootstrap
    ├── 05_subgroups.R split-sample subgroup refits
    └── 06_outputs.R   recovery tables + figures
```

If they're loose in `~/Documents` right now, just move them into that structure.

## What to do in the first session

1. `cd` into the project folder. Confirm the folder structure above.
2. `R --version` — if not installed, install R (e.g., `brew install r` on macOS, or from cran.r-project.org).
3. Open R or use `Rscript -e 'install.packages(c("apollo","numDeriv","ggplot2"))'`. **apollo pulls compiled dependencies (RcppArmadillo, maxLik, etc.) — first install takes several minutes.**
4. `Rscript run_all.R` from the project folder.
5. **This is the first time the apollo fit has actually been run.** Expect debugging on apollo function-name specifics. The diagonal model is the trusted path; the correlated branch is the highest-risk block.
6. Send back the contents of `outputs/recovery_params.csv` and `outputs/recovery_wtp.csv` — those show whether the model recovers the known true parameters and bundle WTPs from the simulated data.
7. Then tackle the open items above in whatever order the user wants.
