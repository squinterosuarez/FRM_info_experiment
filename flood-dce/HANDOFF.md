# Project handoff — UK flood-protection DCE analysis pipeline

Paste this into your first message to Claude in the terminal (or just say "read HANDOFF.md"). It picks up where the last session left off.

## What this is

A discrete choice experiment on UK flood-protection preferences. N ≈ 1,500 English adults (post-ethics-approval). Two arms, randomised: **treatment** respondents see their personal flood-risk band (from postcode) before the DCE; **control** sees national flood statistics. **Both arms answer a prior risk-belief item BEFORE the manipulation** — this is what makes the conditional analysis causal. Five attributes, 6 choice sets per respondent, 4 blocks of 6, status quo present in every set.

## Research goal

Does adaptation read as a public, welfare, club, or private good — and does learning about your own flood risk shift that reading? The core hypothesis is directional: people who learn they're at *higher* risk than expected shift one way (toward collective / targeted protection, against risk-priced contributions — "protect me but don't charge me"); people who learn they're at *lower* risk shift the opposite way (toward opt-in, accept risk-pricing — "I'm safe, let it be individualised").

## Analysis spec — current state (reconciled 2026-06-15)

Mixed multinomial logit in **apollo**. Random parameters: ASC + 8 attribute effects (diagonal normal); cost fixed; correlated as robustness.

**Primary = two layers, both fit by `run_all.R` on the same data:**

1. **ITT (main analysis):** `mean_k = mu_k + tau_k·T` — the average effect of treatment *assignment*, identified by randomisation. The headline. Attenuated (opposite-direction subgroups partly cancel), not null.
2. **Conditional ITTs (mechanism):** interact treatment with the **pre-treatment prior-gap category** (underestimator / overestimator / correct / don't-know):
   `mean_k = mu_k + (category baselines) + tau_k·T + (T × category)`. Each is a clean CATE because the category is fixed before treatment. Underestimators shift toward collective provision, overestimators the opposite, correct ≈ 0 (internal placebo), don't-knows pattern with underestimators.

This **replaced** the earlier "directional gap-size" spec as primary (which had itself replaced a plain-ITT-only plan). The gap-*size* directional spec (`dir`) and the 4-cell Treatment × NoPrior spec (`pap`) remain **wired as supplementary** (`CFG$spec_type`), but `dir` is **simulation-only**: the objective risk band is not stored alongside the stated prior, so gap *magnitude* is not estimable on the real data — only the categorical *direction* (the surprise item) is, which is what `cate` uses.

Selecting specs: `CFG$spec_type` ∈ {`itt`, `cate` (primary), `pap`, `dir` (supplementary)}; default `itt`. `run_all.R` runs the itt+cate pair; `run_mc_cate.R` gives their Monte-Carlo operating characteristics; `run_mc_paper.R` / `run_mc_boot.R` / `run_bootstrap_paper.R` cover the supplementary specs.

## Bundles (4) and WTP machinery

Bundles on A1/A2/A3 (A2 = **2 levels** since 2026-06-05: national = 1, local = 2):
- **public** = all / national / flat
- **welfare** = SQ config *(WTP reference = 0)*
- **club** = high-risk area / **local** / flat *(beneficiaries pay)*
- **private** = opt-in / local / risk-priced

WTPs via **generic part-worths** (ASC not involved); A4 dropped (held at SQ → 0); cost held at SQ. Reported: each non-welfare bundle vs welfare, all pairwise contrasts, the **conditional ITTs** on public-vs-private / public-vs-club, and the **attribute-level conditional ITTs** (national-vs-local funding, flat-vs-risk-priced cost-sharing, effectiveness). CIs: delta method (primary) and respondent-clustered bootstrap (robustness; expensive, off by default).

## Headline numbers from simulated data (Monte Carlo, M=200, N=1500)

- **ITT** public-vs-private **+£57** (power 0.58) — attenuated average, not null.
- **Conditional ITTs:** underestimators **+£124** (0.85), don't-know **+£100** (0.49), correct **£0** (0.04 — internal placebo), overestimators **−£144** (0.27, thin cell).
- Coverage ≈ nominal (0.93–0.97) throughout. Sources: `outputs/mc_summary_itt_dir.csv`, `outputs/mc_summary_cate_dir.csv`.

## Modelling decisions made (don't re-litigate; flag if user wants to revisit)

1. **SQ is fixed → ASC absorbs all SQ-level utility** (collinear by construction). Don't try to enter SQ levels through generic coefficients.
2. **Welfare bundle = SQ on A1/A2/A3** → it is the reference, WTP = 0 by definition; report others as moves away from welfare. Pairwise contrasts are the cleanest "public-vs-club good" test.
3. **A4 (effectiveness)** binary in A/B (levels 1 and 3 only); A4.2 is SQ-only and absorbed into the ASC.
4. **Cost fixed**, not random — keeps WTP well-defined.
5. **ASC fixed** (`CFG$asc_random = FALSE`) per PAP-as-written. We tested random-ASC (`TRUE`) earlier and it fit better (LL −8252 vs −8266) and gave better WTP recovery (98% vs 88%) — but pre-registration discipline wins. The fit cost goes into the WTP-space queue, not into relitigating the ASC choice.
6. **A2 collapsed from 3 levels to 2** (decided 2026-06-05): A2 is now **national taxation (1)** vs **local taxes (2)**; the old middle "national+local" level was dropped when the DCE language was simplified. Consequences:
   - The `club_mixed` variant (A2.2 = national+local) **no longer exists**. The single **`club`** bundle is now the local-funded club good (A2.2 = local, beneficiaries pay). `private` is also locally funded.
   - National-vs-local is now a single one-column part-worth (A2 effects-coded to `a2e1` only → 9 params total, was 10).
   - Code: `BUNDLES`/`PAIRWISE` in `R/00_config.R`, `target_wtps` in `04_wtp.R`, and the effects coding in `01_design.R` were all updated to the 2-level A2. **Any simulated WTP/recovery numbers produced before that date predate the collapse and were regenerated.**
7. **No-idea anchor = 2.5** (scale midpoint) for the supplementary `dir` spec — the modal stated prior was VeryLow, which would have made every update look upward, so the midpoint is doing real work. Under the primary `cate` spec the don't-know group is **its own category cell** (`catDK`) with its own conditional ITT — no imputation needed.

## Pre-registration items still open

- ~~ITT → directional change~~ **resolved**: primary is now ITT + conditional-ITT by prior-gap category (itt + cate). Keep the half-page note explaining the change for the student.
- Asymmetric mechanism: underestimator / don't-know cells confirmatory; overestimator cell pre-registered as **lower-powered / secondary**.
- The no-idea handling named as an assumption (own-cell under `cate`; midpoint anchor under `dir`).
- The cost/A5 inconsistency (PAP says A1–A5 random, then cost fixed): user is updating PAP; A5 was a leftover from a 6-attribute version.

## Code-side TODOs (sound in PAP, not yet implemented)

- **Qualtrics → database cleaning script** — the real-data bridge (see README's column contract). Not yet built.
- Treatment-on-dispersion (Δσ by arm).
- WTP-space robustness model.
- BH-FDR multiple-testing adjustment within families.
- Separate no-idea channel as robustness.

## Files

```
project/
├── README.md
├── HANDOFF.md            (this file)
├── run_all.R             primary itt+cate driver; DATABASE_RDS flag for real data
├── run_mc_cate.R         MC operating characteristics for itt + cate (primary)
├── run_mc_paper.R, run_mc_boot.R, run_bootstrap_paper.R   supplementary (pap/dir) drivers
├── run_smoke_*.R         fast smoke checks
├── make_*.R              figures (recovery/mechanism/attributes) + attribute WTP tables
├── sim_check.py          Python mirror of the DGP (legacy cross-check)
└── R/
    ├── 00_config.R       settings, TRUE parameters, bundles, spec/DGP switches
    ├── 01_design.R       locked DCE design (9-param, 2-level A2)
    ├── 02_simulate.R     DGP: respondents, priors, prior-gap categories, choices
    ├── 03_estimate_mmnl.R  apollo MMNL (spec-branching: itt / cate / pap / dir)
    ├── 04_wtp.R          bundle / pairwise / conditional-ITT / attribute WTPs
    ├── 05_subgroups.R    split-sample subgroup refits (directional-era robustness)
    └── 06_outputs.R      recovery tables + figures (skipped on real data)
```

## What to do in the first session

1. `cd` into the project folder. Confirm the structure above.
2. `R --version` — if not installed, install R (`brew install r` on macOS, or cran.r-project.org).
3. `Rscript -e 'install.packages(c("apollo","numDeriv","ggplot2"))'`. **apollo pulls compiled dependencies (RcppArmadillo, maxLik, etc.) — first install takes several minutes.**
4. `Rscript run_all.R` — fits **itt + cate** on simulated data; writes `wtp_delta_itt.csv`, `wtp_delta_cate.csv`, `recovery_wtp_itt.csv`, `recovery_wtp_cate.csv`.
5. Inspect `outputs/recovery_wtp_*.csv` (does the model recover the known true bundle WTPs?) and `outputs/wtp_delta_*.csv`.
6. **Real data:** build the cleaned database per README's column contract, then `DATABASE_RDS=path/to/data.rds Rscript run_all.R`. The recovery tables auto-skip (no known truth); the WTP tables are written.
