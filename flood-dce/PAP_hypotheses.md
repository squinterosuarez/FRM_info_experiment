# Pre-analysis plan — hypotheses

UK flood-protection DCE. Two-arm randomised information treatment: **treatment**
respondents see their personalised flood-risk band before the DCE; **control**
see national flood statistics. Both arms answer a prior risk-belief item
*before* the manipulation — this is what makes the conditional analysis causal.

Estimation: mixed multinomial logit (apollo). Random ASC + attribute
coefficients (diagonal normal); cost fixed; ASC fixed per PAP-as-written.
WTP = −100·u/β_cost from generic part-worths (ASC not involved; cost held at the
status quo). All WTP contrasts and SEs by the delta method (robust VCOV);
respondent-clustered bootstrap as robustness.

**Bundles** (on targeting a1 and cost-sharing a3; welfare = status quo = WTP
reference 0):

- **public**  = protect all households / flat cost-share  (a1=1, a3=1)
- **welfare**  = status-quo config (reference; WTP ≡ 0)     (a1=3, a3=2)
- **club**    = high-risk-area targeting / flat cost-share (a1=2, a3=1)
- **private**  = opt-in only / risk-priced                 (a1=4, a3=3)

The two families below are a deliberate pair. **H1 (ITT)** answers *does
information matter on average*, identified by randomisation and agnostic to the
direction of belief revision. **H2 (conditional ITT)** answers *why and for
whom*, and is where the signed directional predictions live. The ITT is
attenuated by construction because H2's opposite-signed cells partly cancel
within it.

---

## H1. Intention-to-treat — average effect of information assignment

### Estimand

Let `T ∈ {0,1}` be randomised assignment to the personalised-risk treatment. For
a WTP contrast `C`, the population-average WTP under the MMNL mean preferences in
each arm is `WTP_C(T) = −100·u_C(μ + τ·T)/β_cost`, and

```
ITT_C = WTP_C(1) − WTP_C(0).
```

Because `T` is randomised, `ITT_C` is identified without reference to whether, or
in which direction, any respondent revised their risk belief. It is the average
effect of *offering* personalised risk information, pooled over the whole sample.

### Family 1a — Provision-regime reading (headline)

| ID  | Contrast            | Estimand (code)            | H0      | H1        |
|-----|---------------------|----------------------------|---------|-----------|
| H1a | public vs private   | `ITT_public_vs_private`    | ITT = 0 | ITT ≠ 0   |
| H1b | public vs club      | `ITT_public_vs_club`       | ITT = 0 | ITT ≠ 0   |

H1a is the primary ITT test (universal public-good vs individualised
private-good reading). H1b isolates whether any shift is about *universality*
(public) vs *beneficiary-targeting* (club), both collectively provided.

### Family 1b — Distributive and design levers

Pooled (unconditional) attribute ITTs, each the treated−control difference in
the WTP for moving one attribute between its policy-relevant levels.

| ID  | Lever         | Level contrast (a-codes)        | H0      | H1        |
|-----|---------------|---------------------------------|---------|-----------|
| H1c | Cost-sharing  | flat vs risk-priced (a3: 1 vs 3)| ITT = 0 | ITT ≠ 0   |
| H1d | Targeting     | protect-all vs opt-in (a1: 1 vs 4)| ITT = 0 | ITT ≠ 0 |
| H1e | Effectiveness | most vs least (a4: 1 vs 3)      | ITT = 0 | ITT ≠ 0   |

H1c is the "who pays" lever; H1d the coverage-breadth lever; H1e a near-placebo
channel — information about *risk* has no clear reason to move the valuation of
scheme *efficacy*, so a null supports the reading that H1a–H1d are about the
good's distributive character rather than generic engagement.

### Inference

- Two-sided Wald on each `ITT_C` at α = 0.05.
- Benjamini–Hochberg FDR *within* each family (1a, 1b) separately; H1a is the
  single pre-specified primary ITT, reported undiscounted alongside.
- **Direction: registered two-sided.** The ITT pools respondents who revise in
  opposite directions, so the average is attenuated and is signed by the
  majority sub-population rather than by a within-person prediction. Stating an
  expected sign here would import the updating-direction logic this family is
  meant to exclude; the signed predictions are pre-registered as H2.

---

## H2. Conditional ITT — effect by pre-treatment prior-gap category (mechanism)

### Estimand and identification

Each respondent is assigned, *before* the manipulation, to a mutually exclusive
prior-gap category from the surprise-direction item: **underestimator**
(`catUnder` — learned their risk is *higher* than believed), **overestimator**
(`catOver` — *lower*), **don't-know** (`catDK` — no prior), or **correct**
(accurate prior; reference cell, all dummies 0). For category `g` and contrast
`C`,

```
CITT_{g,C} = WTP_C(T=1, g) − WTP_C(T=0, g).
```

Category membership is fixed pre-treatment and `T` is randomised *within* each
category, so every `CITT_{g,C}` is a clean conditional ATE, not a selection
contrast. Mean function `mean_k = μ_k + (category baselines) + τ_k·T +
(T × category)`; the baselines absorb pre-existing cross-cell preference
differences, so the interaction carries the causal effect.

### Family 2a — Provision-regime reading, by category (headline mechanism)

| ID   | Cell × contrast              | Estimand (code)                  | H0 | H1 (predicted) | Role               |
|------|------------------------------|----------------------------------|----|----------------|--------------------|
| H2a  | under, public vs private     | `CITT_under__public_vs_private`  | =0 | > 0            | Confirmatory       |
| H2b  | don't-know, public vs private| `CITT_dk__public_vs_private`     | =0 | > 0            | Confirmatory       |
| H2c  | over, public vs private      | `CITT_over__public_vs_private`   | =0 | < 0            | Secondary (low power) |
| H2d  | correct, public vs private   | `CITT_correct__public_vs_private`| =0 | ≈ 0            | Internal placebo (two-sided) |
| H2e–h| same four cells, public vs club | `CITT_<cell>__public_vs_club` | =0 | under/dk > 0; over < 0; correct ≈ 0 | as above |

### Family 2b — Distributive and design levers, by category

The "protect me, **don't charge me**" prediction is sharpest on cost-sharing.
Attribute CITTs `ACITT_<lever>__<cell>`, treated−control within cell.

| ID   | Lever (level contrast)                  | Cell             | Estimand (code)            | H1 (predicted)        | Role         |
|------|-----------------------------------------|------------------|----------------------------|-----------------------|--------------|
| H2i  | Cost-sharing, flat vs risk-priced (a3 1v3) | under         | `ACITT_flat__under`        | > 0 (against pricing) | Confirmatory |
| H2j  | Cost-sharing, flat vs risk-priced       | dk               | `ACITT_flat__dk`           | > 0                   | Confirmatory |
| H2k  | Cost-sharing, flat vs risk-priced       | over             | `ACITT_flat__over`         | < 0 (accepts pricing) | Secondary    |
| H2l  | Targeting, protect-all vs opt-in (a1 1v4)| under/dk/over   | `ACITT_targeting__<cell>`  | under/dk > 0; over < 0| Supporting   |
| H2m  | Effectiveness, most vs least (a4 1v3)   | all cells        | `ACITT_effective__<cell>`  | ≈ 0                   | Placebo      |
| H2n  | any lever, correct                      | correct          | `ACITT_<lever>__correct`   | ≈ 0                   | Internal placebo (two-sided) |

### Family 2c — Cross-cell mechanism contrasts (directional-separation test)

The cell-vs-zero tests above do not, by themselves, establish that the cells move
in *opposite* directions. These contrasts do. They are linear combinations of the
within-cell CITTs and ride the same delta-method VCOV; emitted by
`target_wtps_cate()` as `SEP_*` / `ORD_*`.

| ID  | Contrast                | Estimand (code)                              | H0          | H1 (predicted) | Role                    |
|-----|-------------------------|----------------------------------------------|-------------|----------------|-------------------------|
| H2o | Directional separation  | `SEP_under_minus_over__public_vs_private`    | =0          | > 0            | **Primary mechanism test** |
| H2p | Monotone ordering       | `ORD_under_minus_correct__*` ≥ 0 **and** `ORD_correct_minus_over__*` ≥ 0 | not ordered | both ≥ 0       | Confirmatory pattern    |
| H2q | Placebo anchoring       | `CITT_correct__public_vs_private`            | —           | ≈ 0, inside under/over CIs | Internal validity |

H2o is the cleanest single statement of the theory: it differences out anything
common to "receiving information" and isolates the *sign-flip with surprise
direction*. A significant, correctly-signed H2o with a null H2q is the result the
paper turns on. (`SEP_*`/`ORD_*` are also produced for `public_vs_club`.)

### Inference

- **Test direction:** confirmatory cells (under, dk) and the separation/ordering
  tests (H2o, H2p) registered **one-sided** in the predicted direction —
  direction *is* the hypothesis. The **over** cell (H2c, H2k) is **secondary and
  lower-powered** (one-sided, read as directional/suggestive): it is intrinsically
  rare — a High-band respondent cannot be an overestimator, ~7% of stated-prior
  respondents. The **correct** cell (H2d, H2n, H2q) is a **two-sided** placebo
  where the prediction is the null.
- **Multiplicity:** Benjamini–Hochberg FDR *within* each family (2a, 2b)
  separately; H2a and H2o are the pre-specified primary mechanism tests,
  reported undiscounted alongside the FDR-adjusted set.
- **Don't-know cell:** that DK patterns with underestimators (H2b, H2j) is an
  **empirical test on the real data**, not an imposed assumption — the real-data
  CATE estimates DK as its own free cell.

---

## Power and reporting note

The ITT (H1) is the randomisation-identified, assumption-light headline; it is
expected to be non-null but attenuated. The conditional ITTs (H2) carry the
directional hypotheses and the inferential weight. Within H2, the
underestimator and don't-know cells (and the H2o separation contrast) are
confirmatory; the overestimator cell is pre-registered as lower-powered /
secondary; the correct cell is an internal placebo. Monte-Carlo operating
characteristics (truth / bias / power / 95% coverage) are reported for every
registered estimand, including the H2o/H2p cross-cell contrasts.
