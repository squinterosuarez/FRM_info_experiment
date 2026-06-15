## 00_config.R
## Settings, TRUE parameters, 4 bundles (welfare = reference), helpers.
##
## Four specs are wired in parallel (CFG$spec_type selects which is fit):
##   "itt"  (PRIMARY)       — mean_k = mu_k + tau_k·T   (plain ITT; main analysis)
##   "cate" (PRIMARY)       — mu_k + category baselines + tau_k·T + T×category dummies
##                            (conditional ITT by pre-treatment prior-gap category;
##                             the mechanism). itt + cate run together via
##                             run_all.R (analysis) and run_mc_cate.R (operating chars).
##   "pap"  (supplementary) — mean_k = mu_k + α_k·T + β_k·NP + γ_k·T·NP (4-cell T×NoPrior)
##   "dir"  (supplementary) — mean_k = mu_k + dt_k·T + up_k·T·gapUp + dn_k·T·gapDown (gap-based)
## Two DGPs are wired in parallel:
##   "directional" (default) — gap-based DGP using TRUE_dt/up/dn (realism story)
##   "2x2"                   — 2x2 DGP using TRUE_alpha/beta/gamma
## CFG$spec_type and CFG$dgp_type select which is fit and which generates data.


suppressMessages({ library(apollo); library(numDeriv); library(ggplot2) })

PATHS <- list(out="outputs", data="data")
for (p in PATHS) dir.create(p, showWarnings=FALSE, recursive=TRUE)

CFG <- list(
  seed=20260528, N=1500, n_blocks=4, tasks_per_blk=6, n_alts=3,
  prop_treat=0.50,
  design_path = "../output/dce_design_compact.csv",  # locked design (long format)
  ## risk + prior-belief generation
  risk_probs = c(VeryLow=0.23, Low=0.25, Medium=0.22, High=0.30),  # oversample High
  p_noidea   = 0.25,                                               # share with no prior
  shift_vals = c(-1,0,1,2), shift_probs = c(0.10,0.35,0.35,0.20),  # +ve => underestimate
  noidea_anchor = 2.5,   # imputed prior for 'no idea' = scale midpoint (uninformative)
  ## estimation
  n_draws=500, cost_random=FALSE, asc_random=FALSE,  # PAP-as-written: ASC fixed
  do_bootstrap=FALSE, boot_B=200,
  ## spec & DGP switches
  spec_type = "itt",         # fallback default; full primary analysis = itt + cate (run_all.R)
  dgp_type  = "directional"  # "directional" (default) | "2x2"
)
set.seed(CFG$seed)

ATTR <- list(a1_levels=1:4, a2_levels=1:2, a3_levels=1:3,
             a4_levels=c(1,3), cost_levels=c(75,150,300),
             sq=list(a1=3,a2=1,a3=2,a4=2,cost=0))
EC <- list(a1_base=4, a2_base=2, a3_base=3, a4_base=3)

ec_row <- function(value, levels, base, prefix) {
  nb <- setdiff(levels, base)
  out <- setNames(numeric(length(nb)), paste0(prefix,"e",seq_along(nb)))
  if (value==base) out[] <- -1 else out[paste0(prefix,"e",match(value,nb))] <- 1
  out
}

## ---- random-parameter set & TRUE values (shared with simulator) ------
RP <- c("asc","a1e1","a1e2","a1e3","a2e1","a3e1","a3e2","a4e1")

TRUE_mu <- c(asc=-0.40, a1e1=0.50,a1e2=0.00,a1e3=0.40, a2e1=0.30,
             a3e1=-0.20,a3e2=0.40, a4e1=0.50, cost=-0.80)
## being informed, no surprise (correct prior): near zero
TRUE_dt <- c(asc=-0.05, a1e1=0,a1e2=0,a1e3=0, a2e1=0, a3e1=0,a3e2=0, a4e1=0, cost=0)
## per upward step (learned higher than expected): "protect me, don't charge me"
TRUE_up <- c(asc=-0.15, a1e1=-0.10,a1e2=0.20,a1e3=0.00, a2e1=0.15,
             a3e1=0.20,a3e2=0.10, a4e1=0.15, cost=0)
## per downward step (learned lower than expected): SQ / opt-in / let risky pay
TRUE_dn <- c(asc=0.20, a1e1=-0.20,a1e2=-0.05,a1e3=0.00, a2e1=-0.15,
             a3e1=-0.15,a3e2=-0.10, a4e1=0.00, cost=0)
## 'no idea' control: single intercept on ASC. Direction for no-idea people
## now flows through the imputed-prior gap (same up/dn coefficients as everyone).
TRUE_np_asc <- -0.10
## 'no idea' people now get their OWN treatment effect (de-imputed); set equal
## to the per-step upward effect so a treated DK behaves like a one-step
## underestimator. Consumed by the directional DGP's DK branch in 02_simulate.R.
TRUE_dk <- TRUE_up
TRUE_sd <- c(asc=1.0, a1e1=0.5,a1e2=0.5,a1e3=0.4, a2e1=0.4, a3e1=0.4,a3e2=0.4, a4e1=0.4)

## ---- TRUE values for the 2x2 (T × NoPrior) DGP ----
## mean_k = mu_k + α_k·T + β_k·NoPrior + γ_k·T·NoPrior
## Encodes "non-updaters (treated who had a correct/any prior) ~ same as control;
## updaters (treated who had no prior) get the canonical self-interest shift".
##   α_k: treated-prior shift  (small "salience" effect; mirrors TRUE_dt)
##   β_k: control no-prior shift (≈ 0; no-prior people in control don't differ)
##   γ_k: additional shift on updaters (mirrors TRUE_up: "high-risk targeting,
##        national funding, flat tax")
TRUE_alpha <- c(asc=-0.05, a1e1=0,a1e2=0,a1e3=0, a2e1=0, a3e1=0,a3e2=0, a4e1=0, cost=0)
TRUE_beta  <- c(asc= 0.00, a1e1=0,a1e2=0,a1e3=0, a2e1=0, a3e1=0,a3e2=0, a4e1=0, cost=0)
TRUE_gamma <- c(asc=-0.10, a1e1=-0.10,a1e2=0.20,a1e3=0.00, a2e1=0.15,
                a3e1= 0.20,a3e2=0.10, a4e1=0.15, cost=0)

## ---- bundles on (A1,A2,A3); welfare == SQ config == reference ----
## A2 now has two levels: 1 = national taxation, 2 = local taxes (beneficiaries pay).
## The club good is funded LOCALLY (A2.2 = beneficiary group pays); public and
## welfare are nationally funded (A2.1). The old national+local "club_mixed"
## variant no longer exists (A2 collapsed from 3 -> 2 levels).
BUNDLES <- list(
  public     = c(a1=1, a2=1, a3=1),   # all / national / flat
  welfare    = c(a1=3, a2=1, a3=2),   # SQ config (reference; WTP defined = 0)
  club       = c(a1=2, a2=2, a3=1),   # high-risk area / LOCAL / flat
  private    = c(a1=4, a2=2, a3=3)    # opt-in / local / risk-priced
)
BUNDLE_REF <- "welfare"
PAIRWISE <- list(c("public","club"), c("public","private"), c("club","private"))

message(sprintf("00_config.R loaded.  spec_type=%s  dgp_type=%s",
                CFG$spec_type, CFG$dgp_type))
