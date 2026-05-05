# ==============================================================
# DCE CONFIGURATION FILE
# ==============================================================
#
# This is the ONLY file you need to edit to change the experiment.
# After editing, run:  Rscript run.R
#
# All output files will be regenerated in the /output folder.
#
# ==============================================================


# --------------------------------------------------------------
# 1. ATTRIBUTES AND LEVELS
# --------------------------------------------------------------
#
# Each attribute is a list with:
#   name        = short name for column headers
#   concept     = what this attribute captures (for documentation)
#   levels      = character vector of ALL levels (full wording, used in design CSVs)
#   card_levels = character vector of ALL levels (shortened wording for the choice card)
#   sq          = which level number is the status quo (1-indexed)
#   ab_only     = which levels appear in the designed A/B alternatives
#                 (use NULL to include all levels)
#   card_label  = the row label shown on the choice card
#
# For the COST attribute, also include:
#   cost_values = numeric vector matching the levels
#   is_cost     = TRUE
#
# IMPORTANT: the last attribute must be the cost attribute.
#
# To change the experiment, edit the levels and labels below.
# Then run: Rscript run.R
# --------------------------------------------------------------

ATTRIBUTES <- list(

  A1 = list(
    name      = "A1_excludability",
    concept   = "Excludability",
    card_label = "Who is protected?",
    levels    = c(
      "All properties in England are protected, regardless of flood risk level",
      "Only properties in high- and medium-risk areas are protected",
      "Only properties that opt in receive individual protection (e.g. flood barriers, flood doors)"
    ),
    card_levels = c(
      "All properties, regardless of flood risk",
      "Only properties in high- or medium-risk areas",
      "Only properties that opt in receive individual protection"
    ),
    sq        = 2,
    ab_only   = NULL   # all levels appear in A/B
  ),

  A2 = list(
    name      = "A2_financing",
    concept   = "Financing mechanism",
    card_label = "Where should the money come from?",
    levels    = c(
      "Fully funded through national taxation and council tax \u2014 all taxpayers contribute",
      "Co-funded: partly through national taxation, with an additional levy on council tax in areas that benefit",
      "Fully funded through an additional levy on council tax in areas that benefit \u2014 no national tax funding"
    ),
    card_levels = c(
      "Through national taxation and council tax, shared across all taxpayers",
      "Partly through national taxation, and a new dedicated council tax levy",
      "Entirely through an additional local council tax levy in areas that benefit"
    ),
    sq        = 1,
    ab_only   = NULL
  ),

  A3 = list(
    name      = "A3_contribution",
    concept   = "Contribution basis",
    card_label = "How is each household's contribution calculated?",
    levels    = c(
      "Flat contribution - every household pays the same",
      "Wealth-based \u2014 wealthier households pay more",
      "Risk-based \u2014 households in higher-risk areas pay more",
      "Based on both wealth and flood risk"
    ),
    card_levels = c(
      "Flat \u2014 every household pays the same",
      "Wealth-based \u2014 wealthier households pay more",
      "Risk-based \u2014 households in higher-risk areas pay more",
      "Based on both wealth and flood risk"
    ),
    sq        = 4,
    ab_only   = NULL
  ),

  A4 = list(
    name      = "A4_polluter",
    concept   = "Polluter-pays principle",
    card_label = "Do polluters pay extra?",
    levels    = c(
      "No polluter surcharge",
      "Firms with high greenhouse-gas emissions pay an additional surcharge"
    ),
    card_levels = c(
      "No polluter surcharge",
      "Yes \u2014 firms with high greenhouse-gas emissions pay a surcharge"
    ),
    sq        = 1,
    ab_only   = NULL
  ),

  A5 = list(
    name      = "A5_effectiveness",
    concept   = "Effectiveness",
    card_label = "How effective is the programme?",
    levels    = c(
      "Current protection maintained \u2014 flood risk stays the same as today",
      "Every property moves to at least one lower risk category",
      "Every property reaches low or very low risk"
    ),
    card_levels = c(
      "No change \u2014 flood risk stays the same as today",
      "Every property moves to at least one lower risk category",
      "Every property reaches low or very low risk"
    ),
    sq        = 1,
    ab_only   = c(2, 3)   # level 1 is SQ-only
  ),

  A6 = list(
    name        = "A6_cost",
    concept     = "Cost",
    card_label  = "Additional cost to your household per year",
    levels      = c("\u00a30", "\u00a325", "\u00a375", "\u00a3150", "\u00a3300"),
    card_levels = c("\u00a30", "\u00a325", "\u00a375", "\u00a3150", "\u00a3300"),
    cost_values = c(0, 25, 75, 150, 300),
    is_cost     = TRUE,
    sq          = 1,
    ab_only     = c(2, 3, 4, 5)   # \u00a30 is SQ-only
  )
)


# --------------------------------------------------------------
# 2. DESIGN PARAMETERS
# --------------------------------------------------------------

N_SETS     <- 32     # total choice sets (must be divisible by N_BLOCKS)
N_BLOCKS   <- 4      # number of blocks (respondents see N_SETS/N_BLOCKS tasks)
N_STARTS   <- 20     # random starts for exchange algorithm (more = slower but better)
MAX_ITER   <- 100    # max iterations per start
LAMBDA     <- 0.15   # balance penalty weight (higher = more balanced, less D-efficient)
SEED       <- 2026   # random seed for reproducibility

# --- Bayesian D-efficiency ---
# N_DRAWS = number of pseudo-MC draws from the prior used to compute the
# expected D-error. Set to 1 to recover local D-efficiency at PRIOR_MEAN.
# Larger = more accurate Bayesian objective but longer runtime (cost grows
# linearly in N_DRAWS).
# Typical values: 6 (fast), 20 (default), 50+ (rigorous).
N_DRAWS    <- 20

# --- Cost coding scale ---
# Cost values are divided by COST_SCALE before entering the design matrix.
# With COST_SCALE = 100, the cost coefficient (PRIOR_MEAN[N_PAR]) is interpreted
# as marginal utility per \u00a3100. Change this only if you also adjust the cost prior.
COST_SCALE <- 100


# --------------------------------------------------------------
# 3. PRIORS
# --------------------------------------------------------------
#
# You must specify prior means and SDs for each parameter.
# The parameter order is:
#   1. ASC_SQ (alternative-specific constant for status quo)
#   2. Effects-coded columns for each non-cost attribute, in order
#      (each attribute contributes K_ab - 1 columns where K_ab is the number
#       of levels appearing in the A/B alternatives; the last A/B-level is
#       the reference = -1 on all columns)
#   3. Cost as a continuous variable (per COST_SCALE units, default \u00a3100)
#
# IDENTIFICATION NOTE: when an attribute has `ab_only` set and SQ is at a
# level *outside* `ab_only`, that level's effect is absorbed into ASC_SQ and
# the attribute contributes only K_ab - 1 parameters (not K_full - 1).
# In this config that affects A5 (1 param instead of 2) and explains the
# total of 11 parameters below.
#
# PRIOR_MEAN: best guess of each coefficient (use 0 for no directional belief).
# PRIOR_SD:   uncertainty around that guess.
#             - Set all to 0 to use local D-efficiency only.
#             - Non-zero values trigger Bayesian D-efficiency over N_DRAWS draws.
#
# After running a pilot, replace these with estimated values.

PRIOR_MEAN <- c(
  0.2,            # ASC_SQ: mild status quo bias (now also absorbs A5=L1 effect)
  0, 0,           # A1: no prior (2 cols, 3 A/B levels)
  0, 0,           # A2: no prior (2 cols, 3 A/B levels)
  0, 0, 0,        # A3: no prior (3 cols, 4 A/B levels)
  0,              # A4: no prior (1 col, 2 A/B levels)
  -0.45,          # A5: 1 col contrasting L2 vs L3 (negative = L2 < L3)
  -0.8            # A6: cost per \u00a3100 (negative = dislike)
)

PRIOR_SD <- c(
  0.3,            # ASC_SQ
  rep(0.4, 2),    # A1
  rep(0.4, 2),    # A2
  rep(0.4, 3),    # A3
  0.4,            # A4
  0.3,            # A5 (one parameter)
  0.4             # A6
)


# --------------------------------------------------------------
# 4. CHOICE CARD TEXT
# --------------------------------------------------------------

CARD_TITLE       <- "Choice Task"
CARD_INSTRUCTION <- "Please compare the following flood protection programmes and choose the one you would most prefer."
CARD_COL_A       <- "Programme A"
CARD_COL_B       <- "Programme B"
CARD_COL_SQ      <- "Current System<br>(No Change)"
CHOICE_LABELS    <- c("Programme A", "Programme B", "Current System (No Change)")
