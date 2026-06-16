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
# --------------------------------------------------------------

ATTRIBUTES <- list(

  A1 = list(
    name      = "A1_excludability",
    concept   = "Excludability (who is protected by the programme)",
    card_label = "Who is protected by the programme?",
    levels    = c(
      "All households in England at any level of risk",
      "Households in high-risk areas",
      "Households in high-risk areas, priority for deprived communities",
      "Only households that opt in to receive protection"
    ),
    card_levels = c(
      "Every household in England",
      "Households in high-risk areas",
      "Households in high-risk areas, priority for deprived areas",
      "Only households that opt in"
    ),
    sq        = 3,
    ab_only   = NULL   # all four levels appear in A/B
  ),

  A2 = list(
    name      = "A2_funding_mechanism",
    concept   = "Funding mechanism (where the money comes from)",
    card_label = "Who shares the cost?",
    levels    = c(
      "Everyone in England, through national taxes \u2014 lower-risk areas help fund higher-risk areas",
      "The local area, through local taxes \u2014 each area funds its own protection"
    ),
    card_levels = c(
      "Everyone in England, through national taxes",
      "The local area, through local taxes"
    ),
    sq        = 1,
    ab_only   = NULL
  ),

  A3 = list(
    name      = "A3_distributional_fairness",
    concept   = "Distributional fairness (how each household's contribution is calculated)",
    card_label = "How is each household's share worked out?",
    levels    = c(
      "Flat contribution \u2014 every household pays the same amount",
      "Wealthier households pay more, independent of risk",
      "Households at higher risk pay more, independent of wealth"
    ),
    card_levels = c(
      "Every household pays the same",
      "Wealthier households pay more",
      "Households most at risk pay more"
    ),
    sq        = 2,
    ab_only   = NULL
  ),

  A4 = list(
    name      = "A4_effectiveness",
    concept   = "Effectiveness of the measure",
    card_label = "How effective is the programme?",
    levels    = c(
      "Risk reduced to minimum level (very low)",
      "Risk reduced by one category (e.g. high to medium)",
      "Small flood risk reduction, risk category unchanged"
    ),
    card_levels = c(
      "High - risk reduced to very low",
      "Medium - risk reduced",
      "Low - small risk reduction"
    ),
    sq        = 2,
    ab_only   = c(1, 3)   # level 2 (one-category reduction) is SQ-only
  ),

  A5 = list(
    name        = "A5_cost",
    concept     = "Additional cost to your household per year",
    card_label  = "Additional cost to your household per year",
    levels      = c("\u00a30", "\u00a375", "\u00a3150", "\u00a3300"),
    card_levels = c("\u00a30", "\u00a375", "\u00a3150", "\u00a3300"),
    cost_values = c(0, 75, 150, 300),
    is_cost     = TRUE,
    sq          = 1,
    ab_only     = c(2, 3, 4)   # \u00a30 is SQ-only
  )
)


# --------------------------------------------------------------
# 2. DESIGN PARAMETERS
# --------------------------------------------------------------

N_SETS     <- 24     # total choice sets (must be divisible by N_BLOCKS)
N_BLOCKS   <- 4      # number of blocks (respondents see N_SETS/N_BLOCKS tasks)
                     # 24 / 4 = 6 tasks per respondent
N_STARTS   <- 20     # random starts for exchange algorithm
MAX_ITER   <- 100    # max iterations per start
LAMBDA     <- 0.15   # balance penalty weight
SEED       <- 2026   # random seed for reproducibility

# --- Bayesian D-efficiency ---
N_DRAWS    <- 500

# --- Cost coding scale ---
# Cost values are divided by COST_SCALE before entering the design matrix.
# With COST_SCALE = 100, the cost coefficient is interpreted as marginal
# utility per £100.
COST_SCALE <- 100


# --------------------------------------------------------------
# 3. PRIORS
# --------------------------------------------------------------
#
# Parameter order:
#   1. ASC_SQ (alternative-specific constant for status quo)
#   2. Effects-coded columns for each non-cost attribute, in order:
#        A1 excludability:               4 A/B levels -> 3 columns
#        A2 funding mechanism:           2 A/B levels -> 1 columns
#        A3 distributional fairness:     3 A/B levels -> 2 columns
#        A4 effectiveness:               2 A/B levels (SQ at L2, outside ab_only) -> 1 column
#      Non-cost coefficients: 7
#   3. Cost as a continuous variable (per COST_SCALE units, default £100)
#
# Total parameters: 1 (ASC) + 7 (attributes) + 1 (cost) = 9
#
# NOTE on the A4 effectiveness sign: ab_only = c(1, 3) means the A/B
# variation is between L1 (very low, most effective) and L3 (small
# reduction, least effective). With L3 as the reference (-1 in effects
# coding) and L1 as +1, a POSITIVE coefficient encodes preference for
# more effective.

PRIOR_MEAN <- c(
  0.2,            # ASC_SQ: mild status quo bias
  0, 0, 0,        # A1 excludability: no prior (3 cols)
  0,            # A2 funding mechanism: no prior (1 cols)
  0, 0,           # A3 distributional fairness: no prior (2 cols)
  0.45,           # A4 effectiveness: L1 vs L3 (positive = prefer more effective)
  -0.8            # A5 cost: per £100 (negative = dislike higher cost)
)

PRIOR_SD <- c(
  0.3,            # ASC_SQ
  rep(0.4, 3),    # A1 excludability
  rep(0.4, 1),    # A2 funding mechanism
  rep(0.4, 2),    # A3 distributional fairness
  0.3,            # A4 effectiveness
  0.4             # A5 cost
)


# --------------------------------------------------------------
# 4. CHOICE CARD TEXT
# --------------------------------------------------------------

CARD_TITLE       <- "Comparison"
CARD_INSTRUCTION <- "Please compare the following flood protection programmes and choose the one you would most prefer."
CARD_COL_A       <- "Programme A"
CARD_COL_B       <- "Programme B"
CARD_COL_SQ      <- "Current System<br>(No Change)"
CHOICE_LABELS    <- c("Programme A", "Programme B", "Current System (No Change)")
