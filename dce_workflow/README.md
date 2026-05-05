# DCE Workflow — Flood Risk Management Choice Experiment

## What this is

A self-contained pipeline that generates a D-efficient Discrete Choice Experiment (DCE) design and produces all files needed to implement it in Qualtrics. If you change the attributes, levels, or design parameters, re-running one command regenerates everything.

## Quick start

```bash
# 1. Make sure R is installed (R 4.x with MASS package)
# 2. Navigate to this folder
cd dce_workflow

# 3. Run the pipeline
Rscript run.R

# 4. All output files appear in output/
```

## Project structure

```
dce_workflow/
├── config.R                  ← EDIT THIS to change the experiment
├── run.R                     ← Generates everything (do not edit)
├── README.md                 ← This file
└── output/                   ← Generated files (do not edit by hand)
    ├── dce_design_full.csv
    ├── dce_design_compact.csv
    ├── qualtrics_choicecard.html
    ├── qualtrics_javascript.js
    ├── qualtrics_setup_guide.md
    └── design_diagnostics.txt
```

## How to change the experiment

**All changes go in `config.R`.** The file is heavily commented. The main things you might change:

| What | Where in config.R | Notes |
|------|-------------------|-------|
| Add/remove/rename attributes | `ATTRIBUTES` list | Each attribute is a named list element |
| Change level wording | `levels` vector inside each attribute | |
| Change which levels are SQ-only | `ab_only` vector | Set to `NULL` if all levels appear in A/B |
| Change the status quo level | `sq` field | 1-indexed into the levels vector |
| Change cost values | `cost_values` in the cost attribute | Must match the `levels` vector |
| Change choice card labels | `SQ_CARD_LABELS`, `CARD_*` variables | Short text shown to respondents |
| Change number of choice sets | `N_SETS` | Must be divisible by `N_BLOCKS` |
| Change number of blocks | `N_BLOCKS` | More blocks = fewer tasks per respondent |
| Change priors | `PRIOR_MEAN`, `PRIOR_SD` | Update after pilot data |
| Change algorithm settings | `N_STARTS`, `MAX_ITER`, `LAMBDA` | More starts = slower but better design |

After editing, run `Rscript run.R` and replace the HTML/JS in Qualtrics with the new output files.

**Important:** If you change the number or names of attributes, you must also update `PRIOR_MEAN` and `PRIOR_SD` to match the new parameter count. The script will throw an error if they don't match, telling you exactly how many parameters are expected.

## Current experiment design

| Attribute | Levels | In A/B |
|-----------|--------|--------|
| A1 Excludability | All / Higher-risk (SQ) / Opt-in | 3 |
| A2 Financing | General tax (SQ) / Co-funded / Local levy | 3 |
| A3 Contribution basis | Flat / Wealth / Risk / Wealth+risk (SQ) | 4 |
| A4 Polluter pays | No (SQ) / Yes | 2 |
| A5 Effectiveness | Current (SQ only) / One step lower / Low risk | 2 |
| A6 Cost | £0 (SQ only) / £25 / £75 / £150 / £300 | 4 |

Design space: 3 × 3 × 4 × 2 × 2 × 4 = **576 profiles**

32 choice sets, 4 blocks of 8 tasks, 12 parameters (incl. ASC).

## Dependencies

- **R** (version 4.x)
- **MASS** package (pre-installed with base R)
- No other packages required.

## Qualtrics implementation

See `output/qualtrics_setup_guide.md` for detailed instructions. Summary:

1. Add embedded data fields (`DCE_Block`, `task1_cs`–`task8_cs`, `task1_choice`–`task8_choice`)
2. Add a block randomiser (assigns `DCE_Block` = 1/2/3/4)
3. Create 8 Multiple Choice questions using the HTML and JavaScript from the output files
4. Change `var taskNum = 1;` to the correct number in each question's JavaScript
