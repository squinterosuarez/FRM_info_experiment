# Qualtrics Setup Guide

## Overview

This experiment uses **32 choice sets** divided into **4 blocks** of **8 tasks** each.
Each respondent is randomly assigned to one block and completes all tasks in that block.
Each task presents two designed programmes plus a fixed status quo option.

Implementation uses **Loop & Merge**: you create one question template that repeats
8 times, with JavaScript dynamically populating the choice card each time.

## Files in this folder

| File | Purpose |
|------|---------|
| `config.R` | Attribute definitions, priors, and parameters — **edit this to change the experiment** |
| `run.R` | Master script — generates everything (do not edit) |
| `output/dce_design_full.csv` | Complete design with attribute labels |
| `output/dce_design_compact.csv` | Design with level numbers only (for analysis) |
| `output/qualtrics_choicecard.html` | HTML for the choice card question text |
| `output/qualtrics_javascript.txt` | JavaScript to paste into the question (open with a text editor, do NOT double-click) |
| `output/qualtrics_setup_guide.md` | This file |
| `output/design_diagnostics.txt` | Level balance and quality checks |

## Step 1: Embedded Data

In **Survey Flow**, add or update the **Embedded Data** element at the top with these fields (leave all values blank):

- `DCE_Block`
- `task1_cs` through `task8_cs`
- `task1_choice` through `task8_choice`

## Step 2: Block Randomisation

Add a **Randomizer** in the Survey Flow (set to "Randomly present 1, Evenly Present"). Create 4 branches:

- Branch 1: Set Embedded Data `DCE_Block` = `1`
- Branch 2: Set Embedded Data `DCE_Block` = `2`
- Branch 3: Set Embedded Data `DCE_Block` = `3`
- Branch 4: Set Embedded Data `DCE_Block` = `4`

Place this randomiser **before** the choice experiment block but **after** consent.
It must be **independent** of any treatment randomisation.

## Step 3: Set Up the Choice Experiment Block with Loop & Merge

### 3a. Create the question

In your ChoiceExperiment block, create **one** Multiple Choice question (single answer).

Set **3 answer choices**:

- 1: Programme A
- 2: Programme B
- 3: Current System (No Change)

### 3b. Add the HTML

1. Click on the question text area
2. Click the HTML source button (`<>`) in the Rich Content Editor
3. Delete everything in the editor
4. Open `output/qualtrics_choicecard.html` in a text editor and copy its contents
5. Paste into the Qualtrics HTML editor
6. Click the source button again to return to the visual editor — you should see an empty table

### 3c. Add the JavaScript

1. Click the question gear icon (or three dots) → **Add JavaScript**
2. **Delete** everything in the Qualtrics JavaScript editor (it has placeholder code)
3. Open `output/qualtrics_javascript.txt` in a **text editor** (Notepad, VS Code, etc.)
   - **Do NOT double-click the file** — on Windows this tries to execute it and shows an error
4. Copy the entire contents and paste into the Qualtrics JavaScript editor
5. Click **Save**

### 3d. Enable Loop & Merge

1. Go to the **Block Editor** (not Survey Flow)
2. Click on the ChoiceExperiment block name or its options menu
3. Click **Loop & Merge**
4. Add **8 rows** with Field 1 set as follows:

   | Loop 1 | Field 1 = `1` |
   | Loop 2 | Field 1 = `2` |
   | Loop 3 | Field 1 = `3` |
   | Loop 4 | Field 1 = `4` |
   | Loop 5 | Field 1 = `5` |
   | Loop 6 | Field 1 = `6` |
   | Loop 7 | Field 1 = `7` |
   | Loop 8 | Field 1 = `8` |

5. Optionally tick **Randomize Loop Order** to shuffle task order across respondents
6. Click **Save**

## Step 4: Verify

- [ ] Preview the survey and complete all 8 tasks
- [ ] Test with `DCE_Block` manually set to each value (1, 2, 3, 4) to confirm different choice sets appear
- [ ] After a test run, export the data and check that:
  - `task1_cs` through `task8_cs` contain choice set IDs (numbers)
  - `task1_choice` through `task8_choice` contain A, B, or SQ
- [ ] Test on mobile — the table may need font size adjustment for narrow screens

## Updating the Experiment

If you change attributes, levels, priors, or design parameters:

1. Edit `config.R`
2. Run `Rscript run.R`
3. In Qualtrics, replace the question HTML with the new `qualtrics_choicecard.html`
4. Replace the JavaScript with the new `qualtrics_javascript.txt`
5. If the number of tasks per block changed, update the Loop & Merge rows accordingly

## Data Analysis

After exporting from Qualtrics, merge with `dce_design_compact.csv`:

```r
library(tidyverse)

design <- read_csv("output/dce_design_compact.csv")
survey <- read_csv("qualtrics_export.csv")

# Reshape: one row per respondent x task
long <- survey %>%
  pivot_longer(
    cols = matches("task[0-9]+_cs"),
    names_to = "task", names_pattern = "task(\\d+)_cs",
    values_to = "choice_set"
  ) %>%
  left_join(
    survey %>% pivot_longer(
      cols = matches("task[0-9]+_choice"),
      names_to = "task", names_pattern = "task(\\d+)_choice",
      values_to = "choice"
    ) %>% select(ResponseId, task, choice),
    by = c("ResponseId", "task")
  )

# Merge with design matrix
analysis <- long %>%
  left_join(design, by = c("choice_set", "DCE_Block" = "block")) %>%
  mutate(chosen = case_when(
    choice == "A"  & alternative == "Program_A"  ~ 1,
    choice == "B"  & alternative == "Program_B"  ~ 1,
    choice == "SQ" & alternative == "Status_Quo" ~ 1,
    TRUE ~ 0
  ))
```

