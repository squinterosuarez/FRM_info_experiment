## End-to-end pipeline on SIMULATED data.
## Spec & DGP are set in CFG (00_config.R): default spec_type="pap", dgp_type="directional".
## Real data: skip 02, point `database` at cleaned data (see 02 column
## contract + README), then run 03+.
## For the Monte Carlo + bootstrap study, see run_mc_boot.R.
here <- function(...) file.path("R", ...)
source(here("00_config.R"))
source(here("01_design.R"))
source(here("02_simulate.R"))
source(here("03_estimate_mmnl.R"))
source(here("04_wtp.R"))
source(here("05_subgroups.R"))
source(here("06_outputs.R"))
message("\nDONE. See ./outputs for tables and figures.")
