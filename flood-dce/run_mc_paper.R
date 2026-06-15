## =====================================================================
## run_mc_paper.R -- the two MC passes the conference paper needs.
## Both on the DIRECTIONAL DGP (the realistic gap-based mechanism):
##   Pass 1 (directional spec): headline detection -- power + CI-coverage
##           on the upward-surprise bundle contrasts (target_wtps_dir).
##   Pass 2 (PAP spec):         the masking result -- ITT/UPDATER power
##           is low because pooling upward+downward updaters cancels.
## Identical seeds => both passes fit the SAME simulated datasets, so the
## spec comparison is apples-to-apples. Per-iter cache => resumable.
##
## Launch from flood-dce/ under caffeinate:
##   caffeinate -i Rscript run_mc_paper.R
## =====================================================================
here <- function(...) file.path("R", ...)
source(here("00_config.R"))          # defaults dgp_type="directional"
source(here("01_design.R"))
source(here("02_simulate.R"))
source(here("03_estimate_mmnl.R"))
source(here("04_wtp.R"))
source(here("06_outputs.R"))         # true_theta_vec(), helpers
source(here("07_montecarlo.R"))

M <- 200; DRAWS <- 500

## ---- Pass 1: directional spec (headline) ----
CFG$spec_type <- "dir"; CFG$dgp_type <- "directional"
message("\n==== MC PASS 1/2: directional spec (headline detection) ====")
run_mc(M = M, draws = DRAWS,
       mc_dir         = file.path(PATHS$out, "mc_dir_dir"),
       apollo_workdir = file.path(PATHS$out, "mc_dir_dir_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_dir_dir.csv"))

## ---- Pass 2: PAP spec on the same directional DGP (masking) ----
CFG$spec_type <- "pap"; CFG$dgp_type <- "directional"
message("\n==== MC PASS 2/2: PAP spec on directional DGP (masking) ====")
run_mc(M = M, draws = DRAWS,
       mc_dir         = file.path(PATHS$out, "mc_pap_dir"),
       apollo_workdir = file.path(PATHS$out, "mc_pap_dir_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_pap_dir.csv"))

message("\nDONE. Headlines:")
message("  outputs/mc_summary_dir_dir.csv  (directional spec -> detection power/coverage)")
message("  outputs/mc_summary_pap_dir.csv  (PAP spec        -> masking: low ITT power)")
cat("ALL_MC_DONE\n")
