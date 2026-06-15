## =====================================================================
## run_bootstrap_paper.R -- respondent-clustered bootstrap on the
## DIRECTIONAL fit (a SUPPLEMENTARY spec; itt + cate are primary). Gives empirical clustered
## CIs for absolute bundle WTPs + directional Delta-contrasts, to sit
## against the delta-method CIs (which run ~16-33% narrow on WTP LEVELS).
## B=200, draws=500. Per-rep cache in outputs/bootstrap_dir/ => resumable.
## Runs AFTER the MC (chained on ALL_MC_DONE) to avoid CPU contention.
## =====================================================================
here <- function(...) file.path("R", ...)
source(here("00_config.R"))
CFG$spec_type <- "dir"; CFG$dgp_type <- "directional"   # supplementary spec/DGP
source(here("01_design.R"))
source(here("02_simulate.R"))        # representative dataset (seed=CFG$seed)
source(here("03_estimate_mmnl.R"))   # auto-fits model_dir -> model_main
source(here("04_wtp.R"))
source(here("06_outputs.R"))

boot_clustered(database, model_main, B = 200, draws = 500,
               boot_dir       = file.path(PATHS$out, "bootstrap_dir"),
               apollo_workdir = file.path(PATHS$out, "bootstrap_dir_apollo"))

## Preserve under a spec-tagged name (aggregate writes wtp_bootstrap.csv).
src <- file.path(PATHS$out, "wtp_bootstrap.csv")
if (file.exists(src))
  file.copy(src, file.path(PATHS$out, "wtp_bootstrap_dir.csv"), overwrite = TRUE)

message("\nDONE. outputs/wtp_bootstrap_dir.csv (clustered CIs on directional fit)")
cat("BOOTSTRAP_DONE\n")
