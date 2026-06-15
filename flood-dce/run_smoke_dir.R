## Smoke test: directional spec, 9-param design. Confirms the pipeline runs
## end-to-end at 9 params and times 2 MC iterations to size the full run.
here <- function(...) file.path("R", ...)
source(here("00_config.R"))
CFG$spec_type <- "dir"        # headline detection lives in the directional spec
source(here("01_design.R"))
source(here("02_simulate.R"))
source(here("03_estimate_mmnl.R"))   # one full directional fit (single-draw recovery)
source(here("04_wtp.R"))
source(here("06_outputs.R"))         # true_theta_vec() for MC aggregation
source(here("07_montecarlo.R"))
t0 <- Sys.time()
run_mc(M = 2, draws = 500,
       mc_dir         = "outputs/mc_smoke_dir",
       apollo_workdir = "outputs/mc_smoke_dir_apollo",
       out_csv        = "outputs/mc_smoke_dir.csv")
cat(sprintf("\nSMOKE_TOTAL_SECS %.0f\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
