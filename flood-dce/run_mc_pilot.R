## run_mc_pilot.R -- FAST sanity pilot of the A2-dropped pipeline. Mirrors
## run_mc_cate.R (itt + cate, same DGP/seeds) but with M=20 and draws=200, and
## writes to SEPARATE pilot caches so it does not touch the (stale) production
## mc_*_dir caches. NOT for final numbers: power/coverage are indicative only
## because draws < 500. Throwaway -- delete after inspecting.
here <- function(...) file.path("R", ...)
source(here("00_config.R"))
source(here("01_design.R"))
source(here("02_simulate.R"))
source(here("03_estimate_mmnl.R"))
source(here("04_wtp.R"))
source(here("06_outputs.R"))
source(here("07_montecarlo.R"))

M <- 20; DRAWS <- 200

CFG$spec_type <- "itt"; CFG$dgp_type <- "directional"
message("\n==== PILOT 1/2: itt spec ====")
run_mc(M = M, draws = DRAWS,
       mc_dir         = file.path(PATHS$out, "mc_itt_pilot"),
       apollo_workdir = file.path(PATHS$out, "mc_itt_pilot_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_itt_pilot.csv"))

CFG$spec_type <- "cate"; CFG$dgp_type <- "directional"
message("\n==== PILOT 2/2: cate spec ====")
run_mc(M = M, draws = DRAWS,
       mc_dir         = file.path(PATHS$out, "mc_cate_pilot"),
       apollo_workdir = file.path(PATHS$out, "mc_cate_pilot_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_cate_pilot.csv"))

cat("\nPILOT_DONE\n")
