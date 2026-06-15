## =====================================================================
## run_mc_cate.R -- the two MC passes the (revised) conference paper needs,
## under the de-imputed directional DGP (don't-knows get TRUE_dk).
##   Pass 1 (itt spec):  the plain ITT -- main analysis. Attenuated average
##           effect; recovery + power on ITT_public_vs_private.
##   Pass 2 (cate spec): conditional ITTs by pre-treatment prior-gap category
##           -- the mechanism. CITT_under_* / CITT_dk_* are the headline rows.
## Identical seeds => both passes fit the SAME simulated datasets.
## Per-iter cache => resumable. The gap-size (dir) layer is intentionally
## NOT run here: it is not estimable on the real data without the risk band.
##
## Launch from flood-dce/ under caffeinate:
##   caffeinate -i Rscript run_mc_cate.R
## =====================================================================
here <- function(...) file.path("R", ...)
source(here("00_config.R"))          # defaults dgp_type="directional"
source(here("01_design.R"))
source(here("02_simulate.R"))
## NB: sourcing 03 does one source-time fit at the config-default spec; its
## outputs are ignored (we drive itt/cate explicitly below). Matches run_mc_paper.R.
source(here("03_estimate_mmnl.R"))
source(here("04_wtp.R"))
source(here("06_outputs.R"))         # true_theta_vec(), helpers
source(here("07_montecarlo.R"))

M <- 200; DRAWS <- 500

## ---- Pass 1: plain ITT (main analysis) ----
CFG$spec_type <- "itt"; CFG$dgp_type <- "directional"
message("\n==== MC PASS 1/2: itt spec (plain ITT, main analysis) ====")
run_mc(M = M, draws = DRAWS,
       mc_dir         = file.path(PATHS$out, "mc_itt_dir"),
       apollo_workdir = file.path(PATHS$out, "mc_itt_dir_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_itt_dir.csv"))

## ---- Pass 2: conditional ITT by category (mechanism) ----
CFG$spec_type <- "cate"; CFG$dgp_type <- "directional"
message("\n==== MC PASS 2/2: cate spec (conditional ITT by prior-gap category) ====")
run_mc(M = M, draws = DRAWS,
       mc_dir         = file.path(PATHS$out, "mc_cate_dir"),
       apollo_workdir = file.path(PATHS$out, "mc_cate_dir_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_cate_dir.csv"))

message("\nDONE. Headlines:")
message("  outputs/mc_summary_itt_dir.csv   (plain ITT -> attenuated average effect, power)")
message("  outputs/mc_summary_cate_dir.csv  (conditional ITTs -> CITT_under_*, CITT_dk_*)")
cat("ALL_MC_CATE_DONE\n")
