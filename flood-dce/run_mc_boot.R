## Long-running: Monte Carlo + bootstrap study under the PAP spec.
## Two DGPs are reported in parallel:
##   1. directional DGP — the realistic gap-based mechanism. PAP fitted on
##      misspecified data; tests whether PAP-2x2 cell ATEs recover sensible
##      policy-relevant numbers even when reality is gap-heterogeneous.
##   2. 2x2 DGP — data generated under the same 2x2 spec the model fits.
##      Tests exact recovery of α/β/γ.
##
## All loops are resumable (per-iter cache in outputs/<run>/iter_NNNN.rds).
## Run from project root, ideally under caffeinate to prevent suspension:
##
##   caffeinate -i Rscript run_mc_boot.R
##
## Wall-time estimate: each MC at M=200, draws=500 ≈ 7–10 h on a 4-core laptop;
## bootstrap at B=200, draws=500 ≈ 5–10 h. Combined ≈ 20–30 h. Resume on restart.
## =====================================================================
here <- function(...) file.path("R", ...)

## --------- Pass 1: PAP on directional DGP (realism) ---------
source(here("00_config.R"))   # defaults to spec_type="pap", dgp_type="directional"
source(here("01_design.R"))
source(here("02_simulate.R"))         # writes database under directional DGP
source(here("03_estimate_mmnl.R"))    # auto-fits model_pap on this database
source(here("04_wtp.R"))              # writes wtp_delta.csv
source(here("06_outputs.R"))          # writes recovery_*.csv
source(here("07_montecarlo.R"))

run_mc(M = 200, draws = 500,
       mc_dir         = file.path(PATHS$out, "mc_pap_dir"),
       apollo_workdir = file.path(PATHS$out, "mc_pap_dir_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_pap_dir.csv"))

## Bootstrap on the primary directional-DGP fit
boot_clustered(database, model_main, B = 200, draws = 500,
               boot_dir       = file.path(PATHS$out, "bootstrap_pap_dir"),
               apollo_workdir = file.path(PATHS$out, "bootstrap_pap_dir_apollo"))

## Preserve primary-fit artifacts before pass 2 overwrites them.
for (f in c("model_pap.rds", "recovery_params.csv", "recovery_wtp.csv",
            "wtp_delta.csv", "wtp_bootstrap.csv")) {
  src <- file.path(PATHS$out, f)
  if (!file.exists(src)) next
  dst <- file.path(PATHS$out, sub("\\.([^.]+)$", "_pap_dir.\\1", f))
  file.copy(src, dst, overwrite = TRUE)
}

## --------- Pass 2: PAP on 2x2 DGP (clean recovery) ---------
CFG$dgp_type <- "2x2"
## Wipe primary-fit objects so 03_estimate_mmnl.R refits on the new DGP
for (.o in c("model_pap","model_main")) if (exists(.o, envir=globalenv()))
  rm(list=.o, envir=globalenv())

source(here("02_simulate.R"))         # regenerates database under 2x2 DGP
source(here("03_estimate_mmnl.R"))    # refits model_pap on 2x2 data
source(here("06_outputs.R"))          # writes recovery tables for 2x2

run_mc(M = 200, draws = 500,
       mc_dir         = file.path(PATHS$out, "mc_pap_2x2"),
       apollo_workdir = file.path(PATHS$out, "mc_pap_2x2_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_pap_2x2.csv"))

## Preserve 2x2-DGP primary-fit artifacts.
for (f in c("model_pap.rds", "recovery_params.csv", "recovery_wtp.csv",
            "wtp_delta.csv")) {
  src <- file.path(PATHS$out, f)
  if (!file.exists(src)) next
  dst <- file.path(PATHS$out, sub("\\.([^.]+)$", "_pap_2x2.\\1", f))
  file.copy(src, dst, overwrite = TRUE)
}

message("\nDONE. Headlines:")
message("  outputs/mc_summary_pap_dir.csv      (MC under directional DGP)")
message("  outputs/mc_summary_pap_2x2.csv      (MC under 2x2 DGP)")
message("  outputs/wtp_bootstrap_pap_dir.csv   (bootstrap on primary fit)")
message("  outputs/{recovery_*,wtp_delta,model}_pap_dir.{csv,rds}")
message("  outputs/{recovery_*,wtp_delta,model}_pap_2x2.{csv,rds}")
