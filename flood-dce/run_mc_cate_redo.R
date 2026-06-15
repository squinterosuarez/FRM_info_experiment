## run_mc_cate_redo.R — re-run ONLY the cate pass so the per-iter delta-method
## tables include the new single-attribute conditional ITTs (ACITT_targeting/
## national/flat/effective × category). Same seeds => the existing bundle/ITT/
## conditional-ITT numbers (Table X Panels A–C) are unchanged; only new rows are
## added. The itt pass is untouched (ACITT estimands are cate-only).
##
## Clears outputs/mc_cate_dir first because the OLD cached fits stored a delta
## table without the ACITT rows; re-fitting is required to get their CIs.
## Resumable thereafter. Launch under caffeinate:
##   caffeinate -i Rscript run_mc_cate_redo.R > mc_cate_redo.log 2>&1
here <- function(...) file.path("R", ...)
source(here("00_config.R"))
source(here("01_design.R"))
source(here("02_simulate.R"))
source(here("03_estimate_mmnl.R"))
source(here("04_wtp.R"))
source(here("06_outputs.R"))
source(here("07_montecarlo.R"))

CFG$spec_type <- "cate"; CFG$dgp_type <- "directional"
unlink(file.path(PATHS$out, "mc_cate_dir"), recursive=TRUE)   # force re-fit with new estimands
message("\n==== MC cate REDO: bundle + conditional + attribute ITTs (delta CIs) ====")
run_mc(M = 200, draws = 500,
       mc_dir         = file.path(PATHS$out, "mc_cate_dir"),
       apollo_workdir = file.path(PATHS$out, "mc_cate_dir_apollo"),
       out_csv        = file.path(PATHS$out, "mc_summary_cate_dir.csv"))
cat("CATE_REDO_DONE\n")
