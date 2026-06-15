## End-to-end PRIMARY analysis: ITT (main analysis) + conditional-ITT by
## pre-treatment prior-gap category (the mechanism) -- the two passes the paper
## and the PAP are built on. Both specs are fit on the SAME data and each writes
## its own WTP table. The pap (4-cell T x NoPrior) and dir (gap-size) specs are
## SUPPLEMENTARY (see run_mc_paper.R / run_mc_boot.R). For operating
## characteristics (power, coverage) under the simulated DGP, see run_mc_cate.R.
## Split-sample subgroup robustness lives in R/05_subgroups.R (run separately).
##
## SIMULATED data (default): 02_simulate.R generates `database` from the DGP.
## REAL data: set env var DATABASE_RDS to a cleaned long-format choice file
## (see the 02_simulate.R column contract); 02 is then skipped. Under itt/cate
## the don't-know category share is taken from the data itself -- no p_NP
## plug-in is needed (unlike the pap spec).
here <- function(...) file.path("R", ...)
source(here("00_config.R"))
source(here("01_design.R"))

real_path <- Sys.getenv("DATABASE_RDS")
REAL <- nzchar(real_path)
REAL_DATA <- REAL   # signal to 06_outputs.R: skip truth-based recovery tables on real data
if (REAL) {
  message(sprintf("[run_all] REAL data: %s (skipping 02_simulate.R)", real_path))
  database <- readRDS(real_path)
} else {
  source(here("02_simulate.R"))        # writes `database` from the DGP
}

source(here("03_estimate_mmnl.R"))     # fit_mmnl(); source-time auto-fit at config default
source(here("04_wtp.R"))               # target_wtps(), wtp_delta()
source(here("06_outputs.R"))           # true_theta_vec(), wtp_recovery_table()

## ---- Primary analysis: two passes (itt, then cate) on the same data ----
for (sp in c("itt", "cate")) {
  CFG$spec_type <- sp
  m <- fit_mmnl(database, model_name = sprintf("primary_%s", sp), correlated = FALSE)
  assign(paste0("model_", sp), m, envir = globalenv())
  saveRDS(m, file.path(PATHS$out, sprintf("model_%s.rds", sp)))
  write.csv(wtp_delta(m), file.path(PATHS$out, sprintf("wtp_delta_%s.csv", sp)),
            row.names = FALSE)
  if (!REAL)                           # recovery is only defined against a known truth
    write.csv(wtp_recovery_table(m),
              file.path(PATHS$out, sprintf("recovery_wtp_%s.csv", sp)), row.names = FALSE)
  message(sprintf("[run_all] spec=%-4s -> wtp_delta_%s.csv%s", sp, sp,
                  if (!REAL) sprintf(" + recovery_wtp_%s.csv", sp) else ""))
}
message("\nDONE. Primary ITT + conditional-ITT WTP tables in ./outputs.")
