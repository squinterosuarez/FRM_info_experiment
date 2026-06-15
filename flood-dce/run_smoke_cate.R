## Smoke test for the itt/cate MC chain. Tiny M/draws, throwaway cache.
here <- function(...) file.path("R", ...)
source(here("00_config.R"))
CFG$n_draws <- 50          # speed the source-time fit too
source(here("01_design.R"))
source(here("02_simulate.R"))
source(here("03_estimate_mmnl.R"))
source(here("04_wtp.R"))
source(here("06_outputs.R"))
source(here("07_montecarlo.R"))

M <- 2; DRAWS <- 50
sd_dir <- file.path(PATHS$out, "smoke_cate")
unlink(sd_dir, recursive=TRUE); dir.create(sd_dir, recursive=TRUE, showWarnings=FALSE)

show_head <- function(csv, rows) {
  s <- read.csv(csv)
  keep <- s[grepl(rows, s$quantity), c("quantity","truth","mc_mean","power_alpha05","coverage_95")]
  cat("\n----", basename(csv), "----\n"); print(keep, row.names=FALSE, digits=4)
}

CFG$spec_type <- "itt"; CFG$dgp_type <- "directional"
run_mc(M=M, draws=DRAWS,
       mc_dir=file.path(sd_dir,"itt"), apollo_workdir=file.path(sd_dir,"itt_apollo"),
       out_csv=file.path(sd_dir,"mc_itt.csv"))
show_head(file.path(sd_dir,"mc_itt.csv"), "^ITT_")

CFG$spec_type <- "cate"; CFG$dgp_type <- "directional"
run_mc(M=M, draws=DRAWS,
       mc_dir=file.path(sd_dir,"cate"), apollo_workdir=file.path(sd_dir,"cate_apollo"),
       out_csv=file.path(sd_dir,"mc_cate.csv"))
show_head(file.path(sd_dir,"mc_cate.csv"), "^CITT_")

cat("\nSMOKE_CATE_DONE\n")
