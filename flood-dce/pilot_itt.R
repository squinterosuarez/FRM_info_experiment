## pilot_itt.R — run the pipeline's plain-ITT MMNL on the real pilot via the
## real-data path (no simulation). N=60 -> indicative only; random-coefficient
## SDs are weakly identified at this N. draws reduced for speed.
here <- function(...) file.path("R", ...)
suppressMessages(source(here("00_config.R")))
CFG$spec_type <- "itt"; CFG$n_draws <- 300
REAL <- TRUE; REAL_DATA <- TRUE
database <- readRDS("data/pilot_database.rds")
suppressMessages({ source(here("01_design.R")); source(here("03_estimate_mmnl.R")); source(here("04_wtp.R")) })
cat("\n=== ITT MMNL estimates (real pilot, N=60) ===\n")
print(round(model_main$estimate, 3))
cat("\n=== ITT WTP delta (treated - control on headline contrasts) ===\n")
print(wtp_delta(model_main))
cat("\nPILOT_ITT_DONE\n")
