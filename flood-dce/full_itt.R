## full_itt.R — run the pipeline's plain-ITT MMNL on the full sample via the
## real-data path (no simulation). draws reduced for speed.
here <- function(...) file.path("R", ...)
suppressMessages(source(here("00_config.R")))
CFG$spec_type <- "itt"; CFG$n_draws <- 300
REAL <- TRUE; REAL_DATA <- TRUE
database <- readRDS("data/full_database.rds")
suppressMessages({ source(here("01_design.R")); source(here("03_estimate_mmnl.R")); source(here("04_wtp.R")) })
cat(sprintf("\n=== ITT MMNL estimates (full sample, N=%d) ===\n", length(unique(database$ID))))
print(round(model_main$estimate, 3))
cat("\n=== ITT WTP delta (treated - control on headline contrasts) ===\n")
print(wtp_delta(model_main))
cat("\nFULL_ITT_DONE\n")
