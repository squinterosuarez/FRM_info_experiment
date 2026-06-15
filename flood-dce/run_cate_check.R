## =====================================================================
## run_cate_check.R  — single-draw recovery check for the new specs.
## Fits spec_type="itt" (plain ITT) and spec_type="cate" (conditional ITT
## by pre-treatment prior-gap category) on ONE simulated dataset and prints
## recovered conditional ITTs next to the DGP truth.
##
## Requires TRUE_dk in config (we use TRUE_dk <- TRUE_up). dgp_type must be
## "directional". Fast settings: 100 draws, no hessian — this is a sign/
## magnitude sanity check, not the Monte Carlo.
## =====================================================================
suppressMessages({
  source("R/00_config.R")
  source("R/01_design.R")
  source("R/02_simulate.R")           # builds `database` + `resp_df` under directional DGP
  ## Pre-seed the primary-spec object so sourcing 03 does NOT auto-fit it.
  assign(paste0("model_", CFG$spec_type), list(), envir=globalenv())
  source("R/03_estimate_mmnl.R")      # loads fit_mmnl() etc.
})

stopifnot(exists("TRUE_dk"))
cat(sprintf("\nDGP=%s  N=%d  treated=%.2f\n", CFG$dgp_type, CFG$N, CFG$prop_treat))
cat("Prior-gap categories (pre-treatment):\n"); print(table(resp_df$prior_gap_cat))

## ---- fit the two new specs on the single draw -----------------------
fit_itt  <- fit_mmnl(database, spec_type="itt",  draws=100, silent=TRUE,
                     compute_hessian=FALSE, model_name="check_itt")
fit_cate <- fit_mmnl(database, spec_type="cate", draws=100, silent=TRUE,
                     compute_hessian=FALSE, model_name="check_cate")
b <- fit_cate$estimate

## ---- DGP truth for each category's conditional ITT ------------------
## correct: TRUE_dt ; under: TRUE_dt + TRUE_up*E[gapUp|under,treated] ;
## over: TRUE_dt + TRUE_dn*E[gapDown|over,treated] ; DK: TRUE_dk (=TRUE_up).
tr <- resp_df$treatment==1
mUp <- mean(resp_df$gapUp[tr & resp_df$catUnder==1])
mDn <- mean(resp_df$gapDown[tr & resp_df$catOver==1])
cat(sprintf("\nE[gapUp|under,treated]=%.2f   E[gapDown|over,treated]=%.2f\n", mUp, mDn))

cmp <- function(k) {
  tau <- b[[paste0("tau_",k)]]
  rec <- c(correct = tau,
           under   = tau + b[[paste0("dU_",k)]],
           over    = tau + b[[paste0("dO_",k)]],
           dk      = tau + b[[paste0("dDK_",k)]])
  tru <- c(correct = TRUE_dt[[k]],
           under   = TRUE_dt[[k]] + TRUE_up[[k]]*mUp,
           over    = TRUE_dt[[k]] + TRUE_dn[[k]]*mDn,
           dk      = TRUE_dk[[k]])
  data.frame(attr=k, cell=names(rec),
             truth=round(tru,3), recovered=round(rec,3),
             row.names=NULL)
}
out <- do.call(rbind, lapply(RP, cmp))
cat("\n==== Conditional ITT recovery (cate spec, single draw) ====\n")
print(out, row.names=FALSE)

cat("\n==== Plain ITT (itt spec) tau_k ====\n")
itt <- fit_itt$estimate[paste0("tau_", RP)]
print(round(itt,3))

cat("\nDK check: recovered DK conditional ITT should track TRUE_dk (=TRUE_up).\n")
cat("CHECK_DONE\n")
