## Fit the no-prior separate-channel robustness MMNL and compare to pooled.
## Re-uses saved database from the primary pipeline run. Produces:
##   outputs/model_npsplit.rds
##   outputs/recovery_params_npsplit.csv  (full split-model recovery)
##   outputs/recovery_wtp_npsplit.csv     (WTP recovery incl. no-prior groups)
##   outputs/npsplit_vs_pooled_params.csv (side-by-side up_/up_np_ and dn_/dn_np_)

here <- function(...) file.path("R", ...)
source(here("00_config.R"))
source(here("01_design.R"))

## Load saved artefacts BEFORE sourcing 03/04/06 so their auto-blocks skip
## the pooled-model refit and CSV re-write.
database  <- readRDS(file.path(PATHS$data, "sim_database.rds"))
model_dir <- readRDS(file.path(PATHS$out, "model_dir.rds"))

source(here("03_estimate_mmnl.R"))    # auto-fit guarded by !exists(model_dir)
source(here("04_wtp.R"))               # auto-WTP block: cheap, just re-prints
source(here("06_outputs.R"))           # auto-recovery block: cheap, just re-prints

## Build split-model starting values: pooled estimates for shared params,
## init up_np_/dn_np_ at up_/dn_ (the truth under the pooled DGP).
start_split <- build_start(correlated=FALSE, cost_random=CFG$cost_random,
                           asc_random=CFG$asc_random, noprior_separate=TRUE)
shared <- intersect(names(start_split), names(model_dir$estimate))
start_split[shared] <- model_dir$estimate[shared]
for (k in RP) {
  start_split[paste0("up_np_",k)] <- model_dir$estimate[paste0("up_",k)]
  start_split[paste0("dn_np_",k)] <- model_dir$estimate[paste0("dn_",k)]
}

message("\n==== Fitting NO-PRIOR SEPARATE-CHANNEL model ====")
model_npsplit <- fit_mmnl(database, "mmnl_dir_npsplit",
                          noprior_separate=TRUE, start=start_split,
                          compute_hessian=FALSE)
apollo_modelOutput(model_npsplit)
saveRDS(model_npsplit, file.path(PATHS$out, "model_npsplit.rds"))

## Recovery tables for the split model
rec  <- recovery_table(model_npsplit)
write.csv(rec, file.path(PATHS$out, "recovery_params_npsplit.csv"), row.names=FALSE)
wrec <- wtp_recovery_table(model_npsplit)
write.csv(wrec, file.path(PATHS$out, "recovery_wtp_npsplit.csv"), row.names=FALSE)

## Side-by-side up_ / up_np_ and dn_ / dn_np_ with Wald test for equality.
vc <- pick_vcov(model_npsplit)
side <- function(prefix_sp, prefix_np) {
  rows <- list()
  for (k in RP) {
    a <- paste0(prefix_sp, "_", k); b <- paste0(prefix_np, "_", k)
    est_a <- model_npsplit$estimate[a]; est_b <- model_npsplit$estimate[b]
    se_a  <- sqrt(vc[a,a]);            se_b  <- sqrt(vc[b,b])
    se_d  <- sqrt(vc[a,a] + vc[b,b] - 2*vc[a,b])
    diff  <- unname(est_b - est_a)
    rows[[length(rows)+1]] <- data.frame(
      param=k,
      stated_prior=unname(est_a), sp_se=unname(se_a),
      no_prior   =unname(est_b), np_se=unname(se_b),
      diff=diff, diff_se=unname(se_d), diff_z=unname(diff/se_d)
    )
  }
  do.call(rbind, rows)
}
sb_up <- cbind(channel="upward", side("up", "up_np"))
sb_dn <- cbind(channel="downward", side("dn", "dn_np"))
sb <- rbind(sb_up, sb_dn)
write.csv(sb, file.path(PATHS$out, "npsplit_vs_pooled_params.csv"), row.names=FALSE)

cat("\n-- Side-by-side: up_ (stated-prior) vs up_np_ (no-prior) --\n")
print(sb_up, digits=3, row.names=FALSE)
cat("\n-- Side-by-side: dn_ (stated-prior) vs dn_np_ (no-prior) --\n")
print(sb_dn, digits=3, row.names=FALSE)

## Joint Wald test that all (up_np - up) and (dn_np - dn) are zero
idx_a <- c(paste0("up_",RP), paste0("dn_",RP))
idx_b <- c(paste0("up_np_",RP), paste0("dn_np_",RP))
d_vec <- model_npsplit$estimate[idx_b] - model_npsplit$estimate[idx_a]
A <- vc[idx_a, idx_a]; B <- vc[idx_b, idx_b]; AB <- vc[idx_a, idx_b]
V_d <- A + B - AB - t(AB)
chi2 <- as.numeric(t(d_vec) %*% solve(V_d) %*% d_vec)
df <- length(d_vec)
p <- pchisq(chi2, df, lower.tail=FALSE)
cat(sprintf("\nJoint Wald test of pooling (H0: up_np = up AND dn_np = dn for all k):\n  chi^2(%d) = %.2f, p = %.3f\n",
            df, chi2, p))

## DELTA_NP_* WTP rows
cat("\n-- No-prior pooling test in WTP (GBP): DELTA_NP_* = (no-prior contrast) - (stated-prior contrast) --\n")
print(wrec[grepl("^DELTA_NP_", wrec$quantity),
           c("quantity","true","estimate","se","lo","hi","covered95")],
      digits=3, row.names=FALSE)

message("\nDONE. See outputs/*_npsplit.csv and npsplit_vs_pooled_params.csv.")
