## =====================================================================
## 07_montecarlo.R  -- Monte Carlo replication study on the simulated DGP.
##
## Purpose: turn the single-realisation recovery exercise into a proper
## power / bias / coverage study. For each of M replicates we:
##   1. set a per-iter seed,
##   2. regenerate respondents + database from 02_simulate.R,
##   3. refit the directional MMNL,
##   4. cache the estimate + delta-method WTP table to disk.
##
## On aggregation we report, per directional estimand:
##   truth, MC mean, MC bias, MC SD, sign-correct rate, power (CI excl. 0),
##   coverage (CI covers truth), n_reps, n_failed.
##
## Per-iter caching makes the loop resumable: re-running run_mc() skips
## iterations whose iter_NNNN.rds already exists.
##
## NOT auto-run. Invoke explicitly:
##   source("R/00_config.R"); source("R/01_design.R"); source("R/02_simulate.R")
##   source("R/03_estimate_mmnl.R"); source("R/04_wtp.R")
##   source("R/06_outputs.R")   # for true_theta_vec()
##   source("R/07_montecarlo.R")
##   run_mc(M=200, draws=500)
## =====================================================================

MC_DIR <- file.path(PATHS$out, "mc")
MC_APOLLO_WORKDIR <- file.path(PATHS$out, "mc_apollo_workdir")

mc_iter_file <- function(m) file.path(MC_DIR, sprintf("iter_%04d.rds", m))

run_mc <- function(M = 200, draws = CFG$n_draws, seed_base = CFG$seed,
                   n_cores = 4, mc_dir = MC_DIR,
                   apollo_workdir = MC_APOLLO_WORKDIR,
                   out_csv = file.path(PATHS$out, "mc_summary.csv")) {
  dir.create(mc_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(apollo_workdir, showWarnings = FALSE, recursive = TRUE)
  message(sprintf("[mc] M=%d  draws=%d  spec=%s  dgp=%s  cache=%s",
                  M, draws, CFG$spec_type, CFG$dgp_type, mc_dir))

  for (m in seq_len(M)) {
    f <- file.path(mc_dir, sprintf("iter_%04d.rds", m))
    if (file.exists(f)) next                            # resume: skip done iters
    t0 <- Sys.time()
    set.seed(seed_base + m)
    resp_m <- simulate_respondents()
    db_m   <- simulate_choices(resp_m, design_wide)
    fit_m  <- tryCatch(
      fit_mmnl(db_m, model_name = sprintf("mc_%04d", m),
               draws = draws, n_cores = n_cores,
               silent = TRUE, compute_hessian = FALSE,
               output_dir = apollo_workdir),
      error = function(e) structure(list(err = conditionMessage(e)),
                                    class = "mc_fit_fail"))
    if (inherits(fit_m, "mc_fit_fail")) {
      saveRDS(list(iter = m, status = "fit_fail", err = fit_m$err,
                   elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))), f)
      message(sprintf("[mc] %4d/%d  FIT_FAIL: %s", m, M, fit_m$err))
      next
    }
    vc <- tryCatch(pick_vcov(fit_m), error = function(e) NULL)
    wtp_dm <- if (!is.null(vc))
      tryCatch(delta_method(target_wtps, fit_m$estimate, vc),
               error = function(e) NULL) else NULL
    saveRDS(list(iter = m, status = "ok",
                 estimate = fit_m$estimate,
                 LLout    = fit_m$LLout,
                 wtp_dm   = wtp_dm,
                 elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))), f)
    if (m %% 5 == 0 || m <= 3)
      message(sprintf("[mc] %4d/%d  ok  (%.0fs, LL=%.1f)",
                      m, M,
                      as.numeric(difftime(Sys.time(), t0, units = "secs")),
                      if (is.null(fit_m$LLout)) NA_real_ else fit_m$LLout))
  }
  invisible(aggregate_mc(mc_dir = mc_dir, out_csv = out_csv))
}

aggregate_mc <- function(mc_dir = MC_DIR,
                         out_csv = file.path(PATHS$out, "mc_summary.csv")) {
  files <- list.files(mc_dir, pattern = "^iter_\\d+\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    message("[mc] no iter files in ", mc_dir)
    return(invisible(NULL))
  }
  iters <- lapply(files, readRDS)
  ok    <- Filter(function(x) identical(x$status, "ok"), iters)
  n_fail <- length(iters) - length(ok)
  if (length(ok) == 0) {
    message(sprintf("[mc] %d iters all failed; nothing to aggregate.", length(iters)))
    return(invisible(NULL))
  }

  truth_full <- target_wtps(true_theta_vec())
  est_mat <- do.call(rbind, lapply(ok, function(x) target_wtps(x$estimate)))
  qnames  <- colnames(est_mat)
  truth   <- truth_full[qnames]

  signs_truth <- sign(truth)
  signs_est   <- sign(est_mat)
  ## sign_correct: only meaningful for non-zero truth
  sign_correct <- vapply(seq_along(qnames), function(j) {
    if (signs_truth[j] == 0) NA_real_
    else mean(signs_est[, j] == signs_truth[j])
  }, numeric(1))

  ## power = P(95% CI excludes 0); coverage = P(95% CI covers truth)
  has_dm <- vapply(ok, function(x) !is.null(x$wtp_dm), logical(1))
  power_05 <- coverage_95 <- rep(NA_real_, length(qnames))
  if (any(has_dm)) {
    ok_dm <- ok[has_dm]
    pmat <- vapply(qnames, function(q) {
      vapply(ok_dm, function(x) {
        r <- x$wtp_dm[x$wtp_dm$quantity == q, , drop = FALSE]
        if (nrow(r) == 0 || !is.finite(r$lo) || !is.finite(r$hi)) NA_real_
        else as.numeric(!(0 >= r$lo && 0 <= r$hi))
      }, numeric(1))
    }, numeric(length(ok_dm)))
    cmat <- vapply(qnames, function(q) {
      vapply(ok_dm, function(x) {
        r <- x$wtp_dm[x$wtp_dm$quantity == q, , drop = FALSE]
        if (nrow(r) == 0 || !is.finite(r$lo) || !is.finite(r$hi)) NA_real_
        else as.numeric(truth[q] >= r$lo && truth[q] <= r$hi)
      }, numeric(1))
    }, numeric(length(ok_dm)))
    power_05    <- colMeans(pmat, na.rm = TRUE)
    coverage_95 <- colMeans(cmat, na.rm = TRUE)
  }

  summary <- data.frame(
    quantity        = qnames,
    truth           = unname(truth),
    mc_mean         = unname(colMeans(est_mat)),
    mc_bias         = unname(colMeans(est_mat) - truth),
    mc_sd           = unname(apply(est_mat, 2, sd)),
    sign_correct    = unname(sign_correct),
    power_alpha05   = unname(power_05),
    coverage_95     = unname(coverage_95),
    n_reps          = length(ok),
    n_failed        = n_fail,
    row.names = NULL, stringsAsFactors = FALSE
  )
  write.csv(summary, out_csv, row.names = FALSE)
  message(sprintf("[mc] aggregated %d ok iters (%d failed) -> %s",
                  length(ok), n_fail, out_csv))
  summary
}

message("07_montecarlo.R loaded. Invoke run_mc(M=200, draws=500).")
