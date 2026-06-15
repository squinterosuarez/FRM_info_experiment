## =====================================================================
## 04_wtp.R  (directional version)
## Bundle WTPs via GENERIC part-worths (ASC not involved), A4 dropped
## (held at SQ -> 0 under effects coding), cost at SQ. Welfare = reference.
## Reported per updater GROUP, plus pairwise contrasts, plus the directional
## estimands (how a contrast moves between groups). CIs: delta method (fast)
## and respondent-clustered bootstrap (CFG$do_bootstrap).
## WTP(GBP) = -100 * (utility) / b_cost.
## =====================================================================
GBP <- 100

## ====================================================================
## GROUPS define the "whose preference are we computing" cell for WTP.
## Two parallel sets, keyed by spec_type:
##   GROUPS_DIR  — directional spec (gap-based)
##   GROUPS_PAP  — 4-cell PAP spec (T × NoPrior); supplementary (itt + cate are primary)
## The active `GROUPS` is set from CFG$spec_type at source time below.
## ====================================================================
GROUPS_DIR <- list(
  control      = list(treatment=0, gapUp=0, gapDown=0),
  treated_none = list(treatment=1, gapUp=0, gapDown=0),
  upward1      = list(treatment=1, gapUp=1, gapDown=0),
  upward2      = list(treatment=1, gapUp=2, gapDown=0),
  downward1    = list(treatment=1, gapUp=0, gapDown=1),
  upward1_np   = list(treatment=1, gapUp=1, gapDown=0, noPrior=1),
  downward1_np = list(treatment=1, gapUp=0, gapDown=1, noPrior=1)
)
GROUPS_PAP <- list(
  ctl_prior     = list(treatment=0, noPrior=0),  # control, had prior
  ctl_noprior   = list(treatment=0, noPrior=1),  # control, no prior
  treat_prior   = list(treatment=1, noPrior=0),  # non-updater
  treat_noprior = list(treatment=1, noPrior=1)   # updater
)
## ITT spec: just treated vs control (plain ITT, the main analysis).
GROUPS_ITT <- list(
  control = list(treatment=0),
  treated = list(treatment=1)
)
## CATE spec: treatment × pre-treatment prior-gap category. "correct" is the
## reference cell (all category dummies 0); the conditional ITT for each
## category is treated_cell − control_cell.
GROUPS_CATE <- list(
  ctl_correct = list(treatment=0),
  ctl_under   = list(treatment=0, catUnder=1),
  ctl_over    = list(treatment=0, catOver=1),
  ctl_dk      = list(treatment=0, catDK=1),
  trt_correct = list(treatment=1),
  trt_under   = list(treatment=1, catUnder=1),
  trt_over    = list(treatment=1, catOver=1),
  trt_dk      = list(treatment=1, catDK=1)
)
GROUPS <- switch(CFG$spec_type, pap=GROUPS_PAP, itt=GROUPS_ITT,
                 cate=GROUPS_CATE, GROUPS_DIR)

p0 <- function(theta, nm) if (nm %in% names(theta)) unname(theta[[nm]]) else 0  # absent -> 0

## mean_k computes the random-coefficient mean for attribute k under group g.
## Branches on CFG$spec_type so 04_wtp.R is spec-agnostic.
mean_k <- function(theta, k, g, spec_type = CFG$spec_type) {
  T  <- g$treatment
  NP <- if (is.null(g$noPrior)) 0 else g$noPrior
  U  <- if (is.null(g$catUnder)) 0 else g$catUnder
  O  <- if (is.null(g$catOver))  0 else g$catOver
  DK <- if (is.null(g$catDK))    0 else g$catDK
  if (spec_type == "itt") {
    p0(theta, paste0("mu_",k)) + T * p0(theta, paste0("tau_",k))
  } else if (spec_type == "cate") {
    p0(theta, paste0("mu_",k)) +
      U*p0(theta,paste0("cU_",k)) + O*p0(theta,paste0("cO_",k)) + DK*p0(theta,paste0("cDK_",k)) +
      T  * p0(theta,paste0("tau_",k)) +
      T*U*p0(theta,paste0("dU_",k)) + T*O*p0(theta,paste0("dO_",k)) + T*DK*p0(theta,paste0("dDK_",k))
  } else if (spec_type == "pap") {
    p0(theta, paste0("mu_",k)) +
      T  * p0(theta, paste0("alpha_",k)) +
      NP * p0(theta, paste0("beta_", k)) +
      T*NP * p0(theta, paste0("gamma_",k))
  } else {  # "dir"
    up_nm <- if (NP==1 && paste0("up_np_",k) %in% names(theta)) paste0("up_np_",k) else paste0("up_",k)
    dn_nm <- if (NP==1 && paste0("dn_np_",k) %in% names(theta)) paste0("dn_np_",k) else paste0("dn_",k)
    gU <- if (is.null(g$gapUp))   0 else g$gapUp
    gD <- if (is.null(g$gapDown)) 0 else g$gapDown
    p0(theta, paste0("mu_",k)) +
      T*( p0(theta,paste0("dt_",k)) + p0(theta,up_nm)*gU + p0(theta,dn_nm)*gD )
  }
}

partworths <- function(theta, g) {
  G <- function(k) mean_k(theta, k, g)
  a1 <- c(G("a1e1"),G("a1e2"),G("a1e3")); a1 <- c(a1, -sum(a1))     # levels 1..4
  a2 <- G("a2e1");                        a2 <- c(a2, -a2)          # levels 1..2
  a3 <- c(G("a3e1"),G("a3e2"));           a3 <- c(a3, -sum(a3))     # 1..3
  a4 <- G("a4e1");                        a4 <- c(a4, -a4)          # [1]=most eff (lvl1), [2]=least (lvl3)
  list(a1=a1, a2=a2, a3=a3, a4=a4, cost=G("cost"))
}

## generic utility of a bundle (A1,A2,A3 only)
bundle_V <- function(pw, bundle) pw$a1[bundle["a1"]] + pw$a2[bundle["a2"]] + pw$a3[bundle["a3"]]

## WTP of bundle vs the welfare reference, for a given group
bundle_wtp <- function(theta, bundle_name, g) {
  pw <- partworths(theta, g)
  unname(-GBP * (bundle_V(pw, BUNDLES[[bundle_name]]) - bundle_V(pw, BUNDLES[[BUNDLE_REF]])) / pw$cost)
}
## WTP of pairwise contrast (b1 vs b2) for a group
pair_wtp <- function(theta, b1, b2, g) {
  pw <- partworths(theta, g)
  unname(-GBP * (bundle_V(pw, BUNDLES[[b1]]) - bundle_V(pw, BUNDLES[[b2]])) / pw$cost)
}
## WTP (£/yr) of a single-attribute level contrast (hi vs lo) within a group.
## Used for the distributive estimands: a2 national(1) vs local(2) funding,
## a3 flat(1) vs risk-priced(3) cost-sharing.
attr_contrast_wtp <- function(theta, g, attr, hi, lo) {
  pw <- partworths(theta, g)
  unname(-GBP * (pw[[attr]][hi] - pw[[attr]][lo]) / pw$cost)
}

## ---- spec-specific target vectors ----------------------------------
## DIR_PAIRS are the three headline bundle contrasts the abstract cares about.
DIR_PAIRS <- list(c("public","club"), c("public","private"))

target_wtps_dir <- function(theta) {
  out <- numeric(0)
  non_ref <- setdiff(names(BUNDLES), BUNDLE_REF)
  for (gname in names(GROUPS_DIR)) for (b in non_ref)
    out[sprintf("%s__vs_welfare__%s", b, gname)] <- bundle_wtp(theta, b, GROUPS_DIR[[gname]])
  for (gname in names(GROUPS_DIR)) for (pr in PAIRWISE)
    out[sprintf("%s_vs_%s__%s", pr[1], pr[2], gname)] <-
      pair_wtp(theta, pr[1], pr[2], GROUPS_DIR[[gname]])
  for (gname in c("upward1","downward1","treated_none"))
    for (pr in DIR_PAIRS) {
      d <- pair_wtp(theta, pr[1], pr[2], GROUPS_DIR[[gname]]) -
           pair_wtp(theta, pr[1], pr[2], GROUPS_DIR[["control"]])
      out[sprintf("DELTA_%s_vs_%s__%s_minus_control", pr[1], pr[2], gname)] <- d
    }
  for (gname in c("upward1","downward1"))
    for (pr in DIR_PAIRS) {
      g_np <- paste0(gname, "_np")
      d <- pair_wtp(theta, pr[1], pr[2], GROUPS_DIR[[g_np]]) -
           pair_wtp(theta, pr[1], pr[2], GROUPS_DIR[[gname]])
      out[sprintf("DELTA_NP_%s_vs_%s__%s_np_minus_stated", pr[1], pr[2], gname)] <- d
    }
  out
}

## PAP estimands: bundle/pairwise WTPs in each of 4 cells, plus
##   ITT_X         = Pr(NP)·(treat_noprior − ctl_noprior) + Pr(¬NP)·(treat_prior − ctl_prior)
##   UPDATER_X     = treat_noprior − ctl_noprior
##   NONUPDATER_X  = treat_prior   − ctl_prior
## for the headline contrasts. p_NP defaults to CFG$p_noidea (the population
## marginal); override on real data by computing the sample share.
target_wtps_pap <- function(theta, p_NP = CFG$p_noidea) {
  out <- numeric(0)
  non_ref <- setdiff(names(BUNDLES), BUNDLE_REF)
  for (gname in names(GROUPS_PAP)) for (b in non_ref)
    out[sprintf("%s__vs_welfare__%s", b, gname)] <- bundle_wtp(theta, b, GROUPS_PAP[[gname]])
  for (gname in names(GROUPS_PAP)) for (pr in PAIRWISE)
    out[sprintf("%s_vs_%s__%s", pr[1], pr[2], gname)] <-
      pair_wtp(theta, pr[1], pr[2], GROUPS_PAP[[gname]])
  for (pr in DIR_PAIRS) {
    upd  <- pair_wtp(theta, pr[1], pr[2], GROUPS_PAP[["treat_noprior"]]) -
            pair_wtp(theta, pr[1], pr[2], GROUPS_PAP[["ctl_noprior"]])
    nupd <- pair_wtp(theta, pr[1], pr[2], GROUPS_PAP[["treat_prior"]]) -
            pair_wtp(theta, pr[1], pr[2], GROUPS_PAP[["ctl_prior"]])
    itt  <- p_NP*upd + (1 - p_NP)*nupd
    out[sprintf("UPDATER_%s_vs_%s",     pr[1], pr[2])] <- upd
    out[sprintf("NONUPDATER_%s_vs_%s",  pr[1], pr[2])] <- nupd
    out[sprintf("ITT_%s_vs_%s",         pr[1], pr[2])] <- itt
  }
  out
}

## ITT estimands: bundle/pairwise WTPs in control & treated, plus the plain
## ITT delta = treated − control on the headline contrasts.
target_wtps_itt <- function(theta) {
  out <- numeric(0)
  non_ref <- setdiff(names(BUNDLES), BUNDLE_REF)
  for (gname in names(GROUPS_ITT)) for (b in non_ref)
    out[sprintf("%s__vs_welfare__%s", b, gname)] <- bundle_wtp(theta, b, GROUPS_ITT[[gname]])
  for (gname in names(GROUPS_ITT)) for (pr in PAIRWISE)
    out[sprintf("%s_vs_%s__%s", pr[1], pr[2], gname)] <-
      pair_wtp(theta, pr[1], pr[2], GROUPS_ITT[[gname]])
  for (pr in DIR_PAIRS) {
    d <- pair_wtp(theta, pr[1], pr[2], GROUPS_ITT[["treated"]]) -
         pair_wtp(theta, pr[1], pr[2], GROUPS_ITT[["control"]])
    out[sprintf("ITT_%s_vs_%s", pr[1], pr[2])] <- d
  }
  out
}

## CATE estimands: bundle/pairwise WTPs in each treatment×category cell, plus
## the conditional ITT (treated_cell − control_cell) for each category on the
## headline contrasts. CITT_under_* and CITT_dk_* are the ones the paper reads.
CATE_CELLS <- list(correct=c("trt_correct","ctl_correct"),
                   under  =c("trt_under",  "ctl_under"),
                   over   =c("trt_over",   "ctl_over"),
                   dk     =c("trt_dk",     "ctl_dk"))
## Single-attribute conditional ITTs (treated − control by category), on the
## policy-relevant level contrast for each attribute. national/flat are the
## distributive levers; effective = scheme efficacy; targeting = coverage breadth.
ATTR_CITTS <- list(
  targeting = list(attr="a1", hi=1, lo=4),  # all households vs opt-in only
  national  = list(attr="a2", hi=1, lo=2),  # national (cross-subsidy) vs local
  flat      = list(attr="a3", hi=1, lo=3),  # flat vs risk-priced
  effective = list(attr="a4", hi=1, lo=2)   # most vs least effective (levels 1 vs 3)
)
target_wtps_cate <- function(theta) {
  out <- numeric(0)
  non_ref <- setdiff(names(BUNDLES), BUNDLE_REF)
  for (gname in names(GROUPS_CATE)) for (b in non_ref)
    out[sprintf("%s__vs_welfare__%s", b, gname)] <- bundle_wtp(theta, b, GROUPS_CATE[[gname]])
  for (gname in names(GROUPS_CATE)) for (pr in PAIRWISE)
    out[sprintf("%s_vs_%s__%s", pr[1], pr[2], gname)] <-
      pair_wtp(theta, pr[1], pr[2], GROUPS_CATE[[gname]])
  for (pr in DIR_PAIRS) for (cl in names(CATE_CELLS)) {
    gg <- CATE_CELLS[[cl]]
    d <- pair_wtp(theta, pr[1], pr[2], GROUPS_CATE[[gg[1]]]) -
         pair_wtp(theta, pr[1], pr[2], GROUPS_CATE[[gg[2]]])
    out[sprintf("CITT_%s__%s_vs_%s", cl, pr[1], pr[2])] <- d
  }
  for (an in names(ATTR_CITTS)) {
    a <- ATTR_CITTS[[an]]
    for (cl in names(CATE_CELLS)) {
      gg <- CATE_CELLS[[cl]]
      out[sprintf("ACITT_%s__%s", an, cl)] <-
        attr_contrast_wtp(theta, GROUPS_CATE[[gg[1]]], a$attr, a$hi, a$lo) -
        attr_contrast_wtp(theta, GROUPS_CATE[[gg[2]]], a$attr, a$hi, a$lo)
    }
  }
  out
}

## Dispatch on CFG$spec_type so downstream code (delta_method, bootstrap,
## montecarlo) can call target_wtps() generically.
target_wtps <- function(theta) {
  switch(CFG$spec_type,
         pap  = target_wtps_pap(theta),
         itt  = target_wtps_itt(theta),
         cate = target_wtps_cate(theta),
         target_wtps_dir(theta))
}

delta_method <- function(fun, est, vcov) {
  est <- est[colnames(vcov)]; pt <- fun(est)
  J <- numDeriv::jacobian(fun, est)
  se <- sqrt(pmax(0, diag(J %*% vcov %*% t(J))))
  data.frame(quantity=names(pt), estimate=as.numeric(pt), se=se,
             lo=pt-1.96*se, hi=pt+1.96*se, row.names=NULL)
}
## prefer robust (sandwich) -> Hessian -> BHHH. With hessianRoutine="none"
## apollo fills varcov/robvarcov with NAs but always provides BHHHvarcov.
pick_vcov <- function(model) {
  ok <- function(M) !is.null(M) && all(is.finite(M))
  if (ok(model$robvarcov)) return(model$robvarcov)
  if (ok(model$varcov))    return(model$varcov)
  if (ok(model$BHHHvarcov)) return(model$BHHHvarcov)
  stop("No usable covariance matrix on model.")
}
wtp_delta <- function(model) delta_method(target_wtps, model$estimate, pick_vcov(model))

## Respondent-clustered bootstrap with per-rep on-disk cache and resume.
## Each rep saves rep_NNNN.rds in boot_dir; re-running boot_clustered() skips
## reps whose file already exists. Use aggregate_bootstrap() to recompute
## the CSV from cached files (e.g. mid-run, or after a crash).
BOOT_DIR <- file.path(PATHS$out, "bootstrap")
BOOT_APOLLO_WORKDIR <- file.path(PATHS$out, "bootstrap_apollo_workdir")

boot_clustered <- function(database, model_main, B = CFG$boot_B, n_cores = 4,
                           draws = CFG$n_draws,
                           boot_dir = BOOT_DIR,
                           apollo_workdir = BOOT_APOLLO_WORKDIR,
                           seed_base = CFG$seed) {
  dir.create(boot_dir,       showWarnings = FALSE, recursive = TRUE)
  dir.create(apollo_workdir, showWarnings = FALSE, recursive = TRUE)
  ids   <- unique(database$ID)
  start <- model_main$estimate
  message(sprintf("[boot] B=%d  cache=%s", B, boot_dir))
  for (b in seq_len(B)) {
    f <- file.path(boot_dir, sprintf("rep_%04d.rds", b))
    if (file.exists(f)) next                            # resume: skip done reps
    t0 <- Sys.time()
    set.seed(seed_base + 1e6L + b)                      # offset from MC seed space
    smp <- sample(ids, length(ids), TRUE)
    nd <- do.call(rbind, Map(function(id, k) {
      d <- database[database$ID == id, , drop = FALSE]; d$ID <- k; d
    }, smp, seq_along(smp)))
    nd <- nd[order(nd$ID, nd$task), ]
    fit <- tryCatch(
      fit_mmnl(nd, sprintf("boot_%04d", b), start = start, draws = draws,
               silent = TRUE, compute_hessian = FALSE, n_cores = n_cores,
               output_dir = apollo_workdir),
      error = function(e) structure(list(err = conditionMessage(e)),
                                    class = "boot_fit_fail"))
    if (inherits(fit, "boot_fit_fail")) {
      saveRDS(list(rep = b, status = "fit_fail", err = fit$err,
                   elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))), f)
      message(sprintf("[boot] %4d/%d  FIT_FAIL: %s", b, B, fit$err))
      next
    }
    saveRDS(list(rep = b, status = "ok",
                 wtp = target_wtps(fit$estimate),
                 LLout = fit$LLout,
                 elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))), f)
    if (b %% 25 == 0 || b <= 3)
      message(sprintf("[boot] %4d/%d  ok  (%.0fs)",
                      b, B,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  aggregate_bootstrap(model_main, boot_dir = boot_dir)
}

aggregate_bootstrap <- function(model_main, boot_dir = BOOT_DIR,
                                out_csv = file.path(PATHS$out, "wtp_bootstrap.csv")) {
  files <- list.files(boot_dir, pattern = "^rep_\\d+\\.rds$", full.names = TRUE)
  if (length(files) == 0) { message("[boot] no rep files in ", boot_dir); return(invisible(NULL)) }
  reps <- lapply(files, readRDS)
  ok   <- Filter(function(x) identical(x$status, "ok"), reps)
  n_fail <- length(reps) - length(ok)
  if (length(ok) == 0) { message("[boot] all reps failed."); return(invisible(NULL)) }
  M <- do.call(rbind, lapply(ok, function(x) x$wtp))
  out <- data.frame(
    quantity = colnames(M),
    estimate = target_wtps(model_main$estimate),
    lo       = apply(M, 2, quantile, 0.025, na.rm = TRUE),
    hi       = apply(M, 2, quantile, 0.975, na.rm = TRUE),
    n_reps   = nrow(M),
    n_failed = n_fail,
    row.names = NULL
  )
  write.csv(out, out_csv, row.names = FALSE)
  message(sprintf("[boot] aggregated %d ok reps (%d failed) -> %s",
                  nrow(M), n_fail, out_csv))
  out
}

if (exists("model_main")) {
  wtp_dm <- wtp_delta(model_main)
  write.csv(wtp_dm, file.path(PATHS$out,"wtp_delta.csv"), row.names=FALSE); print(wtp_dm)
  if (isTRUE(CFG$do_bootstrap)) {
    wtp_bs <- boot_clustered(database, model_main)
    write.csv(wtp_bs, file.path(PATHS$out,"wtp_bootstrap.csv"), row.names=FALSE); print(wtp_bs)
  }
}
message(sprintf("04_wtp.R loaded (spec_type=%s, %d target quantities).",
                CFG$spec_type, length(GROUPS) * (length(BUNDLES)-1 + length(PAIRWISE)) +
                  ifelse(CFG$spec_type=="pap", 3*3, 0)))
