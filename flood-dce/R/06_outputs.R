## =====================================================================
## 06_outputs.R -- recovery diagnostics + figures.
## true_theta_vec() branches on (CFG$spec_type, CFG$dgp_type).
## On real data the "true" columns vanish; everything else still runs.
## =====================================================================

## Cache of E[gapUp | T,NP] and E[gapDown | T,NP] under the directional DGP,
## used to marginalise directional TRUE values onto the PAP-spec α/γ truths.
## Computed once via a large analytic draw of respondents (N_ref).
.dir_dgp_moments <- local({
  cache <- NULL
  function(N_ref = 50000, seed_ref = 999999L) {
    if (!is.null(cache)) return(cache)
    ## Reproduce simulate_respondents() draws at large N to estimate moments
    ## of (gapUp, gapDown) conditional on (treatment, noPrior).
    old_seed <- if (exists(".Random.seed", envir=globalenv()))
      get(".Random.seed", envir=globalenv()) else NULL
    set.seed(seed_ref)
    actual_rank <- sample(4:1, N_ref, TRUE, prob=rev(CFG$risk_probs))
    treatment   <- rbinom(N_ref, 1, CFG$prop_treat)
    no_idea     <- runif(N_ref) < CFG$p_noidea
    shift       <- sample(CFG$shift_vals, N_ref, TRUE, prob=CFG$shift_probs)
    prior_rank  <- pmin(pmax(actual_rank - shift, 1), 4)
    prior_used  <- ifelse(no_idea, CFG$noidea_anchor, prior_rank)
    gap     <- actual_rank - prior_used
    gapUp   <- pmax(gap, 0); gapDown <- pmax(-gap, 0)
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir=globalenv())
    m <- function(mask) c(gU=mean(gapUp[mask]), gD=mean(gapDown[mask]))
    cache <<- list(
      T1_NP0 = m(treatment==1 & !no_idea),
      T1_NP1 = m(treatment==1 &  no_idea),
      p_NP   = mean(no_idea)
    )
    cache
  }
})

true_theta_vec_dir <- function() {
  rp <- RP
  c(setNames(TRUE_mu[rp], paste0("mu_",rp)), mu_cost=unname(TRUE_mu["cost"]),
    setNames(TRUE_dt[rp], paste0("dt_",rp)), dt_cost=unname(TRUE_dt["cost"]),
    setNames(TRUE_up[rp], paste0("up_",rp)),
    setNames(TRUE_dn[rp], paste0("dn_",rp)),
    setNames(TRUE_up[rp], paste0("up_np_",rp)),
    setNames(TRUE_dn[rp], paste0("dn_np_",rp)),
    c(np_asc=unname(TRUE_np_asc)),
    setNames(TRUE_sd[rp], paste0("sd_",rp)))
}

true_theta_vec_pap <- function() {
  rp <- RP
  base <- c(setNames(TRUE_mu[rp], paste0("mu_",rp)), mu_cost=unname(TRUE_mu["cost"]),
            setNames(TRUE_sd[rp], paste0("sd_",rp)))
  if (CFG$dgp_type == "2x2") {
    ## Direct: data was generated from the 2x2 spec, so PAP truths are TRUE_α/β/γ.
    c(base,
      setNames(TRUE_alpha[rp], paste0("alpha_",rp)), alpha_cost=unname(TRUE_alpha["cost"]),
      setNames(TRUE_beta [rp], paste0("beta_", rp)), beta_cost =unname(TRUE_beta ["cost"]),
      setNames(TRUE_gamma[rp], paste0("gamma_",rp)), gamma_cost=unname(TRUE_gamma["cost"]))
  } else if (CFG$dgp_type == "directional") {
    ## Marginalise the directional TRUE values onto the 4 PAP cells.
    ##   α_k = dt_k + up_k·E[gapUp|T=1,NP=0] + dn_k·E[gapDown|T=1,NP=0]
    ##   β_k = 0   (μ_k doesn't depend on NP in directional DGP)
    ##   γ_k = up_k·(E[gapUp|T=1,NP=1]−E[gapUp|T=1,NP=0])
    ##       + dn_k·(E[gapDown|T=1,NP=1]−E[gapDown|T=1,NP=0])
    ##       + np_asc·1{k=asc}
    mm <- .dir_dgp_moments()
    alpha_attr <- TRUE_dt[rp] +
                  TRUE_up[rp] * mm$T1_NP0["gU"] +
                  TRUE_dn[rp] * mm$T1_NP0["gD"]
    gamma_attr <- TRUE_up[rp] * (mm$T1_NP1["gU"] - mm$T1_NP0["gU"]) +
                  TRUE_dn[rp] * (mm$T1_NP1["gD"] - mm$T1_NP0["gD"])
    gamma_attr["asc"] <- gamma_attr["asc"] + TRUE_np_asc
    alpha_cost <- unname(TRUE_dt["cost"]) +
                  unname(TRUE_up["cost"]) * mm$T1_NP0["gU"] +
                  unname(TRUE_dn["cost"]) * mm$T1_NP0["gD"]
    gamma_cost <- unname(TRUE_up["cost"]) * (mm$T1_NP1["gU"] - mm$T1_NP0["gU"]) +
                  unname(TRUE_dn["cost"]) * (mm$T1_NP1["gD"] - mm$T1_NP0["gD"])
    c(base,
      setNames(unname(alpha_attr), paste0("alpha_",rp)), alpha_cost=alpha_cost,
      setNames(rep(0, length(rp)),  paste0("beta_", rp)), beta_cost =0,
      setNames(unname(gamma_attr), paste0("gamma_",rp)), gamma_cost=gamma_cost)
  } else stop("Unknown CFG$dgp_type: ", CFG$dgp_type)
}

## Cache of category-conditional gap moments + category shares under the
## de-imputed directional DGP. Mirrors the updated simulate_respondents():
## category is built from raw prior_rank vs actual_rank (no anchor), DK is
## its own cell. Used to marginalise TRUE values onto the itt/cate truths.
.cate_dgp_moments <- local({
  cache <- NULL
  function(N_ref = 50000, seed_ref = 999999L) {
    if (!is.null(cache)) return(cache)
    old_seed <- if (exists(".Random.seed", envir=globalenv()))
      get(".Random.seed", envir=globalenv()) else NULL
    set.seed(seed_ref)
    actual_rank <- sample(4:1, N_ref, TRUE, prob=rev(CFG$risk_probs))
    treatment   <- rbinom(N_ref, 1, CFG$prop_treat)
    no_idea     <- runif(N_ref) < CFG$p_noidea
    shift       <- sample(CFG$shift_vals, N_ref, TRUE, prob=CFG$shift_probs)
    prior_rank  <- pmin(pmax(actual_rank - shift, 1), 4)
    prior_used  <- ifelse(no_idea, CFG$noidea_anchor, prior_rank)
    gap     <- actual_rank - prior_used
    gapUp   <- pmax(gap, 0); gapDown <- pmax(-gap, 0)
    under <- !no_idea & (actual_rank >  prior_rank)
    over  <- !no_idea & (actual_rank <  prior_rank)
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir=globalenv())
    T1 <- treatment==1
    cache <<- list(
      eUp_under = mean(gapUp[T1 & under]),     # E[gapUp | underestimator, treated]
      eDn_over  = mean(gapDown[T1 & over]),     # E[gapDown | overestimator, treated]
      eUp_nonDK = mean(gapUp[T1 & !no_idea]),   # for the population ITT
      eDn_nonDK = mean(gapDown[T1 & !no_idea]),
      p_DK      = mean(no_idea)
    )
    cache
  }
})

## Plain-ITT truth: tau_k = population-average treatment shift, the
## category-share-weighted mix of the non-DK gap effect and the DK effect.
true_theta_vec_itt <- function() {
  rp <- RP; mm <- .cate_dgp_moments()
  shift_nonDK <- TRUE_dt[rp] + TRUE_up[rp]*mm$eUp_nonDK + TRUE_dn[rp]*mm$eDn_nonDK
  tau_attr <- (1-mm$p_DK)*shift_nonDK + mm$p_DK*TRUE_dk[rp]
  shift_nonDK_cost <- unname(TRUE_dt["cost"]) +
    unname(TRUE_up["cost"])*mm$eUp_nonDK + unname(TRUE_dn["cost"])*mm$eDn_nonDK
  tau_cost <- (1-mm$p_DK)*shift_nonDK_cost + mm$p_DK*unname(TRUE_dk["cost"])
  c(setNames(TRUE_mu[rp], paste0("mu_",rp)), mu_cost=unname(TRUE_mu["cost"]),
    setNames(unname(tau_attr), paste0("tau_",rp)), tau_cost=unname(tau_cost),
    setNames(TRUE_sd[rp], paste0("sd_",rp)))
}

## CATE truth: correct cell = TRUE_dt; under/over deviations = gap-weighted
## TRUE_up/TRUE_dn; DK deviation = TRUE_dk − TRUE_dt; category mains = 0
## (baseline tastes don't differ by category in the DGP).
true_theta_vec_cate <- function() {
  rp <- RP; mm <- .cate_dgp_moments()
  tau_attr <- TRUE_dt[rp]
  dU_attr  <- TRUE_up[rp]*mm$eUp_under
  dO_attr  <- TRUE_dn[rp]*mm$eDn_over
  dDK_attr <- TRUE_dk[rp] - TRUE_dt[rp]
  z <- setNames(rep(0, length(rp)), rp)
  c(setNames(TRUE_mu[rp], paste0("mu_",rp)), mu_cost=unname(TRUE_mu["cost"]),
    setNames(unname(tau_attr), paste0("tau_",rp)), tau_cost=unname(TRUE_dt["cost"]),
    setNames(unname(z), paste0("cU_",rp)),
    setNames(unname(z), paste0("cO_",rp)),
    setNames(unname(z), paste0("cDK_",rp)),
    setNames(unname(dU_attr),  paste0("dU_",rp)),
    setNames(unname(dO_attr),  paste0("dO_",rp)),
    setNames(unname(dDK_attr), paste0("dDK_",rp)),
    setNames(TRUE_sd[rp], paste0("sd_",rp)))
}

true_theta_vec <- function() {
  switch(CFG$spec_type,
         pap  = true_theta_vec_pap(),
         itt  = true_theta_vec_itt(),
         cate = true_theta_vec_cate(),
         true_theta_vec_dir())
}

recovery_table <- function(model) {
  est <- model$estimate
  se  <- sqrt(diag(pick_vcov(model)))[names(est)]
  est[grepl("^sd_",names(est))] <- abs(est[grepl("^sd_",names(est))])   # sign-unidentified
  truth <- true_theta_vec(); tv <- truth[names(est)]; z <- (est-tv)/se
  data.frame(parameter=names(est), true=unname(tv), estimate=unname(est),
             se=unname(se), z_vs_true=unname(z), covered95=unname(abs(z)<1.96), row.names=NULL)
}

wtp_recovery_table <- function(model) {
  dm <- wtp_delta(model); truth <- target_wtps(true_theta_vec())
  dm$true <- truth[dm$quantity]; dm$covered95 <- with(dm, true>=lo & true<=hi)
  dm[,c("quantity","true","estimate","se","lo","hi","covered95")]
}

fig_coef_recovery <- function(rec) {
  d <- rec[!is.na(rec$true),]; d$lo<-d$estimate-1.96*d$se; d$hi<-d$estimate+1.96*d$se
  ggplot(d, aes(true, estimate)) +
    geom_abline(slope=1, intercept=0, linetype=2, colour="grey50") +
    geom_errorbar(aes(ymin=lo, ymax=hi), width=0, colour="grey60") + geom_point() +
    labs(x="True", y="Estimate (95% CI)", title="Parameter recovery",
         subtitle="directional MMNL on simulated data") + theme_minimal(base_size=11)
}

fig_directional <- function(wtp_rec) {
  d <- wtp_rec[grepl("^DELTA_", wtp_rec$quantity),]
  d$label <- sub("^DELTA_","",d$quantity)
  ggplot(d, aes(estimate, label)) +
    geom_vline(xintercept=0, colour="grey70") +
    geom_errorbarh(aes(xmin=lo, xmax=hi), height=0.2, colour="grey60") + geom_point() +
    { if (any(!is.na(d$true))) geom_point(aes(x=true), shape=4, colour="red", size=2) } +
    labs(x="Shift in bundle-contrast WTP vs control (GBP)", y=NULL,
         title="Directional information effects", subtitle="x = true (simulation)") +
    theme_minimal(base_size=10)
}

if (exists("model_main")) {
  rec <- recovery_table(model_main)
  write.csv(rec, file.path(PATHS$out,"recovery_params.csv"), row.names=FALSE)
  cat("\n-- Parameter recovery (coverage =",
      round(mean(rec$covered95[!is.na(rec$true)]),2),") --\n"); print(rec, digits=3)
  wtp_rec <- wtp_recovery_table(model_main)
  write.csv(wtp_rec, file.path(PATHS$out,"recovery_wtp.csv"), row.names=FALSE)
  cat("\n-- WTP recovery --\n"); print(wtp_rec, digits=3)
  ggsave(file.path(PATHS$out,"fig_coef_recovery.png"), fig_coef_recovery(rec), width=6, height=5, dpi=150)
  if (identical(CFG$spec_type, "dir"))
    ggsave(file.path(PATHS$out,"fig_directional.png"), fig_directional(wtp_rec),
           width=7, height=5, dpi=150)
}
message(sprintf("06_outputs.R loaded (spec_type=%s, dgp_type=%s).",
                CFG$spec_type, CFG$dgp_type))
