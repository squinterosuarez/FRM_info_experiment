## =====================================================================
## 03_estimate_mmnl.R
## MMNL fit. Mean function branches on CFG$spec_type:
##   spec_type = "pap"  →  mean_k = mu_k + α_k·T + β_k·NP + γ_k·T·NP
##   spec_type = "dir"  →  mean_k = mu_k + dt_k·T + up_k·T·gapUp + dn_k·T·gapDown
##                                  (+ np_asc·T·NP on ASC only)
## random part: diagonal (primary) or Cholesky-correlated (robustness).
## cost fixed; ASC fixed by default per PAP (CFG$asc_random = FALSE).
## =====================================================================
ATTR_SUFFIX <- c("a1e1","a1e2","a1e3","a2e1","a3e1","a3e2","a4e1","cost100")

build_start <- function(correlated, cost_random, asc_random, noprior_separate=FALSE,
                        omit_params=character(0), spec_type=CFG$spec_type) {
  rp_rand <- if (asc_random) RP else setdiff(RP, "asc")
  mus <- c(setNames(rep(0,length(RP)), paste0("mu_",RP)), mu_cost=-0.10)
  if (spec_type == "pap") {
    alpha <- c(setNames(rep(0,length(RP)), paste0("alpha_",RP)), alpha_cost=0)
    beta  <- c(setNames(rep(0,length(RP)), paste0("beta_", RP)), beta_cost =0)
    gamma <- c(setNames(rep(0,length(RP)), paste0("gamma_",RP)), gamma_cost=0)
    start <- c(mus, alpha, beta, gamma)
    if (noprior_separate)
      stop("noprior_separate is meaningless under spec_type='pap' (NoPrior is the cell coordinate).")
  } else if (spec_type == "dir") {
    dt  <- c(setNames(rep(0,length(RP)), paste0("dt_",RP)), dt_cost=0)
    up  <- setNames(rep(0,length(RP)), paste0("up_",RP))
    dn  <- setNames(rep(0,length(RP)), paste0("dn_",RP))
    np  <- c(np_asc=0)
    start <- c(mus, dt, up, dn, np)
    if (noprior_separate) {
      up_np <- setNames(rep(0,length(RP)), paste0("up_np_",RP))
      dn_np <- setNames(rep(0,length(RP)), paste0("dn_np_",RP))
      start <- c(start, up_np, dn_np)
    }
  } else stop("Unknown spec_type: ", spec_type)
  if (!correlated) {
    start <- c(start, setNames(rep(0.10,length(rp_rand)), paste0("sd_",rp_rand)))
  } else {
    Ln <- unlist(lapply(seq_along(rp_rand), function(i) sprintf("L_%d_%d", i, 1:i)))
    Ls <- setNames(rep(0,length(Ln)), Ln); Ls[grepl("^L_(\\d+)_\\1$",Ln)] <- 0.10
    start <- c(start, Ls)
  }
  if (cost_random) start <- c(start, sd_cost=0.10)
  if (length(omit_params)) start <- start[!names(start) %in% omit_params]
  start
}

## Build apollo_randCoeff as a self-contained function (no closure vars).
## Apollo rebinds environment(apollo_randCoeff) to an env containing only
## apollo_beta + database + draws, so any reference to outer-scope config
## (correlated, rp_rand, etc.) would fail at runtime. Bake values into the
## function body via text-templating.
make_randCoeff <- function(correlated, cost_random, asc_random, noprior_separate=FALSE,
                           omit_params=character(0), spec_type=CFG$spec_type) {
  rp_rand <- if (asc_random) RP else setdiff(RP, "asc")
  has <- function(p) !(p %in% omit_params)
  ## Build mean_k as a sum of only those terms whose coefficient is kept;
  ## any omitted term is dropped (not just zeroed) so apollo never sees
  ## the parameter — avoiding singular-gradient issues from structurally
  ## constant interaction columns on a subgroup.
  mean_expr <- function(k) {
    terms <- character(0)
    if (has(paste0("mu_",k))) terms <- c(terms, sprintf("mu_%s", k))
    if (spec_type == "pap") {
      if (has(paste0("alpha_",k))) terms <- c(terms, sprintf("alpha_%s*treatment", k))
      if (has(paste0("beta_", k))) terms <- c(terms, sprintf("beta_%s*noPrior",  k))
      if (has(paste0("gamma_",k))) terms <- c(terms, sprintf("gamma_%s*(treatment*noPrior)", k))
    } else {  # "dir"
      if (has(paste0("dt_",k)))  terms <- c(terms, sprintf("dt_%s*treatment", k))
      if (noprior_separate) {
        if (has(paste0("up_",k)))    terms <- c(terms, sprintf("up_%s*(treatment*gapUp*(1-noPrior))", k))
        if (has(paste0("dn_",k)))    terms <- c(terms, sprintf("dn_%s*(treatment*gapDown*(1-noPrior))", k))
        if (has(paste0("up_np_",k))) terms <- c(terms, sprintf("up_np_%s*(treatment*gapUp*noPrior)", k))
        if (has(paste0("dn_np_",k))) terms <- c(terms, sprintf("dn_np_%s*(treatment*gapDown*noPrior)", k))
      } else {
        if (has(paste0("up_",k))) terms <- c(terms, sprintf("up_%s*(treatment*gapUp)", k))
        if (has(paste0("dn_",k))) terms <- c(terms, sprintf("dn_%s*(treatment*gapDown)", k))
      }
      if (k == "asc" && has("np_asc"))
        terms <- c(terms, "np_asc*(treatment*noPrior)")
    }
    if (length(terms) == 0) "0" else paste(terms, collapse=" + ")
  }
  lines <- c("function(apollo_beta, apollo_inputs) {",
             "  randcoeff <- list()")
  for (k in RP) {
    me <- mean_expr(k)
    if (!correlated) {
      add <- if (k %in% rp_rand && has(paste0("sd_",k))) sprintf(" + sd_%s*draws_%s", k, k) else ""
      lines <- c(lines, sprintf("  randcoeff[[\"b_%s\"]] <- %s%s", k, me, add))
    } else {
      if (k %in% rp_rand) {
        i <- match(k, rp_rand)
        chol <- paste(sprintf("L_%d_%d*draws_%s", i, seq_len(i), rp_rand[seq_len(i)]),
                      collapse=" + ")
        lines <- c(lines, sprintf("  randcoeff[[\"b_%s\"]] <- %s + %s", k, me, chol))
      } else {
        lines <- c(lines, sprintf("  randcoeff[[\"b_%s\"]] <- %s", k, me))
      }
    }
  }
  cost_terms <- character(0)
  if (has("mu_cost")) cost_terms <- c(cost_terms, "mu_cost")
  if (spec_type == "pap") {
    if (has("alpha_cost")) cost_terms <- c(cost_terms, "alpha_cost*treatment")
    if (has("beta_cost"))  cost_terms <- c(cost_terms, "beta_cost*noPrior")
    if (has("gamma_cost")) cost_terms <- c(cost_terms, "gamma_cost*(treatment*noPrior)")
  } else {
    if (has("dt_cost")) cost_terms <- c(cost_terms, "dt_cost*treatment")
  }
  if (cost_random && has("sd_cost")) cost_terms <- c(cost_terms, "sd_cost*draws_cost")
  cost_body <- if (length(cost_terms)) paste(cost_terms, collapse=" + ") else "0"
  cost_line <- if (cost_random)
    sprintf("  randcoeff[[\"b_cost\"]] <- -exp(%s)", cost_body)
  else
    sprintf("  randcoeff[[\"b_cost\"]] <- %s", cost_body)
  lines <- c(lines, cost_line, "  return(randcoeff)", "}")
  eval(parse(text=paste(lines, collapse="\n")))
}

apollo_prob_fun <- function(apollo_beta, apollo_inputs, functionality="estimate") {
  apollo_attach(apollo_beta, apollo_inputs); on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P<-list(); V<-list()
  V[["A"]] <- b_a1e1*A_a1e1+b_a1e2*A_a1e2+b_a1e3*A_a1e3+b_a2e1*A_a2e1+
              b_a3e1*A_a3e1+b_a3e2*A_a3e2+b_a4e1*A_a4e1+b_cost*A_cost100
  V[["B"]] <- b_a1e1*B_a1e1+b_a1e2*B_a1e2+b_a1e3*B_a1e3+b_a2e1*B_a2e1+
              b_a3e1*B_a3e1+b_a3e2*B_a3e2+b_a4e1*B_a4e1+b_cost*B_cost100
  V[["SQ"]] <- b_asc
  s <- list(alternatives=c(A=1,B=2,SQ=3), avail=list(A=av_A,B=av_B,SQ=av_SQ),
            choiceVar=choice, V=V)
  P[["model"]] <- apollo_mnl(s, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

fit_mmnl <- function(database, model_name=NULL, correlated=FALSE,
                     cost_random=CFG$cost_random, asc_random=CFG$asc_random,
                     noprior_separate=FALSE,
                     draws=CFG$n_draws, start=NULL, n_cores=4, silent=FALSE,
                     compute_hessian=FALSE, omit_params=character(0),
                     output_dir=PATHS$out, spec_type=CFG$spec_type) {
  if (is.null(model_name)) model_name <- sprintf("mmnl_%s", spec_type)
  apollo_initialise()
  dir.create(output_dir, showWarnings=FALSE, recursive=TRUE)
  apollo_control <- list(modelName=model_name,
                         modelDescr=sprintf("Flood DCE -- MMNL (spec=%s)", spec_type),
                         indivID="ID", mixing=TRUE, panelData=TRUE, nCores=n_cores,
                         outputDirectory=output_dir)
  if (is.null(start))
    start <- build_start(correlated, cost_random, asc_random, noprior_separate,
                         omit_params, spec_type)
  rp_rand <- if (asc_random) RP else setdiff(RP, "asc")
  dn <- paste0("draws_", rp_rand); if (cost_random) dn <- c(dn, "draws_cost")
  apollo_draws <- list(interDrawsType="mlhs", interNDraws=draws, interUnifDraws=character(0),
                       interNormDraws=dn, intraDrawsType="halton", intraNDraws=0,
                       intraUnifDraws=character(0), intraNormDraws=character(0))
  for (nm in c("database","apollo_control")) assign(nm, get(nm), envir=.GlobalEnv)
  assign("apollo_beta", start, envir=.GlobalEnv)
  assign("apollo_fixed", character(0), envir=.GlobalEnv)
  assign("apollo_draws", apollo_draws, envir=.GlobalEnv)
  assign("apollo_randCoeff",
         make_randCoeff(correlated, cost_random, asc_random, noprior_separate,
                        omit_params, spec_type),
         envir=.GlobalEnv)
  assign("apollo_probabilities", apollo_prob_fun, envir=.GlobalEnv)
  apollo_inputs <- apollo_validateInputs(silent=silent)
  est <- list(silent=silent, writeIter=FALSE,
              hessianRoutine=if (compute_hessian) "analytic" else "none")
  apollo_estimate(get("apollo_beta",.GlobalEnv), character(0),
                  apollo_prob_fun, apollo_inputs, estimate_settings=est)
}

## Auto-fit the primary model on source. Object name is `model_pap` (when
## CFG$spec_type="pap") or `model_dir` ("dir"); generic alias `model_main`
## always points at whatever was fitted, so downstream modules can be
## spec-agnostic. Skip if the spec-specific object is already in globalenv.
if (identical(environment(), globalenv())) {
  .obj <- paste0("model_", CFG$spec_type)
  if (!exists(.obj, envir=globalenv())) {
    .m <- fit_mmnl(database, correlated=FALSE)
    assign(.obj, .m, envir=globalenv())
    apollo_modelOutput(.m)
    saveRDS(.m, file.path(PATHS$out, paste0(.obj, ".rds")))
  } else {
    .m <- get(.obj, envir=globalenv())
  }
  assign("model_main", .m, envir=globalenv())
}
message(sprintf("03_estimate_mmnl.R loaded (spec_type=%s).", CFG$spec_type))
