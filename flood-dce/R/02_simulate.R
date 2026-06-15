## =====================================================================
## 02_simulate.R  (directional version)
## Simulate prior beliefs, actual risk, the expected-vs-actual gap, and
## choices from the directional DGP. Output: apollo-ready `database`.
## =====================================================================

simulate_respondents <- function() {
  N <- CFG$N
  actual_rank <- sample(4:1, N, TRUE, prob=rev(CFG$risk_probs))    # 4=High..1=VeryLow
  treatment   <- rbinom(N, 1, CFG$prop_treat)
  no_idea     <- runif(N) < CFG$p_noidea
  shift       <- sample(CFG$shift_vals, N, TRUE, prob=CFG$shift_probs)
  prior_rank  <- pmin(pmax(actual_rank - shift, 1), 4)
  prior_rank[no_idea] <- 0L                                        # 0 = "No idea" (record)
  prior_used <- ifelse(no_idea, CFG$noidea_anchor, prior_rank)     # impute midpoint
  gap    <- actual_rank - prior_used                               # no-idea: vs midpoint
  gapUp  <- pmax(gap, 0); gapDown <- pmax(-gap, 0)
  noPrior <- as.integer(no_idea)
  upd <- ifelse(treatment==0, "control",
         ifelse(gap>0, "upward", ifelse(gap<0, "downward", "none")))  # by effective gap
  ## Pre-treatment prior-gap CATEGORY (NO imputation; DK kept as its own cell).
  ## Built from raw prior_rank vs actual_rank, so it is fixed before treatment.
  ##   under   = objective risk ABOVE stated prior (underestimator)
  ##   over    = objective risk BELOW stated prior (overestimator)
  ##   correct = prior matches objective
  ##   dontknow= no stated prior (no midpoint anchor here; see gapUp/gapDown for that)
  ## Consumed by spec_type="cate"; the midpoint-anchored gapUp/gapDown above are
  ## left untouched so the gap-size robustness spec ("dir") still has its inputs.
  prior_gap_cat <- ifelse(no_idea, "dontknow",
                   ifelse(actual_rank >  prior_rank, "under",
                   ifelse(actual_rank <  prior_rank, "over", "correct")))
  prior_gap_cat <- factor(prior_gap_cat, levels=c("correct","under","over","dontknow"))
  catUnder <- as.integer(prior_gap_cat=="under")
  catOver  <- as.integer(prior_gap_cat=="over")
  catDK    <- as.integer(prior_gap_cat=="dontknow")
  data.frame(ID=seq_len(N),
             block=sample(rep(seq_len(CFG$n_blocks), length.out=N)),
             treatment=treatment, actual_rank=actual_rank, prior_rank=prior_rank,
             gapUp=gapUp, gapDown=gapDown, noPrior=noPrior,
             catUnder=catUnder, catOver=catOver, catDK=catDK, prior_gap_cat=prior_gap_cat,
             updater_type=factor(upd, levels=c("control","none","upward","downward")),
             risk_high=as.integer(actual_rank==4))
}

## Draw a respondent's β_i. Branches on CFG$dgp_type.
##   directional: stated-prior people  β = μ + T·(dt + up·gapUp + dn·gapDown) + σ·η
##                don't-know people     β = μ + T·dk + σ·η   (own effect, de-imputed)
##   2x2:         β = μ + α·T + β_np·NP + γ·T·NP + σ·η
draw_beta_i <- function(r) {
  b <- TRUE_mu
  if (CFG$dgp_type == "directional") {
    if (r$noPrior == 1) {
      b[names(TRUE_dk)] <- b[names(TRUE_dk)] + r$treatment*TRUE_dk     # DK: own effect (de-imputed)
    } else {
      b[names(TRUE_dt)] <- b[names(TRUE_dt)] +
          r$treatment*(TRUE_dt + TRUE_up*r$gapUp + TRUE_dn*r$gapDown) # direction via gap
    }
  } else if (CFG$dgp_type == "2x2") {
    b[names(TRUE_alpha)] <- b[names(TRUE_alpha)] +
        r$treatment*TRUE_alpha +
        r$noPrior  *TRUE_beta  +
        r$treatment*r$noPrior*TRUE_gamma
  } else stop("Unknown CFG$dgp_type: ", CFG$dgp_type)
  eta <- setNames(rnorm(length(RP)), RP)
  if (!CFG$asc_random) eta["asc"] <- 0
  b[RP] <- b[RP] + TRUE_sd[RP]*eta
  b
}

simulate_choices <- function(resp_df, design) {
  suf  <- c("a1e1","a1e2","a1e3","a2e1","a3e1","a3e2","a4e1","cost100")
  cA <- paste0("A_",suf); cB <- paste0("B_",suf)
  bmap <- c(suf[-length(suf)], "cost")            # cost100 -> coeff 'cost'
  rows <- vector("list", nrow(resp_df))
  for (i in seq_len(nrow(resp_df))) {
    r <- resp_df[i,]; b <- draw_beta_i(r)
    battr <- b[bmap]
    d <- design[design$block==r$block,,drop=FALSE]
    VA <- as.numeric(as.matrix(d[,cA]) %*% battr)
    VB <- as.numeric(as.matrix(d[,cB]) %*% battr)
    VSQ <- rep(b["asc"], nrow(d))
    g <- function(n) -log(-log(runif(n)))
    U <- cbind(VA+g(nrow(d)), VB+g(nrow(d)), VSQ+g(nrow(d)))
    choice <- max.col(U, "first")
    out <- data.frame(ID=r$ID, block=r$block, task=d$task,
                      treatment=r$treatment, gapUp=r$gapUp, gapDown=r$gapDown,
                      noPrior=r$noPrior, catUnder=r$catUnder, catOver=r$catOver, catDK=r$catDK,
                      updater_type=r$updater_type,
                      actual_rank=r$actual_rank, risk_high=r$risk_high, choice=choice)
    out <- cbind(out, d[,c(cA,cB)]); out$av_A<-1L; out$av_B<-1L; out$av_SQ<-1L
    rows[[i]] <- out
  }
  db <- do.call(rbind, rows); db <- db[order(db$ID, db$task),]; rownames(db)<-NULL; db
}

resp_df  <- simulate_respondents()
database <- simulate_choices(resp_df, design_wide)
saveRDS(resp_df,  file.path(PATHS$data,"sim_respondents.rds"))
saveRDS(database, file.path(PATHS$data,"sim_database.rds"))
message(sprintf("02_simulate.R: %d obs. SQ share=%.3f. Treated updater cells:",
                nrow(database), mean(database$choice==3)))
print(table(resp_df$updater_type))
message("02_simulate.R: prior-gap categories (pre-treatment):")
print(table(resp_df$prior_gap_cat))
