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
  data.frame(ID=seq_len(N),
             block=sample(rep(seq_len(CFG$n_blocks), length.out=N)),
             treatment=treatment, actual_rank=actual_rank, prior_rank=prior_rank,
             gapUp=gapUp, gapDown=gapDown, noPrior=noPrior,
             updater_type=factor(upd, levels=c("control","none","upward","downward")),
             risk_high=as.integer(actual_rank==4))
}

## Draw a respondent's β_i. Branches on CFG$dgp_type.
##   directional: β = μ + T·(dt + up·gapUp + dn·gapDown) + np_asc·T·NP·1{k=asc} + σ·η
##   2x2:         β = μ + α·T + β_np·NP + γ·T·NP + σ·η
draw_beta_i <- function(r) {
  b <- TRUE_mu
  if (CFG$dgp_type == "directional") {
    b[names(TRUE_dt)] <- b[names(TRUE_dt)] +
        r$treatment*(TRUE_dt + TRUE_up*r$gapUp + TRUE_dn*r$gapDown)   # direction via gap
    b["asc"] <- b["asc"] + r$treatment*r$noPrior*TRUE_np_asc          # no-idea ASC control
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
                      noPrior=r$noPrior, updater_type=r$updater_type,
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
