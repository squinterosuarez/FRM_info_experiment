## pilot_mnl.R — fixed-coefficient conditional logit on the pilot (N=60), by
## prior-gap category. Pilot-appropriate (no random coefficients: too few people).
## 3 alternatives A/B/SQ; A/B carry effects-coded attributes, SQ utility = ASC.
## Answers: when each group leaves the status quo, WHICH attributes pull them.
db <- readRDS("data/pilot_database.rds")
suf  <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1","cost100")
XA <- as.matrix(db[, paste0("A_",suf)]); XB <- as.matrix(db[, paste0("B_",suf)])
y  <- db$choice
pnm <- c("ASC_SQ", suf)

negll <- function(p, idx) {
  asc <- p[1]; b <- p[-1]
  vA <- XA[idx,,drop=FALSE] %*% b; vB <- XB[idx,,drop=FALSE] %*% b; vS <- rep(asc, length(idx))
  m  <- pmax(vA, vB, vS); den <- exp(vA-m) + exp(vB-m) + exp(vS-m)
  yi <- y[idx]
  ll <- ifelse(yi==1, vA-m, ifelse(yi==2, vB-m, vS-m)) - log(den)
  -sum(ll)
}
fit_grp <- function(idx) {
  o <- optim(rep(0, length(pnm)), negll, idx=idx, method="BFGS", hessian=TRUE,
             control=list(maxit=500, reltol=1e-10))
  se <- sqrt(diag(solve(o$hessian)))
  bcost <- o$par[match("cost100", pnm)]
  wtp <- -100 * o$par / bcost            # GBP/yr per attribute effect; ASC row = nonsense, blank it
  data.frame(param=pnm, est=round(o$par,3), se=round(se,3),
             z=round(o$par/se,2), wtp_gbp=round(wtp,0))
}
report <- function(name, idx) {
  cat(sprintf("\n================ %s  (n_resp=%d, n_obs=%d) ================\n",
              name, length(unique(db$ID[idx])), length(idx)))
  r <- fit_grp(idx); r$wtp_gbp[r$param=="ASC_SQ"] <- NA; r$wtp_gbp[r$param=="cost100"] <- NA
  print(r, row.names=FALSE)
}

report("ALL (pooled)",       seq_len(nrow(db)))
report("OVERESTIMATORS",     which(db$catOver==1))
report("UNDERESTIMATORS",    which(db$catUnder==1))
report("CORRECT",            which(db$catCorrect <- (db$catOver==0 & db$catUnder==0 & db$catDK==0)))
cat("\nLEVEL KEY (effects-coded; positive = prefer vs the reference level):\n")
cat("  a1e1 = protect ALL households   | a1e2 = high-risk areas | a1e3 = high-risk+deprived   (ref: opt-in only)\n")
cat("  a3e1 = FLAT cost-split          | a3e2 = wealthier pay more                             (ref: risk-priced)\n")
cat("  a4e1 = MOST effective                                                                   (ref: least effective)\n")
cat("  cost100 = utility per +£100     | ASC_SQ = pull toward Keep-current (higher = more SQ)\n")
cat("\nPILOT_MNL_DONE\n")
