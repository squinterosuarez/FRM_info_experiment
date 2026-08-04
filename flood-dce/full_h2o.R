## full_h2o.R — PAP hypothesis H2o (directional separation) on the full sample.
##
## H2o:  SEP = CITT_under - CITT_over  on the public-vs-private WTP contrast,
##       where CITT_cell = WTP(treated, cell) - WTP(control, cell).
##       Predicted > 0 (one-sided): underestimators move toward the public good,
##       overestimators the opposite.
##
## Per-cell own-cost fits are unstable (small cells -> WTP ratio blows up when the
## cost coefficient wanders near 0). So: pooled fixed-coef MNL on the under u over
## respondents with a COMMON cost coefficient (the main spec fixes cost anyway)
## and cell-specific
## numerator part-worths for the four (category x arm) cells. SEs are
## respondent-clustered; H2o and its components by the delta method.
##
## public  = protect-all (a1=1) / flat (a3=1);  private = opt-in (a1=4) / risk-priced (a3=3)
## Effects coding => public_V - private_V loads numerator as 2*a1e1+a1e2+a1e3 + 2*a3e1+a3e2.

db <- readRDS("data/full_database.rds")
num <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1")   # cost handled separately (common)
XAn <- as.matrix(db[, paste0("A_", num)]); XAc <- db$A_cost100
XBn <- as.matrix(db[, paste0("B_", num)]); XBc <- db$B_cost100
y <- db$choice

## ---- restrict to the two H2o cells; build a 4-level cell factor ----
keep  <- which(db$catUnder == 1 | db$catOver == 1)
cellf <- ifelse(db$catUnder[keep] == 1, "under", "over")
cellf <- paste0(cellf, "_", ifelse(db$treatment[keep] == 1, "T", "C"))
cells <- c("under_C","under_T","over_C","over_T")
cidx  <- lapply(cells, function(c) keep[cellf == c]); names(cidx) <- cells
id    <- db$ID[keep]

cat("H2o cells (respondents / obs):\n")
for (c in cells) cat(sprintf("  %-9s %2d / %3d\n", c,
    length(unique(db$ID[cidx[[c]]])), length(cidx[[c]])))

## ---- parameter layout: ASC_<cell> + 6 numerator part-worths per cell + 1 common cost
pnm <- c(as.vector(t(outer(cells, c("ASC", num), function(a,b) paste0(b,"_",a)))), "cost100")
pidx <- function(cell, term) match(paste0(term,"_",cell), pnm)
icost <- match("cost100", pnm)

## per-observation negative log-likelihood (vector); cell selects which b applies
obs_nll <- function(p) {
  bc <- p[icost]
  vA <- numeric(length(keep)); vB <- numeric(length(keep)); vS <- numeric(length(keep))
  for (c in cells) {
    rows <- which(cellf == c)                 # positions within `keep`
    if (!length(rows)) next
    bcell <- p[pidx(c, num)]; asc <- p[pidx(c,"ASC")]
    vA[rows] <- XAn[keep[rows], ] %*% bcell + XAc[keep[rows]] * bc
    vB[rows] <- XBn[keep[rows], ] %*% bcell + XBc[keep[rows]] * bc
    vS[rows] <- asc
  }
  m <- pmax(vA, vB, vS); den <- exp(vA-m) + exp(vB-m) + exp(vS-m)
  yi <- y[keep]
  -(ifelse(yi==1, vA-m, ifelse(yi==2, vB-m, vS-m)) - log(den))
}
negll <- function(p) sum(obs_nll(p))

o <- optim(setNames(rep(0, length(pnm)), pnm), negll, method="BFGS",
           hessian=TRUE, control=list(maxit=2000, reltol=1e-11))
par <- o$par
cat(sprintf("\nconverged=%s  logLik=%.2f  common cost100=%.3f\n",
            o$convergence==0, -o$value, par[icost]))

## ---- respondent-clustered sandwich covariance ----
S <- numDeriv::jacobian(obs_nll, par)         # n_obs x n_par per-obs score of NLL
H <- o$hessian
clu <- Reduce(`+`, lapply(split(seq_along(id), id), function(r){
  sg <- colSums(S[r, , drop=FALSE]); outer(sg, sg) }))
Hinv <- solve(H)
V <- Hinv %*% clu %*% Hinv
dimnames(V) <- list(pnm, pnm)

## ---- estimands: public-vs-private WTP numerator weights (effects-coded) ----
## num vector order: a1e1,a1e2,a1e3,a3e1,a3e2,a4e1
w <- c(2, 1, 1, 2, 1, 0)                      # 2*a1e1+a1e2+a1e3 + 2*a3e1+a3e2 ; a4 not in contrast
numV <- function(p, cell) sum(w * p[pidx(cell, num)])
estimands <- function(p) {
  bc <- p[icost]
  citt_under <- -100 * (numV(p,"under_T") - numV(p,"under_C")) / bc
  citt_over  <- -100 * (numV(p,"over_T")  - numV(p,"over_C"))  / bc
  c(CITT_under = citt_under, CITT_over = citt_over,
    H2o_SEP_under_minus_over = citt_under - citt_over)
}
g  <- unname(estimands(par))
J  <- numDeriv::jacobian(estimands, par)
se <- sqrt(pmax(0, diag(J %*% V %*% t(J))))
z  <- g / se

## PAP-registered one-sided direction per estimand: under & SEP predicted >0,
## over predicted <0. p is the prob of a draw as extreme in the predicted sign.
dir <- c(CITT_under = +1, CITT_over = -1, H2o_SEP_under_minus_over = +1)
p1  <- ifelse(dir > 0, pnorm(-z), pnorm(z))

res <- data.frame(
  quantity = names(dir), estimate = round(g, 1), se = round(se, 1),
  lo = round(g - 1.96*se, 1), hi = round(g + 1.96*se, 1),
  z  = round(z, 2), p_onesided = round(p1, 3),
  row.names = NULL)

cat("\n================ H2o on pilot (public vs private, GBP/yr) ================\n")
print(res, row.names = FALSE)
cat("\nReading: CITT_under > 0 and CITT_over < 0 => H2o (separation) > 0, as predicted.\n")
cat("p_onesided is for the PAP-registered direction of each row",
    "(under & SEP: >0 ; over: <0).\n")
nUC <- length(unique(db$ID[cidx[["under_C"]]])); nUT <- length(unique(db$ID[cidx[["under_T"]]]))
cat(sprintf("\nNOTE: underestimator cell is %d control / %d treated -- the confirmatory\ncell is small relative to overestimators, so read its CI accordingly.\n", nUC, nUT))
cat("\nFULL_H2O_DONE\n")
