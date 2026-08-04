## make_itt_figure.R — POOLED ITT figure (no category conditioning / no CATE).
## Treated - control shift in WTP for each PAP-registered ITT contrast, from the
## plain treatment x attribute fixed-coef MNL (same stable model as full_a1_and_itt.R
## part 2). WTP is nonlinear in the params (treated & control have different cost
## slopes b_cost, b_cost+t_cost), so CIs are delta-method over the full VCOV.
suppressMessages({ library(ggplot2); library(numDeriv) })
db  <- readRDS("data/full_database.rds")
suf <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1","cost100")
num <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1")          # numerator part-worths
XA <- as.matrix(db[, paste0("A_",suf)]); XB <- as.matrix(db[, paste0("B_",suf)])
y  <- db$choice; Tt <- db$treatment

## ---- fit treatment x attribute MNL (base b + treatment shift t) ----
ip <- c("ASC","tASC", paste0("b_",suf), paste0("t_",suf))
nll_itt <- function(p){
  asc<-p["ASC"]; tasc<-p["tASC"]; b<-p[paste0("b_",suf)]; tb<-p[paste0("t_",suf)]
  vA<-XA%*%b + Tt*(XA%*%tb); vB<-XB%*%b + Tt*(XB%*%tb); vS<-asc + Tt*tasc
  m<-pmax(vA,vB,vS); den<-exp(vA-m)+exp(vB-m)+exp(vS-m)
  -sum(ifelse(y==1,vA-m,ifelse(y==2,vB-m,vS-m))-log(den)) }
o <- optim(setNames(rep(0,length(ip)),ip), nll_itt, method="BFGS", hessian=TRUE,
           control=list(maxit=1000,reltol=1e-11))
par <- o$par; V <- solve(o$hessian); dimnames(V) <- list(ip, ip)

## ---- registered ITT contrasts as weights on the 6 numerator part-worths ----
## (effects coding: e.g. protect-all[lvl1] - opt-in[ref lvl4] = 2*a1e1 + a1e2 + a1e3)
W <- list(
  `Public vs private\n(H1a — headline)`        = c(2, 1, 1, 2, 1, 0),
  `Public vs club\n(H1b)`                       = c(1,-1, 0, 0, 0, 0),
  `Targeting: protect-all\nvs opt-in (H1d)`     = c(2, 1, 1, 0, 0, 0),
  `Cost-share: flat\nvs risk-priced (H1c)`      = c(0, 0, 0, 2, 1, 0),
  `Effectiveness: most\nvs least (H1e)`         = c(0, 0, 0, 0, 0, 2))
fam <- c("Provision regime","Provision regime",
         "Distributive & design levers","Distributive & design levers","Distributive & design levers")

## ITT_C = WTP(treated) - WTP(control) for one weight vector w
itt_wtp <- function(p, w){
  bnum <- p[paste0("b_",num)]; tnum <- p[paste0("t_",num)]
  bc <- p["b_cost100"]; tc <- p["t_cost100"]
  wtpC <- -100 * sum(w*bnum)          / bc
  wtpT <- -100 * sum(w*(bnum+tnum))   / (bc + tc)
  unname(wtpT - wtpC) }

est <- sapply(W, function(w) itt_wtp(par, w))
se  <- sapply(W, function(w){
  g <- numDeriv::grad(function(p) itt_wtp(p, w), par)
  sqrt(as.numeric(t(g) %*% V %*% g)) })

## ---- inference: two-sided Wald + BH-FDR within each PAP family (1a / 1b) ----
## Family 1a = provision regime (H1a, H1b); Family 1b = distributive/design levers
## (H1c, H1d, H1e). PAP: two-sided Wald, BH-FDR within family; H1a is the single
## primary ITT, also reported undiscounted (its raw p).
z     <- est / se
p_raw <- 2 * pnorm(-abs(z))
p_bh  <- ave(p_raw, fam, FUN = function(pp) p.adjust(pp, method = "BH"))
hid   <- sub(".*\\((H1[a-e]).*", "\\1", names(W))          # pull H1a..H1e from label

D <- data.frame(contrast=factor(names(W), levels=rev(names(W))),
                family=factor(fam, levels=c("Provision regime","Distributive & design levers")),
                itt=est, lo=est-1.96*se, hi=est+1.96*se, row.names=NULL)

res <- data.frame(id=hid, family=fam, contrast=gsub("\n"," ",names(W)),
                  itt=round(est,1), se=round(se,1),
                  lo=round(est-1.96*se,1), hi=round(est+1.96*se,1),
                  z=round(z,2), p_raw=round(p_raw,3), p_bh=round(p_bh,3),
                  row.names=NULL)
cat("\n================ POOLED ITT (treated - control WTP, GBP/yr) ================\n")
cat("p_raw = two-sided Wald; p_bh = Benjamini-Hochberg FDR WITHIN family (1a / 1b).\n")
cat("H1a is the primary ITT -> read its p_raw (undiscounted) alongside p_bh.\n\n")
print(res, row.names=FALSE)
write.csv(res, "outputs/itt_pooled.csv", row.names=FALSE)
cat("\nwrote outputs/itt_pooled.csv\n")

## ---- figure ----
p <- ggplot(D, aes(itt, contrast, colour=family)) +
  geom_vline(xintercept=0, colour="grey60") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.20, linewidth=.8) +
  geom_point(size=3.4) +
  scale_colour_manual(values=c(`Provision regime`="#1b7837",
                               `Distributive & design levers`="#762a83"), name=NULL) +
  labs(x="ITT: treated - control WTP (£/year)   (>0 = information raises WTP for this contrast)",
       y=NULL,
       title="Does information move preferences on average? (pooled ITT)",
       subtitle=paste0("Full sample N=", length(unique(db$ID)),
         "; pooled over categories (no CATE). Tx x attribute fixed-coef MNL; bars = 95% CI (delta method).")) +
  theme_minimal(base_size=15) +
  theme(plot.subtitle=element_text(size=12, colour="grey30"),
        axis.text.y=element_text(face="bold"),
        legend.position="top")
ggsave("outputs/fig_itt_pooled.png", p, width=11.2, height=6.2, dpi=160)
cat("\nwrote outputs/fig_itt_pooled.png\n")
cat("\nMAKE_ITT_FIG_DONE\n")
