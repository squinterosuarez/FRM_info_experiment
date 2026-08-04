## pilot_a1_and_itt.R
##  (1) A1 coverage ("who is protected") WTP by prior-gap category -> figure.
##  (2) Plain ITT (treated vs control, NO category conditioning): fixed-coef MNL
##      with treatment x attribute interactions. Stable at N=60 (vs the MMNL).
suppressMessages(library(ggplot2))
db <- readRDS("data/pilot_database.rds")
suf <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1","cost100"); pnm <- c("ASC_SQ", suf)
XA <- as.matrix(db[, paste0("A_",suf)]); XB <- as.matrix(db[, paste0("B_",suf)])
y <- db$choice; Tt <- db$treatment

## ---------- shared fixed-coef MNL (subgroup) ----------
negll <- function(p, idx){ asc<-p[1]; b<-p[-1]
  vA<-XA[idx,]%*%b; vB<-XB[idx,]%*%b; vS<-rep(asc,length(idx))
  m<-pmax(vA,vB,vS); den<-exp(vA-m)+exp(vB-m)+exp(vS-m); yi<-y[idx]
  -sum(ifelse(yi==1,vA-m,ifelse(yi==2,vB-m,vS-m))-log(den)) }
wtp_ci <- function(par,V,k){ bk<-unname(par[k]); bc<-unname(par["cost100"]); w<- -100*bk/bc
  g<-c(-100/bc,100*bk/bc^2); ix<-c(k,"cost100"); se<-sqrt(as.numeric(t(g)%*%V[ix,ix]%*%g))
  c(wtp=w, lo=w-1.96*se, hi=w+1.96*se) }
fit_cat <- function(idx){ o<-optim(rep(0,length(pnm)),negll,idx=idx,method="BFGS",
  hessian=TRUE,control=list(maxit=600,reltol=1e-10))
  list(par=setNames(o$par,pnm), V=`dimnames<-`(solve(o$hessian),list(pnm,pnm))) }

## ---------- (1) A1 coverage figure ----------
cats <- list(`Underestimators\n(learned higher-risk)`=which(db$catUnder==1),
             `Correct\n(as expected)`=which(db$catOver==0&db$catUnder==0&db$catDK==0),
             `Overestimators\n(learned lower-risk)`=which(db$catOver==1))
lev <- c(a1e1="Protect ALL\nhouseholds", a1e2="High-risk\nareas only", a1e3="High-risk +\ndeprived priority")
D <- do.call(rbind, lapply(names(cats), function(nm){ f<-fit_cat(cats[[nm]])
  do.call(rbind, lapply(names(lev), function(k){
    data.frame(category=nm, level=lev[k], t(wtp_ci(f$par,f$V,k)), row.names=NULL) })) }))
D$category <- factor(D$category, levels=rev(names(cats)))
D$level    <- factor(D$level, levels=lev)
p <- ggplot(D, aes(wtp, category, colour=category)) +
  geom_vline(xintercept=0, colour="grey60") +
  geom_errorbarh(aes(xmin=lo,xmax=hi), height=.18, linewidth=.7) + geom_point(size=3) +
  facet_wrap(~level, ncol=1) +
  scale_colour_manual(values=c(`Underestimators\n(learned higher-risk)`="#b2182b",
    `Correct\n(as expected)`="grey40",`Overestimators\n(learned lower-risk)`="#2166ac"), guide="none") +
  labs(x="WTP (£/year) vs. opt-in only   (>0 = prefer this coverage; <0 = prefer opt-in)", y=NULL,
       title="Who should be protected? Coverage preference by what people learned",
       subtitle="Pilot N=60; fixed-coef MNL by surprise category; bars = 95% CI. Reference: opt-in only.") +
  theme_minimal(base_size=15.5) +
  theme(strip.text=element_text(face="bold"), 
        plot.subtitle=element_text(size=12.5,colour="grey30"),
        axis.text.y=element_text(face="bold")
        )
p
ggsave("outputs/fig_coverage_overunder.png", p, width=10.3, height=7.2, dpi=160)
cat("wrote outputs/fig_coverage_overunder.png\n\n")
cat("== A1 coverage WTP (vs opt-in) by category ==\n"); print(D[,c("category","level","wtp","lo","hi")], row.names=FALSE, digits=3)

## ---------- (2) plain ITT: treatment x attribute interactions ----------
ip <- c("ASC","tASC", paste0("b_",suf), paste0("t_",suf))   # base + treatment shift
nll_itt <- function(p){
  asc<-p["ASC"]; tasc<-p["tASC"]; b<-p[paste0("b_",suf)]; tb<-p[paste0("t_",suf)]
  vA<-XA%*%b + Tt*(XA%*%tb); vB<-XB%*%b + Tt*(XB%*%tb); vS<-asc + Tt*tasc
  m<-pmax(vA,vB,vS); den<-exp(vA-m)+exp(vB-m)+exp(vS-m)
  -sum(ifelse(y==1,vA-m,ifelse(y==2,vB-m,vS-m))-log(den)) }
o <- optim(setNames(rep(0,length(ip)),ip), nll_itt, method="BFGS", hessian=TRUE,
           control=list(maxit=1000,reltol=1e-11))
se <- sqrt(diag(solve(o$hessian)))
itt <- data.frame(param=ip, est=round(o$par,3), se=round(se,3), z=round(o$par/se,2))
treat_rows <- itt[grepl("^tASC$|^t_", itt$param), ]
cat("\n================ PLAIN ITT (treated - control, NO category conditioning) ================\n")
cat("Treatment-interaction terms (the ITT). tASC>0 = treatment pushes toward Keep-current;\n")
cat("t_<attr> = treatment shift in that attribute's weight.\n\n")
print(treat_rows, row.names=FALSE)
cat(sprintf("\nLR-style check: any treatment term |z|>1.96? -> %s\n",
            ifelse(any(abs(treat_rows$z)>1.96, na.rm=TRUE), "YES", "NO (no detectable pooled ITT)")))
cat("\nPILOT_A1_ITT_DONE\n")
