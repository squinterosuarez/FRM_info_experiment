## make_costshare_figure.R — full-sample figure: preference over WHO PAYS (A3 cost-
## sharing) by prior-gap category. Fixed-coefficient MNL per category; WTP (GBP/yr)
## for each cost-split level vs the risk-priced reference, with delta-method 95% CIs.
suppressMessages(library(ggplot2))
db <- readRDS("data/full_database.rds")
suf <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1","cost100"); pnm <- c("ASC_SQ", suf)
XA <- as.matrix(db[, paste0("A_",suf)]); XB <- as.matrix(db[, paste0("B_",suf)]); y <- db$choice

negll <- function(p, idx){ asc<-p[1]; b<-p[-1]
  vA<-XA[idx,]%*%b; vB<-XB[idx,]%*%b; vS<-rep(asc,length(idx))
  m<-pmax(vA,vB,vS); den<-exp(vA-m)+exp(vB-m)+exp(vS-m); yi<-y[idx]
  -sum(ifelse(yi==1,vA-m,ifelse(yi==2,vB-m,vS-m))-log(den)) }

## WTP (GBP/yr) for attribute coef k vs reference, with delta-method SE
wtp_ci <- function(par, V, k){
  bk<-unname(par[k]); bc<-unname(par["cost100"]); w <- -100*bk/bc
  g <- c(-100/bc, 100*bk/bc^2); ix <- c(k,"cost100")
  se <- sqrt(as.numeric(t(g) %*% V[ix,ix] %*% g))
  c(wtp=w, lo=w-1.96*se, hi=w+1.96*se)
}
fit_cat <- function(idx){
  o <- optim(rep(0,length(pnm)), negll, idx=idx, method="BFGS", hessian=TRUE,
             control=list(maxit=500,reltol=1e-10))
  par <- setNames(o$par,pnm); V <- solve(o$hessian); dimnames(V) <- list(pnm,pnm)
  rbind(`Flat:\neveryone pays same`         = wtp_ci(par,V,"a3e1"),
        `Wealthier\npay more`               = wtp_ci(par,V,"a3e2"))
}
cats <- list(`Underestimators\n(learned higher-risk)` = which(db$catUnder==1),
             `Correct\n(as expected)`                 = which(db$catOver==0 & db$catUnder==0 & db$catDK==0),
             `Overestimators\n(learned lower-risk)`   = which(db$catOver==1))
D <- do.call(rbind, lapply(names(cats), function(nm){
  m <- fit_cat(cats[[nm]]); data.frame(category=nm, level=rownames(m), m, row.names=NULL) }))
D$category <- factor(D$category, levels=rev(names(cats)))   # under at top

p <- ggplot(D, aes(wtp, category, colour=category)) +
  geom_vline(xintercept=0, colour="grey60") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.18, linewidth=.7) +
  geom_point(size=3) +
  facet_wrap(~level, ncol=1) +
  scale_colour_manual(values=c(`Underestimators\n(learned higher-risk)`="#b2182b",
                               `Correct\n(as expected)`="grey40",
                               `Overestimators\n(learned lower-risk)`="#2166ac"), guide="none") +
  labs(x="WTP (£/year) vs. “those most at risk pay” (>0 = prefer this option; <0 = prefer risk-priced)",
       y=NULL,
       title="Who should pay? Cost-sharing preference by what people learned",
       subtitle=paste0("Full sample N=", length(unique(db$ID)),
         "; fixed-coefficient MNL by surprise category; bars = 95% CI (delta method).\nOverestimators (learned lower-risk) lean to risk-priced; underestimators lean to flat.")) +
  theme_minimal(base_size=15.5) +
  theme(strip.text=element_text(face="bold"), 
        plot.subtitle=element_text(size=12.5, colour="grey30"),
        axis.text.y=element_text(face="bold"))
p
ggsave("outputs/fig_costshare_overunder.png", p, width=10.5, height=7.2, dpi=160)
cat("wrote outputs/fig_costshare_overunder.png\n\n")
print(D, row.names=FALSE, digits=3)
cat("\nCOSTSHARE_FIG_DONE\n")
