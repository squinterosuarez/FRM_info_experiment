## make_crossover_figure.R — the mechanism picture: WTP for universal coverage
## (protect ALL vs opt-in, GBP/yr) by prior-gap category x arm. Fixed-coef MNL
## per cell; delta-method CIs on WTP = 100*b_a1e1 / (-b_cost100).
## Shows the pre-registered crossover: info walks overestimators DOWN and
## underestimators UP; correct-belief respondents do not move.
suppressMessages(library(ggplot2))
db <- readRDS("data/full_database.rds")
suf <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1","cost100")
XA <- as.matrix(db[, paste0("A_",suf)]); XB <- as.matrix(db[, paste0("B_",suf)])
y <- db$choice

negll <- function(p, idx){ asc<-p[1]; b<-p[-1]
  vA<-XA[idx,,drop=FALSE]%*%b; vB<-XB[idx,,drop=FALSE]%*%b; vS<-rep(asc,length(idx))
  m<-pmax(vA,vB,vS); den<-exp(vA-m)+exp(vB-m)+exp(vS-m); yi<-y[idx]
  v<-ifelse(yi==1,vA,ifelse(yi==2,vB,vS)); -sum((v-m)-log(den)) }

wtp_cell <- function(idx){
  o  <- optim(rep(0,8), negll, idx=idx, method="BFGS", hessian=TRUE)
  V  <- solve(o$hessian)
  b1 <- o$par[2]; bc <- o$par[8]                  # a1e1, cost100
  W  <- 100*b1/(-bc)
  g  <- c(-100/bc, 100*b1/bc^2)                   # dW/db1, dW/dbc
  se <- sqrt(t(g) %*% V[c(2,8),c(2,8)] %*% g)[1]
  c(wtp=W, lo=W-1.96*se, hi=W+1.96*se)
}

cells <- expand.grid(cat=c("Under","Correct","Over"), arm=0:1, stringsAsFactors=FALSE)
D <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i){
  idx <- which(db$catlab==cells$cat[i] & db$treatment==cells$arm[i])
  w   <- wtp_cell(idx)
  data.frame(cat=cells$cat[i], arm=cells$arm[i],
             n=length(unique(db$ID[idx])), t(w))
}))

catlabs <- c(Under   = "Underestimators\n(told: risk higher than you thought)",
             Correct = "Correct beliefs\n(told: risk as you expected)",
             Over    = "Overestimators\n(told: risk lower than you thought)")
D$catlab <- factor(catlabs[D$cat], levels=rev(catlabs[c("Over","Correct","Under")]))
D$armlab <- factor(ifelse(D$arm==0, "Control (info after DCE)", "Treated (info before DCE)"),
                   levels=c("Control (info after DCE)", "Treated (info before DCE)"))
## dodge: control upper row, treated lower row within each category band
D$ypos <- as.numeric(D$catlab) + ifelse(D$arm==0, .16, -.16)

## shift annotation per category (treated - control)
S <- do.call(rbind, lapply(split(D, D$cat), function(d)
  data.frame(cat=d$cat[1], y=as.numeric(d$catlab[1]),
             x0=d$wtp[d$arm==0], x1=d$wtp[d$arm==1],
             delta=d$wtp[d$arm==1]-d$wtp[d$arm==0])))
S$lab <- sprintf("shift %s£%.0f", ifelse(S$delta>=0,"+","−"), abs(S$delta))

p <- ggplot(D, aes(wtp, ypos, colour=armlab)) +
  geom_vline(xintercept=0, colour="grey60") +
  geom_segment(data=S, aes(x=x0, xend=x1, y=y, yend=y), inherit.aes=FALSE,
               colour="grey55", linewidth=.5,
               arrow=arrow(length=unit(6,"pt"), type="closed")) +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.12, linewidth=.8) +
  geom_point(size=3.4) +
  geom_text(data=S, aes(x=pmax(x0,x1)+8, y=y, label=lab), inherit.aes=FALSE,
            colour="grey30", size=3.9, hjust=0) +
  scale_colour_manual(values=c("Control (info after DCE)"="#2a78d6",
                               "Treated (info before DCE)"="#eb6834")) +
  scale_y_continuous(breaks=1:3, labels=levels(D$catlab),
                     limits=c(.6, 3.4), expand=c(0,0)) +
  scale_x_continuous(expand=expansion(mult=c(.05,.12))) +
  labs(title="Information walks the two surprised groups in opposite directions",
       subtitle=paste0("WTP for universal coverage (protect ALL households vs opt-in), £/household/yr.\n",
                       "Fixed-coefficient MNL per cell, 95% delta-method CIs. N=1053."),
       x="WTP (£/yr)", y=NULL, colour=NULL) +
  theme_minimal(base_size=15) +
  theme(plot.subtitle=element_text(size=11.5, colour="grey30"),
        legend.position="top", legend.justification="left",
        panel.grid.minor=element_blank(), panel.grid.major.y=element_blank())

ggsave("outputs/fig_crossover_universal.png", p, width=11.2, height=5.6, dpi=160)
for (i in seq_len(nrow(D))) with(D[i,], cat(sprintf(
  "%-8s arm=%d  n=%3d  WTP=%6.1f  [%6.1f, %6.1f]\n", cat, arm, n, wtp, lo, hi)))
cat("wrote outputs/fig_crossover_universal.png\n")
