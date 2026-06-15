## make_distrib_figure.R — Figure 3: conditional ITTs on the policy attributes
## (distributive: flat cost-sharing, national funding; plus effectiveness),
## by prior-gap category. Recovered from the cached cate MC fits.
suppressMessages({
  library(ggplot2)
  source("R/00_config.R"); CFG$spec_type <- "cate"
  source("R/04_wtp.R"); source("R/06_outputs.R")
})

attr_wtp <- function(theta, g, which) {
  pw <- partworths(theta, g)
  d <- switch(which, national = pw$a2[1]-pw$a2[2], flat = pw$a3[1]-pw$a3[3],
              effective = pw$a4[1]-pw$a4[2])         # most(1) vs least(lvl3) effective
  unname(-100 * d / pw$cost)
}
cells <- list(Underestimators=c("trt_under","ctl_under"),
              `Don't-know`   =c("trt_dk","ctl_dk"),
              `Correct (placebo)`=c("trt_correct","ctl_correct"),
              Overestimators =c("trt_over","ctl_over"))
attrs <- c(flat="Flat (non-risk-priced) cost-sharing",
           national="National (cross-subsidy) funding",
           effective="Effectiveness (most vs least effective)")

shift <- function(theta, w, cl) {
  g <- cells[[cl]]
  attr_wtp(theta, GROUPS_CATE[[g[1]]], w) - attr_wtp(theta, GROUPS_CATE[[g[2]]], w)
}
grid <- expand.grid(w=names(attrs), cl=names(cells), stringsAsFactors=FALSE)
truth_v <- mapply(function(w,cl) shift(true_theta_vec_cate(), w, cl), grid$w, grid$cl)

files <- list.files("outputs/mc_cate_dir", pattern="^iter_\\d+\\.rds$", full.names=TRUE)
ok <- Filter(function(x) identical(x$status,"ok"), lapply(files, readRDS))
Mraw <- sapply(ok, function(x) mapply(function(w,cl) shift(x$estimate,w,cl), grid$w, grid$cl))
mc_mean <- rowMeans(Mraw); mc_sd <- apply(Mraw,1,sd)

D <- data.frame(grid, truth=truth_v, mc_mean=mc_mean, mc_sd=mc_sd)
D$lo <- D$mc_mean-1.96*D$mc_sd; D$hi <- D$mc_mean+1.96*D$mc_sd
D$facet <- factor(attrs[D$w], levels=attrs)          # Flat (top), National, Effectiveness
D$cl <- factor(D$cl, levels=c("Overestimators","Correct (placebo)","Don't-know","Underestimators"))

f3 <- ggplot(D, aes(mc_mean, cl)) +
  geom_vline(xintercept=0, colour="grey70") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.16, colour="grey50") +
  geom_point(aes(x=truth), shape=4, size=3, stroke=1.1, colour="red") +
  geom_point(size=3, colour="#2166ac") +
  facet_wrap(~facet, ncol=1) +
  labs(x="Shift in willingness to pay, treated − control (£/year)", y=NULL,
       title="Figure 3. Where the information effect lands, by what respondents learned",
       subtitle="Top two panels: distributive levers (who pays). Bottom: effectiveness (how well it works).\nDot = Monte Carlo mean, bar = 95% sampling interval, red × = true value") +
  theme_minimal(base_size=12) + theme(strip.text=element_text(face="bold", hjust=0))
ggsave("outputs/fig3_attributes.png", f3, width=7.4, height=7.4, dpi=160)
file.remove("outputs/fig3_distributive.png")          # superseded by the 3-panel version
cat("wrote outputs/fig3_attributes.png\n")
