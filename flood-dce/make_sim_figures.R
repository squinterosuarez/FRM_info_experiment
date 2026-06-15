## make_sim_figures.R — paper figures from the completed itt/cate Monte Carlo.
suppressMessages(library(ggplot2))
itt  <- read.csv("outputs/mc_summary_itt_dir.csv",  stringsAsFactors=FALSE)
cate <- read.csv("outputs/mc_summary_cate_dir.csv", stringsAsFactors=FALSE)
g <- function(df, q) df[df$quantity==q, c("truth","mc_mean","mc_sd","power_alpha05","coverage_95")]

## ---- assemble a tidy frame of the headline quantities ----
spec <- list(
  list(itt,  "public_vs_private__control", "Control: public vs private", "Preference (control)"),
  list(itt,  "public_vs_club__control",    "Control: public vs club",    "Preference (control)"),
  list(itt,  "club_vs_private__control",   "Control: club vs private",   "Preference (control)"),
  list(itt,  "ITT_public_vs_private",      "ITT: public vs private",     "Average effect (ITT)"),
  list(itt,  "ITT_public_vs_club",         "ITT: public vs club",        "Average effect (ITT)"),
  list(cate, "CITT_under__public_vs_private",   "Underestimators", "Conditional ITT"),
  list(cate, "CITT_dk__public_vs_private",      "Don't-know",      "Conditional ITT"),
  list(cate, "CITT_correct__public_vs_private", "Correct (placebo)", "Conditional ITT"),
  list(cate, "CITT_over__public_vs_private",    "Overestimators",  "Conditional ITT")
)
D <- do.call(rbind, lapply(spec, function(s) {
  r <- g(s[[1]], s[[2]]); r$label <- s[[3]]; r$group <- s[[4]]; r
}))
D$lo <- D$mc_mean - 1.96*D$mc_sd; D$hi <- D$mc_mean + 1.96*D$mc_sd

## ---- Figure 1: recovery (true value vs Monte Carlo mean) ----
f1 <- ggplot(D, aes(truth, mc_mean, colour=group)) +
  geom_abline(slope=1, intercept=0, linetype=2, colour="grey55") +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0, alpha=.5) +
  geom_point(size=2.6) +
  scale_colour_manual(values=c("Preference (control)"="#1b9e77",
                               "Average effect (ITT)"="#7570b3",
                               "Conditional ITT"="#d95f02")) +
  labs(x="True value (£/year)", y="Monte Carlo mean estimate (£/year)",
       colour=NULL,
       title="Figure 1. The pipeline recovers the truth",
       subtitle="200 replications; points on the dashed 45° line = unbiased recovery") +
  theme_minimal(base_size=12) + theme(legend.position="bottom")
ggsave("outputs/fig1_recovery.png", f1, width=7, height=5.2, dpi=160)

## ---- Figure 2: the mechanism — conditional ITTs by prior-gap category ----
C <- D[D$group=="Conditional ITT", ]
C$label <- factor(C$label, levels=c("Overestimators","Correct (placebo)","Don't-know","Underestimators"))
C$plab  <- sprintf("power %.0f%%", 100*C$power_alpha05)
f2 <- ggplot(C, aes(mc_mean, label)) +
  geom_vline(xintercept=0, colour="grey70") +
  geom_errorbarh(aes(xmin=lo, xmax=hi), height=.16, colour="grey50") +
  geom_point(aes(x=truth), shape=4, size=3, stroke=1.1, colour="red") +
  geom_point(size=3, colour="#d95f02") +
  geom_text(aes(label=plab), hjust=0, nudge_y=.22, size=3.3, colour="grey35") +
  labs(x="Effect of information on public-vs-private WTP (£/year)", y=NULL,
       title="Figure 2. The mechanism the design is built to detect",
       subtitle="Dot = Monte Carlo mean, bar = 95% sampling interval, red × = true value") +
  theme_minimal(base_size=12)
ggsave("outputs/fig2_mechanism.png", f2, width=7.4, height=4.2, dpi=160)

cat("wrote outputs/fig1_recovery.png and outputs/fig2_mechanism.png\n")
print(D[,c("label","truth","mc_mean","power_alpha05","coverage_95")], row.names=FALSE)
