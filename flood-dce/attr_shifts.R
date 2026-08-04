## attr_shifts.R — conditional ITTs on ALL policy attributes (targeting,
## flat cost-sharing, effectiveness), by prior-gap category. Truth from
## true_theta_vec_cate(); recovery from cached cate MC fits (no re-run).
## (The funding attribute was dropped, so the "national" estimand is gone.)
suppressMessages({
  source("R/00_config.R"); CFG$spec_type <- "cate"
  source("R/04_wtp.R"); source("R/06_outputs.R")
})
sel <- function(v) v[grepl("^ACITT_", names(v))]
truth <- sel(target_wtps_cate(true_theta_vec_cate()))
files <- list.files("outputs/mc_cate_dir", pattern="^iter_\\d+\\.rds$", full.names=TRUE)
ok <- Filter(function(x) identical(x$status,"ok"), lapply(files, readRDS))
M <- do.call(rbind, lapply(ok, function(x) sel(target_wtps_cate(x$estimate))))
mc_mean <- colMeans(M); mc_sd <- apply(M,2,sd)
appx_pow <- pnorm(abs(truth)/mc_sd - 1.96) + pnorm(-abs(truth)/mc_sd - 1.96)
res <- data.frame(quantity=names(truth), truth=round(truth,1), mc_mean=round(mc_mean,1),
                  mc_sd=round(mc_sd,1), approx_power=round(appx_pow,2), row.names=NULL)
for (a in c("targeting","flat","effective")) {
  cat("\n====", toupper(a), "(treated − control, £/yr) ====\n")
  print(res[grepl(paste0("^ACITT_",a,"__"), res$quantity),], row.names=FALSE)
}
cat("\nATTR_DONE\n")
