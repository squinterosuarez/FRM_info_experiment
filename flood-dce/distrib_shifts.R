## distrib_shifts.R — conditional ITTs on the DISTRIBUTIVE attribute
## (a3 cost-sharing: flat vs risk-priced), by prior-gap category. Truth from
## true_theta_vec_cate(); recovery from the cached cate MC estimates (no re-run).
## (The funding attribute was dropped, so the national-vs-local estimand is gone.)
suppressMessages({
  source("R/00_config.R"); CFG$spec_type <- "cate"
  source("R/04_wtp.R"); source("R/06_outputs.R")
})

## single-attribute WTP (£/yr) for a level contrast, same convention as bundle_wtp
attr_wtp <- function(theta, g, which) {
  pw <- partworths(theta, g)
  d <- switch(which,
    flat     = pw$a3[1] - pw$a3[3])   # flat (1) vs risk-priced (3) cost-sharing
  unname(-100 * d / pw$cost)
}
cells <- list(under=c("trt_under","ctl_under"), over=c("trt_over","ctl_over"),
              correct=c("trt_correct","ctl_correct"), dk=c("trt_dk","ctl_dk"))

## conditional ITT (treated − control) on each distributive attribute, per cell
distrib <- function(theta) {
  out <- numeric(0)
  for (w in c("flat")) for (cl in names(cells)) {
    g <- cells[[cl]]
    out[paste0(w,"_",cl)] <- attr_wtp(theta, GROUPS_CATE[[g[1]]], w) -
                             attr_wtp(theta, GROUPS_CATE[[g[2]]], w)
  }
  out
}

truth <- distrib(true_theta_vec_cate())

## recover from cached cate MC point estimates
files <- list.files("outputs/mc_cate_dir", pattern="^iter_\\d+\\.rds$", full.names=TRUE)
ok <- Filter(function(x) identical(x$status,"ok"), lapply(files, readRDS))
M <- do.call(rbind, lapply(ok, function(x) distrib(x$estimate)))
mc_mean <- colMeans(M); mc_sd <- apply(M,2,sd)
## approx power using the MC sampling SD as the SE (delta SE ≈ MC SD here, coverage ~95%)
appx_pow <- pnorm(abs(truth)/mc_sd - 1.96) + pnorm(-abs(truth)/mc_sd - 1.96)

res <- data.frame(quantity=names(truth), truth=round(truth,1),
                  mc_mean=round(mc_mean,1), mc_sd=round(mc_sd,1),
                  approx_power=round(appx_pow,2), row.names=NULL)
cat("\n==== Conditional ITTs on the DISTRIBUTIVE attribute (£/yr, treated − control by category) ====\n")
cat("flat = WTP for flat vs risk-priced cost-sharing\n\n")
print(res[grepl("^flat",res$quantity),], row.names=FALSE)

## context: baseline (control) levels of each distributive WTP, by category
cat("\n---- baseline (control-arm) distributive WTP levels, for context ----\n")
base <- data.frame(
  cell=names(cells),
  flat_ctl    =round(sapply(cells, function(g) attr_wtp(true_theta_vec_cate(), GROUPS_CATE[[g[2]]], "flat")),1),
  row.names=NULL)
print(base, row.names=FALSE)
cat("\nDISTRIB_DONE\n")
