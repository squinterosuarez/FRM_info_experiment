## =====================================================================
## 05_subgroups.R  (directional version)
## The directional model already absorbs updater effects via interactions,
## so subgroups here are split-sample ROBUSTNESS refits. Default split:
## actual high-risk vs not. For real data, swap `split_col` for flood
## experience / insurance / ideology (PAP secondary analyses).
##
## Reduced-model logic: any directional interaction column that is
## structurally constant within the subgroup (e.g. gapDown == 0 for all
## high-risk respondents because actual_rank == 4 caps the gap at zero)
## non-identifies its coefficient. We detect such columns at the
## subgroup level and DROP the corresponding parameters from the model
## entirely (via omit_params on fit_mmnl). Holding them at 0 with
## apollo_fixed instead produces a singular gradient and an apollo
## internal dim-mismatch when the identifying column is literally zero.
## =====================================================================

## Directional interaction columns per parameter family.
## Each entry is a function(db) -> the within-respondent design column whose
## variation identifies the family. If var(col) == 0 on the subgroup, every
## parameter in that family is non-identified and gets dropped from the model.
SUBGROUP_DROP_RULES <- list(
  dt    = function(db) db$treatment,
  up    = function(db) db$treatment * db$gapUp,
  dn    = function(db) db$treatment * db$gapDown,
  up_np = function(db) db$treatment * db$gapUp   * db$noPrior,
  dn_np = function(db) db$treatment * db$gapDown * db$noPrior
)

## Return parameter names in `start` that should be dropped from the model
## because their identifying design column has zero variance on subgroup `db`.
detect_omit_params <- function(db, start) {
  omit <- character(0)
  for (fam in names(SUBGROUP_DROP_RULES)) {
    col <- SUBGROUP_DROP_RULES[[fam]](db)
    if (length(col) == 0 || stats::var(col) <= .Machine$double.eps) {
      hits <- grep(paste0("^", fam, "_"), names(start), value=TRUE)
      omit <- c(omit, hits)
    }
  }
  np_col <- db$treatment * db$noPrior
  if (length(np_col) == 0 || stats::var(np_col) <= .Machine$double.eps) {
    if ("np_asc" %in% names(start)) omit <- c(omit, "np_asc")
  }
  unique(omit)
}

fit_split <- function(database, mask, name) {
  ids <- unique(database$ID[mask]); db <- database[database$ID %in% ids,]
  db <- db[order(db$ID, db$task),]
  if (length(unique(db$ID))<150) warning(sprintf("Subgroup '%s': only %d respondents.",
                                                  name, length(unique(db$ID))))
  start <- build_start(correlated=FALSE, cost_random=CFG$cost_random,
                       asc_random=CFG$asc_random, noprior_separate=FALSE)
  omit <- detect_omit_params(db, start)
  if (length(omit))
    message(sprintf("Subgroup '%s': dropping %d non-identified params: %s",
                    name, length(omit), paste(omit, collapse=", ")))
  fit_mmnl(db, paste0("sg_",name), silent=TRUE, compute_hessian=FALSE,
           omit_params=omit)
}

run_subgroups <- function(database, split_col="risk_high") {
  groups <- list(high = database[[split_col]]==1, not_high = database[[split_col]]==0)
  models <- lapply(names(groups), function(g)
    tryCatch(fit_split(database, groups[[g]], g),
             error=function(e) {
               warning(sprintf("Subgroup '%s' fit FAILED: %s", g, conditionMessage(e)),
                       immediate.=TRUE, call.=FALSE)
               NULL
             }))
  names(models) <- names(groups)
  rows <- list()
  for (g in names(models)) { m <- models[[g]]; if (is.null(m)) next
    for (b in setdiff(names(BUNDLES), BUNDLE_REF))
      rows[[length(rows)+1]] <- data.frame(subgroup=g, bundle=b,
        wtp_vs_welfare = bundle_wtp(m$estimate, b,
          if (identical(CFG$spec_type,"pap")) GROUPS_PAP[["ctl_prior"]] else GROUPS_DIR[["control"]])) }
  tbl <- do.call(rbind, rows)
  write.csv(tbl, file.path(PATHS$out,"wtp_subgroups.csv"), row.names=FALSE)
  list(models=models, wtp=tbl)
}
if (exists("database")) { subgroups <- run_subgroups(database); print(subgroups$wtp) }
message("05_subgroups.R (directional) loaded.")
