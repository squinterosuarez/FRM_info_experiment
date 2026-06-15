## make_attr_tables.R — Tables to accompany Figure 3 (fig3_attributes.png).
## Three attribute-level conditional-ITT tables, by pre-treatment prior-gap
## category, each reporting Truth / MC mean / Power / Coverage. Values are the
## ACITT_* rows of the completed cate Monte Carlo (mc_summary_cate_dir.csv) —
## the same quantities plotted in Figure 3.
cate <- read.csv("outputs/mc_summary_cate_dir.csv", stringsAsFactors = FALSE)

## attribute family -> (csv key, table title)
attrs <- list(
  national  = "Table N. WTP for national (cross-subsidy) vs. local funding",
  flat      = "Table R. WTP for flat (non-risk-priced) cost-sharing [redistributive mechanism]",
  effective = "Table E. WTP for scheme effectiveness (most vs. least effective)"
)
## category cell -> display label, in the same order as Table X Panel C
cells <- list(
  under   = "Underestimators (learned higher-risk)",
  over    = "Overestimators (learned lower-risk)",
  correct = "Correct estimators *(internal placebo)*",
  dk      = "Don't-know group"
)

fmt_eff <- function(x) {                       # signed integer £ (matches Table X)
  s <- ifelse(round(x) > 0, "+", "")
  paste0(s, format(round(x), trim = TRUE))
}

emit <- function(an, title) {
  rows <- lapply(names(cells), function(cl) {
    r <- cate[cate$quantity == sprintf("ACITT_%s__%s", an, cl), ]
    sprintf("| %s | %s | %s | %.2f | %.2f |",
            cells[[cl]], fmt_eff(r$truth), fmt_eff(r$mc_mean),
            r$power_alpha05, r$coverage_95)
  })
  c(sprintf("**%s.**", title),
    "All quantities are the treated − control shift in WTP (£/year).",
    "",
    "| Prior-gap category | Truth | MC mean | Power | Coverage |",
    "|---|---:|---:|---:|---:|",
    unlist(rows), "")
}

out <- unlist(lapply(names(attrs), function(an) emit(an, attrs[[an]])))
writeLines(out, "outputs/attr_tables.md")
cat(out, sep = "\n")
