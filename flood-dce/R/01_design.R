## =====================================================================
## 01_design.R
## Build the choice design. Output: `design_wide`, one row per (block,task)
## with effects-coded columns for alternatives A and B. SQ carries no
## attribute columns (its whole utility is the ASC -- see README, point 1).
##
## Primary path: `read_locked_design(CFG$design_path)` reads the locked
## Ngene CSV in long format (one row per choice_set × alternative) and
## pivots it to one row per (block, task) with A_/B_ columns.
##
## Fallback: `build_placeholder_design()` produces a random main-effects
## design for pipeline testing only. Not for analysis.
## =====================================================================

## Read the locked design from a long-format CSV (columns: choice_set, block,
## alternative, A1_level..A4_level, A5_value [= cost in £]). Drops Status_Quo
## rows (the ASC absorbs SQ utility), pivots A/B into wide columns, and
## reindexes choice_set -> task (1..tasks_per_blk within block).
read_locked_design <- function(path = CFG$design_path) {
  if (!file.exists(path)) stop("Locked design not found at: ", path,
                               "\n  Override CFG$design_path or use build_placeholder_design().")
  d <- read.csv(path, stringsAsFactors = FALSE)
  ## Column map (2026-06-17, funding attribute dropped, instrument renumbered):
  ##   new A1_level = excludability  -> internal a1
  ##   new A2_level = fairness       -> internal a3
  ##   new A3_level = effectiveness  -> internal a4
  ##   new A4_value = cost (GBP)     -> internal cost
  ## (Internal names keep the original a1/a3/a4 gap; see 00_config.R note.)
  required <- c("choice_set","block","alternative",
                "A1_level","A2_level","A3_level","A4_value")
  miss <- setdiff(required, names(d))
  if (length(miss)) stop("Locked design missing columns: ", paste(miss, collapse=", "))
  d <- d[d$alternative != "Status_Quo", , drop = FALSE]
  d$cost <- d$A4_value
  alt_tag <- ifelse(d$alternative == "Program_A", "A",
             ifelse(d$alternative == "Program_B", "B", NA_character_))
  if (any(is.na(alt_tag))) stop("Unrecognised alternative labels in design.")
  ## one row per (block, choice_set); task = within-block order
  ids <- unique(d[, c("choice_set","block")])
  ids <- ids[order(ids$block, ids$choice_set), , drop = FALSE]
  ids$task <- ave(ids$choice_set, ids$block, FUN = seq_along)
  attrs <- c("A1_level","A2_level","A3_level","cost")
  new   <- c("a1","a3","a4","cost")
  out <- ids
  for (alt in c("A","B")) {
    sel <- d[alt_tag == alt, c("choice_set","block", attrs)]
    names(sel)[match(attrs, names(sel))] <- paste0(alt, "_", new)
    out <- merge(out, sel, by = c("choice_set","block"), all.x = TRUE, sort = FALSE)
  }
  out <- out[order(out$block, out$task), , drop = FALSE]
  out$choice_set <- NULL
  ## Range / structure checks
  stopifnot(all(out$A_a4 %in% ATTR$a4_levels), all(out$B_a4 %in% ATTR$a4_levels))
  stopifnot(all(out$A_cost %in% ATTR$cost_levels), all(out$B_cost %in% ATTR$cost_levels))
  ident <- with(out, A_a1==B_a1 & A_a3==B_a3 & A_a4==B_a4 & A_cost==B_cost)
  if (any(ident)) stop(sprintf("Locked design has %d task(s) where A == B.", sum(ident)))
  n_blocks_obs <- length(unique(out$block))
  ntask_obs    <- max(table(out$block))
  if (n_blocks_obs != CFG$n_blocks)
    stop(sprintf("Locked design has %d blocks; CFG$n_blocks = %d. Update CFG to match.",
                 n_blocks_obs, CFG$n_blocks))
  if (ntask_obs != CFG$tasks_per_blk)
    stop(sprintf("Locked design has %d tasks/block; CFG$tasks_per_blk = %d. Update CFG to match.",
                 ntask_obs, CFG$tasks_per_blk))
  if (!all(table(out$block) == CFG$tasks_per_blk))
    stop("Locked design has uneven block sizes: ", paste(table(out$block), collapse=", "))
  rownames(out) <- NULL
  out
}

## Randomised, constraint-respecting placeholder. Random main-effects designs
## identify the 10 params fine at this N; this is ONLY to exercise the
## pipeline, not a recommendation for the real instrument.
build_placeholder_design <- function() {
  n_sets <- CFG$n_blocks * CFG$tasks_per_blk           # 24
  draw_alt <- function() data.frame(
    a1   = sample(ATTR$a1_levels,   n_sets, TRUE),
    a3   = sample(ATTR$a3_levels,   n_sets, TRUE),
    a4   = sample(ATTR$a4_levels,   n_sets, TRUE),
    cost = sample(ATTR$cost_levels, n_sets, TRUE)
  )
  repeat {
    A <- draw_alt(); B <- draw_alt()
    ident <- rowSums(A == B) == ncol(A)               # forbid A == B in a task
    if (!any(ident)) break
  }
  d <- data.frame(
    block = rep(seq_len(CFG$n_blocks), each = CFG$tasks_per_blk),
    task  = rep(seq_len(CFG$tasks_per_blk), times = CFG$n_blocks)
  )
  for (nm in names(A)) d[[paste0("A_", nm)]] <- A[[nm]]
  for (nm in names(B)) d[[paste0("B_", nm)]] <- B[[nm]]
  d
}

## Add effects-coded columns (A_a1e1.. , B_a1e1.. , *_cost100) and check rank.
effects_code_design <- function(d) {
  code_alt <- function(d, alt) {
    pull <- function(a) d[[paste0(alt, "_", a)]]
    ec_mat <- function(vals, levels, base, pre) {
      m <- vapply(vals, ec_row, numeric(length(levels) - 1),
                  levels = levels, base = base, prefix = pre)
      ## vapply returns a matrix when >1 column, but a bare vector when the
      ## attribute has a single effects-coded column (e.g. 2-level A2). Coerce
      ## the single-column case to an n x 1 matrix with the right column name.
      if (is.matrix(m)) t(m)
      else matrix(m, ncol = 1, dimnames = list(NULL, paste0(pre, "e1")))
    }
    a1 <- ec_mat(pull("a1"), ATTR$a1_levels, EC$a1_base, paste0(alt, "_a1"))
    a3 <- ec_mat(pull("a3"), ATTR$a3_levels, EC$a3_base, paste0(alt, "_a3"))
    ## a4 (effectiveness) is binary in A/B: effects-coded indicator for level 1 (base = level 3)
    a4 <- matrix(ifelse(pull("a4") == 1, 1, -1), ncol = 1,
                 dimnames = list(NULL, paste0(alt, "_a4e1")))
    cost100 <- matrix(pull("cost") / 100, ncol = 1,
                      dimnames = list(NULL, paste0(alt, "_cost100")))
    cbind(a1, a3, a4, cost100)
  }
  coded <- cbind(d, code_alt(d, "A"), code_alt(d, "B"))

  ## Identification check: the A-vs-B difference matrix must be full column rank.
  diff_cols <- function(suf) coded[[paste0("A_", suf)]] - coded[[paste0("B_", suf)]]
  attr_suffix <- c("a1e1","a1e2","a1e3","a3e1","a3e2","a4e1","cost100")
  X <- sapply(attr_suffix, diff_cols)
  r <- qr(X)$rank
  if (r < ncol(X))
    warning(sprintf("Design A-vs-B contrast is rank-deficient (%d/%d). Attributes not identified.",
                    r, ncol(X)))
  else
    message(sprintf("01_design.R: A-vs-B contrast full rank (%d/%d). OK.", r, ncol(X)))
  coded
}

design_wide <- effects_code_design(read_locked_design())
message("01_design.R loaded (LOCKED design from ", CFG$design_path, "). design_wide: ",
        nrow(design_wide), " sets across ", CFG$n_blocks, " blocks (", CFG$tasks_per_blk,
        " tasks/block).")
