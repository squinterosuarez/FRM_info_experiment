# ==============================================================
# DCE WORKFLOW - MASTER SCRIPT
# ==============================================================
#
# Usage:  Rscript run.R
#
# Edit config.R to change the experiment, then re-run this script.
# All output files are written to /output.
# ==============================================================


# Ensure UTF-8 output for non-ASCII characters (\u00a3, \u2014, etc.) in CSVs and
# Qualtrics files. R's default "C" locale writes <U+xxxx> escapes instead of
# the actual characters. We try several common UTF-8 locale names and fall
# back silently if none are available -- subsequent writeLines/write.csv
# calls still pass enc2utf8(...) explicitly for robustness.
for (loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_GB.UTF-8")) {
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
}

# jsonlite is used to safely emit the LABELS object as JSON
# (handles all string escaping correctly, including quotes, backslashes,
# newlines, "</script>", and non-ASCII characters).
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install with: install.packages('jsonlite')")
}

cat("\n")
cat("============================================================\n")
cat("  DCE WORKFLOW - Generating design and Qualtrics files\n")
cat("============================================================\n\n")

# --------------------------------------------------------------
# LOAD CONFIGURATION
# --------------------------------------------------------------

# FIX 8: robust script-directory detection that works under both
# `Rscript run.R` and `source("run.R")`.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  src <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(src)) return(dirname(normalizePath(src)))
  getwd()
}
script_dir <- get_script_dir()
setwd(script_dir)

source("config.R")
cat("Configuration loaded from config.R\n")

OUTPUT_DIR <- file.path(script_dir, "output")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# Back-compatible defaults for newly added config parameters.
if (!exists("N_DRAWS"))    N_DRAWS    <- 20
if (!exists("COST_SCALE")) COST_SCALE <- 100

# FIX 7: explicit divisibility check on blocking.
if (N_SETS %% N_BLOCKS != 0) {
  stop(sprintf("N_SETS (%d) must be divisible by N_BLOCKS (%d). Edit config.R.",
               N_SETS, N_BLOCKS))
}


# ==============================================================
# PART 1: DERIVE DESIGN PARAMETERS FROM CONFIG
# ==============================================================

n_attr <- length(ATTRIBUTES)
attr_keys <- names(ATTRIBUTES)

sq_levels <- integer(n_attr)
ab_levels <- vector("list", n_attr)
names(sq_levels) <- attr_keys
names(ab_levels) <- attr_keys

for (k in attr_keys) {
  a <- ATTRIBUTES[[k]]
  sq_levels[k] <- a$sq
  if (is.null(a$ab_only)) {
    ab_levels[[k]] <- seq_along(a$levels)
  } else {
    ab_levels[[k]] <- a$ab_only
  }
}

# Identify the cost attribute
cost_key <- NULL
for (k in attr_keys) {
  if (isTRUE(ATTRIBUTES[[k]]$is_cost)) {
    cost_key <- k
    break
  }
}
if (is.null(cost_key)) stop("No attribute has is_cost = TRUE. One attribute must be the cost.")

# Count effects-coded parameters and build descriptive parameter names.
# IMPORTANT: parameters are counted over the levels that actually appear in
# the A/B alternatives (`ab_levels[[k]]`), NOT over the full level set.
#
# Why: when an attribute's SQ-level is outside `ab_only`, that level only ever
# appears in the SQ alternative. Its effect cannot be separated from ASC_SQ
# (the two are perfectly collinear), so we drop the corresponding dummy and
# let ASC_SQ absorb the missing-level effect. With effects coding, an
# attribute with K_ab levels contributes K_ab - 1 parameters.
n_effects_params <- 0
param_names <- "ASC_SQ"
absorbed_msgs <- character()
for (k in attr_keys) {
  a <- ATTRIBUTES[[k]]
  if (k == cost_key) {
    n_effects_params <- n_effects_params + 1   # continuous
    param_names <- c(param_names, sprintf("%s (per %g)", a$name, COST_SCALE))
  } else {
    ab_lv <- ab_levels[[k]]
    n_p <- length(ab_lv) - 1
    n_effects_params <- n_effects_params + n_p
    if (n_p > 0) {
      for (j in seq_len(n_p)) {
        param_names <- c(param_names,
                         sprintf("%s_L%d_vs_L%d", a$name, ab_lv[j], ab_lv[length(ab_lv)]))
      }
    }
    if (!is.null(a$ab_only) && !(a$sq %in% a$ab_only)) {
      absorbed_msgs <- c(absorbed_msgs,
                         sprintf("    %s: SQ-level=%d, A/B-levels={%s} (effect of L%d absorbed into ASC_SQ)",
                                 a$name, a$sq, paste(a$ab_only, collapse=","), a$sq))
    }
  }
}
N_PAR <- n_effects_params + 1   # +1 for ASC_SQ

cat(sprintf("  Attributes:  %d\n", n_attr))
cat(sprintf("  Parameters:  %d (incl. ASC_SQ)\n", N_PAR))
if (length(absorbed_msgs) > 0) {
  cat("  Identification: SQ-level effects absorbed into ASC_SQ for:\n")
  for (msg in absorbed_msgs) cat(msg, "\n", sep = "")
}

# Validate priors
if (length(PRIOR_MEAN) != N_PAR) {
  stop(sprintf("PRIOR_MEAN has %d elements but %d parameters expected. Check config.R.",
               length(PRIOR_MEAN), N_PAR))
}
if (length(PRIOR_SD) != N_PAR) {
  stop(sprintf("PRIOR_SD has %d elements but %d parameters expected. Check config.R.",
               length(PRIOR_SD), N_PAR))
}
if (any(PRIOR_SD < 0)) stop("PRIOR_SD entries must be non-negative.")


# ==============================================================
# PART 2: CODING FUNCTIONS
# ==============================================================

effects_code <- function(level, n_levels) {
  code <- rep(0, n_levels - 1)
  if (level == n_levels) {
    code <- rep(-1, n_levels - 1)
  } else {
    code[level] <- 1
  }
  code
}

# FIX 10: cost rescaling now uses the configurable COST_SCALE instead of a
# hidden /100 inside the function body. The cost prior must be on the same scale.
#
# IDENTIFICATION FIX: encode each non-cost attribute over its `ab_levels`
# only. If the SQ alternative carries a level that does NOT appear in the
# A/B alternatives (i.e. ab_only excludes it), the SQ contributes zeros for
# that attribute's effects -- ASC_SQ absorbs the missing-level effect.
# Without this, ASC_SQ + (sum of dummies) is constant across alternatives
# and the design's information matrix is rank-deficient by one.
code_profile <- function(lvls) {
  coded <- c()
  for (i in seq_along(attr_keys)) {
    k <- attr_keys[i]
    a <- ATTRIBUTES[[k]]
    if (k == cost_key) {
      coded <- c(coded, a$cost_values[lvls[i]] / COST_SCALE)
    } else {
      ab_lv <- ab_levels[[k]]
      n_p  <- length(ab_lv) - 1
      if (n_p == 0) next   # degenerate: single-level attribute, no params
      pos <- match(lvls[i], ab_lv)
      if (is.na(pos)) {
        # SQ alt at a level outside ab_only: zero contribution.
        coded <- c(coded, rep(0, n_p))
      } else {
        coded <- c(coded, effects_code(pos, length(ab_lv)))
      }
    }
  }
  coded
}


# ==============================================================
# PART 3: CANDIDATE SET
# ==============================================================

cand_grid <- expand.grid(ab_levels)
names(cand_grid) <- attr_keys

# FIX 2: drop any candidate whose level vector exactly matches the SQ on
# every attribute. With this config, A5 and A6 already exclude their SQ
# levels via `ab_only`, so the filter removes nothing -- but for any future
# config where all attributes have ab_only = NULL, this stops the optimiser
# from placing a designed alternative that is identical to the status quo.
sq_vec_for_ab <- sq_levels[attr_keys]
sq_match <- apply(cand_grid, 1, function(row) all(row == sq_vec_for_ab))
n_dropped <- sum(sq_match)
if (n_dropped > 0) {
  cand_grid <- cand_grid[!sq_match, , drop = FALSE]
  cat(sprintf("  Dropped %d candidate(s) identical to SQ\n", n_dropped))
}
rownames(cand_grid) <- NULL
N_CAND <- nrow(cand_grid)

cand_coded <- t(apply(cand_grid, 1, function(row) code_profile(as.numeric(row))))

sq_coded <- code_profile(sq_levels)
sq_full  <- c(1, sq_coded)   # ASC_SQ = 1
cat(sprintf("  Candidates:  %d profiles\n", N_CAND))


# ==============================================================
# PART 4: BAYESIAN D-EFFICIENT DESIGN (MODIFIED FEDEROV)
# ==============================================================

# FIX 1: actually use PRIOR_SD. We draw `N_DRAWS` parameter vectors from
# the prior and minimise the *expected* D-error. If all PRIOR_SD == 0 the
# code reduces to local D-efficiency at PRIOR_MEAN automatically (1 draw).

is_local_d <- all(PRIOR_SD == 0) || N_DRAWS <= 1
if (is_local_d) {
  beta_draws <- matrix(PRIOR_MEAN, nrow = 1)
  cat(sprintf("\nObjective:   LOCAL D-error (point estimate at PRIOR_MEAN)\n"))
} else {
  set.seed(SEED)
  z <- matrix(rnorm(N_DRAWS * N_PAR), N_DRAWS, N_PAR)
  beta_draws <- sweep(sweep(z, 2, PRIOR_SD, "*"), 2, PRIOR_MEAN, "+")
  cat(sprintf("\nObjective:   BAYESIAN D-error over %d prior draws\n", N_DRAWS))
}
R_DRAWS <- nrow(beta_draws)
beta_draws_t <- t(beta_draws)   # N_PAR x R, used inside the inner loop

# Per-set, per-draw information matrix. Returns a list of length R.
set_info_all <- function(idx1, idx2) {
  X <- rbind(c(0, cand_coded[idx1, ]),
             c(0, cand_coded[idx2, ]),
             sq_full)
  V <- X %*% beta_draws_t                             # 3 x R
  V <- sweep(V, 2, apply(V, 2, max), "-")             # numerical stabilisation
  EV <- exp(V)
  P <- sweep(EV, 2, colSums(EV), "/")                 # 3 x R
  out <- vector("list", R_DRAWS)
  for (r in seq_len(R_DRAWS)) {
    p <- P[, r]
    out[[r]] <- crossprod(X, (diag(p) - tcrossprod(p)) %*% X)
  }
  out
}

d_error_one <- function(info_mat) {
  d <- determinant(info_mat, logarithm = TRUE)
  if (d$sign <= 0 || !is.finite(d$modulus)) return(Inf)
  exp(-as.numeric(d$modulus) / N_PAR)
}

# Mean D-error across draws (== local D-error when R_DRAWS == 1)
mean_d_error <- function(info_list) {
  derrs <- vapply(info_list, d_error_one, numeric(1))
  mean(derrs)
}

# Optimised candidate-evaluation path using the matrix-determinant lemma
# (Sylvester's identity):
#     det(A + X' M X) = det(A) * det(I_3 + M X A^{-1} X')
# A == info_other (K x K) is shared across all candidates at a given (s, j),
# so we invert it once and reuse. Per candidate we then only need the K x K
# multiplication X %*% A^{-1} and a 3x3 determinant -- ~3-5x faster than
# evaluating det of the full K x K info matrix per candidate.
prep_swap_state <- function(info_other) {
  info_other_inv <- vector("list", R_DRAWS)
  log_det_other  <- numeric(R_DRAWS)
  ok <- logical(R_DRAWS)
  for (r in seq_len(R_DRAWS)) {
    A <- info_other[[r]]
    ld <- determinant(A, logarithm = TRUE)
    if (ld$sign > 0 && is.finite(ld$modulus)) {
      inv <- tryCatch(solve(A), error = function(e) NULL)
      if (!is.null(inv)) {
        info_other_inv[[r]] <- inv
        log_det_other[r]    <- as.numeric(ld$modulus)
        ok[r] <- TRUE
      }
    }
  }
  list(info_other = info_other, inv = info_other_inv,
       log_det = log_det_other, ok = ok)
}

candidate_mean_d_err <- function(state, idx1, idx2) {
  X <- rbind(c(0, cand_coded[idx1, ]),
             c(0, cand_coded[idx2, ]),
             sq_full)
  V <- X %*% beta_draws_t
  V <- sweep(V, 2, apply(V, 2, max), "-")
  EV <- exp(V)
  P <- sweep(EV, 2, colSums(EV), "/")
  derrs <- numeric(R_DRAWS)
  I3 <- diag(3)
  for (r in seq_len(R_DRAWS)) {
    p <- P[, r]
    M <- diag(p) - tcrossprod(p)
    if (state$ok[r]) {
      # Sylvester form
      XAX <- X %*% state$inv[[r]] %*% t(X)            # 3x3
      ldK <- determinant(I3 + M %*% XAX, logarithm = TRUE)
      if (ldK$sign > 0 && is.finite(ldK$modulus)) {
        log_det_full <- state$log_det[r] + as.numeric(ldK$modulus)
        derrs[r] <- exp(-log_det_full / N_PAR)
      } else {
        derrs[r] <- Inf
      }
    } else {
      # Fallback: rank-deficient info_other, evaluate full K x K det
      info_new <- state$info_other[[r]] + crossprod(X, M %*% X)
      derrs[r] <- d_error_one(info_new)
    }
  }
  mean(derrs)
}

# Vectorised batch evaluator: computes mean D-error for ALL N_CAND candidates
# at once, given a fixed `other_idx` and j-position. Uses BLAS-friendly matrix
# products to avoid R loop overhead in the per-candidate hot path.
#
# Mathematical structure: for each candidate c, X is 3 x K with rows
# (x_c, x_o, x_s) where x_o (other) and x_s (SQ) are FIXED across candidates.
# Only x_c varies. For each draw r, we compute X %*% inv %*% X^T as a 3x3
# matrix whose blocks are: scalar x_c' inv x_c (varies), 1x2 row x_c' inv [x_o x_s]
# (varies), and the 2x2 [x_o x_s]' inv [x_o x_s] (fixed). The variable parts
# vectorise across candidates as cand_full %*% inv (one BLAS matmul).
# Returns a vector of mean D-errors of length N_CAND, with Inf for the
# excluded indices (current_idx, other_idx) -- callers must skip those.
candidate_batch_d_err <- function(state, other_idx, j) {
  other_full <- c(0, cand_coded[other_idx, ])              # length K
  K <- N_PAR

  # Fixed-row utilities (per draw): R-vectors
  V_other <- as.numeric(other_full %*% beta_draws_t)
  V_sq    <- as.numeric(sq_full   %*% beta_draws_t)
  # Candidate-row utilities (first column of cand_full is 0, so we use beta[2:K])
  V_cand  <- cand_coded %*% beta_draws_t[-1, , drop = FALSE]     # N_CAND x R

  # cand_full as N_CAND x K (zero-pad first column for ASC_SQ)
  cand_full <- cbind(0, cand_coded)

  log_det_full_mat <- matrix(NA_real_, N_CAND, R_DRAWS)
  use_fast <- state$ok

  for (r in seq_len(R_DRAWS)) {
    # Choice probabilities for ALL candidates under draw r
    Vo <- V_other[r]; Vs <- V_sq[r]
    Vc <- V_cand[, r]
    Vmax <- pmax(Vc, Vo, Vs)
    e1 <- exp(Vc - Vmax); e2 <- exp(Vo - Vmax); e3 <- exp(Vs - Vmax)
    den <- e1 + e2 + e3
    p1 <- e1 / den; p2 <- e2 / den; p3 <- e3 / den
    # M = diag(p) - p p'  -- entries per candidate
    m11 <- p1 * (1 - p1)
    m12 <- -p1 * p2; m13 <- -p1 * p3
    m22 <- p2 * (1 - p2); m23 <- -p2 * p3
    m33 <- p3 * (1 - p3)

    if (use_fast[r]) {
      inv <- state$inv[[r]]
      # Variable cross-products (vectorised):
      # CINV[c, ] = cand_full[c, ] %*% inv     (N_CAND x K)
      CINV <- cand_full %*% inv
      v_cc <- rowSums(CINV * cand_full)               # x_c' inv x_c, N_CAND vec
      v_co <- as.numeric(CINV %*% other_full)          # x_c' inv x_o
      v_cs <- as.numeric(CINV %*% sq_full)             # x_c' inv x_s
      # Fixed cross-products (scalars; reuse if needed)
      io   <- as.numeric(inv %*% other_full)
      is_  <- as.numeric(inv %*% sq_full)
      v_oo <- sum(other_full * io)
      v_ss <- sum(sq_full * is_)
      v_os <- sum(other_full * is_)

      # K3[c] := I_3 + M_c %*% V_c   where V_c is the 3x3 (x_i' inv x_j) matrix.
      # det(K3[c]) computed analytically per candidate (vectorised).
      # Build the 3x3 entries of M %*% V per candidate.
      # MV[i,j] = sum_k M[i,k] V[k,j]
      # V_c =
      #   [ v_cc, v_co, v_cs ]
      #   [ v_co, v_oo, v_os ]
      #   [ v_cs, v_os, v_ss ]
      # (note V is symmetric since inv is symmetric.)
      # MV[1,1] = m11 v_cc + m12 v_co + m13 v_cs
      # MV[1,2] = m11 v_co + m12 v_oo + m13 v_os
      # MV[1,3] = m11 v_cs + m12 v_os + m13 v_ss
      # MV[2,1] = m12 v_cc + m22 v_co + m23 v_cs
      # MV[2,2] = m12 v_co + m22 v_oo + m23 v_os
      # MV[2,3] = m12 v_cs + m22 v_os + m23 v_ss
      # MV[3,1] = m13 v_cc + m23 v_co + m33 v_cs
      # MV[3,2] = m13 v_co + m23 v_oo + m33 v_os
      # MV[3,3] = m13 v_cs + m23 v_os + m33 v_ss
      MV11 <- m11 * v_cc + m12 * v_co + m13 * v_cs
      MV12 <- m11 * v_co + m12 * v_oo + m13 * v_os
      MV13 <- m11 * v_cs + m12 * v_os + m13 * v_ss
      MV21 <- m12 * v_cc + m22 * v_co + m23 * v_cs
      MV22 <- m12 * v_co + m22 * v_oo + m23 * v_os
      MV23 <- m12 * v_cs + m22 * v_os + m23 * v_ss
      MV31 <- m13 * v_cc + m23 * v_co + m33 * v_cs
      MV32 <- m13 * v_co + m23 * v_oo + m33 * v_os
      MV33 <- m13 * v_cs + m23 * v_os + m33 * v_ss
      # K3 = I + MV
      a <- 1 + MV11; b <- MV12; cc <- MV13
      d <- MV21; e <- 1 + MV22; f <- MV23
      g <- MV31; h <- MV32; i <- 1 + MV33
      det3 <- a * (e * i - f * h) - b * (d * i - f * g) + cc * (d * h - e * g)
      log_det_full_mat[, r] <- ifelse(det3 > 0,
                                      state$log_det[r] + log(det3),
                                      NA_real_)
    } else {
      # Fallback: rank-deficient info_other -> use brute-force per candidate.
      # Slower but only triggers in degenerate states.
      A <- state$info_other[[r]]
      for (c_idx in seq_len(N_CAND)) {
        x_c <- cand_full[c_idx, ]
        Mc <- matrix(c(m11[c_idx], m12[c_idx], m13[c_idx],
                       m12[c_idx], m22[c_idx], m23[c_idx],
                       m13[c_idx], m23[c_idx], m33[c_idx]), 3, 3)
        Xc <- rbind(x_c, other_full, sq_full)
        info_new <- A + crossprod(Xc, Mc %*% Xc)
        ld <- determinant(info_new, logarithm = TRUE)
        log_det_full_mat[c_idx, r] <- if (ld$sign > 0 && is.finite(ld$modulus))
          as.numeric(ld$modulus) else NA_real_
      }
    }
  }

  # Mean D-error across draws: exp(-mean(log_det)/N_PAR), with Inf where any
  # draw produced a non-PD matrix (matches mean(Inf, ...) = Inf semantics).
  d_err_per_draw <- exp(-log_det_full_mat / N_PAR)
  d_err_per_draw[is.na(d_err_per_draw)] <- Inf
  rowMeans(d_err_per_draw)
}

# Level-balance penalty (unchanged in spirit; counts only A/B alternatives).
init_level_counts <- function(design) {
  counts <- list()
  for (k in attr_keys) {
    n_lvls <- length(ab_levels[[k]])
    counts[[k]] <- integer(n_lvls)
    for (alt in 1:2) {
      vals <- cand_grid[[k]][design[, alt]]
      for (v in vals) {
        idx <- match(v, ab_levels[[k]])
        counts[[k]][idx] <- counts[[k]][idx] + 1L
      }
    }
  }
  counts
}

imbalance_from_counts <- function(counts, n_obs) {
  imbal <- 0
  for (k in names(counts)) {
    expected <- n_obs / length(counts[[k]])
    imbal <- imbal + sum((counts[[k]] - expected)^2 / expected)
  }
  imbal
}

penalised_error <- function(d_err, imbal) d_err * (1 + LAMBDA * imbal)

# --- Run the exchange algorithm ---

set.seed(SEED)
n_obs_total <- N_SETS * 2

cat(sprintf("Algorithm:   modified Federov, %d starts, up to %d iters, lambda=%.2f\n\n",
            N_STARTS, MAX_ITER, LAMBDA))

global_best_design <- NULL
global_best_error  <- Inf
global_best_d_err  <- Inf
global_best_imb    <- Inf

t0 <- proc.time()

for (start in 1:N_STARTS) {

  # FIX 13: per-start seed so individual starts are independently reproducible.
  set.seed(SEED + start)

  design <- matrix(NA, N_SETS, 2)
  for (s in 1:N_SETS) design[s, ] <- sample(N_CAND, 2, replace = FALSE)

  # info_full is now a *list* of R info matrices, one per draw.
  set_infos <- vector("list", N_SETS)
  info_full <- replicate(R_DRAWS, matrix(0, N_PAR, N_PAR), simplify = FALSE)
  for (s in 1:N_SETS) {
    set_infos[[s]] <- set_info_all(design[s, 1], design[s, 2])
    for (r in seq_len(R_DRAWS)) {
      info_full[[r]] <- info_full[[r]] + set_infos[[s]][[r]]
    }
  }

  lvl_counts    <- init_level_counts(design)
  current_imbal <- imbalance_from_counts(lvl_counts, n_obs_total)
  current_d_err <- mean_d_error(info_full)
  current_obj   <- penalised_error(current_d_err, current_imbal)

  improved <- TRUE
  iter <- 0

  while (improved && iter < MAX_ITER) {
    improved <- FALSE
    iter <- iter + 1

    # FIX 11: randomise scan order so improvement isn't biased by index.
    set_order <- sample(N_SETS)
    for (s in set_order) {
      for (j in sample(2)) {
        current_idx <- design[s, j]
        other_idx   <- design[s, 3 - j]

        # info_full minus this set, per draw
        info_minus <- vector("list", R_DRAWS)
        for (r in seq_len(R_DRAWS)) {
          info_minus[[r]] <- info_full[[r]] - set_infos[[s]][[r]]
        }

        # Update temp counts (level-balance) for the OUTGOING profile.
        old_profile <- as.numeric(cand_grid[current_idx, ])
        temp_counts <- lvl_counts
        for (a in seq_along(attr_keys)) {
          k <- attr_keys[a]
          idx <- match(old_profile[a], ab_levels[[k]])
          temp_counts[[k]][idx] <- temp_counts[[k]][idx] - 1L
        }

        # Prepare reusable state for the lemma-based candidate evaluator.
        swap_state <- prep_swap_state(info_minus)

        # Batch-evaluate ALL candidates' D-errors in one shot (BLAS).
        d_err_vec <- candidate_batch_d_err(swap_state, other_idx, j)
        d_err_vec[c(current_idx, other_idx)] <- Inf  # exclude current alts

        # Vectorised level-imbalance penalty per candidate. For each attribute,
        # incrementing the candidate's level by 1 changes one count; build the
        # squared-deviation contribution per (candidate, attribute) and sum.
        new_imbal_vec <- numeric(N_CAND)
        for (a in seq_along(attr_keys)) {
          k <- attr_keys[a]
          ab_lv <- ab_levels[[k]]
          n_lv  <- length(ab_lv)
          expected <- n_obs_total / n_lv
          # Counts after removing OUTGOING profile (already in temp_counts)
          base_cnt <- temp_counts[[k]]
          # For each candidate, the level it has on this attribute (1..n_lv)
          cand_lvls <- match(cand_grid[[k]], ab_lv)
          # Sum of squared deviations / expected for each candidate.
          # Without candidate's contribution: sum over levels l != cand_l of (base_cnt[l] - exp)^2 / exp
          # With candidate's contribution: that plus ((base_cnt[cand_l] + 1) - exp)^2 / exp
          # Simplification: total = sum((base_cnt - exp)^2)/exp + delta(cand_l)
          # delta(cand_l) = ((base_cnt[cand_l] + 1) - exp)^2 / exp - (base_cnt[cand_l] - exp)^2 / exp
          #               = (2*(base_cnt[cand_l] - exp) + 1) / exp
          base_term  <- sum((base_cnt - expected)^2) / expected
          delta_per_lv <- (2 * (base_cnt - expected) + 1) / expected
          new_imbal_vec <- new_imbal_vec + base_term + delta_per_lv[cand_lvls]
        }

        new_obj_vec <- penalised_error(d_err_vec, new_imbal_vec)
        # penalised_error must accept vector args (it does: d_err + lambda * imbal).
        best_swap_idx <- which.min(new_obj_vec)
        best_swap_obj <- new_obj_vec[best_swap_idx]
        if (!is.finite(best_swap_obj) || best_swap_obj >= current_obj) {
          best_swap_idx <- current_idx
          best_swap_obj <- current_obj
        }

        if (best_swap_idx != current_idx) {
          # Apply the swap: only NOW do we materialise the per-draw set info
          # (one set_info_all call instead of N_CAND of them).
          if (j == 1) best_swap_info_s <- set_info_all(best_swap_idx, other_idx)
          else        best_swap_info_s <- set_info_all(other_idx, best_swap_idx)

          new_profile <- as.numeric(cand_grid[best_swap_idx, ])
          for (a in seq_along(attr_keys)) {
            k <- attr_keys[a]
            idx_new <- match(new_profile[a], ab_levels[[k]])
            lvl_counts[[k]][idx_new] <- lvl_counts[[k]][idx_new] + 1L
            idx_old <- match(old_profile[a], ab_levels[[k]])
            lvl_counts[[k]][idx_old] <- lvl_counts[[k]][idx_old] - 1L
          }
          design[s, j]   <- best_swap_idx
          set_infos[[s]] <- best_swap_info_s
          for (r in seq_len(R_DRAWS)) {
            info_full[[r]] <- info_minus[[r]] + best_swap_info_s[[r]]
          }
          current_d_err  <- mean_d_error(info_full)
          current_imbal  <- imbalance_from_counts(lvl_counts, n_obs_total)
          current_obj    <- penalised_error(current_d_err, current_imbal)
          improved       <- TRUE
        }
      }
    }
  }

  cat(sprintf("  Start %2d: %2d iters, D=%.4f, imbal=%.2f, obj=%.4f\n",
              start, iter, current_d_err, current_imbal, current_obj))

  if (current_obj < global_best_error) {
    global_best_error  <- current_obj
    global_best_design <- design
    global_best_d_err  <- current_d_err
    global_best_imb    <- current_imbal
  }
}

FINAL_D_ERR <- global_best_d_err
FINAL_IMBAL <- global_best_imb
elapsed     <- (proc.time() - t0)[3]

cat(sprintf("\nBest: D=%.4f, imbalance=%.2f  (%.1fs elapsed)\n\n",
            FINAL_D_ERR, FINAL_IMBAL, elapsed))

# FIX 12: identification check on the final information matrix at PRIOR_MEAN.
final_info_pm <- matrix(0, N_PAR, N_PAR)
{
  bd_save <- beta_draws_t
  beta_draws_t_local <- matrix(PRIOR_MEAN, ncol = 1)
  R_save <- R_DRAWS
  beta_draws_t <- beta_draws_t_local
  R_DRAWS <- 1L
  for (s in 1:N_SETS) {
    final_info_pm <- final_info_pm + set_info_all(global_best_design[s, 1],
                                                  global_best_design[s, 2])[[1]]
  }
  beta_draws_t <- bd_save
  R_DRAWS <- R_save
}
final_rank <- qr(final_info_pm)$rank
if (final_rank < N_PAR) {
  warning(sprintf(
    "Final design info matrix is rank-deficient (rank %d, expected %d). One or more parameters are not identified -- inspect level balance and ab_only settings.",
    final_rank, N_PAR))
} else {
  cat(sprintf("Identification: OK (info matrix rank %d / %d)\n\n", final_rank, N_PAR))
}


# ==============================================================
# PART 5: BLOCKING
# ==============================================================

cat("Assigning blocks...\n")

decode_design <- function(design) {
  result <- data.frame()
  for (s in 1:nrow(design)) {
    for (j in 1:2) {
      prof <- cand_grid[design[s, j], ]
      row <- data.frame(choice_set = s, alternative = paste0("Program_", LETTERS[j]),
                        stringsAsFactors = FALSE)
      for (k in attr_keys) {
        row[[paste0(k, "_level")]] <- prof[[k]]
        row[[paste0(k, "_label")]] <- ATTRIBUTES[[k]]$levels[prof[[k]]]
        if (k == cost_key) row[[paste0(k, "_value")]] <- ATTRIBUTES[[k]]$cost_values[prof[[k]]]
      }
      result <- rbind(result, row)
    }
    sq_row <- data.frame(choice_set = s, alternative = "Status_Quo", stringsAsFactors = FALSE)
    for (k in attr_keys) {
      sq_row[[paste0(k, "_level")]] <- sq_levels[k]
      sq_row[[paste0(k, "_label")]] <- ATTRIBUTES[[k]]$levels[sq_levels[k]]
      if (k == cost_key) sq_row[[paste0(k, "_value")]] <- ATTRIBUTES[[k]]$cost_values[sq_levels[k]]
    }
    result <- rbind(result, sq_row)
  }
  result
}

full_design <- decode_design(global_best_design)

sets <- unique(full_design$choice_set)
block_size <- N_SETS / N_BLOCKS

best_assignment   <- NULL
best_block_imbal  <- Inf

set.seed(SEED + 1)
for (attempt in 1:500) {
  perm <- sample(sets)
  assignment <- integer(length(sets))
  assignment[perm] <- rep(1:N_BLOCKS, each = block_size)
  names(assignment) <- as.character(sets)

  imbal <- 0
  for (b in 1:N_BLOCKS) {
    block_sets <- sets[assignment == b]
    block_rows <- full_design[full_design$choice_set %in% block_sets &
                              full_design$alternative != "Status_Quo", ]
    for (k in attr_keys) {
      col <- paste0(k, "_level")
      # FIX 3: include all possible levels so missing levels are visible
      # rather than silently dropped by `factor()`.
      freq <- table(factor(block_rows[[col]], levels = ab_levels[[k]]))
      expected <- sum(freq) / length(freq)
      imbal <- imbal + sum((freq - expected)^2)
    }
  }
  if (imbal < best_block_imbal) {
    best_block_imbal <- imbal
    best_assignment <- assignment
  }
}

full_design$block <- best_assignment[as.character(full_design$choice_set)]
cat("Done.\n\n")


# ==============================================================
# PART 6: EXPORT DESIGN CSVs
# ==============================================================

write.csv(full_design, file.path(OUTPUT_DIR, "dce_design_full.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

compact_cols <- c("choice_set", "block", "alternative",
                  paste0(attr_keys, "_level"),
                  paste0(cost_key, "_value"))
write.csv(full_design[, compact_cols],
          file.path(OUTPUT_DIR, "dce_design_compact.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("Design CSVs written.\n")


# ==============================================================
# PART 7: GENERATE QUALTRICS HTML
# ==============================================================

n_tasks <- N_SETS / N_BLOCKS

html_lines <- c(
  '<style>',
  '.dce-table { width:100%; border-collapse:collapse; font-size:13px; line-height:1.5; }',
  '.dce-table th, .dce-table td { padding:10px 12px; border:1px solid #ccc; vertical-align:top; }',
  '.dce-table thead th { font-weight:bold; font-size:13px; text-align:center; padding:12px; }',
  '.col-attr { width:18%; text-align:left; font-weight:bold; background:#f0f4f8; }',
  '.col-prog, .col-sq { width:27.3%; text-align:center; cursor:pointer; transition:background 0.15s; }',
  '.col-prog:hover, .col-sq:hover { background:#e6f1fb; }',
  '.col-header-a, .col-header-b { background:#2c5f8a; color:white; cursor:pointer; }',
  '.col-header-sq { background:#6b6b6b; color:white; cursor:pointer; }',
  '.col-selected { background:rgba(44,95,138,0.06); }',
  '.col-header-a.selected { background:#1a4d78; box-shadow:inset 0 -3px 0 #fff; }',
  '.col-header-b.selected { background:#1a4d78; box-shadow:inset 0 -3px 0 #fff; }',
  '.col-header-sq.selected { background:#4a4a4a; box-shadow:inset 0 -3px 0 #fff; }',
  '.select-row td { padding:14px 12px; text-align:center; }',
  '.radio-circle { display:inline-flex; align-items:center; justify-content:center; width:22px; height:22px; border-radius:50%; border:2px solid #999; background:white; cursor:pointer; transition:all 0.15s; }',
  '.radio-circle.checked { border-color:#2c5f8a; }',
  '.radio-dot { width:12px; height:12px; border-radius:50%; background:#2c5f8a; transform:scale(0); transition:transform 0.15s; }',
  '.radio-circle.checked .radio-dot { transform:scale(1); }',
  '.select-label { font-size:12px; color:#666; margin-top:6px; }',
  '</style>',
  '',
  sprintf('<p><strong>TASK_TITLE ${lm://CurrentLoopNumber} of %d</strong></p>', n_tasks),
  sprintf('<p style="font-size:14px; color:#555; margin-bottom:12px;">%s</p>', CARD_INSTRUCTION),
  '',
  '<table class="dce-table" id="dce-table">',
  '<thead>',
  '<tr>',
  '<th class="col-attr"></th>',
  sprintf('<th class="col-header-a" id="hA" onclick="dce_pick(\'A\')">%s</th>', CARD_COL_A),
  sprintf('<th class="col-header-b" id="hB" onclick="dce_pick(\'B\')">%s</th>', CARD_COL_B),
  sprintf('<th class="col-header-sq" id="hSQ" onclick="dce_pick(\'SQ\')">%s</th>', CARD_COL_SQ),
  '</tr>',
  '</thead>',
  '<tbody>'
)

for (k in attr_keys) {
  a <- ATTRIBUTES[[k]]
  sq_card_text <- a$card_levels[a$sq]
  html_lines <- c(html_lines,
    '<tr>',
    sprintf('<td class="col-attr">%s</td>', a$card_label),
    sprintf('<td class="col-prog cA" id="PA_%s" onclick="dce_pick(\'A\')"></td>', k),
    sprintf('<td class="col-prog cB" id="PB_%s" onclick="dce_pick(\'B\')"></td>', k),
    sprintf('<td class="col-sq cSQ" onclick="dce_pick(\'SQ\')">%s</td>', sq_card_text),
    '</tr>'
  )
}

html_lines <- c(html_lines,
  '<tr class="select-row">',
  '<td class="col-attr">Your choice</td>',
  '<td class="col-prog cA" onclick="dce_pick(\'A\')"><div class="radio-circle" id="rA"><div class="radio-dot"></div></div><div class="select-label">Select A</div></td>',
  '<td class="col-prog cB" onclick="dce_pick(\'B\')"><div class="radio-circle" id="rB"><div class="radio-dot"></div></div><div class="select-label">Select B</div></td>',
  '<td class="col-sq cSQ" onclick="dce_pick(\'SQ\')"><div class="radio-circle" id="rSQ"><div class="radio-dot"></div></div><div class="select-label">Keep current</div></td>',
  '</tr>',
  '</tbody>',
  '</table>'
)

html_lines <- gsub("TASK_TITLE", CARD_TITLE, html_lines)

writeLines(enc2utf8(html_lines), file.path(OUTPUT_DIR, "qualtrics_choicecard.html"), useBytes = TRUE)
cat("Choice card HTML written.\n")


# ==============================================================
# PART 8: GENERATE QUALTRICS JAVASCRIPT
# ==============================================================

# FIX 6: build the DESIGN and LABELS objects in R, then emit them as JSON
# via jsonlite. This handles all string escaping correctly (quotes,
# backslashes, newlines, "</script>", and non-ASCII characters), eliminating
# the silent breakage / injection risk in the original gsub-based encoder.

unbox <- jsonlite::unbox

# --- DESIGN object: list keyed by block ---
design_for_js <- list()
for (b in 1:N_BLOCKS) {
  block_data <- full_design[full_design$block == b & full_design$alternative != "Status_Quo", ]
  block_sets <- unique(block_data$choice_set)
  tasks <- vector("list", length(block_sets))
  for (i in seq_along(block_sets)) {
    s  <- block_sets[i]
    cs <- block_data[block_data$choice_set == s, ]
    pa <- cs[cs$alternative == "Program_A", ]
    pb <- cs[cs$alternative == "Program_B", ]
    PA <- list(); PB <- list()
    for (k in attr_keys) {
      if (k == cost_key) {
        PA[[k]] <- ATTRIBUTES[[k]]$cost_values[pa[[paste0(k, "_level")]]]
        PB[[k]] <- ATTRIBUTES[[k]]$cost_values[pb[[paste0(k, "_level")]]]
      } else {
        PA[[k]] <- pa[[paste0(k, "_level")]]
        PB[[k]] <- pb[[paste0(k, "_level")]]
      }
    }
    tasks[[i]] <- list(cs = unbox(s), PA = PA, PB = PB)
  }
  design_for_js[[as.character(b)]] <- tasks
}
design_json <- jsonlite::toJSON(design_for_js, auto_unbox = TRUE, pretty = TRUE)

# --- LABELS object: per-attribute level lookup using card_levels ---
non_cost_keys <- attr_keys[attr_keys != cost_key]
labels_for_js <- list()
for (k in non_cost_keys) {
  a <- ATTRIBUTES[[k]]
  lvls_to_include <- if (is.null(a$ab_only)) seq_along(a$card_levels) else a$ab_only
  entry <- list()
  for (lv in lvls_to_include) {
    entry[[as.character(lv)]] <- a$card_levels[lv]
  }
  labels_for_js[[k]] <- entry
}
labels_json <- jsonlite::toJSON(labels_for_js, auto_unbox = TRUE, pretty = TRUE)

attrs_json <- jsonlite::toJSON(non_cost_keys, auto_unbox = FALSE)

js_lines <- c(
  '// ==============================================================',
  '// DCE QUALTRICS JAVASCRIPT',
  '// ==============================================================',
  '// Auto-generated -- do not edit manually.',
  '// To update: edit config.R, then run: Rscript run.R',
  '//',
  '// HOW TO USE IN QUALTRICS:',
  '// 1. Open this file in a TEXT EDITOR (Notepad, VS Code, etc.)',
  '//    Do NOT double-click it -- that will try to execute it.',
  '// 2. Create a Multiple Choice question with the HTML from',
  '//    qualtrics_choicecard.html as the question text.',
  sprintf('// 3. Set answer choices: %s', paste(CHOICE_LABELS, collapse = " / ")),
  '// 4. Click the question gear > Add JavaScript',
  '// 5. Delete the default content, then paste EVERYTHING below.',
  sprintf('// 6. Enable Loop & Merge on the block (%d rows, Field 1 = 1..%d).', n_tasks, n_tasks),
  '// ==============================================================',
  '',
  'Qualtrics.SurveyEngine.addOnReady(function() {',
  '',
  '  // --- DESIGN MATRIX ---',
  paste0('  var DESIGN = ', design_json, ';'),
  '',
  '  // --- LABEL LOOKUP (card_levels: shortened wording) ---',
  paste0('  var LABELS = ', labels_json, ';'),
  '',
  '  // --- GET BLOCK AND TASK ---',
  '  var block = "${e://Field/DCE_Block}";',
  '  var taskNum = parseInt("${lm://Field/1}", 10);',
  '  var taskIndex = taskNum - 1;',
  '  var task = DESIGN[block][taskIndex];',
  '',
  '  // --- POPULATE TABLE ---',
  paste0('  var attrs = ', attrs_json, ';'),
  '  for (var i = 0; i < attrs.length; i++) {',
  '    var a = attrs[i];',
  '    document.getElementById("PA_" + a).innerHTML = LABELS[a][task.PA[a]];',
  '    document.getElementById("PB_" + a).innerHTML = LABELS[a][task.PB[a]];',
  '  }',
  '',
  sprintf('  // Cost attribute (%s)', cost_key),
  sprintf('  document.getElementById("PA_%s").innerHTML = "\\u00a3" + task.PA.%s;', cost_key, cost_key),
  sprintf('  document.getElementById("PB_%s").innerHTML = "\\u00a3" + task.PB.%s;', cost_key, cost_key),
  '',
  '  // --- STORE CHOICE SET ID ---',
  '  Qualtrics.SurveyEngine.setEmbeddedData("task" + taskNum + "_cs", task.cs);',
  '',
  '  // --- COLUMN SELECTION LOGIC ---',
  '  var qObj = this;',
  '',
  '  // Hide the default Qualtrics answer choices (the table radio is the UI)',
  '  var choiceContainer = document.getElementById("QR~" + qObj.questionId);',
  '  if (choiceContainer) { choiceContainer.style.display = "none"; }',
  '  var choiceList = document.querySelector("#" + qObj.questionId + " .ChoiceStructure");',
  '  if (choiceList) { choiceList.style.display = "none"; }',
  '',
  '  // Restore visual state if the respondent navigates back',
  '  function dce_apply_visual(opt) {',
  '    document.querySelectorAll(".radio-circle").forEach(function(el) { el.classList.remove("checked"); });',
  '    if (opt && document.getElementById("r" + opt)) document.getElementById("r" + opt).classList.add("checked");',
  '    document.querySelectorAll(".cA").forEach(function(el) { el.classList.toggle("col-selected", opt === "A"); });',
  '    document.querySelectorAll(".cB").forEach(function(el) { el.classList.toggle("col-selected", opt === "B"); });',
  '    document.querySelectorAll(".cSQ").forEach(function(el) { el.classList.toggle("col-selected", opt === "SQ"); });',
  '    document.querySelectorAll(".col-header-a,.col-header-b,.col-header-sq").forEach(function(el) { el.classList.remove("selected"); });',
  '    if (opt === "A")  document.getElementById("hA").classList.add("selected");',
  '    if (opt === "B")  document.getElementById("hB").classList.add("selected");',
  '    if (opt === "SQ") document.getElementById("hSQ").classList.add("selected");',
  '  }',
  '',
  '  // Restore prior selection on back-navigation (read from current MC value)',
  '  var sel = qObj.getSelectedChoices();',
  '  if (sel && sel.length) {',
  '    var rev = {1:"A", 2:"B", 3:"SQ"};',
  '    dce_apply_visual(rev[sel[0]]);',
  '  }',
  '',
  '  // dce_pick is called by onclick in the HTML',
  '  window.dce_pick = function(opt) {',
  '    dce_apply_visual(opt);',
  '    var choiceMap = {"A": 1, "B": 2, "SQ": 3};',
  '    qObj.setChoiceValue(choiceMap[opt], true);',
  '    Qualtrics.SurveyEngine.setEmbeddedData("task" + taskNum + "_choice", opt);',
  '  };',
  '',
  '});'
)

writeLines(enc2utf8(js_lines), file.path(OUTPUT_DIR, "qualtrics_javascript.txt"), useBytes = TRUE)
cat("Qualtrics JavaScript written.\n")



# ==============================================================
# PART 9: DIAGNOSTICS
# ==============================================================

diag_lines <- c(
  "============================================================",
  "  DCE DESIGN DIAGNOSTICS",
  sprintf("  Generated: %s", Sys.time()),
  "============================================================",
  "")

diag_lines <- c(diag_lines, sprintf("Design space:    %d candidate profiles", N_CAND))
diag_lines <- c(diag_lines, sprintf("Choice sets:     %d", N_SETS))
diag_lines <- c(diag_lines, sprintf("Blocks:          %d (%d tasks each)", N_BLOCKS, n_tasks))
diag_lines <- c(diag_lines, sprintf("Parameters:      %d", N_PAR))
diag_lines <- c(diag_lines, sprintf("Objective:       %s",
                                   if (is_local_d) "Local D-error at PRIOR_MEAN"
                                   else            sprintf("Bayesian D-error over %d prior draws", R_DRAWS)))
diag_lines <- c(diag_lines, sprintf("D-error (best):  %.4f", FINAL_D_ERR))
diag_lines <- c(diag_lines, sprintf("Level imbalance: %.2f", FINAL_IMBAL))
diag_lines <- c(diag_lines, sprintf("Info-matrix rank: %d / %d", final_rank, N_PAR))
diag_lines <- c(diag_lines, sprintf("Cost scale:      %g (cost coef interpreted per cost-scale units)", COST_SCALE))
diag_lines <- c(diag_lines, sprintf("Seed:            %d", SEED))
diag_lines <- c(diag_lines, "",
                "--- Parameters (PRIOR_MEAN order) ---")
for (j in seq_along(param_names)) {
  diag_lines <- c(diag_lines, sprintf("  [%2d] %-30s prior mean=%6.2f, sd=%4.2f",
                                       j, param_names[j], PRIOR_MEAN[j], PRIOR_SD[j]))
}
diag_lines <- c(diag_lines, "")

ab_rows <- full_design[full_design$alternative != "Status_Quo", ]

diag_lines <- c(diag_lines, "--- Level balance (A/B alternatives only) ---", "")

for (k in attr_keys) {
  a <- ATTRIBUTES[[k]]
  col <- paste0(k, "_level")
  # FIX 3: include all expected (ab_only) levels so missing ones show n=0.
  freq <- table(factor(ab_rows[[col]], levels = ab_levels[[k]]))
  diag_lines <- c(diag_lines, sprintf("%s (%s):", k, a$concept))
  for (i in seq_along(freq)) {
    lvl <- as.integer(names(freq)[i])
    pct <- if (sum(freq) > 0) 100 * freq[i] / sum(freq) else 0
    lbl <- if (nchar(a$levels[lvl]) > 50) paste0(substr(a$levels[lvl], 1, 50), "...") else a$levels[lvl]
    flag <- if (freq[i] == 0) "  [WARN: level missing]" else ""
    diag_lines <- c(diag_lines, sprintf("  Level %d: n=%d (%.1f%%)  %s%s", lvl, freq[i], pct, lbl, flag))
  }
  diag_lines <- c(diag_lines, "")
}

diag_lines <- c(diag_lines, "--- Level balance by block ---", "")
for (b in 1:N_BLOCKS) {
  block_rows <- ab_rows[ab_rows$block == b, ]
  diag_lines <- c(diag_lines, sprintf("Block %d (%d choice sets):", b, length(unique(block_rows$choice_set))))
  for (k in attr_keys) {
    col <- paste0(k, "_level")
    # FIX 3: include all expected levels.
    freq <- table(factor(block_rows[[col]], levels = ab_levels[[k]]))
    diag_lines <- c(diag_lines, sprintf("  %s: %s", k,
                    paste(sprintf("L%s=%d", names(freq), freq), collapse = ", ")))
  }
  diag_lines <- c(diag_lines, "")
}

# Dominance / equality checks
diag_lines <- c(diag_lines, "--- Dominance and equality checks ---")
n_dom <- 0; n_a_eq_sq <- 0; n_b_eq_sq <- 0
for (s in unique(full_design$choice_set)) {
  cs <- full_design[full_design$choice_set == s, ]
  pa <- cs[cs$alternative == "Program_A", ]
  pb <- cs[cs$alternative == "Program_B", ]
  sq <- cs[cs$alternative == "Status_Quo", ]

  # A vs B identical except cost
  same_non_cost <- TRUE
  for (k in attr_keys) {
    if (k == cost_key) next
    if (pa[[paste0(k, "_level")]] != pb[[paste0(k, "_level")]]) { same_non_cost <- FALSE; break }
  }
  if (same_non_cost && pa[[paste0(cost_key, "_value")]] != pb[[paste0(cost_key, "_value")]]) {
    diag_lines <- c(diag_lines, sprintf("  WARNING: Set %d -- A and B identical except cost", s))
    n_dom <- n_dom + 1
  }

  # A == SQ on every attribute
  a_eq_sq <- all(sapply(attr_keys, function(k) pa[[paste0(k, "_level")]] == sq[[paste0(k, "_level")]]))
  if (a_eq_sq) {
    diag_lines <- c(diag_lines, sprintf("  WARNING: Set %d -- Program A identical to SQ", s))
    n_a_eq_sq <- n_a_eq_sq + 1
  }
  # B == SQ
  b_eq_sq <- all(sapply(attr_keys, function(k) pb[[paste0(k, "_level")]] == sq[[paste0(k, "_level")]]))
  if (b_eq_sq) {
    diag_lines <- c(diag_lines, sprintf("  WARNING: Set %d -- Program B identical to SQ", s))
    n_b_eq_sq <- n_b_eq_sq + 1
  }
}
if (n_dom == 0 && n_a_eq_sq == 0 && n_b_eq_sq == 0) {
  diag_lines <- c(diag_lines, "  None found.")
}

# Attribute differences
diag_lines <- c(diag_lines, "", "--- Attribute differences (A vs B) ---")
sets_v <- unique(full_design$choice_set)
diffs <- integer(length(sets_v))
for (i in seq_along(sets_v)) {
  s <- sets_v[i]
  cs <- full_design[full_design$choice_set == s & full_design$alternative != "Status_Quo", ]
  diffs[i] <- sum(sapply(attr_keys, function(k) cs[1, paste0(k, "_level")] != cs[2, paste0(k, "_level")]))
}
diag_lines <- c(diag_lines,
  sprintf("  Min: %d  Max: %d  Mean: %.1f", min(diffs), max(diffs), mean(diffs)),
  sprintf("  Distribution: %s",
          paste(sprintf("%d-diff=%d", sort(unique(diffs)),
                        as.integer(table(diffs)[as.character(sort(unique(diffs)))])),
                collapse = ", ")))

# Per-attribute overlap rate
diag_lines <- c(diag_lines, "", "--- Attribute overlap rate (A and B share level) ---")
for (k in attr_keys) {
  col <- paste0(k, "_level")
  overlaps <- 0
  for (s in sets_v) {
    cs <- full_design[full_design$choice_set == s & full_design$alternative != "Status_Quo", ]
    if (cs[1, col] == cs[2, col]) overlaps <- overlaps + 1
  }
  diag_lines <- c(diag_lines, sprintf("  %s: %d/%d (%.0f%%)",
                                      k, overlaps, length(sets_v),
                                      100 * overlaps / length(sets_v)))
}

writeLines(enc2utf8(diag_lines), file.path(OUTPUT_DIR, "design_diagnostics.txt"), useBytes = TRUE)
cat("Diagnostics written.\n")


# ==============================================================
# DONE
# ==============================================================

cat("\n============================================================\n")
cat("  ALL FILES GENERATED IN output/\n")
cat("============================================================\n")
cat("  dce_design_full.csv        -- full design with labels\n")
cat("  dce_design_compact.csv     -- level numbers for analysis\n")
cat("  qualtrics_choicecard.html  -- HTML for question text\n")
cat("  qualtrics_javascript.txt   -- JS to paste into question\n")
cat("  design_diagnostics.txt     -- balance & quality checks\n")
cat("============================================================\n\n")
