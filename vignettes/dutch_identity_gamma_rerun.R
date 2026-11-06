# dutch_identity_gamma_rerun.R
#
# One-off partial re-run: adds the Γ rank-1 diagnostics (and their parametric
# bootstrap) to the existing dutchdata/dutch_identity_results.rds WITHOUT
# refetching any data or refitting Sections 4-6.
#
# Everything the new diagnostics need is already in the cached result: the full
# 2^J table of pattern counts and the fitted restricted-log-quadratic
# parameters. The independence and SOE GLMs are refitted from those same stored
# counts, and the script asserts that the pattern counts and Table 1 come back
# unchanged before it writes anything.
#
# dutch_identity_compute.R remains authoritative for a full run; this script
# exists so that updating a published figure does not require re-running the
# corpus-wide sections.
#
# Usage:
#   Rscript vignettes/dutch_identity_gamma_rerun.R   # from project root

source("vignettes/dutch_identity_helpers.R")

out_file <- "vignettes/dutchdata/dutch_identity_results.rds"
res      <- readRDS(out_file)
s        <- res$s1_3

item_cols <- s$item_cols
all_pats  <- s$patterns
J <- s$J; N <- s$N
X <- as.matrix(all_pats[, item_cols])

message("Table: ", s$table_name, "  J=", J, "  N=", N)

# ---- pattern counts must be untouched ---------------------------------------
stopifnot(nrow(all_pats) == 2^J, sum(all_pats$count) == N)
stopifnot(!anyNA(all_pats$count))

# ---- refit the two GLMs from the stored counts -------------------------------
ind_model <- glm(as.formula(paste("count ~", paste(item_cols, collapse = " + "))),
                 data = all_pats, family = poisson())
soe_model <- glm(as.formula(paste0("count ~ (", paste(item_cols, collapse = " + "), ")^2")),
                 data = all_pats, family = poisson())

log_mu_ind <- as.numeric(predict(ind_model, type = "link"))
log_mu_soe <- as.numeric(predict(soe_model, type = "link"))
# reuse the stored log-quadratic optimum, so Table 1 cannot move
log_mu_rq  <- log_quad_log_mu(s$opt_rq$par, X)

ll <- c(ind = pois_ll(log_mu_ind, all_pats$count),
        rq  = pois_ll(log_mu_rq,  all_pats$count),
        soe = pois_ll(log_mu_soe, all_pats$count))

# ---- Table 1 must come back byte-identical ----------------------------------
stopifnot(isTRUE(all.equal(as.numeric(s$bic_table$Log_lik), as.numeric(ll),
                           tolerance = 0)))
mll <- c(ind = mult_ll(log_mu_ind, all_pats$count),
         rq  = mult_ll(log_mu_rq,  all_pats$count),
         soe = mult_ll(log_mu_soe, all_pats$count))
stopifnot(isTRUE(all.equal(as.numeric(s$ll_multinomial), as.numeric(mll),
                           tolerance = 0)))
gap_share <- (ll[["rq"]] - ll[["ind"]]) / (ll[["soe"]] - ll[["ind"]])
stopifnot(isTRUE(all.equal(s$rq_gap_share, gap_share, tolerance = 0)))
stopifnot(isTRUE(all.equal(unname(gamma_from_soe(soe_model, item_cols)),
                           unname(s$gamma_matrix), tolerance = 0)))
message("Table 1, the multinomial log-likelihoods, the gap share and Γ all reproduce exactly.")

# ---- the new diagnostics -----------------------------------------------------
gamma_rank1 <- gamma_rank1_diagnostics(
  pats      = all_pats[, c(item_cols, "count")],
  item_cols = item_cols,
  soe_model = soe_model,
  log_mu_rq = log_mu_rq,
  gamma_rq  = s$rq_convergence$gamma,
  N         = N,
  n_boot    = 200,
  seed      = 20260101
)

f <- gamma_rank1$eigen_facts
message(sprintf("trace(Γ) = %.3g ; Σλ = %.3g ; %d positive eigenvalue(s)",
                f$trace, f$eig_sum, f$n_positive))
message(sprintf("λ₁/Σ|λ| = %.4f (ceiling 0.5)", f$leading_abs_share))
message(sprintf("off-diagonal rank-1 share: %.4f weighted, %.4f unweighted, %.4f at the fitted γ",
                gamma_rank1$rank1_weighted$share,
                gamma_rank1$rank1_unweighted$share,
                gamma_rank1$share_from_rq_gamma))
message(sprintf("bootstrap (%d reps, %d failed, %d warned): weighted share pctl %.3f, median %.4f, [%.4f, %.4f]",
                gamma_rank1$n_boot, gamma_rank1$n_boot_failed, gamma_rank1$n_boot_warned,
                gamma_rank1$pctl$share_w,
                median(gamma_rank1$boot$share_w),
                quantile(gamma_rank1$boot$share_w, 0.025),
                quantile(gamma_rank1$boot$share_w, 0.975)))
message(sprintf("bootstrap replicates with exactly one positive eigenvalue: %.1f%%",
                100 * gamma_rank1$boot_prop_one_positive))

res$s1_3$gamma_rank1 <- gamma_rank1
res$date_run <- Sys.Date()
res$irw_pkg_version <- as.character(utils::packageVersion("irw"))
saveRDS(res, out_file)
message("Updated ", out_file)
