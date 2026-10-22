# Parametric bootstrap: is the two-stage screen's 90J selection on nomt
# something it would produce from data generated under an EXACTLY bivariate
# normal person prior? Generate from the fitted MVN hierarchical model, refit,
# re-run the identical copula characterisation, and count how often it says 90J.
Sys.setenv(OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
defs <- readLines("vignettes/speed_accuracy_copula_compute.R")[1:382]
eval(parse(text = paste(defs, collapse = "\n")), envir = globalenv())
set.seed(20261022)

SPD <- "vignettes/speed_accuracy_copula_data"
# Stage 1 (fetch + one real fit) is regenerated if its intermediate is absent;
# that intermediate is a full LNIRT object and is deliberately not committed.
real_file <- file.path(SPD, "nomt_realfit.rds")
if (!file.exists(real_file)) {
  tbl <- "nomt_hooper_2024_study2"
  d <- resolve_trials(irw_fetch(tbl), tbl)$df
  d$rt <- as.numeric(as.character(d$rt)); d$resp <- as.numeric(as.character(d$resp))
  d <- d[is.finite(d$rt) & d$rt > 0 & is.finite(d$resp), ]
  ids <- sort(unique(d$id)); its <- sort(unique(d$item))
  Y0 <- matrix(NA_real_, length(ids), length(its)); RT0 <- Y0
  ri <- cbind(match(d$id, ids), match(d$item, its))
  Y0[ri] <- d$resp; RT0[ri] <- log(d$rt)
  kp <- apply(Y0, 2, function(x) length(unique(na.omit(x))) > 1)
  Y0 <- Y0[, kp, drop = FALSE]; RT0 <- RT0[, kp, drop = FALSE]
  invisible(capture.output(f0 <- LNIRT::LNIRT(RT = RT0, Y = Y0, XG = XG,
                                              burnin = BURNIN, residual = FALSE, td = FALSE)))
  saveRDS(list(Y = Y0, RT = RT0, fit = f0), real_file)
}
real <- readRDS(real_file)
Y <- real$Y; RT <- real$RT; pm <- real$fit$Post.Means
N <- nrow(Y); J <- ncol(Y)

a <- as.numeric(pm$Item.Discrimination); b <- as.numeric(pm$Item.Difficulty)
lam <- as.numeric(pm$Time.Intensity);    phi <- as.numeric(pm$Time.Discrimination)
s2 <- as.numeric(pm$Sigma2); if (length(s2) == 1) s2 <- rep(s2, J)
mu <- c(as.numeric(pm$Mu.Person.Ability), as.numeric(pm$Mu.Person.Speed))
if (!all(is.finite(mu))) mu <- c(0, 0)
S <- matrix(c(pm$Var.Person.Ability, pm$Cov.Person.Ability.Speed,
              pm$Cov.Person.Ability.Speed, pm$Var.Person.Speed), 2, 2)
cat("true rho =", round(S[1,2]/sqrt(S[1,1]*S[2,2]), 3), "\n")

rmvn2 <- function(n, mu, S) {
  L <- chol(S); matrix(rnorm(2*n), n, 2) %*% L + matrix(mu, n, 2, byrow = TRUE)
}

# Two candidate parameterisations each for the accuracy and timing parts; pick
# whichever reproduces the real item statistics, rather than assuming.
sim_Y <- function(th, form) {
  p <- if (form == "a(th-b)") pnorm(outer(th, a, "*") - matrix(a*b, N, J, byrow=TRUE))
       else                   pnorm(outer(th, a, "*") - matrix(b,   N, J, byrow=TRUE))
  (matrix(runif(N*J), N, J) < p) * 1
}
sim_RT <- function(sp, form) {
  m <- if (form == "lam-phi*sp") matrix(lam, N, J, byrow=TRUE) - outer(sp, phi, "*")
       else                      matrix(lam, N, J, byrow=TRUE) - matrix(sp, N, J)
  m + matrix(rnorm(N*J, 0, rep(sqrt(s2), each = N)), N, J)
}

z <- rmvn2(N, mu, S)
realp <- colMeans(Y, na.rm=TRUE); realrt_m <- colMeans(RT, na.rm=TRUE); realrt_s <- apply(RT,2,sd,na.rm=TRUE)
for (fy in c("a(th-b)","a*th-b")) {
  d <- sim_Y(z[,1], fy)
  cat(sprintf("  Y form %-9s: RMSE item p = %.4f\n", fy, sqrt(mean((colMeans(d)-realp)^2))))
}
for (fr in c("lam-phi*sp","lam-sp")) {
  d <- sim_RT(z[,2], fr)
  cat(sprintf("  RT form %-11s: RMSE item mean = %.4f  RMSE item sd = %.4f\n",
              fr, sqrt(mean((colMeans(d)-realrt_m)^2)), sqrt(mean((apply(d,2,sd)-realrt_s)^2))))
}
rmse_y <- sapply(c("a(th-b)", "a*th-b"),
                 function(f) sqrt(mean((colMeans(sim_Y(z[,1], f)) - realp)^2)))
Y_FORM  <- names(which.min(rmse_y))
RT_FORM <- "lam-phi*sp"   # phi is near 1 here; the two forms are indistinguishable
cat("using:", Y_FORM, "/", RT_FORM, "\n")

one_rep <- function(r) {
  set.seed(50000 + r)
  z  <- rmvn2(N, mu, S)
  Ys <- sim_Y(z[,1], Y_FORM); RTs <- sim_RT(z[,2], RT_FORM)
  keep <- apply(Ys, 2, function(x) length(unique(x)) > 1)
  Ys <- Ys[, keep, drop=FALSE]; RTs <- RTs[, keep, drop=FALSE]
  f <- tryCatch({ invisible(capture.output(
         ff <- LNIRT::LNIRT(RT=RTs, Y=Ys, XG=XG, burnin=BURNIN, residual=FALSE, td=FALSE))); ff },
       error=function(e) NULL)
  if (is.null(f)) return(NULL)
  out <- tryCatch(characterise_copula(as.numeric(f$Post.Means$Person.Ability),
                                      as.numeric(f$Post.Means$Person.Speed),
                                      10, ncol(Ys), paste0("sim", r)),
                  error=function(e) NULL)
  if (is.null(out)) return(NULL)
  out$rep <- r
  out
}

R <- as.integer(Sys.getenv("NREP", "40"))
plan(multisession, workers = 2)
res <- future_map(seq_len(R), one_rep, .options = furrr_options(seed = TRUE))
plan(sequential)
res <- bind_rows(res)
saveRDS(list(results = res, n_rep = nrow(res), date_run = Sys.Date()),
        file.path(SPD, "null_calibration.rds"))

cat("\n==== NULL CALIBRATION, ", nrow(res), " replicates, true copula = Gaussian ====\n", sep="")
print(table(res$best_family_name))
cat("\n90J selected in", sum(res$is_joe90), "of", nrow(res), "replicates\n")
cat("dAIC vs Gaussian: median", round(median(res$daic_vs_gaussian),1),
    " 95th pct", round(quantile(res$daic_vs_gaussian, .95),1),
    " max", round(max(res$daic_vs_gaussian),1), "\n")
cat("asym_emp: median", round(median(res$asym_emp),3),
    " 95th pct", round(quantile(res$asym_emp,.95),3), "\n")
cat("p_radsym < .05 in", sum(res$p_radsym < .05), "of", nrow(res), "\n")
cat("\nOBSERVED on real nomt: 90J, dAIC 34.4, asym_emp 0.368, p_radsym 0.0015\n")
cat("empirical p for dAIC:", mean(res$daic_vs_gaussian >= 34.42), "\n")
cat("empirical p for asym:", mean(res$asym_emp >= 0.368), "\n")
