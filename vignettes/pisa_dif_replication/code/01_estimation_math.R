# 01_estimation_math.R
# Replication of Ouyang et al. (2026) Step 1 (EM estimation) for PISA 2022 math.
# EM functions are verbatim from their estimation_math.R.
# Execution block adapted to use IRW-derived data from 00_prep_math.R.
#
# Differences from the original:
#   - Data read from data/ (IRW-derived) instead of working directory
#   - Outputs written to output/ instead of working directory
#   - a_ini seeded from their published values (output/a_ini_math_2022.csv)
#   - Equal sampling weights (IRW does not carry W_FSTUWT)
#   - Item loop parallelized with foreach %dopar% (items are independent in M-step)
#   - Per-iteration checkpoint saved to output/checkpoint.rds for safe resume
#
# Usage: Rscript code/01_estimation_math.R   (from vignettes/pisa_dif_replication/)

library(fastGHQuad)
library(quantreg)
library(MASS)
library(foreach)
library(doParallel)

N_CORES <- max(1, detectCores() - 1)
registerDoParallel(cores = N_CORES)
message("Using ", N_CORES, " cores")

gauss <- gaussHermiteData(n = 35)
w <- gauss$w
z <- gauss$x

# ------------------------------------------------------------------------------
# EM helper functions (verbatim from authors)
# ------------------------------------------------------------------------------

tran <- function(z, x, mu, sigma) {
  n <- length(z); N <- dim(x)[1]; p <- dim(x)[2]
  mean.vec <- apply((rep(1,N) %*% t(mu)) * x, 1, sum)
  var.vec  <- apply((rep(1,N) %*% t(sigma^2)) * x, 1, sum)
  sd.vec   <- var.vec^0.5
  sqrt(2) * (sd.vec %*% t(rep(1,n))) * (rep(1,N) %*% t(z)) + mean.vec %*% t(rep(1,n))
}

evaluate_prod <- function(v) exp(sum(v, na.rm = TRUE))

quad <- function(a, b, g, x, dat, z, mu, sigma, w) {
  n <- length(z); N <- dim(dat)[1]; J <- dim(dat)[2]; p <- dim(x)[2]
  Theta <- tran(z, x, mu, sigma)
  th <- Theta[,1]; ta <- rep(1,N) %*% t(a); tb <- rep(1,N) %*% t(b); tg <- x %*% t(g)
  temp <- (th %*% t(rep(1,J))) * ta + tb + tg
  prob <- exp(temp) / (1 + exp(temp))
  q <- apply(log((prob^dat) * ((1-prob)^(1-dat))), 1, evaluate_prod)
  for (k in 2:n) {
    th <- Theta[,k]; temp <- (th %*% t(rep(1,J))) * ta + tb + tg
    prob <- exp(temp) / (1 + exp(temp))
    q <- cbind(q, apply(log((prob^dat) * ((1-prob)^(1-dat))), 1, evaluate_prod))
  }
  Z <- rowSums(q * (rep(1,N) %*% t(w)))
  (q * (rep(1,N) %*% t(w))) / (Z %*% t(rep(1,n)))
}

target_function1 <- function(phi_j, x, Y_j, weights, z, mu_pres, sigma_pres, q) {
  N <- dim(x)[1]; p <- dim(x)[2]; n <- length(z)
  Theta <- tran(z, x, mu_pres, sigma_pres)
  temp <- Theta * 1 + phi_j * (rep(1,N) %*% t(rep(1,n))) + (x %*% rep(0,p)) %*% t(rep(1,n))
  t2 <- Y_j %*% t(rep(1,n)) * temp - log(1 + exp(temp))
  -sum(q * (weights %*% t(rep(1,n))) * t2, na.rm = TRUE) / N
}

target_function_ab <- function(ab, g, x, Y_j, weights, z, mu_pres, sigma_pres, q) {
  N <- dim(x)[1]; n <- length(z)
  Theta <- tran(z, x, mu_pres, sigma_pres)
  temp <- Theta * ab + (x %*% g) %*% t(rep(1,n))
  t2 <- Y_j %*% t(rep(1,n)) * temp - log(1 + exp(temp))
  -sum(q * (weights %*% t(rep(1,n))) * t2, na.rm = TRUE) / N
}

target_function_g <- function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q) {
  N_k <- dim(x_k)[1]; n <- length(z)
  gp[k] <- gk
  q_k <- q[ind_k,]; weight_k <- weights[ind_k]
  Theta <- tran(z, x_k, mu_pres, sigma_pres)
  temp <- Theta * ab[1] + (x_k %*% gp) %*% t(rep(1,n))
  t2 <- Y_k %*% t(rep(1,n)) * temp - log(1 + exp(temp))
  -sum(q_k * (weight_k %*% t(rep(1,n))) * t2, na.rm = TRUE) / N_k
}

target_function_g1 <- function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q) {
  N_k <- dim(x_k)[1]; n <- length(z)
  gp[k] <- gk
  q_k <- q[ind_k,]; weight_k <- weights[ind_k]
  Theta <- tran(z, x_k, mu_pres, sigma_pres)
  temp <- Theta * 1 + (x_k %*% gp) %*% t(rep(1,n))
  t2 <- Y_k %*% t(rep(1,n)) * temp - log(1 + exp(temp))
  -sum(q_k * (weight_k %*% t(rep(1,n))) * t2, na.rm = TRUE) / N_k
}

target_var <- function(para_k, k, x, weights, z, mu_pres, sigma_pres, q) {
  N <- dim(x)[1]; n <- length(z)
  sig <- sigma_pres; sig[k] <- para_k
  Theta <- tran(z, x, mu_pres, sigma_pres)
  mean.v <- apply(x * (rep(1,N) %*% t(mu_pres)), 1, sum, na.rm = TRUE)
  var.v  <- apply(x * (rep(1,N) %*% t(sig^2)),   1, sum, na.rm = TRUE)
  t1 <- -0.5 * ((Theta - mean.v %*% t(rep(1,n))) / (var.v^0.5 %*% t(rep(1,n))))^2
  t2 <- -0.5 * log(2 * pi * var.v)
  -(sum(t2 * weights, na.rm = TRUE) + sum(q * t1 * (weights %*% t(rep(1,n))), na.rm = TRUE)) / N
}

grad_phi1 <- function(phi_j, x, Y_j, weights, z, mu_pres, sigma_pres, q) {
  N <- dim(x)[1]; p <- dim(x)[2]; n <- length(z)
  Theta <- tran(z, x, mu_pres, sigma_pres)
  temp <- Theta + phi_j * (rep(1,N) %*% t(rep(1,n)))
  prob <- exp(temp) / (1 + exp(temp))
  -sum((Y_j %*% t(rep(1,n)) - prob) * q * (weights %*% t(rep(1,n))), na.rm = TRUE) / N
}

grad_phi_ab <- function(ab, g, x, Y_j, weights, z, mu_pres, sigma_pres, q) {
  N <- dim(x)[1]; n <- length(z)
  Theta <- tran(z, x, mu_pres, sigma_pres)
  temp <- Theta * ab + (x %*% g) %*% t(rep(1,n))
  prob <- exp(temp) / (1 + exp(temp))
  -sum(Theta * (Y_j %*% t(rep(1,n)) - prob) * q * (weights %*% t(rep(1,n))), na.rm = TRUE) / N
}

grad_phi_g1 <- function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q) {
  N_k <- dim(x_k)[1]; n <- length(z)
  gp[k] <- gk
  q_k <- q[ind_k,]; weight_k <- weights[ind_k]
  Theta <- tran(z, x_k, mu_pres, sigma_pres)
  temp <- Theta + (x_k %*% gp) %*% t(rep(1,n))
  prob <- exp(temp) / (1 + exp(temp))
  -sum((Y_k %*% t(rep(1,n)) - prob) * q_k * (weight_k %*% t(rep(1,n))), na.rm = TRUE) / N_k
}

grad_phi_g <- function(ab, k, gk, gp, x_k, Y_k, weights, z, ind_k, mu_pres, sigma_pres, q) {
  N_k <- dim(x_k)[1]; n <- length(z)
  gp[k] <- gk
  q_k <- q[ind_k,]; weight_k <- weights[ind_k]
  Theta <- tran(z, x_k, mu_pres, sigma_pres)
  temp <- Theta * ab[1] + (x_k %*% gp) %*% t(rep(1,n))
  prob <- exp(temp) / (1 + exp(temp))
  -sum((Y_k %*% t(rep(1,n)) - prob) * q_k * (weight_k %*% t(rep(1,n))), na.rm = TRUE) / N_k
}

grad_var <- function(para_k, k, x, weights, z, mu_pres, sigma_pres, q) {
  N <- dim(x)[1]; n <- length(z)
  sig <- sigma_pres; sig[k] <- para_k
  Theta <- tran(z, x, mu_pres, sigma_pres)
  mean.v <- apply(x * (rep(1,N) %*% t(mu_pres)), 1, sum, na.rm = TRUE)
  var.v  <- apply(x * (rep(1,N) %*% t(sig^2)),   1, sum, na.rm = TRUE)
  t1 <- sum(-weights * x[,k] / 2 / var.v, na.rm = TRUE)
  t2 <- sum(((x[,k] %*% t(rep(1,n))) * q * (weights %*% t(rep(1,n))) *
             (Theta - mean.v %*% t(rep(1,n)))^2) / 2 / ((var.v %*% t(rep(1,n)))^2), na.rm = TRUE)
  -(t1 + t2) / N
}

mml <- function(a, b, g, x, dat, weights, z, mu, sigma, w) {
  N <- dim(x)[1]; n <- length(z); J <- dim(dat)[2]
  Theta <- tran(z, x, mu, sigma)
  th <- Theta[,1]
  temp <- (th %*% t(a)) + rep(1,N) %*% t(b) + x %*% t(g)
  prob <- exp(temp) / (1 + exp(temp))
  res <- apply((prob^dat) * ((1-prob)^(1-dat)), 1, prod, na.rm = TRUE) * w[1]
  for (k in 2:n) {
    th <- Theta[,k]; temp <- (th %*% t(a)) + rep(1,N) %*% t(b) + x %*% t(g)
    prob <- exp(temp) / (1 + exp(temp))
    res <- res + apply((prob^dat) * ((1-prob)^(1-dat)), 1, prod, na.rm = TRUE) * w[k]
  }
  -sum((weights %*% t(rep(1,J))) * log(res * pi^(-0.5)))
}

# ------------------------------------------------------------------------------
# Parallelized EM
# ------------------------------------------------------------------------------

EM_2PL_inference <- function(a, b, g, x, dat, weights, z, mu, sigma, w, ite, tol = 0.001,
                             checkpoint_file = NULL) {
  N <- dim(x)[1]; p <- dim(x)[2]; n <- length(z); J <- dim(dat)[2]

  phi0 <- matrix(0, J, 2 + p)
  phi1 <- cbind(a, b, g)
  mu0 <- rep(0, p); mu1 <- mu
  sigma0 <- rep(0, p); sigma1 <- sigma

  # Precompute per-country subsets (used in M-step)
  ind_group <- dat_group <- x_group <- vector("list", p)
  for (k in 1:p) {
    ind_group[[k]] <- which(x[,k] == 1)
    dat_group[[k]] <- dat[ind_group[[k]], ]
    x_group[[k]]   <- x[ind_group[[k]], ]
  }

  # Resume from checkpoint if available
  iter <- 0
  if (!is.null(checkpoint_file) && file.exists(checkpoint_file)) {
    ck <- readRDS(checkpoint_file)
    phi1 <- ck$phi1; mu1 <- ck$mu1; sigma1 <- ck$sigma1; iter <- ck$iter
    message("Resumed from checkpoint at iteration ", iter)
    # Force entry into while loop
    phi0 <- phi1 + tol + 1
    mu0  <- mu1  + tol + 1
    sigma0 <- sigma1 + tol + 1
  }

  while (max(c(abs(phi0 - phi1), abs(mu0 - mu1), abs(sigma0 - sigma1))) > tol) {
    iter <- iter + 1
    message("EM iteration ", iter, " [", Sys.time(), "]")
    phi0 <- phi1; mu0 <- mu1; sigma0 <- sigma1

    # E-step
    quadrature <- quad(a = phi0[,1], b = phi0[,2], g = phi0[, 3:(2+p)],
                       x, dat, z, mu0, sigma0, w)

    # M-step: fixed constraints
    mu1        <- rep(0, p)
    phi1[, 2]  <- rep(0, J)
    phi1[1, 1] <- 0.4738117   # anchor item discrimination fixed

    # Parallelize over items — each item's parameters are independent given quadrature
    phi1_rows <- foreach(j = seq_len(J), .combine = rbind,
                         .export = c("tran", "evaluate_prod",
                                     "target_function1", "target_function_ab",
                                     "target_function_g", "target_function_g1",
                                     "grad_phi1", "grad_phi_ab",
                                     "grad_phi_g", "grad_phi_g1")) %dopar% {
      row_j <- phi1[j, ]
      if (j == 1) {
        for (k in seq_len(p)) {
          res <- optim(par = row_j[2+k], fn = target_function_g1, gr = grad_phi_g1,
                       method = "L-BFGS-B",
                       ab = row_j[c(1,2)], k = k, gp = row_j[3:(2+p)],
                       x_k = x_group[[k]], Y_k = dat_group[[k]][, j],
                       ind_k = ind_group[[k]], weights = weights, z = z,
                       mu_pres = mu1, sigma_pres = sigma1, q = quadrature,
                       lower = -5, upper = 5)
          row_j[2+k] <- res$par
        }
      } else {
        for (l in seq_len(ite)) {
          res_ab <- optim(par = row_j[1], fn = target_function_ab, gr = grad_phi_ab,
                          method = "L-BFGS-B",
                          g = row_j[3:(2+p)], x = x, Y_j = dat[, j],
                          weights = weights, z = z,
                          mu_pres = mu0, sigma_pres = sigma0, q = quadrature,
                          lower = -5, upper = 5)
          row_j[1] <- res_ab$par
          for (k in seq_len(p)) {
            res_g <- optim(par = row_j[2+k], fn = target_function_g, gr = grad_phi_g,
                           method = "L-BFGS-B",
                           ab = row_j[c(1,2)], k = k, gp = row_j[3:(2+p)],
                           x_k = x_group[[k]], Y_k = dat_group[[k]][, j],
                           weights = weights, ind_k = ind_group[[k]], z = z,
                           mu_pres = mu0, sigma_pres = sigma0, q = quadrature,
                           lower = -5, upper = 5)
            row_j[2+k] <- res_g$par
          }
        }
      }
      row_j
    }
    phi1 <- phi1_rows

    # Update sigma (sequential — 37 optim calls, fast)
    for (k in seq_len(p)) {
      res <- optim(par = sigma0[k], fn = target_var, gr = grad_var,
                   method = "L-BFGS-B",
                   k = k, x = x, z = z, weights = weights,
                   mu_pres = mu0, sigma_pres = sigma0, q = quadrature,
                   lower = 0.1, upper = 5)
      sigma1[k] <- res$par
    }

    MLL <- mml(a = phi1[,1], b = phi1[,2], g = phi1[, 3:(2+p)],
               x, dat, weights, z, mu = mu1, sigma = sigma1, w)
    message("  MLL = ", round(MLL, 2), "  max_delta = ",
            round(max(c(abs(phi0 - phi1), abs(mu0 - mu1), abs(sigma0 - sigma1))), 4))

    # Checkpoint after each completed iteration
    if (!is.null(checkpoint_file)) {
      saveRDS(list(phi1 = phi1, mu1 = mu1, sigma1 = sigma1, iter = iter),
              checkpoint_file)
      message("  Checkpoint saved.")
    }
  }

  list(mu = mu1, sigma = sigma1, alpha.vec = phi1[,1], beta.vec = phi1[,2],
       gamma.vec = phi1[, 3:(p+2)], post = quadrature)
}

# ------------------------------------------------------------------------------
# Execution
# ------------------------------------------------------------------------------

set.seed(6)

message("Reading IRW-derived data...")
dat     <- read.csv("data/PISA_2022_math_sas.csv", header = TRUE)
dat     <- dat[,-1]; colnames(dat) <- NULL; dat <- data.matrix(dat)
x       <- read.csv("data/countries_2022_math_sas.csv", header = TRUE)
x       <- x[,-1]; colnames(x) <- NULL; x <- data.matrix(x)
weights <- read.csv("data/PISA_2022_math_samplingweights_sas.csv", header = TRUE)[,1]

N <- dim(dat)[1]; J <- dim(dat)[2]; p <- dim(x)[2]
message("N=", N, "  J=", J, "  p=", p)

a_ini <- read.csv("output/a_ini_math_2022.csv", header = FALSE)[,1]
if (length(a_ini) != J) stop("a_ini length ", length(a_ini), " != J=", J)

b_ini     <- rep(0, J)
g_ini     <- matrix(0, J, p)
mu_ini    <- rep(0, p)
sigma_ini <- runif(p, 0.2, 1)

message("Starting EM (tol=0.05, ite=1, cores=", N_CORES, ")...")
r <- EM_2PL_inference(a = a_ini, b = b_ini, g = g_ini, x = x, dat = dat,
                      weights = weights, z = z, mu = mu_ini, sigma = sigma_ini,
                      w = w, ite = 1, tol = 0.05,
                      checkpoint_file = "output/checkpoint.rds")

write.table(r$gamma.vec, sep=",", col.names=FALSE, row.names=FALSE, file="output/gamma_math_2022_d0.csv")
write.table(r$alpha.vec, sep=",", col.names=FALSE, row.names=FALSE, file="output/alpha_math_2022_d0.csv")
write.table(r$beta.vec,  sep=",", col.names=FALSE, row.names=FALSE, file="output/beta_math_2022_d0.csv")
write.table(r$mu,        sep=",", col.names=FALSE, row.names=FALSE, file="output/mu_math_2022_d0.csv")
write.table(r$sigma,     sep=",", col.names=FALSE, row.names=FALSE, file="output/sigma_math_2022_d0.csv")

message("Done. Results written to output/")
