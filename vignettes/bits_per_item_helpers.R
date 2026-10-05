# bits_per_item_helpers.R
#
# Information-theoretic quantities for dichotomous IRT models, in bits.
# Sourced by bits_per_item_compute.R and bits_per_item_checks.R.
#
# Conventions
#   - Items follow mirt's slope-intercept form: P_j(theta) = plogis(a_j * theta + d_j).
#   - The latent distribution g(theta) is represented by a fixed quadrature grid:
#     `nodes` spanning +/- 6 SD of the model's own fitted latent distribution,
#     with normal-density weights renormalised to sum to 1. Every quantity below
#     (exact, Lord-Wingersky, enumeration, Monte Carlo) uses the same discrete
#     g, so they are directly comparable; the Monte Carlo draws theta from the
#     grid for the same reason.
#   - log base 2 throughout.

log2s <- function(x) ifelse(x > 0, log2(x), 0)   # 0 * log 0 := 0

# Binary entropy h(p), vectorised, with h(0) = h(1) = 0.
h2 <- function(p) -(p * log2s(p)) - ((1 - p) * log2s(1 - p))

entropy <- function(p) {
  p <- p[p > 0]
  -sum(p * log2(p))
}

make_grid <- function(sd = 1, n_nodes = 81, span = 6) {
  nodes <- seq(-span, span, length.out = n_nodes) * sd
  w <- dnorm(nodes, 0, sd)
  list(nodes = nodes, w = w / sum(w))
}

# Q x n matrix of P_j(theta_q).
p_matrix <- function(a, d, nodes) plogis(outer(nodes, a) + matrix(d, length(nodes), length(a), byrow = TRUE))

# H(X | theta) = sum_q w_q sum_j h(P_qj)
h_x_given_theta <- function(P, w) sum(w * rowSums(h2(P)))

# Per-item I(theta; X_j) = h(Pbar_j) - sum_q w_q h(P_qj)
item_info_bits <- function(P, w) h2(colSums(w * P)) - colSums(w * h2(P))

# Lord-Wingersky: Q x (n+1) matrix of P(S = s | theta_q), s = 0..n.
lord_wingersky <- function(P) {
  Q <- nrow(P); n <- ncol(P)
  L <- matrix(0, Q, n + 1); L[, 1] <- 1
  for (j in seq_len(n)) {
    p <- P[, j]
    new <- L[, 1:(j + 1), drop = FALSE] * (1 - p)
    new[, 2:(j + 1)] <- new[, 2:(j + 1)] + L[, 1:j, drop = FALSE] * p
    L[, 1:(j + 1)] <- new
  }
  L
}

# Add one item to an existing L (Q x (k+1)) -> Q x (k+2).
lw_add <- function(L, p) {
  k1 <- ncol(L)
  out <- cbind(L * (1 - p), 0)
  out[, 2:(k1 + 1)] <- out[, 2:(k1 + 1)] + L * p
  out
}

# I(theta; S) from an LW matrix.
info_from_L <- function(L, w) {
  ps <- colSums(w * L)
  entropy(ps) - sum(w * apply(L, 1, entropy))
}
info_sum <- function(P, w) info_from_L(lord_wingersky(P), w)

# Exact I(theta; X) by enumerating all 2^n patterns (n <= 16).
info_pattern_exact <- function(P, w) {
  n <- ncol(P)
  stopifnot(n <= 16)
  X <- as.matrix(expand.grid(rep(list(0:1), n)))
  lP <- log(P); lQ <- log(1 - P)
  # log p(x | theta_q): patterns x nodes
  lpx <- X %*% t(lP) + (1 - X) %*% t(lQ)
  px_theta <- exp(lpx)
  px <- as.vector(px_theta %*% w)
  H_X <- entropy(px)
  H_X_theta <- h_x_given_theta(P, w)
  list(I = H_X - H_X_theta, H_X = H_X, H_X_theta = H_X_theta, px = px, X = X)
}

# Monte Carlo I(theta; X) = E[log2 p(x|theta) - log2 p(x)], theta ~ discrete g.
# Draws in batches until the MC SE is below `target_se` (or max_draws).
info_pattern_mc <- function(P, w, target_se = 0.005, batch = 20000, max_draws = 1e6) {
  Q <- nrow(P); n <- ncol(P)
  lP <- log(P); lQ <- log(1 - P)
  vals <- numeric(0)
  repeat {
    q <- sample.int(Q, batch, replace = TRUE, prob = w)
    U <- matrix(runif(batch * n), batch, n)
    X <- (U < P[q, , drop = FALSE]) * 1
    lpx <- X %*% t(lP) + (1 - X) %*% t(lQ)          # batch x Q, natural log
    lp_true <- lpx[cbind(seq_len(batch), q)]
    mx <- apply(lpx, 1, max)
    lp_marg <- mx + log(as.vector(exp(lpx - mx) %*% w))
    vals <- c(vals, (lp_true - lp_marg) / log(2))
    se <- sd(vals) / sqrt(length(vals))
    if (se < target_se || length(vals) >= max_draws) break
  }
  list(I = mean(vals), se = se, draws = length(vals))
}

info_pattern <- function(P, w, ...) {
  if (ncol(P) <= 16) {
    e <- info_pattern_exact(P, w)
    list(I = e$I, se = 0, method = "enumeration")
  } else {
    m <- info_pattern_mc(P, w, ...)
    list(I = m$I, se = m$se, method = "monte_carlo", draws = m$draws)
  }
}

# Length curves: I(theta; S_k) for k = 1..n along random orders and a greedy order.
length_curves <- function(P, w, n_random = 5) {
  n <- ncol(P)
  rand <- sapply(seq_len(n_random), function(r) {
    ord <- sample.int(n)
    L <- matrix(1, nrow(P), 1)
    vapply(seq_len(n), function(k) {
      L <<- lw_add(L, P[, ord[k]])
      info_from_L(L, w)
    }, numeric(1))
  })
  rand <- matrix(rand, nrow = n)
  # Greedy: at each step add the item maximising I(theta; S_k).
  remaining <- seq_len(n); chosen <- integer(0)
  L <- matrix(1, nrow(P), 1); greedy <- numeric(n)
  for (k in seq_len(n)) {
    cand <- vapply(remaining, function(j) info_from_L(lw_add(L, P[, j]), w), numeric(1))
    best <- remaining[which.max(cand)]
    L <- lw_add(L, P[, best]); greedy[k] <- max(cand)
    chosen <- c(chosen, best); remaining <- setdiff(remaining, best)
  }
  data.frame(k = seq_len(n), random_mean = rowMeans(rand),
             random_min = apply(rand, 1, min), random_max = apply(rand, 1, max),
             greedy = greedy, greedy_item = chosen)
}

# Reliability of the EAP-from-sum-score: Var(E[theta | S]) / Var(theta).
eap_sum_reliability <- function(P, nodes, w) {
  L <- lord_wingersky(P)
  ps <- colSums(w * L)
  post_mean <- colSums(w * L * nodes) / ps
  m <- sum(w * nodes); v <- sum(w * (nodes - m)^2)
  sum(ps * (post_mean - m)^2) / v
}

# Marginal reliability, integral of var*TI / (var*TI + 1) over g. Equal to
# mirt::marginal_rxx() when var(theta) = 1. mirt 1.46.1's marginal_rxx()
# divides by TI + var (not TI + 1/var) and integrates over N(0, 1) whatever
# the fitted variance, so it is wrong for Rasch fits with var(theta) != 1;
# this version is used for both models.
marginal_rel <- function(a, P, nodes, w) {
  TI <- as.vector((P * (1 - P)) %*% (a^2))
  v <- sum(w * nodes^2) - sum(w * nodes)^2
  sum(w * v * TI / (v * TI + 1))
}

gauss_bits <- function(rho) -0.5 * log2(1 - rho)

# KL(observed || implied) for the sum-score distribution, in bits.
sum_score_kl <- function(obs_counts, implied) {
  p <- obs_counts / sum(obs_counts)
  keep <- p > 0
  sum(p[keep] * log2(p[keep] / implied[keep]))
}

# ------------------------------------------------------------------------------
# Data preparation (same single-administration rule as bits_per_item_scout.R)
# ------------------------------------------------------------------------------

lowest_value <- function(x) {
  u <- unique(x[!is.na(x)])
  num <- suppressWarnings(as.numeric(as.character(u)))
  if (all(!is.na(num))) u[which.min(num)] else sort(as.character(u))[1]
}

# Long IRW data -> wide 0/1 matrix (rows = respondents), applying the
# earliest-wave / control-arm rule and the respondent cap.
prepare_resp <- function(df, max_n = 10000, seed = 1) {
  df <- as.data.frame(df)
  info <- list(n_ids_raw = length(unique(df$id)), wave_kept = NA, treat_kept = NA)
  if ("wave" %in% names(df) && length(unique(na.omit(df$wave))) > 1) {
    info$wave_kept <- as.character(lowest_value(df$wave))
    df <- df[!is.na(df$wave) & as.character(df$wave) == info$wave_kept, ]
  }
  if ("treat" %in% names(df) && length(unique(na.omit(df$treat))) > 1) {
    info$treat_kept <- as.character(lowest_value(df$treat))
    df <- df[!is.na(df$treat) & as.character(df$treat) == info$treat_kept, ]
  }
  df <- df[!is.na(df$resp), c("id", "item", "resp")]
  vals <- sort(unique(df$resp))
  if (length(vals) != 2) stop("responses are not dichotomous: ", paste(vals, collapse = ","))
  df$resp <- as.integer(df$resp == vals[2])        # recode to 0/1 if stored otherwise
  ids <- unique(df$id)
  info$n_ids_rule <- length(ids)
  info$subsampled <- length(ids) > max_n
  if (info$subsampled) {
    set.seed(seed)
    df <- df[df$id %in% sample(ids, max_n), ]
  }
  resp <- irw_long2resp(df)
  resp$id <- NULL
  keep <- vapply(resp, function(x) length(unique(na.omit(x))) > 1, logical(1))
  info$n_items_raw <- ncol(resp)
  info$n_zero_var <- sum(!keep)
  resp <- resp[, keep, drop = FALSE]
  info$n_ids_used <- nrow(resp)
  list(resp = resp, info = info)
}

# ------------------------------------------------------------------------------
# Corpus quantities for one fitted model
# ------------------------------------------------------------------------------

model_bits <- function(a, d, sd, resp, mc_se = 0.005) {
  g <- make_grid(sd)
  P <- p_matrix(a, d, g$nodes)
  L <- lord_wingersky(P)
  I_S <- info_from_L(L, g$w)
  pat <- info_pattern(P, g$w, target_se = mc_se)
  H_XT <- h_x_given_theta(P, g$w)
  complete <- stats::complete.cases(resp)
  s_obs <- tabulate(rowSums(resp[complete, , drop = FALSE]) + 1, nbins = ncol(resp) + 1)
  list(
    H_X_given_theta = H_XT, I_S = I_S, I_X = pat$I, I_X_se = pat$se,
    I_X_method = pat$method, H_X = pat$I + H_XT,
    efficiency = pat$I / ncol(P),
    rho_marginal = marginal_rel(a, P, g$nodes, g$w),
    rho_eap_sum = eap_sum_reliability(P, g$nodes, g$w),
    sum_kl_bits = sum_score_kl(s_obs, colSums(g$w * L)),
    n_complete = sum(complete),
    item_bits = item_info_bits(P, g$w),
    P = P, g = g
  )
}

# ------------------------------------------------------------------------------
# One table end to end: fetch, prepare, fit Rasch + 2PL, compute everything.
# ------------------------------------------------------------------------------

process_table <- function(table_name, max_n = 10000, mc_se = 0.005, n_random = 5) {
  t0 <- Sys.time(); times <- list()
  df <- irw_fetch(table_name)
  times$fetch <- Sys.time()
  prep <- prepare_resp(df, max_n = max_n)
  rm(df)
  resp <- prep$resp
  times$prepare <- Sys.time()

  fit_r <- mirt::mirt(resp, 1, itemtype = "Rasch", verbose = FALSE)
  fit_2 <- mirt::mirt(resp, 1, itemtype = "2PL", verbose = FALSE)
  times$fit <- Sys.time()

  cr <- mirt::coef(fit_r, simplify = TRUE)
  c2 <- mirt::coef(fit_2, simplify = TRUE)
  # Rasch: slopes fixed at 1, latent variance estimated -> g = N(0, var).
  # 2PL: latent N(0, 1) fixed, slopes estimated.
  sd_r <- sqrt(cr$cov[1, 1])
  rasch <- model_bits(cr$items[, "a1"], cr$items[, "d"], sd_r, resp, mc_se)
  twopl <- model_bits(c2$items[, "a1"], c2$items[, "d"], 1, resp, mc_se)
  times$bits <- Sys.time()

  curves <- length_curves(twopl$P, twopl$g$w, n_random = n_random)
  times$curves <- Sys.time()

  rxx_2 <- mirt::marginal_rxx(fit_2)   # cross-check for rho_marginal (2PL only)
  strip <- function(m) m[setdiff(names(m), c("P", "g", "item_bits"))]
  summ <- dplyr::bind_rows(
    c(list(model = "Rasch", converged = mirt::extract.mirt(fit_r, "converged"),
           latent_sd = sd_r, mirt_marginal_rxx = NA_real_), strip(rasch)),
    c(list(model = "2PL", converged = mirt::extract.mirt(fit_2, "converged"),
           latent_sd = 1, mirt_marginal_rxx = rxx_2), strip(twopl))
  )
  summ$bits_rho_marginal <- gauss_bits(summ$rho_marginal)
  summ$bits_rho_eap_sum <- gauss_bits(summ$rho_eap_sum)
  summ$table <- table_name
  items <- data.frame(table = table_name, item = colnames(resp),
                      a = c2$items[, "a1"], d = c2$items[, "d"],
                      b = -c2$items[, "d"] / c2$items[, "a1"],
                      bits = twopl$item_bits)
  list(table = table_name, info = prep$info, n_items = ncol(resp),
       summary = summ, items = items, curves = cbind(table = table_name, curves),
       timing = setNames(diff(as.numeric(c(t0, do.call(c, times)))), names(times)))
}
