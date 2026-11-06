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
# Clamped away from 0 and 1 so that log P stays finite for extreme items.
p_matrix <- function(a, d, nodes) {
  P <- plogis(outer(nodes, a) + matrix(d, length(nodes), length(a), byrow = TRUE))
  pmin(pmax(P, 1e-12), 1 - 1e-12)
}

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
# earliest-wave / control-arm rule, the respondent cap, and two item drops:
# items answered by fewer than `min_coverage` of respondents (booklet- or
# branch-specific items that most people never saw), then zero-variance items.
prepare_resp <- function(df, max_n = 10000, seed = 1, min_coverage = 0.5) {
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
  n_ids_in <- length(unique(df$id))
  resp <- irw_long2resp(df)        # drops ids with response density < 0.1 by default
  resp$id <- NULL
  info$n_ids_dropped_long2resp <- n_ids_in - nrow(resp)
  info$n_items_raw <- ncol(resp)
  coverage <- colMeans(!is.na(resp))
  info$n_low_coverage <- sum(coverage < min_coverage)
  resp <- resp[, coverage >= min_coverage, drop = FALSE]
  keep <- vapply(resp, function(x) length(unique(na.omit(x))) > 1, logical(1))
  info$n_zero_var <- sum(!keep)
  resp <- resp[, keep, drop = FALSE]
  info$n_ids_used <- nrow(resp)
  # Degenerate: every respondent gives one response to every item (e.g. a
  # person-level condition stored in resp). There is nothing to scale.
  rng <- apply(as.matrix(resp), 1, function(x) diff(range(x, na.rm = TRUE)))
  if (all(rng == 0)) stop("degenerate: every respondent answers all items identically")
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

# 2PL with a lognormal(0, 1) prior on the slopes, as in
# 2pl_across_datasets_compute.R: weak, centred near a = 1, and it stops runaway
# slopes from inflating the bits. It also keeps slopes positive, so an item
# that runs against the trait gets a small positive slope rather than a
# negative one.
fit_2pl <- function(resp) {
  ni <- ncol(resp)
  spec <- mirt::mirt.model(paste0("F = 1-", ni, "\nPRIOR = (1-", ni, ", a1, lnorm, 0.0, 1.0)"))
  mirt::mirt(resp, spec, itemtype = "2PL", verbose = FALSE)
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
  fit_2 <- fit_2pl(resp)
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

# ------------------------------------------------------------------------------
# Showcase quantities (2PL, latent N(0, 1))
#
# The discretisation device: theta is cut into 2^K equally likely groups
# (quantiles of N(0, 1)). To make binned and continuous quantities exactly
# comparable, both use one fine equal-probability grid: node i sits at
# qnorm((i - 0.5) / M) with weight 1 / M, and group k at level K is the run of
# M / 2^K consecutive nodes. M = 2048 supports K up to 11.
# ------------------------------------------------------------------------------

fine_grid <- function(M = 2048) list(nodes = qnorm((seq_len(M) - 0.5) / M), w = rep(1 / M, M))

# Average the columns of a (rows x M) matrix within each of 2^K groups.
bin_cols <- function(X, K) {
  M <- ncol(X); per <- M / 2^K
  sapply(seq_len(2^K), function(k) rowMeans(X[, ((k - 1) * per + 1):(k * per), drop = FALSE]))
}

# P(X_j = 1 | group), 2^K x n.
group_probs <- function(a, d, K = 6, M = 2048) {
  fg <- fine_grid(M)
  P <- p_matrix(a, d, fg$nodes)              # M x n
  t(bin_cols(t(P), K))
}

group_bounds <- function(K = 6) qnorm(seq(0, 1, length.out = 2^K + 1))

# I(group; X) for K = 1..Kmax, plus I(theta; X) on the same fine grid.
# n <= 16: exact, patterns enumerated in chunks. Larger n: Monte Carlo with
# common random draws across K.
bin_convergence <- function(a, d, Kmax = 8, M = 2048, n_mc = 40000) {
  fg <- fine_grid(M)
  P <- p_matrix(a, d, fg$nodes); n <- ncol(P)
  lP <- log(P); lQ <- log(1 - P)
  Ks <- seq_len(Kmax)
  if (n <= 16) {
    X <- as.matrix(expand.grid(rep(list(0:1), n)))
    HXg <- setNames(numeric(Kmax), Ks); HX <- 0
    for (st in seq(1, nrow(X), by = 4096)) {
      Xc <- X[st:min(nrow(X), st + 4095), , drop = FALSE]
      pxt <- exp(Xc %*% t(lP) + (1 - Xc) %*% t(lQ))     # patterns x M
      px <- rowMeans(pxt)
      HX <- HX - sum(px * log2s(px))
      for (K in Ks) {
        pxg <- bin_cols(pxt, K)                           # patterns x 2^K
        HXg[K] <- HXg[K] - sum(pxg * log2s(pxg)) / 2^K
      }
    }
    H_X_theta <- h_x_given_theta(P, fg$w)
    data.frame(K = c(Ks, Inf), bits = c(HX - HXg, HX - H_X_theta), se = 0, method = "enumeration")
  } else {
    V <- matrix(NA_real_, n_mc, Kmax + 1)                # columns: K = 1..Kmax, continuous
    for (st in seq(1, n_mc, by = 4000)) {
      idx <- st:min(n_mc, st + 3999); b <- length(idx)
      q <- sample.int(M, b, replace = TRUE)
      X <- (matrix(runif(b * n), b, n) < P[q, , drop = FALSE]) * 1
      lpx <- X %*% t(lP) + (1 - X) %*% t(lQ)            # b x M
      mx <- apply(lpx, 1, max)
      e <- exp(lpx - mx)
      lmarg <- mx + log(rowMeans(e))
      for (K in Ks) {
        grp <- (q - 1) %/% (M / 2^K) + 1
        V[idx, K] <- (mx + log(bin_cols(e, K)[cbind(seq_len(b), grp)]) - lmarg) / log(2)
      }
      V[idx, Kmax + 1] <- (lpx[cbind(seq_len(b), q)] - lmarg) / log(2)
    }
    data.frame(K = c(Ks, Inf), bits = colMeans(V), se = apply(V, 2, sd) / sqrt(n_mc),
               method = "monte_carlo")
  }
}

# Expected bits of every item under a posterior over groups.
expected_bits <- function(post, PG) h2(colSums(post * PG)) - colSums(post * h2(PG))

# Replay one respondent's responses in a given order (fixed or adaptive).
# x: named 0/1 vector over items (complete). PG: groups x items.
replay <- function(x, PG, order = c("fixed", "adaptive")) {
  order <- match.arg(order)
  G <- nrow(PG); n <- ncol(PG)
  post <- rep(1 / G, G); remaining <- seq_len(n)
  steps <- vector("list", n + 1)
  steps[[1]] <- list(step = 0, item = NA, response = NA, expected_bits = NA, actual_bits = NA,
                     surprisal = NA, entropy = entropy(post), posterior = post)
  for (k in seq_len(n)) {
    eb <- expected_bits(post, PG)
    j <- if (order == "fixed") k else remaining[which.max(eb[remaining])]
    p1 <- sum(post * PG[, j])
    xj <- x[j]
    like <- if (xj == 1) PG[, j] else 1 - PG[, j]
    p_obs <- if (xj == 1) p1 else 1 - p1
    h_before <- entropy(post)
    post <- post * like / p_obs
    steps[[k + 1]] <- list(step = k, item = colnames(PG)[j], response = xj,
                           expected_bits = eb[j], actual_bits = h_before - entropy(post),
                           surprisal = -log2(p_obs), entropy = entropy(post), posterior = post)
    remaining <- setdiff(remaining, j)
  }
  list(
    steps = do.call(rbind, lapply(steps, function(s) as.data.frame(s[setdiff(names(s), "posterior")]))),
    posterior = do.call(rbind, lapply(steps, `[[`, "posterior"))
  )
}

# Mean posterior entropy after k items over simulated respondents, for fixed,
# random and adaptive orders. Respondents: group drawn uniformly, theta drawn
# from that group's fine-grid nodes, responses from the 2PL at theta.
uncertainty_curves <- function(a, d, K = 6, n_sim = 500, M = 2048) {
  fg <- fine_grid(M); PG <- group_probs(a, d, K, M); n <- length(a)
  q <- sample.int(M, n_sim, replace = TRUE)
  X <- (matrix(runif(n_sim * n), n_sim, n) < p_matrix(a, d, fg$nodes[q])) * 1
  colnames(X) <- colnames(PG) <- names(a)
  ent <- function(ord) {
    t(sapply(seq_len(n_sim), function(i) {
      x <- X[i, ]
      if (ord == "random") {
        perm <- sample.int(n)
        replay(x[perm], PG[, perm, drop = FALSE], "fixed")$steps$entropy
      } else replay(x, PG, ord)$steps$entropy
    }))
  }
  out <- lapply(c("fixed", "random", "adaptive"), function(o) {
    E <- ent(o)
    data.frame(order = o, k = 0:n, mean_entropy = colMeans(E),
               q10 = apply(E, 2, quantile, .1), q90 = apply(E, 2, quantile, .9))
  })
  do.call(rbind, out)
}

# ------------------------------------------------------------------------------
# Sufficiency in information terms (added for the "Is the sum score enough?"
# section). Chain rule: I(theta; X) = I(theta; S) + I(theta; X | S).
# All on a supplied grid (nodes, w), so any prior can be used with item
# parameters held fixed.
# ------------------------------------------------------------------------------

# Grid for an arbitrary prior: a wide fixed theta range, weights from the
# prior density, renormalised.
prior_grid <- function(dens, lo = -10, hi = 10, n_nodes = 401) {
  nodes <- seq(lo, hi, length.out = n_nodes)
  w <- dens(nodes)
  list(nodes = nodes, w = w / sum(w))
}

# Everything that needs the full pattern enumeration, for one model and prior.
# Returns overall quantities and a per-score decomposition.
enumerate_info <- function(a, d, g) {
  n <- length(a); stopifnot(n <= 16)
  P <- p_matrix(a, d, g$nodes)
  X <- as.matrix(expand.grid(rep(list(0:1), n)))
  s <- rowSums(X)
  pxt <- exp(X %*% t(log(P)) + (1 - X) %*% t(log(1 - P)))   # patterns x nodes
  px <- as.vector(pxt %*% g$w)
  H_X <- entropy(px)
  H_X_theta <- h_x_given_theta(P, g$w)
  L <- lord_wingersky(P)                                      # nodes x (n+1)
  ps <- colSums(g$w * L)
  H_S <- entropy(ps)
  I_S <- H_S - sum(g$w * apply(L, 1, entropy))
  I_X <- H_X - H_X_theta
  # Per score level s: H(X | S=s) and I(theta; X | S=s) = H(X|S=s) - H(X|S=s,theta)
  by_s <- do.call(rbind, lapply(0:n, function(k) {
    idx <- which(s == k)
    pk <- ps[k + 1]
    pxs <- px[idx] / pk
    H_Xs <- entropy(pxs)
    Lk <- L[, k + 1]
    post <- g$w * Lk / pk                                      # P(theta | S = s)
    cond <- sweep(pxt[idx, , drop = FALSE], 2, pmax(Lk, 1e-300), "/")  # P(x | s, theta)
    H_cond <- -colSums(cond * log2s(cond))
    data.frame(s = k, p_s = pk, n_patterns = length(idx), H_X_given_s = H_Xs,
               I_X_given_s = H_Xs - sum(post * H_cond))
  }))
  list(I_X = I_X, I_S = I_S, I_X_given_S = I_X - I_S, H_X = H_X, H_S = H_S,
       H_X_given_theta = H_X_theta, by_s = by_s)
}

# I(theta; T) for an arbitrary statistic of the pattern, by grouping
# enumerated patterns on T (rounded).
info_statistic <- function(a, d, g, T_fun, digits = 8) {
  n <- length(a)
  P <- p_matrix(a, d, g$nodes)
  X <- as.matrix(expand.grid(rep(list(0:1), n)))
  key <- round(T_fun(X), digits)
  pxt <- exp(X %*% t(log(P)) + (1 - X) %*% t(log(1 - P)))
  pt_theta <- rowsum(pxt, key)                                 # T levels x nodes
  pt <- as.vector(pt_theta %*% g$w)
  list(I = entropy(pt) - sum(g$w * apply(pt_theta, 2, entropy)), n_levels = nrow(pt_theta))
}

# Rasch (common slope): P(x | S = s) does not involve theta. With slope-intercept
# items, P(x | s) = exp(sum_j x_j d_j) / gamma_s, gamma_s the elementary symmetric
# function of exp(d). Returns the log2 P(x | s) for a 0/1 matrix of patterns.
rasch_log2_p_given_s <- function(X, d) {
  n <- length(d); e <- exp(d)
  gam <- numeric(n + 1); gam[1] <- 1                           # gamma_0..gamma_n
  for (j in seq_len(n)) gam[2:(j + 1)] <- gam[2:(j + 1)] + gam[1:j] * e[j]
  s <- rowSums(X)
  (as.vector(X %*% d) - log(gam[s + 1])) / log(2)
}

# Within-score surprisal distribution under Rasch, for every score level, using
# all items (no theta grid needed, so 2^n patterns are cheap up to n = 20).
rasch_within_score <- function(d) {
  n <- length(d)
  X <- as.matrix(expand.grid(rep(list(0:1), n)))
  lp <- rasch_log2_p_given_s(X, d)
  data.frame(s = rowSums(X), surprisal = -lp, p_given_s = 2^lp)
}

# Percentile of an observed within-score surprisal: probability, among patterns
# with the same score, of a surprisal at or below the observed one.
surprisal_percentile <- function(ws, s_obs, surp_obs) {
  sub <- ws[ws$s == s_obs, ]
  sum(sub$p_given_s[sub$surprisal <= surp_obs + 1e-12])
}

# Fisher information at each node: full data (test information) and the sum
# score's distribution, with dP(S = s | theta)/dtheta carried through the
# Lord-Wingersky recursion analytically (dP_j/dtheta = a_j P_j (1 - P_j)).
fisher_compare <- function(a, d, nodes) {
  P <- p_matrix(a, d, nodes)
  dP <- sweep(P * (1 - P), 2, a, "*")
  Q <- nrow(P); n <- ncol(P)
  L <- matrix(0, Q, n + 1); dL <- matrix(0, Q, n + 1); L[, 1] <- 1
  for (j in seq_len(n)) {
    p <- P[, j]; dp <- dP[, j]
    Lo <- L[, 1:(j + 1), drop = FALSE]; dLo <- dL[, 1:(j + 1), drop = FALSE]
    newL <- Lo * (1 - p); newd <- dLo * (1 - p) - Lo * dp
    newL[, 2:(j + 1)] <- newL[, 2:(j + 1)] + L[, 1:j, drop = FALSE] * p
    newd[, 2:(j + 1)] <- newd[, 2:(j + 1)] + dL[, 1:j, drop = FALSE] * p + L[, 1:j, drop = FALSE] * dp
    L[, 1:(j + 1)] <- newL; dL[, 1:(j + 1)] <- newd
  }
  data.frame(theta = nodes,
             test_info = as.vector((P * (1 - P)) %*% (a^2)),
             sum_score_info = rowSums(ifelse(L > 0, dL^2 / L, 0)))
}
