# bits_per_item_poly_helpers.R
#
# Information-theoretic quantities for polytomous IRT models, in bits, for the
# "More than right and wrong" appendix of the bits-per-item vignette. Sourced
# after bits_per_item_helpers.R (it reuses log2s, entropy, make_grid,
# prior_grid, fine_grid, bin_cols, lowest_value).
#
# Conventions
#   - An item is a Q x K_j matrix of category probabilities at the grid nodes
#     (rows sum to 1); a test is a list of them. Categories are scored
#     0..K_j - 1 in their labelled order.
#   - Every model is put on a standardised latent scale z ~ N(0, 1): for the
#     PCM, which estimates the latent SD sigma with slopes fixed, the items are
#     evaluated at theta = sigma * z. So a grid or prior means the same thing
#     for every model, as in bits_per_item_sufficiency.R.
#   - Category probabilities come from mirt::probtrace(), so one code path
#     serves PCM, GPCM, GRM and the nominal model.
#   - log base 2 throughout.

# Row-wise entropy of a matrix of probabilities (rows sum to 1).
row_entropy <- function(P) -rowSums(P * log2s(P))

# List of Q x K_j matrices for every item of a fitted unidimensional mirt
# model, at standardised nodes z. Clamped away from 0 so logs stay finite.
item_probs <- function(fit, z, sd = 1) {
  Theta <- matrix(sd * z)
  lapply(seq_len(extract.mirt(fit, "nitems")), function(j) {
    P <- probtrace(extract.item(fit, j), Theta)
    P <- pmax(P, 1e-300)
    P / rowSums(P)
  })
}

# Latent SD of a fit (estimated for the PCM, 1 otherwise).
latent_sd <- function(fit) sqrt(coef(fit, simplify = TRUE)$cov[1, 1])

# Per-item I(theta; X_j) = H(Pbar_j) - sum_q w_q H(P_qj), plus the
# model-implied marginal entropy H(Pbar_j).
poly_item_info <- function(Ps, w) {
  t(vapply(Ps, function(P) {
    pbar <- colSums(w * P)
    H <- entropy(pbar)
    c(I = H - sum(w * row_entropy(P)), H_model = H)
  }, numeric(2)))
}

poly_h_x_given_theta <- function(Ps, w) sum(w * Reduce(`+`, lapply(Ps, row_entropy)))

# Polytomous Lord-Wingersky (Thissen, Pommerich, Billeaud & Williams, 1995):
# Q x (max S + 1) matrix of P(S = s | theta_q), S = sum of category scores.
poly_lord_wingersky <- function(Ps) {
  Q <- nrow(Ps[[1]])
  L <- matrix(1, Q, 1)
  for (P in Ps) {
    K <- ncol(P); m <- ncol(L)
    new <- matrix(0, Q, m + K - 1)
    for (k in seq_len(K)) new[, k:(k + m - 1)] <- new[, k:(k + m - 1)] + L * P[, k]
    L <- new
  }
  L
}

poly_info_sum <- function(Ps, w) info_from_L(poly_lord_wingersky(Ps), w)

# All response patterns (0-based categories), rows = patterns.
pattern_grid <- function(K) as.matrix(expand.grid(lapply(K, function(k) 0:(k - 1))))

# log P(x | theta) for a block of patterns: patterns x Q (natural log).
pattern_logp <- function(X, lPt) {
  # lPt[[j]] is K_j x Q (transposed log probabilities)
  out <- lPt[[1]][X[, 1] + 1, , drop = FALSE]
  for (j in seq_along(lPt)[-1]) out <- out + lPt[[j]][X[, j] + 1, , drop = FALSE]
  out
}

# Exact I(theta; X) by enumerating all prod(K_j) patterns in chunks.
# Optionally also I(theta; T) for a statistic T of the pattern (T_fun maps a
# pattern block to a numeric vector; levels are T rounded to `digits`).
poly_info_exact <- function(Ps, w, T_fun = NULL, digits = 8, chunk = 2^16) {
  K <- vapply(Ps, ncol, integer(1)); n_pat <- prod(K)
  X <- pattern_grid(K)
  lPt <- lapply(Ps, function(P) t(log(P)))
  px <- numeric(n_pat)
  for (st in seq(1, n_pat, by = chunk)) {
    idx <- st:min(n_pat, st + chunk - 1)
    px[idx] <- as.vector(exp(pattern_logp(X[idx, , drop = FALSE], lPt)) %*% w)
  }
  H_X <- entropy(px)
  H_XT <- poly_h_x_given_theta(Ps, w)
  out <- list(I = H_X - H_XT, H_X = H_X, H_X_theta = H_XT, n_patterns = n_pat)
  if (!is.null(T_fun)) {
    key <- round(T_fun(X), digits)
    dup <- key %in% key[duplicated(key)]
    H_T <- entropy(as.vector(tapply(px, key, sum)))
    # H(T | theta): patterns with a unique T level contribute as themselves;
    # the rest are pooled within level, node by node.
    H_T_theta <- 0
    for (st in seq(1, n_pat, by = chunk)) {
      idx <- st:min(n_pat, st + chunk - 1)
      u <- idx[!dup[idx]]
      if (length(u)) {
        pu <- exp(pattern_logp(X[u, , drop = FALSE], lPt))
        H_T_theta <- H_T_theta - sum((pu * log2s(pu)) %*% w)
      }
    }
    d <- which(dup)
    if (length(d)) {
      pd <- rowsum(exp(pattern_logp(X[d, , drop = FALSE], lPt)), key[d])
      H_T_theta <- H_T_theta - sum((pd * log2s(pd)) %*% w)
    }
    out$I_T <- H_T - H_T_theta
    out$n_T_levels <- length(unique(key))
  }
  out
}

# Monte Carlo I(theta; X) = E[log2 p(x|theta) - log2 p(x)], theta drawn from
# the discrete grid, batches until the SE is below target_se.
# With `L` (the Lord-Wingersky matrix) it also estimates the sufficiency gap
# I(theta; X | S) directly, as the mean of the per-draw difference
# [log p(x|theta) - log p(x)] - [log p(s|theta) - log p(s)]. Subtracting
# I(theta; S) from a Monte Carlo I(theta; X) would leave the gap with the full
# Monte Carlo error; the per-draw difference is much less variable, and under
# the PCM it is exactly zero on every draw (log p(x|s) does not involve theta).
poly_info_mc <- function(Ps, w, target_se = 0.005, batch = 20000, max_draws = 1e6, L = NULL) {
  Q <- nrow(Ps[[1]]); n <- length(Ps)
  lPt <- lapply(Ps, function(P) t(log(P)))
  cum <- lapply(Ps, function(P) t(apply(P, 1, cumsum)))
  if (!is.null(L)) { lL <- log(pmax(L, 1e-300)); lLm <- log(colSums(w * L)) }
  vals <- gaps <- numeric(0)
  repeat {
    q <- sample.int(Q, batch, replace = TRUE, prob = w)
    X <- vapply(cum, function(C) rowSums(runif(batch) > C[q, , drop = FALSE]), numeric(batch))
    X <- pmin(X, matrix(vapply(Ps, ncol, integer(1)) - 1, batch, n, byrow = TRUE))
    lpx <- pattern_logp(X, lPt)                                     # batch x Q
    lp_true <- lpx[cbind(seq_len(batch), q)]
    mx <- apply(lpx, 1, max)
    lp_marg <- mx + log(as.vector(exp(lpx - mx) %*% w))
    v <- (lp_true - lp_marg) / log(2)
    vals <- c(vals, v)
    if (!is.null(L)) {
      s1 <- rowSums(X) + 1
      gaps <- c(gaps, v - (lL[cbind(q, s1)] - lLm[s1]) / log(2))
    }
    se <- sd(vals) / sqrt(length(vals))
    if (se < target_se || length(vals) >= max_draws) break
  }
  out <- list(I = mean(vals), se = se, draws = length(vals))
  if (!is.null(L)) { out$gap <- mean(gaps); out$gap_se <- sd(gaps) / sqrt(length(gaps)) }
  out
}

MAX_ENUM_PATTERNS <- 2^20

# Returns I(theta; X) and the sufficiency gap I(theta; X | S): by subtraction
# when enumerated (exact), by the direct per-draw estimator under Monte Carlo.
poly_info_pattern <- function(Ps, w, target_se = 0.005) {
  K <- vapply(Ps, ncol, integer(1))
  L <- poly_lord_wingersky(Ps)
  I_S <- info_from_L(L, w)
  if (prod(K) <= MAX_ENUM_PATTERNS) {
    e <- poly_info_exact(Ps, w)
    list(I = e$I, se = 0, method = "enumeration", draws = NA, I_S = I_S, gap = e$I - I_S, gap_se = 0)
  } else {
    m <- poly_info_mc(Ps, w, target_se = target_se, L = L)
    list(I = m$I, se = m$se, method = "monte_carlo", draws = m$draws, I_S = I_S,
         gap = m$gap, gap_se = m$gap_se)
  }
}

# ------------------------------------------------------------------------------
# Collapsing categories. A merge of adjacent categories is a set of cut points
# (cut c sits between categories c-1 and c, c = 1..K-1); the original item is
# all K-1 cuts. Merging is a function of the response, so it can only lose
# information, and the loss is I(theta; X_j | collapsed X_j).
# ------------------------------------------------------------------------------

collapse_probs <- function(P, cuts) {
  K <- ncol(P)
  grp <- findInterval(0:(K - 1), sort(cuts))          # category -> collapsed level
  t(rowsum(t(P), grp))
}

# Every merge of adjacent categories for a K-category item: 2^(K-1) - 1 cut
# sets (the empty set, one category, carries nothing and is left out).
all_cut_sets <- function(K) {
  sets <- unlist(lapply(seq_len(K - 1), function(m) combn(K - 1, m, simplify = FALSE)),
                 recursive = FALSE)
  sets
}

item_collapses <- function(P, w) {
  K <- ncol(P)
  do.call(rbind, lapply(all_cut_sets(K), function(cs) {
    Pc <- collapse_probs(P, cs)
    data.frame(cuts = paste(cs, collapse = ","), k = length(cs) + 1,
               I = entropy(colSums(w * Pc)) - sum(w * row_entropy(Pc)))
  }))
}

# The cut that splits an item at its observed median category: below vs at or
# above the category holding the median, unless that is the lowest category,
# in which case the lowest category against the rest.
median_cut <- function(x, K) {
  m <- stats::median(x, na.rm = TRUE)
  m <- floor(m)                           # categories are 0..K-1
  max(1, min(K - 1, m + (m == 0)))
}

# ------------------------------------------------------------------------------
# Fisher information. For the GPCM, P(k | theta) proportional to
# exp(a k theta + d_k), so d/dtheta log P_k = a (k - E[X | theta]) and the
# item information is a^2 Var(X_j | theta). The dichotomous a^2 P Q is K = 2.
# ------------------------------------------------------------------------------

gpcm_fisher <- function(P, a) {
  k <- 0:(ncol(P) - 1)
  m <- as.vector(P %*% k)
  a^2 * (as.vector(P %*% k^2) - m^2)
}

# Numerical item information, -E[d^2 log P_k / dtheta^2], by central differences.
numeric_fisher <- function(item, theta, h = 1e-3) {
  lp <- function(t) log(probtrace(item, matrix(t)))
  P0 <- probtrace(item, matrix(theta))
  d2 <- (lp(theta + h) - 2 * lp(theta) + lp(theta - h)) / h^2
  -rowSums(P0 * d2)
}

# Marginal reliability on the standardised scale: test information on z is
# sd^2 times the information on theta at theta = sd * z.
poly_marginal_rel <- function(fit, z, w, sd = 1) {
  TI <- as.vector(testinfo(fit, matrix(sd * z))) * sd^2
  sum(w * TI / (TI + 1))
}

# ------------------------------------------------------------------------------
# Data preparation: the arm's single-administration rule, then reversal of the
# negatively keyed items, then 0-based categories on the table's own scale.
# ------------------------------------------------------------------------------

prepare_resp_poly <- function(df, reverse = character(0), max_n = 10000, seed = 1,
                              min_coverage = 0.5) {
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
  if (any(duplicated(paste(df$id, df$item)))) stop("repeated id-item rows")
  lo <- min(df$resp); hi <- max(df$resp)
  if (any(df$resp != round(df$resp))) stop("non-integer responses")
  stopifnot(all(reverse %in% df$item))
  rev_rows <- df$item %in% reverse
  df$resp[rev_rows] <- lo + hi - df$resp[rev_rows]
  df$resp <- as.integer(df$resp - lo)                 # 0..K-1
  info$scale <- c(lo, hi); info$reversed <- reverse
  ids <- unique(df$id)
  info$n_ids_rule <- length(ids)
  info$subsampled <- length(ids) > max_n
  if (info$subsampled) {
    set.seed(seed)
    df <- df[df$id %in% sample(ids, max_n), ]
  }
  n_ids_in <- length(unique(df$id))
  resp <- irw_long2resp(df)
  resp$id <- NULL
  info$n_ids_dropped_long2resp <- n_ids_in - nrow(resp)
  coverage <- colMeans(!is.na(resp))
  info$n_low_coverage <- sum(coverage < min_coverage)
  resp <- resp[, coverage >= min_coverage, drop = FALSE]
  info$n_ids_used <- nrow(resp)
  # K per item is the table's scale. mirt silently recodes an item that skips
  # a category, which would change K, so stop instead.
  K <- hi - lo + 1
  used <- vapply(resp, function(x) length(unique(na.omit(x))), integer(1))
  if (any(used < K)) stop("items with unused categories after subsampling: ",
                          paste(names(resp)[used < K], collapse = ","))
  list(resp = resp, K = setNames(rep(K, ncol(resp)), names(resp)), info = info)
}
