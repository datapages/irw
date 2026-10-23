# bits_per_item_geometry_helpers.R
#
# Geometry helpers for the "Two rulers" section of the bits-per-item vignette:
# the Fisher-Rao metric on the one-dimensional IRT manifold, geodesic ability,
# and channel capacity. Sourced by bits_per_item_geometry.R and by the geometry
# checks. Nothing here touches the existing helpers' results.
#
# With item parameters fixed, a unidimensional IRT model is a one-dimensional
# statistical manifold with line element ds^2 = g(theta) dtheta^2, where g is
# the test information function. g itself changes when theta is rescaled, but
# sqrt(g) dtheta does not, so geodesic ability
#
#   A(theta) = integral_{-inf}^{theta} sqrt(g(t)) dt
#
# and differences in A are invariant to any smooth monotone reparametrisation
# of theta (Tuerlinckx & Robledo, 2026). Information quantities stay in bits
# (log base 2); geodesic quantities are dimensionless.
#
# Parametrisation note: the rest of this vignette stores mirt's intercept form,
# P(theta) = plogis(a * theta + d), so the item location is b = -d / a. The
# paper writes a * (theta - b). They are the same model.

# ------------------------------------------------------------------------------
# Integration range and grid for the geodesic integrals
#
# In the natural units u = a_j (theta - b_j) a single item's contribution to
# sqrt(g) decays like exp(-|u| / 2), and its whole geodesic length is pi
# whatever its slope: integral sqrt(P(1-P)) du = 2 arctan(e^(u/2)) -> pi. The
# length left outside |u| < U is about 4 exp(-U / 2), so U = 20 leaves 0.006%
# of an item's length outside and U = 10 leaves 0.9%.
#
# The point is that the range must be set in u, not in theta: a flat item
# (a = 0.1) needs theta out to b +/- 200 to reach the same truncation as a
# sharp one at b +/- 20. Padding in theta instead silently truncates flat
# tests, which is what the tail diagnostic caught in the pilot.
#
# A wide range and a fine grid cannot both be had cheaply, so the two jobs are
# split: totals (A(+inf), the Ramsay and sum-score lengths) come from adaptive
# quadrature over the full range, which needs no grid at all, while the curves
# A(theta), used for mapping people onto the geodesic scale and for plotting,
# come from the trapezoid rule on this grid. `grid_rel_err` records how far
# the grid's endpoint falls from the adaptive total.
# ------------------------------------------------------------------------------

geo_grid <- function(a, d, n_nodes = NULL, pad_u = 20, dx_target = 0.01,
                     max_nodes = 20001L) {
  b <- -d / a
  lo <- min(b - pad_u / a); hi <- max(b + pad_u / a)
  if (is.null(n_nodes)) {
    n_nodes <- min(max_nodes, max(2001L, ceiling((hi - lo) / dx_target) + 1L))
  }
  nodes <- seq(lo, hi, length.out = n_nodes)
  list(nodes = nodes, dx = nodes[2] - nodes[1], lo = lo, hi = hi,
       pad_u = pad_u, n_nodes = n_nodes)
}

# Adaptive quadrature of an integrand over the grid's full range.
geo_total <- function(f, grid, rel.tol = 1e-8) {
  integrate(f, grid$lo, grid$hi, subdivisions = 10000L, rel.tol = rel.tol)$value
}

# Test information g(theta) = sum_j a_j^2 P_j (1 - P_j).
test_info <- function(a, d, nodes) {
  P <- p_matrix(a, d, nodes)
  as.vector((P * (1 - P)) %*% (a^2))
}

# Cumulative trapezoid integral of f over an equally spaced grid, starting at 0.
cum_trapz <- function(f, dx) {
  c(0, cumsum((f[-1] + f[-length(f)]) / 2) * dx)
}

# Geodesic ability A(theta) on a geo_grid, and the total length A(+inf). The
# total is adaptive over the whole range; the curve is the trapezoid rule on
# the grid. `tail_share` bounds the length left outside the range and
# `grid_rel_err` is how far the curve's endpoint falls from the adaptive
# total; both are reported, never used to correct the result.
geodesic_ability <- function(a, d, grid = NULL) {
  if (is.null(grid)) grid <- geo_grid(a, d)
  g <- test_info(a, d, grid$nodes)
  A <- cum_trapz(sqrt(g), grid$dx)
  total <- geo_total(function(x) sqrt(test_info(a, d, x)), grid)
  # Length left outside the range: about 4 exp(-pad_u / 2) per item.
  tail_abs <- 4 * length(a) * exp(-grid$pad_u / 2)
  list(nodes = grid$nodes, g = g, A = A, A_total = total,
       A_total_grid = A[length(A)],
       grid_rel_err = abs(A[length(A)] - total) / total,
       tail_share = tail_abs / total, grid = grid)
}

# Ramsay (1996) arc length: the Euclidean cousin, which squares each item's
# term under the radical instead of using the Fisher metric.
ramsay_integrand <- function(a, d, nodes) {
  P <- p_matrix(a, d, nodes)
  sqrt(as.vector((P * (1 - P))^2 %*% (a^2)))
}

ramsay_length <- function(a, d, grid = NULL) {
  if (is.null(grid)) grid <- geo_grid(a, d)
  L <- cum_trapz(ramsay_integrand(a, d, grid$nodes), grid$dx)
  list(nodes = grid$nodes, L = L,
       L_total = geo_total(function(x) ramsay_integrand(a, d, x), grid))
}

# ------------------------------------------------------------------------------
# Lord-Wingersky with its theta derivative, for the sum score's Fisher
# information I_S(theta) = sum_s (d p_s / d theta)^2 / p_s. Used for the
# sum-score geodesic length (sufficiency as geometry).
# ------------------------------------------------------------------------------

lw_with_derivative <- function(a, d, nodes) {
  P <- p_matrix(a, d, nodes)
  Q <- nrow(P); n <- ncol(P)
  L <- matrix(0, Q, n + 1); L[, 1] <- 1
  dL <- matrix(0, Q, n + 1)
  for (j in seq_len(n)) {
    p <- P[, j]; dp <- a[j] * p * (1 - p)
    Lk <- L[, 1:j, drop = FALSE]; dLk <- dL[, 1:j, drop = FALSE]
    # new_s = L_s (1 - p) + L_{s-1} p
    Lnew <- cbind(Lk * (1 - p), 0) + cbind(0, Lk * p)
    dLnew <- cbind(dLk * (1 - p) - Lk * dp, 0) + cbind(0, dLk * p + Lk * dp)
    L[, 1:(j + 1)] <- Lnew
    dL[, 1:(j + 1)] <- dLnew
  }
  list(L = L, dL = dL)
}

sum_score_info <- function(a, d, nodes) {
  lw <- lw_with_derivative(a, d, nodes)
  keep <- lw$L > 1e-300
  num <- ifelse(keep, lw$dL^2 / pmax(lw$L, 1e-300), 0)
  rowSums(num)
}

# Geodesic length of the sum-score channel: integral of sqrt(I_S(theta)).
sum_score_geodesic <- function(a, d, grid = NULL) {
  if (is.null(grid)) grid <- geo_grid(a, d)
  I_S <- sum_score_info(a, d, grid$nodes)
  A <- cum_trapz(sqrt(I_S), grid$dx)
  list(nodes = grid$nodes, I_S = I_S, A = A,
       A_total = geo_total(function(x) sqrt(sum_score_info(a, d, x)), grid),
       A_total_grid = A[length(A)])
}

# ------------------------------------------------------------------------------
# Population quantities on the geodesic scale
#
# h(A) = h(theta) + E[log2 sqrt(g(theta))] (change of variables; the Jacobian
# of theta -> A is sqrt(g)). The Clarke-Barron asymptotic approximation is then
#   I(theta; X) ~= h(A) - 0.5 * log2(2 * pi * e),
# the log of the number of unit-error-wide slots the population spans.
# ------------------------------------------------------------------------------

HALF_LOG_2PIE <- 0.5 * log2(2 * pi * exp(1))

geodesic_population <- function(a, d, latent_sd, geo = NULL, pop = NULL) {
  if (is.null(geo)) geo <- geodesic_ability(a, d)
  if (is.null(pop)) pop <- make_grid(latent_sd)
  g_pop <- test_info(a, d, pop$nodes)
  h_theta <- 0.5 * log2(2 * pi * exp(1) * latent_sd^2)
  E_log_sqrt_g <- sum(pop$w * 0.5 * log2(g_pop))
  h_A <- h_theta + E_log_sqrt_g
  # Population SD on the geodesic scale: A interpolated at the population nodes.
  A_pop <- approx(geo$nodes, geo$A, xout = pop$nodes, rule = 2)$y
  mA <- sum(pop$w * A_pop)
  list(h_theta = h_theta, E_log_sqrt_g = E_log_sqrt_g, h_A = h_A,
       I_CB = h_A - HALF_LOG_2PIE,
       sd_A = sqrt(sum(pop$w * (A_pop - mA)^2)), mean_A = mA,
       A_pop = A_pop, A_total = geo$A_total,
       C_asymptotic = log2(geo$A_total / sqrt(2 * pi * exp(1))))
}

# ------------------------------------------------------------------------------
# Channel capacity by Blahut-Arimoto
#
# C = max_q I(q; W) over input distributions on a theta grid. The channel is
# given as a log-probability matrix logW with one row per output and one column
# per input (log P(output | theta)).
#
# Plain Blahut-Arimoto converges linearly and needs thousands of iterations to
# bracket C to 1e-4 bits, which is minutes per table once the output alphabet
# is 2^16 patterns. Two standard accelerations are used here:
#   * over-relaxation, q <- q * exp(accel * c), stepped back to accel = 1 if the
#     lower bound ever falls;
#   * support pruning, since the capacity-achieving input sits on a handful of
#     theta points. Pruned points are certified every `certify_every` sweeps on
#     the full input set, which is what makes the reported upper bound valid
#     (max_theta c(theta) must be a maximum over all inputs, not the active set),
#     and a point whose c exceeds the current lower bound is let back in.
#
# Returns both bounds in bits, the capacity-achieving input distribution, and
# the iteration count.
# ------------------------------------------------------------------------------

ba_capacity <- function(logW, tol_bits = 1e-4, max_it = 20000, accel = 2,
                        prune_tol = 1e-12, certify_every = 25L, chunk = 16384L) {
  n_in <- ncol(logW); n_out <- nrow(logW)
  W <- exp(logW)
  q <- rep(1 / n_in, n_in)
  active <- rep(TRUE, n_in)
  LOG2 <- log(2)

  # c(theta) = sum_x W(x | theta) log(W(x | theta) / p(x)), in nats.
  # p(x) = sum_theta q(theta) W(x | theta) only needs the columns where q > 0,
  # which is what makes the iteration cheap once the support has collapsed.
  c_of <- function(q, cols) {
    pos <- which(q > 0)
    qpos <- q[pos]
    cq <- numeric(length(cols))
    for (st in seq(1L, n_out, by = chunk)) {
      idx <- st:min(n_out, st + chunk - 1L)
      px <- as.vector(W[idx, pos, drop = FALSE] %*% qpos)
      lp <- log(px); lp[!is.finite(lp)] <- -745
      cq <- cq + colSums(W[idx, cols, drop = FALSE] * (logW[idx, cols, drop = FALSE] - lp))
    }
    cq
  }

  it <- 0L; step <- accel; lo_prev <- -Inf
  repeat {
    it <- it + 1L
    certify <- (it %% certify_every == 0L) || !any(!active)
    cols <- if (certify) seq_len(n_in) else which(active)
    cq <- c_of(q, cols)
    lo <- sum(q[cols] * cq)            # I(q; W) at the current input, in nats
    up <- max(cq)                      # valid upper bound only on a certify sweep
    if (certify) {
      active <- rep(FALSE, n_in)
      active[cols] <- cq >= lo         # let points back in that beat the current rate
      active[q > prune_tol] <- TRUE
      gap_bits <- (up - lo) / LOG2
      if (gap_bits < tol_bits || it >= max_it) break
    }
    if (lo < lo_prev - 1e-12 && step > 1) step <- 1   # over-relaxation overshot
    lo_prev <- lo
    qn <- q
    qn[cols] <- q[cols] * exp(step * (cq - max(cq)))
    qn[setdiff(seq_len(n_in), cols)] <- 0
    q <- qn / sum(qn)
    if (!any(q > 0)) stop("Blahut-Arimoto: input distribution collapsed")
  }
  list(C_lower = lo / LOG2, C_upper = up / LOG2, gap_bits = (up - lo) / LOG2,
       q = q, iterations = it, converged = (up - lo) / LOG2 < tol_bits,
       n_support = sum(q > 1e-6), n_in = n_in, n_out = n_out)
}

# Channel grids and channels -----------------------------------------------------

# 201 input points spanning the geodesic range, as specified.
capacity_grid <- function(a, d, n_nodes = 201, pad_u = 20) {
  b <- -d / a
  seq(min(b - pad_u / a), max(b + pad_u / a), length.out = n_nodes)
}

# log P(S = s | theta): (n + 1) outputs x inputs. Exact for any dichotomous
# model, and sufficient under Rasch.
log_channel_sum <- function(a, d, nodes) {
  L <- lord_wingersky(p_matrix(a, d, nodes))      # inputs x (n + 1)
  t(log(pmax(L, 1e-300)))
}

# log P(X = x | theta): 2^n outputs x inputs. Only for n <= 16.
log_channel_pattern <- function(a, d, nodes) {
  n <- length(a); stopifnot(n <= 16)
  P <- p_matrix(a, d, nodes)
  X <- as.matrix(expand.grid(rep(list(0:1), n)))
  X %*% t(log(P)) + (1 - X) %*% t(log(1 - P))
}
