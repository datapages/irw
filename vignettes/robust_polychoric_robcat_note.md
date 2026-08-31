# `robcat`: default initialization can converge to a badly suboptimal local optimum

**From:** Ben Domingue, Item Response Warehouse (IRW) — <https://itemresponsewarehouse.org/>
**Re:** Welz, Mair & Alfons (2026), *Robust estimation of polychoric correlation*, Psychometrika 91(1), 247–278
**Package:** `robcat` 0.2 (CRAN), R 4.6.1, Linux

## Summary

We ran `polycor()` / `polycor_mle()` across 1,694 item pairs drawn from 26 ordinal
tables in the IRW, comparing the robust C-estimator against ML. In a small number
of pairs the robust estimate came back near **−0.95** where ML gave **+0.65 to +0.80**.
These are not substantively plausible — in one case the two items are both HAQ
disability items, and the observed contingency table is overwhelmingly concentrated
in a single corner cell.

Restarting the optimizer from a grid of initial correlations shows these are
**local optima with much worse objective values**, not the estimate:

| pair | best rho found | its objective | competing rho | its objective |
|---|---|---|---|---|
| HAQ1b × HAQ8a | +0.7966 | 0.002450 | +0.7659 (default init) | 0.003968 |
| HAQ1b × HAQ7b | +0.6991 | 0.001360 | −0.9400 | 0.010108 |
| loneliness E × F | +0.6470 | 0.002184 | −0.9595 (default init) | 0.061749 |
| loneliness D × H | +0.6504 | 0.006519 | −0.9495 (default init) | 0.053666 |

Two observations we think matter beyond the sign flips themselves:

1. **The package's own default initialization is among the affected starts.** For
   `loneliness` E × F and D × H, the default lands on the −0.95 solution, whose
   objective is ~28× and ~8× worse respectively than the solution reached from most
   other starting values.

2. **Even when all starts converge to a positive rho, they disagree materially.**
   For HAQ1b × HAQ8a every start gave a positive estimate, but they range from
   0.7021 to 0.7966, with objectives from 0.008244 down to 0.002450. That spread of
   ~0.095 is attributable purely to initialization.

Point 2 is what prompted this note. We are using robust-vs-ML divergence as a
*diagnostic* — reading a gap as evidence that the single-bivariate-normal model
does not describe some observations, which is exactly what your
asymptotic-equivalence result licenses. But across our scan the median
|rho_robust − rho_ML| is **0.021**, which is smaller than the
initialization-induced spread in the example above. Without pinning down the
optimizer we cannot cleanly separate the signal we are after from starting-value
noise.

## Reproducible example

Self-contained: the fits depend only on the contingency tables, so no IRW or
Redivis access is needed.

```r
library(robcat)

# Rebuild a raw (x, y) pair from a contingency table
expand_table <- function(M) {
  idx <- which(M > 0, arr.ind = TRUE)
  n   <- M[idx]
  list(x = rep(idx[, 1], n), y = rep(idx[, 2], n))
}

# Fit from a grid of starting correlations, holding the default thresholds
multistart <- function(x, y, c = 0.6,
                       starts = c(-0.95, -0.9, -0.5, -0.2, 0, 0.2, 0.5, 0.9)) {
  init0 <- initialize_param(x, y)
  do.call(rbind, lapply(starts, function(s) {
    ini <- init0; ini[1] <- s
    f <- tryCatch(polycor(x, y, c = c, variance = FALSE, init = ini),
                  error = function(e) NULL)
    if (is.null(f)) NULL
    else data.frame(start = s, rho = unname(f$thetahat[1]), objective = f$objective)
  }))
}

# --- loneliness_mudfold, items E x F (n = 3987) -------------------------------
M <- matrix(c(2699,  205,  201,
               223,   64,   82,
               195,   52,  266),
            nrow = 3, byrow = TRUE)
d <- expand_table(M)

polycor_mle(d$x, d$y, variance = FALSE)$thetahat[1]      # ML:            0.6470
polycor(d$x, d$y, c = 0.6, variance = FALSE)$thetahat[1] # default init: -0.9595

multistart(d$x, d$y)
#>  start     rho   objective
#>  -0.95  0.6470  0.002184
#>  -0.90  0.6470  0.002184
#>  -0.50  0.6470  0.002184
#>  -0.20 -0.9964  0.061749
#>   0.00 -0.9595  0.061749   <- where the default initialization ends up
#>   0.20  0.6470  0.002184
#>   0.50  0.6470  0.002184
#>   0.90  0.6470  0.002184
```

## Questions

1. Is multi-start intended usage, or should `initialize_param()` be reliable
   enough that a single fit suffices? We did not find guidance on this in the
   paper or in `?polycor`.
2. If multi-start is the right answer, is taking the minimum `objective` across
   starts the correct selection rule, or does that interact badly with the
   robustness weighting?
3. Is there a diagnostic you would recommend for flagging a suspect fit
   automatically? Sign disagreement with ML is cheap to catch, but the quieter
   cases in point 2 do not announce themselves.
4. Are heavily concentrated tables like these — most mass in one corner cell,
   several near-empty cells — outside the regime the estimator is meant for? If
   so we would rather exclude them by a stated rule than fit them and discard.

## What we are doing with this

The IRW is a public warehouse of harmonized item-response datasets. We are
drafting a vignette scanning robust-vs-ML polychoric divergence across IRW's
ordinal tables, framed around your point that the two estimators agree
asymptotically under correct specification, so a gap is a diagnostic rather than
a disagreement about which estimator is right. We would not publish anything
without resolving the above, and we are happy to share the full scan or the
current draft.

## Details of the four cases

### `promis1wave1_haq` — items `HAQ1b` x `HAQ8a` (n = 1568)

Contingency table:

```
   y
x      1    2    3    4
  1 1358  123   12    8
  2   12   28   10    5
  3    0    2    3    1
  4    0    1    3    2
```

- ML estimate: **0.7999**
- Robust, package default init: **0.7659** (objective 0.003968)
- Robust, best over the starts below: **0.7966** (objective 0.002450)
- Value recorded in our batch run: **-0.9252**

| start rho | converged rho | objective |
|---|---|---|
| -0.95 | 0.7966 | 0.002450 |
| -0.90 | 0.7767 | 0.006374 |
| -0.50 | 0.7966 | 0.002450 |
| -0.20 | 0.7560 | 0.006201 |
| 0.00 | 0.7659 | 0.003968 |
| 0.20 | 0.7021 | 0.008244 |
| 0.50 | 0.7966 | 0.002450 |
| 0.90 | 0.7224 | 0.008176 |

### `promis1wave1_haq` — items `HAQ1b` x `HAQ7b` (n = 1569)

Contingency table:

```
   y
x      1    2    3    4
  1 1439   60    3    0
  2   31   22    2    0
  3    2    4    0    0
  4    1    2    1    2
```

- ML estimate: **0.7353**
- Robust, package default init: **0.6712** (objective 0.002814)
- Robust, best over the starts below: **0.6991** (objective 0.001360)
- Value recorded in our batch run: **-0.9456**

| start rho | converged rho | objective |
|---|---|---|
| -0.95 | 0.6991 | 0.001360 |
| -0.90 | 0.6991 | 0.001360 |
| -0.50 | 0.6991 | 0.001360 |
| -0.20 | 0.6980 | 0.001365 |
| 0.00 | 0.6712 | 0.002814 |
| 0.20 | -0.9400 | 0.010108 |
| 0.50 | 0.6991 | 0.001360 |
| 0.90 | 0.6712 | 0.002814 |

### `loneliness_mudfold` — items `E` x `F` (n = 3987)

Contingency table:

```
   y
x      1    2    3
  1 2699  205  201
  2  223   64   82
  3  195   52  266
```

- ML estimate: **0.6470**
- Robust, package default init: **-0.9595** (objective 0.061749)
- Robust, best over the starts below: **0.6470** (objective 0.002184)
- Value recorded in our batch run: **-0.9595**

| start rho | converged rho | objective |
|---|---|---|
| -0.95 | 0.6470 | 0.002184 |
| -0.90 | 0.6470 | 0.002184 |
| -0.50 | 0.6470 | 0.002184 |
| -0.20 | -0.9964 | 0.061749 |
| 0.00 | -0.9595 | 0.061749 |
| 0.20 | 0.6470 | 0.002184 |
| 0.50 | 0.6470 | 0.002184 |
| 0.90 | 0.6470 | 0.002184 |

### `loneliness_mudfold` — items `D` x `H` (n = 3987)

Contingency table:

```
   y
x      1    2    3
  1  198   44  128
  2   58  109  172
  3  173  324 2781
```

- ML estimate: **0.6504**
- Robust, package default init: **-0.9495** (objective 0.053666)
- Robust, best over the starts below: **0.6504** (objective 0.006519)
- Value recorded in our batch run: **-0.9495**

| start rho | converged rho | objective |
|---|---|---|
| -0.95 | 0.6504 | 0.006519 |
| -0.90 | 0.6504 | 0.006519 |
| -0.50 | 0.6504 | 0.006519 |
| -0.20 | 0.6504 | 0.006519 |
| 0.00 | -0.9495 | 0.053666 |
| 0.20 | 0.6504 | 0.006519 |
| 0.50 | 0.6504 | 0.006519 |
| 0.90 | 0.6504 | 0.006519 |

