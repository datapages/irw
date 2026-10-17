# bits_per_item_sufficiency.R
#
# "Is the sum score enough?" block of the bits-per-item vignette. Sourced at
# the end of bits_per_item_compute.R, or run on its own:
#   Rscript vignettes/bits_per_item_sufficiency.R   # from project root
# It reads the showcase payloads in bits_per_item_results.rds (2PL parameters,
# replays) and refits only the Rasch model for the three showcase tables. It
# never rewrites bits_per_item_results.rds.
#
# Sufficiency in information terms: by the chain rule
#   I(theta; X) = I(theta; S) + I(theta; X | S),
# and S is sufficient exactly when I(theta; X | S) = 0 for every prior. The
# corpus "sum-score gap" (I_X - I_S in bits_per_item_results.rds$summary) is
# this conditional mutual information I(theta; X | S).
#
# Model scale: both models are put on theta ~ N(0, 1) for the base prior. The
# Rasch fit estimates a latent SD sigma with slopes fixed at 1; that is the
# same model as a common slope a = sigma on N(0, 1), which is how it is used
# here, so priors in the sweep mean the same thing for both models.
#
# Enumeration over all 2^n patterns on a theta grid is used for n <= 16; for a
# showcase with more items the 12 items with the highest 2PL expected bits are
# used (recorded as `subset`). The theta-free Rasch within-score surprisal needs
# no grid and always uses every item.
#
# Output: bits_per_item_data/sufficiency_results.rds

suppressMessages({
  library(irw)
  library(mirt)
  library(dplyr)
})
if (!exists("lord_wingersky")) source("vignettes/bits_per_item_helpers.R")

suff_file <- "vignettes/bits_per_item_data/sufficiency_results.rds"
main_file <- "vignettes/bits_per_item_data/bits_per_item_results.rds"
MAX_ENUM  <- 16
SUBSET_N  <- 12

if (file.exists(suff_file)) {
  message("Sufficiency results exist, skipping: ", suff_file)
} else {
  showcases <- readRDS(main_file)$showcases

  priors <- c(
    unlist(lapply(c(-1.5, -0.5, 0, 0.5, 1.5), function(m) lapply(c(0.5, 1, 2), function(sd) {
      list(label = sprintf("N(%g, %g²)", m, sd), mean = m, sd = sd, kind = "normal",
           dens = local({ m <- m; sd <- sd; function(x) dnorm(x, m, sd) }))
    })), recursive = FALSE),
    list(list(label = "bimodal: ½ N(−1.5, 0.5²) + ½ N(1.5, 0.5²)", mean = 0, sd = NA,
              kind = "mixture", dens = function(x) 0.5 * dnorm(x, -1.5, 0.5) + 0.5 * dnorm(x, 1.5, 0.5)))
  )
  base <- prior_grid(dnorm)

  one_showcase <- function(nm) {
    sc <- showcases[[nm]]
    set.seed(sum(utf8ToInt(nm)))
    prep <- prepare_resp(irw_fetch(nm))
    resp <- prep$resp
    stopifnot(identical(colnames(resp), sc$items$item))
    fit_r <- mirt(resp, 1, itemtype = "Rasch", verbose = FALSE)
    cr <- coef(fit_r, simplify = TRUE)
    sigma <- sqrt(cr$cov[1, 1])
    models <- list(
      Rasch = list(a = rep(sigma, nrow(sc$items)), d = unname(cr$items[, "d"])),
      `2PL` = list(a = sc$items$a, d = sc$items$d)
    )
    n <- nrow(sc$items)
    subset <- if (n > MAX_ENUM) order(-sc$items$bits)[1:SUBSET_N] else seq_len(n)

    # 1. Prior sweep
    sweep_df <- bind_rows(lapply(priors, function(pr) {
      g <- prior_grid(pr$dens)
      bind_rows(lapply(names(models), function(m) {
        e <- enumerate_info(models[[m]]$a[subset], models[[m]]$d[subset], g)
        data.frame(table = nm, model = m, prior = pr$label, prior_mean = pr$mean, prior_sd = pr$sd,
                   prior_kind = pr$kind, I_X = e$I_X, I_S = e$I_S, I_X_given_S = e$I_X_given_S)
      }))
    }))

    # 2-3. Within-score decomposition and compression ladder, base prior
    base_enum <- lapply(models, function(m) enumerate_info(m$a[subset], m$d[subset], base))
    by_s <- bind_rows(lapply(names(base_enum), function(m) cbind(table = nm, model = m, base_enum[[m]]$by_s)))
    ladder <- bind_rows(lapply(names(base_enum), function(m) {
      e <- base_enum[[m]]
      data.frame(table = nm, model = m, H_X = e$H_X, H_S = e$H_S, I_X = e$I_X, I_S = e$I_S,
                 I_X_given_S = e$I_X_given_S, H_X_given_theta = e$H_X_given_theta)
    }))

    # Weighted score check (2PL): T = sum a_j x_j
    a2 <- models$`2PL`$a[subset]
    wt <- info_statistic(a2, models$`2PL`$d[subset], base, function(X) as.vector(X %*% a2))

    # 4. Within-score surprisal under Rasch (theta-free, all items)
    ws <- rasch_within_score(models$Rasch$d)
    ws_hist <- ws |>
      group_by(s) |>
      mutate(bin = cut(surprisal, breaks = seq(floor(min(surprisal)), ceiling(max(surprisal)) + 0.25, by = 0.25),
                       include.lowest = TRUE)) |>
      group_by(s, bin) |>
      summarise(lo = min(surprisal), hi = max(surprisal), mass = sum(p_given_s), n_patterns = n(),
                .groups = "drop")
    resp_ws <- bind_rows(lapply(sc$replays, function(r) {
      x <- matrix(r$responses, nrow = 1)
      sv <- -rasch_log2_p_given_s(x, models$Rasch$d)
      data.frame(table = nm, respondent = r$respondent, kind = r$kind, sum_score = sum(x),
                 within_surprisal = sv, percentile = surprisal_percentile(ws, sum(x), sv),
                 n_patterns_at_score = choose(n, sum(x)),
                 n_patterns_stranger = sum(ws$s == sum(x) & ws$surprisal > sv + 1e-12))
    }))
    # Patterns sharing a score, for the "same score, different patterns" figure:
    # the two most common score levels, most probable patterns first.
    top_s <- by_s |> filter(model == "Rasch") |> arrange(desc(p_s)) |> slice(1:2) |> pull(s)
    Xall <- as.matrix(expand.grid(rep(list(0:1), n)))
    same_score <- bind_rows(lapply(top_s, function(k) {
      idx <- which(ws$s == k)
      o <- idx[order(-ws$p_given_s[idx])]
      data.frame(table = nm, s = k, rank = seq_along(o), p_given_s = ws$p_given_s[o],
                 surprisal = ws$surprisal[o], pattern = apply(Xall[o, , drop = FALSE], 1, paste, collapse = ""))
    }))

    # 5. Fisher information, full test vs sum score, both models, all items
    fisher <- bind_rows(lapply(names(models), function(m)
      cbind(table = nm, model = m, fisher_compare(models[[m]]$a, models[[m]]$d, make_grid(1)$nodes))))

    list(table = nm, n_items = n, subset = subset, subset_items = sc$items$label[subset],
         rasch_sigma = sigma, rasch_d = models$Rasch$d,
         sweep = sweep_df, by_s = by_s, ladder = ladder,
         weighted_score = data.frame(table = nm, I_T = wt$I, I_X = base_enum$`2PL`$I_X, n_levels = wt$n_levels,
                                     n_patterns = 2^length(subset)),
         within_score_hist = ws_hist, respondents = resp_ws, same_score = same_score,
         fisher = fisher)
  }

  res <- lapply(names(showcases), one_showcase)
  names(res) <- names(showcases)
  saveRDS(list(showcases = res,
               priors = lapply(priors, function(p) p[c("label", "mean", "sd", "kind")]),
               settings = list(MAX_ENUM = MAX_ENUM, SUBSET_N = SUBSET_N,
                               grid = "401 nodes on [-10, 10], weights from the prior density"),
               date_run = Sys.Date()),
          suff_file)
  message("Saved ", suff_file)
}

# Methods reference for the information-theoretic statement of sufficiency.
bib <- "vignettes/bits_per_item_data/references.bib"
if (file.exists(bib) && !any(grepl("cover2006elements", readLines(bib)))) {
  cat("\n@book{cover2006elements,\n  author = {Cover, Thomas M. and Thomas, Joy A.},\n",
      "  title = {Elements of Information Theory},\n  edition = {2},\n",
      "  publisher = {Wiley},\n  address = {Hoboken, NJ},\n  year = {2006}\n}\n",
      file = bib, append = TRUE, sep = "")
}
