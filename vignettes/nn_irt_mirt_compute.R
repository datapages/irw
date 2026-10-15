# Offline companion to nn_irt.qmd: fit R mirt models on the same train/test
# split the page uses and write held-out predictions to nn_irt_data/.
#
# Run from vignettes/ after the page has written nn_irt_data/<table>_split.csv.gz
# (the page writes them on its first render). Then re-render the page; it reads
# nn_irt_data/<table>_mirt_preds.csv.gz if present.
#
#   Rscript nn_irt_mirt_compute.R
#
# Models: 2PL (the marginal-likelihood anchor for the PyTorch 2PL), the
# monotonic-polynomial model (a flexible non-logistic item curve) and the
# logistic positive exponent model (LPE, an asymmetric item curve; custom mirt
# item from asymmetric_irt_helpers.R). Each is fit by MML on the training cells
# (held-out cells set to NA, which mirt treats as missing); held-out cells get
# posterior-predictive probabilities (the item curve integrated over each
# person's posterior given their training cells).

suppressPackageStartupMessages({
  library(mirt)
  library(data.table)
})
HERE <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
source(file.path(HERE, "asymmetric_irt_helpers.R"))
set.seed(20261015)

DATA <- "nn_irt_data"
tables <- sub("_split\\.csv\\.gz$", "", basename(list.files(DATA, "_split\\.csv\\.gz$")))

specs <- list(
  mirt_2pl      = list(itemtype = "2PL"),
  mirt_monopoly = list(itemtype = "monopoly"),
  mirt_lpe      = list(itemtype = "LPE")
)

fit_spec <- function(wide, nm, fit_2pl = NULL) {
  sp <- specs[[nm]]
  if (sp$itemtype == "LPE") {
    out <- fit_custom(as.data.frame(wide), "LPE", make_custom_item("LPE"),
                      ad_start = extract_ad(fit_2pl), shape_init = 0, prior_sd = 1,
                      em_cycles = 2000)
    if (out$error) stop(out$message)
    return(out$mod)
  }
  mirt(wide, model = 1, itemtype = sp$itemtype, verbose = FALSE,
       technical = list(NCYCLES = 1000))
}

# Posterior-predictive probability for each held-out cell: integrate the item
# curve over each person's posterior for theta (prior x likelihood of their
# training responses) on a quadrature grid. Plugging in a point estimate (EAP)
# instead is overconfident for the multidimensional models, where each
# person's extra dimensions are poorly determined from 20-100 responses.
predict_cells <- function(fit, wide, test_long, n_quad = 61) {
  Theta <- matrix(seq(-4, 4, length.out = n_quad), ncol = 1)
  P <- sapply(seq_len(ncol(wide)), function(j) probtrace(extract.item(fit, j), Theta)[, 2])
  P <- pmin(pmax(P, 1e-6), 1 - 1e-6)          # grid x items
  Y1 <- wide; Y1[is.na(Y1)] <- 0               # persons x items, NA -> 0
  Y0 <- 1 - wide; Y0[is.na(Y0)] <- 0
  LL <- Y1 %*% t(log(P)) + Y0 %*% t(log(1 - P))  # persons x grid
  prior <- -0.5 * rowSums(Theta^2)              # standard normal
  LL <- sweep(LL, 2, prior, "+")
  LL <- LL - apply(LL, 1, max)
  W <- exp(LL); W <- W / rowSums(W)
  pr <- W %*% P                                 # persons x items
  dimnames(pr) <- dimnames(wide)
  pr[cbind(match(test_long$id, rownames(wide)), match(test_long$item, colnames(wide)))]
}

for (tb in tables) {
  out <- file.path(DATA, paste0(tb, "_mirt_preds.csv.gz"))
  if (file.exists(out)) { cat(tb, "already done\n"); next }
  d <- fread(file.path(DATA, paste0(tb, "_split.csv.gz")))
  d[, id := as.character(id)][, item := as.character(item)]
  train <- d[test == FALSE]
  wide <- dcast(train, id ~ item, value.var = "resp")
  ids <- wide$id; wide <- as.matrix(wide[, -1]); rownames(wide) <- ids
  wide <- wide[order(rownames(wide)), order(colnames(wide))]
  test_long <- d[test == TRUE]
  res <- test_long[, .(id, item, resp)]
  fit_2pl <- NULL
  for (nm in names(specs)) {
    t0 <- Sys.time()
    fit <- tryCatch(fit_spec(wide, nm, fit_2pl),
      error = function(e) { message(tb, " ", nm, " failed: ", conditionMessage(e)); NULL })
    if (is.null(fit)) { res[[nm]] <- NA_real_; next }
    if (nm == "mirt_2pl") {
      fit_2pl <- fit
      co <- coef(fit, simplify = TRUE)$items      # slope-intercept: a1, d
      fwrite(data.table(item = rownames(co), a = co[, "a1"], d = co[, "d"]),
             file.path(DATA, paste0(tb, "_mirt_2pl_items.csv")))
    }
    res[[nm]] <- predict_cells(fit, wide, test_long)
    cat(sprintf("%-28s %-14s %6.1fs  converged=%s\n", tb, nm,
                as.numeric(difftime(Sys.time(), t0, units = "secs")),
                extract.mirt(fit, "converged")))
  }
  fwrite(res, out)
}
