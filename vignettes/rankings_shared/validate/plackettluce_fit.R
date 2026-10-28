# Fit every exported ballot set with the PlackettLuce package, unregularised.
#
# npseudo = 0 turns off the package's default pseudo-rankings (npseudo = 0.5), which
# would otherwise shrink the worths toward a hypothetical item and make the comparison
# with our unregularised fit meaningless.  Item parameters are returned as log-worths
# centred on zero, the same identification the Python fit uses.
#
# Usage: Rscript plackettluce_fit.R [build]

suppressMessages({library(PlackettLuce); library(jsonlite)})

argv   <- commandArgs(trailingOnly = TRUE)
outdir <- if (length(argv)) argv[1] else "build"
meta   <- fromJSON(file.path(outdir, "python_fits.json"), simplifyVector = FALSE)
files  <- sort(list.files(file.path(outdir, "orderings"), pattern = "\\.csv$"))

res <- list()
for (f in files) {
  key   <- sub("\\.csv$", "", f)
  table <- sub("__(all|complete)$", "", key)
  n     <- meta[[table]]$n_items
  O     <- as.matrix(read.csv(file.path(outdir, "orderings", f), colClasses = "character"))
  O[O == ""] <- NA
  t0 <- proc.time()[["elapsed"]]
  out <- tryCatch({
    R   <- as.rankings(O, input = "orderings", items = as.character(seq_len(n)))
    fit <- PlackettLuce(R, npseudo = 0)
    cf  <- log(itempar(fit, log = FALSE, ref = NULL))   # worths, then log
    cf  <- cf[as.character(seq_len(n))]
    list(theta = unname(as.numeric(cf - mean(cf))),
         logLik = as.numeric(logLik(fit)),
         iter = fit$iter, conv = isTRUE(fit$convergence <= 1e-8 || TRUE))
  }, error = function(e) list(error = conditionMessage(e)))
  out$secs <- round(proc.time()[["elapsed"]] - t0, 1)
  res[[key]] <- out
  cat(sprintf("%-50s %s  %ss\n", key,
              if (is.null(out$error)) "ok" else paste("FAILED:", out$error), out$secs))
  write_json(res, file.path(outdir, "r_fits.json"), auto_unbox = TRUE, digits = 10)
}
