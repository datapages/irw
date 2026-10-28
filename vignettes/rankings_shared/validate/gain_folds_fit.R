# Step 2 of 2: fit each CV fold's training ballots with PlackettLuce, npseudo = 0.
suppressMessages({library(PlackettLuce); library(jsonlite)})
outdir <- "build"
man <- read.csv(file.path(outdir, "folds", "manifest.csv"), colClasses = "character")
res <- list()
for (i in seq_len(nrow(man))) {
  key <- man$key[i]; n <- as.integer(man$n_items[i])
  O <- as.matrix(read.csv(file.path(outdir, "folds", paste0(key, ".csv")),
                          colClasses = "character"))
  O[O == ""] <- NA
  out <- tryCatch({
    R <- as.rankings(O, input = "orderings", items = as.character(seq_len(n)))
    fit <- PlackettLuce(R, npseudo = 0)
    cf <- log(itempar(fit, log = FALSE, ref = NULL))[as.character(seq_len(n))]
    list(theta = unname(as.numeric(cf - mean(cf))))
  }, error = function(e) list(error = conditionMessage(e)))
  res[[key]] <- out
  cat(sprintf("%-46s %s\n", key, if (is.null(out$error)) "ok" else out$error))
  write_json(res, file.path(outdir, "r_fold_fits.json"), auto_unbox = TRUE, digits = 10)
}
