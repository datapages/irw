# guessing_gsweep_compute.R
#
# Sensitivity of the guessing corrections to the assumed guessing level.
#
# guessing_compute.R fixes g at 1/m for every model that takes a fixed g --
# the 1PLg, the Mixture, and Method A purification. That makes the analyst
# right by construction on every table on the page. It is also the one axis
# the Safety Valve paper's Studies 1 and 3 are built on: what happens when the
# assumed g is wrong. This script varies it.
#
# For each table, g_sweep() refits the 1PLg and the Mixture across a grid of
# assumed g values and scores both against a common Rasch baseline (the same
# 1PLg estimator at g = 0), on the same single 80/20 cell holdout the rest of
# the page uses. All three sit on the same estimated-variance prior, so the
# curves isolate the g effect.
#
# The expected asymmetry: the 1PLg pushes every person's floor to the assumed
# g whether or not they are guessing, so mis-specifying g mis-specifies every
# response. The Mixture only applies g to the share of response weight it
# assigns to the guessing class, and can shrink that share when g does not fit
# -- the safety valve. So the 1PLg should be far more sensitive.
#
# Output: vignettes/guessingdata/guessing_gsweep_results.rds
#
# Usage (needs REDIVIS_API_TOKEN):
#   Rscript vignettes/guessing_gsweep_compute.R   # from project root

options(guessing.testmode = TRUE)   # load guessing_compute.R for its helpers only
suppressMessages({
  library(mirt); library(dplyr); library(purrr); library(furrr); library(tibble)
})
source("vignettes/guessing_compute.R")   # prepare_table(), TABLES, QUAD, constants
source("vignettes/guessing_g_sweep.R")   # fit_1plg_fixedq(), g_sweep()
options(guessing.testmode = FALSE)

out_dir   <- "vignettes/guessingdata"
sweep_dir <- file.path(out_dir, "sweeps")
dir.create(sweep_dir, recursive = TRUE, showWarnings = FALSE)
G_GRID    <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50)

# Each table's sweep is cached to its own file and skipped if present, so a
# worker that dies partway (this run was OOM-killed once when sharing the
# machine with the main fit) can be resumed by re-running the script rather
# than starting over.

sweep_one <- function(i) {
  table_name <- TABLES$table[i]; m <- TABLES$m[i]
  out_file <- file.path(sweep_dir, paste0(table_name, ".rds"))
  if (file.exists(out_file)) {
    message("  Skipping (already done): ", table_name)
    return(readRDS(out_file))
  }
  message("  Sweeping: ", table_name)
  # An independent holdout draw at the same fraction as guessing_compute.R --
  # furrr gives each worker its own stream there, so this is not the identical
  # mask. Points on a sweep are therefore comparable to each other, but the
  # value at g = 1/m need not reproduce the main table's Mixture column exactly.
  set.seed(20260830 + i)

  # Reuse the subsampled matrix guessing_compute.R already wrote. Refetching
  # would pull ~45M long-format rows per ENEM table to keep 3,000 people (see
  # the memory note in guessing_compute.R), and would also sample a different
  # set of respondents than the main results table used.
  prep_file <- file.path(prep_dir, paste0(table_name, ".rds"))
  resp <- if (file.exists(prep_file)) {
    readRDS(prep_file)
  } else {
    tryCatch(prepare_table(table_name, m), error = function(e) {
      message("    fetch/prepare failed: ", conditionMessage(e)); NULL
    })
  }
  if (is.null(resp)) return(NULL)

  # Same zero-variance screen the main run applies, for the same reason.
  item_p <- colMeans(resp, na.rm = TRUE)
  degenerate <- is.na(item_p) | item_p %in% c(0, 1)
  if (any(degenerate)) resp <- resp[, !degenerate, drop = FALSE]
  if (ncol(resp) < 5) return(NULL)

  ho <- mask_holdout(resp, HOLDOUT_FRAC)
  res <- tryCatch(
    g_sweep(as.matrix(ho$train), ho$mask_idx, ho$true_vals,
            g_grid = G_GRID, quad = QUAD, verbose = FALSE),
    error = function(e) { message("    sweep failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(res)) return(NULL)
  res$table <- table_name
  res$m <- m
  res$g_nominal <- 1 / m        # the value the main results table assumes
  res <- as_tibble(res)
  saveRDS(res, out_file)
  res
}

if (!isTRUE(getOption("guessing.testmode"))) {
  # 2 workers, not 4: each holds a full response matrix plus 7 refits, and
  # the earlier 4-worker run exhausted memory alongside the main fit.
  plan(multisession, workers = 2)
  all_sweeps <- future_map(seq_len(nrow(TABLES)), sweep_one,
                           .options = furrr_options(seed = TRUE)) |>
    compact() |> bind_rows()
  plan(sequential)

  # Spread of IMV across the assumed-g grid, per table and model: how much the
  # verdict moves when the analyst's assumption about g moves.
  spread <- all_sweeps |>
    group_by(table) |>
    summarise(
      n_g = n(),
      range_1plg = diff(range(imv_1plg, na.rm = TRUE)),
      range_mix  = diff(range(imv_mix,  na.rm = TRUE)),
      imv_1plg_at_nominal = imv_1plg[which.min(abs(g_fit - first(g_nominal)))],
      imv_mix_at_nominal  = imv_mix[which.min(abs(g_fit - first(g_nominal)))],
      worst_1plg = min(imv_1plg, na.rm = TRUE),
      worst_mix  = min(imv_mix,  na.rm = TRUE),
      .groups = "drop"
    )

  print(as.data.frame(spread))

  saveRDS(
    list(sweeps = all_sweeps, spread = spread, g_grid = G_GRID,
         tables = TABLES, holdout_frac = HOLDOUT_FRAC,
         date_run = Sys.Date(), session = sessionInfo()),
    file = file.path(out_dir, "guessing_gsweep_results.rds")
  )
  message("Saved to ", out_dir, "/guessing_gsweep_results.rds")
}
