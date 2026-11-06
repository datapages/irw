#!/usr/bin/env Rscript
#
# intransitivity_extras.R
#
# Two additions to intransitivity.qmd that need the raw tables. They are kept out
# of intransitivity_compute.R so that they run in minutes rather than days:
#   players  per player and Lichess month (all three time controls): pre-game
#            rating, games and points in the month's dense core, and how many of
#            the player's decided triads are cyclic, observed and on average under
#            the same transitive null as the triad statistics (the spinning-top
#            figure, and the chess row of the examples table)
#   bots     games and points of every bot in the dense core of the Kaggle
#            competition shown in the examples table
#   risks    each risk's share of all its comparisons won, for the two risk
#            criteria in the examples table, with Friedman's 2015 death counts
#
# Writes vignettes/intransitivity_data/extras.rds. Fetched tables are cached in
# vignettes/intransitivity_data/work/tables/ (gitignored), shared with the compute
# script. Usage (from the site root):
#   IRW_CORES=2 nice -n 19 Rscript vignettes/intransitivity_extras.R

Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
suppressPackageStartupMessages({
  library(irw)
  library(data.table)
})
source("vignettes/intransitivity_helpers.R")
CORES <- as.integer(Sys.getenv("IRW_CORES", "2"))
# irw_fetch() downloads through redivis, which forks parallelly::availableCores()
# workers (every core) by default; on a big table, forks of a several-GB R session
# exhaust memory. Cap them at CORES too.
options(parallelly.availableCores.custom = function() CORES)
B <- 200
out_dir <- "vignettes/intransitivity_data"
work <- file.path(out_dir, "work")
dir.create(file.path(work, "tables"), showWarnings = FALSE, recursive = TRUE)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
fetch <- function(name) {
  f <- file.path(work, "tables", paste0(name, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  # irw_fetch() reports a failed download and returns NULL rather than an error, so an
  # empty result is retried, then stops the run (never cached as the table)
  for (i in 1:3) {
    logf("fetch", name)
    x <- tryCatch(as.data.table(irw_fetch(name, source = "comp")), error = function(e) NULL)
    if (NROW(x)) break
    Sys.sleep(60 * i)
  }
  if (!NROW(x)) stop("fetch failed: ", name)
  saveRDS(x, f); x
}
yval <- function(w) ifelse(w == "agent_a", 1, ifelse(w == "agent_b", 0, ifelse(w == "draw", 0.5, NA)))
examples <- readRDS(file.path(out_dir, "results.rds"))$examples

# decided and cyclic triads through each agent, from head-to-head majorities
player_triads <- function(d, y, agents) {
  pm <- pair_mats(d, y, agents); P <- ifelse(pm$N > 0, pm$W / pm$N, NA)
  A <- (P > 0.5) * 1; A[is.na(A)] <- 0
  U <- (!is.na(P) & P != 0.5) * 1
  cbind(dec = diag(U %*% U %*% U) / 2, cyc = diag(A %*% A %*% A))
}
points <- function(g, agents) {
  pts <- tapply(c(g$y, 1 - g$y), c(g$a, g$b), sum)[agents]
  n <- table(c(g$a, g$b))[agents]
  list(games = as.integer(n), points = as.numeric(pts))
}

# ---- players: Lichess dense cores, as in intransitivity_compute.R -------------
players <- rbindlist(lapply(c("bullet", "blitz", "classical"), function(tc) {
  x <- fetch(paste0("lichess_2013_", tc))
  x$month <- format(as.POSIXct(x$date, origin = "1970-01-01", tz = "UTC") + 6 * 3600, "%m")
  r <- parallel::mclapply(sort(unique(x$month)), function(m) {
    y <- x[month == m & !is.na(white_elo) & !is.na(black_elo)]
    d <- data.frame(a = y$agent_a, b = y$agent_b, y = yval(y$winner), home = 1, date = y$date,
                    wa = as.numeric(y$white_elo), wb = as.numeric(y$black_elo))
    g <- core(d[!is.na(d$y) & d$a != d$b, ]); ag <- sort(unique(c(g$a, g$b)))
    # agent_a is White, so a's pre-game rating is white_elo
    rating <- tapply(c(g$wa, g$wb), c(g$a, g$b), median)[ag]
    obs <- player_triads(g, g$y, ag)
    gn <- g[, c("a", "b", "y", "home")]; f <- null_fit(gn, ag)
    set.seed(as.integer(m))
    sim <- Reduce(`+`, lapply(seq_len(B), function(i) player_triads(gn, simulate_y(gn, f), ag)))
    pt <- points(g, ag)
    data.table(unit = sprintf("lichess 2013-%s %s", m, tc), tc = tc, month = as.integer(m),
               player = ag, rating = as.numeric(rating), games = pt$games, points = pt$points,
               dec = obs[, "dec"], cyc = obs[, "cyc"], dec_null = sim[, "dec"] / B, cyc_null = sim[, "cyc"] / B)
  }, mc.cores = CORES, mc.preschedule = FALSE)
  rm(x); gc(); logf(tc, "done"); rbindlist(r)
}))

# ---- bots: the Kaggle competition the examples table shows ---------------------
# the same choice as the page: the most lopsided loop among the Kaggle examples
kx <- as.data.table(examples)[grepl("^kaggle ", unit)]
kx[, s := pmin(ab_w / ab_n, bc_w / bc_n, ca_w / ca_n)]
k_unit <- kx$unit[which.max(kx$s)]
kt <- fetch(sub("^kaggle ", "metakaggle_", k_unit))
d <- data.frame(a = as.character(kt$agent_a), b = as.character(kt$agent_b), y = yval(kt$winner),
                home = as.numeric(!is.na(kt$homefield) & kt$homefield == "agent_a"), date = as.numeric(kt$date))
g <- core(d[!is.na(d$y) & d$a != d$b, ]); ag <- sort(unique(c(g$a, g$b))); pt <- points(g, ag)
bots <- data.table(unit = k_unit, bot = ag, games = pt$games, points = pt$points)

# ---- risks: share of all comparisons won ---------------------------------------
# 2015 deaths for the risks in the examples, from Friedman's replication files
# (Risk_Code_Mortality_Risks.do, doi:10.7910/DVN/ZSJA25)
deaths <- c("Air pollution" = 200000, "Alcohol use" = 43138, "Asthma" = 3615, "Car accidents" = 19928,
            "Child abuse" = 1585, "Complications from pregnancy / childbirth" = 1140,
            "Contaminated drinking water" = 100, "Drunk driving" = 9967,
            "Post-traumatic stress disorder (PTSD)" = 1262, "Stomach diseases" = 6351, "Suicides" = 44145)
risks <- rbindlist(lapply(c(incidence = "friedman2019_risk_incidence", harm = "friedman2019_risk_harm"), function(nm) {
  x <- fetch(nm); y <- yval(x$winner)
  won <- tapply(c(y, 1 - y), c(x$agent_a, x$agent_b), mean)
  n <- table(c(x$agent_a, x$agent_b))
  data.table(table = nm, risk = names(won), share_won = as.numeric(won), comparisons = as.integer(n[names(won)]),
             deaths_2015 = unname(deaths[names(won)]))
}))

saveRDS(list(players = players, bots = bots, risks = risks, B = B, date_run = Sys.Date()),
        file.path(out_dir, "extras.rds"))
logf("saved", file.path(out_dir, "extras.rds"))
