#!/usr/bin/env Rscript
#
# intransitivity_compute.R
#
# Are competitions transitive? For every table in the IRW's competition collection,
# test whether there are more cycles (A beats B, B beats C, C beats A) than a
# transitive Bradley-Terry model produces by chance. Functions are in
# intransitivity_helpers.R.
#
# Stages (each resumes from its cache in vignettes/intransitivity_data/work/, which
# is gitignored):
#   1. units       build the analysis units from irw_fetch(source = "comp")
#   2. tests       rank-2 LR test (all units) + triad statistics (units with <= 150 agents)
#   3. calibration size of the LR test on sparse schedules and with a rating offset;
#                  size and power of the LR test and the triad statistics on
#                  simulated leagues
#   4. extras      CEMS per-student cycles; example cycles among Lichess players and
#                  Kaggle bots
#   5. assemble    vignettes/intransitivity_data/results.rds, the only file the page reads
#
# Usage (from the site root):
#   IRW_CORES=2 nice -n 19 Rscript vignettes/intransitivity_compute.R
# IRW_CORES sets the number of worker processes (default 2; more workers make the
# machine hard to use for hours). A full run from scratch is about two days on 2
# cores: the test stage is the long one (Lichess months and Kaggle competitions
# take 5-10 minutes each on 4 cores, the largest judgment tables about 25), and the
# units stage holds one table at a time in memory (the largest Kaggle table has 18
# million rows, a few GB).

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
out_dir <- "vignettes/intransitivity_data"
work <- file.path(out_dir, "work")
for (d in file.path(work, c("tables", "lr", "triads", "calib"))) dir.create(d, showWarnings = FALSE, recursive = TRUE)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")
key <- function(u) paste0(gsub("[^A-Za-z0-9]+", "_", u), ".rds")

# ============================================================================
# 1. Units
# ============================================================================
# Each table is fetched once and cached (as a data.table) under work/tables/.
fetch <- function(name) {
  f <- file.path(work, "tables", paste0(name, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  logf("fetch", name)
  x <- as.data.table(irw_fetch(name, source = "comp"))
  saveRDS(x, f); x
}
yval <- function(w) ifelse(w == "agent_a", 1, ifelse(w == "agent_b", 0, ifelse(w == "draw", 0.5, NA)))
season_of <- function(dt, start_month = 8) { d <- as.POSIXlt(dt, origin = "1970-01-01", tz = "UTC"); d$year + 1900 - (d$mon + 1 < start_month) }
year_of <- function(dt) as.POSIXlt(dt, origin = "1970-01-01", tz = "UTC")$year + 1900

# connected components (divisions that never meet within a season)
components <- function(a, b) {
  ag <- sort(unique(c(a, b))); lab <- setNames(seq_along(ag), ag)
  repeat { old <- lab
    m <- pmin(lab[a], lab[b]); for (i in seq_along(a)) { lab[a[i]] <- min(lab[a[i]], m[i]); lab[b[i]] <- min(lab[b[i]], m[i]) }
    lab <- lab[lab]; names(lab) <- ag
    if (identical(old, lab)) break }
  lab
}
# keep agents with >= min_games (iterated), then the largest connected component
trim <- function(d, min_games = 1) {
  for (i in 1:20) { g <- table(c(d$a, d$b)); k <- names(g)[g >= min_games]; d2 <- d[d$a %in% k & d$b %in% k, ]; if (nrow(d2) == nrow(d)) break; d <- d2 }
  if (!nrow(d)) return(d)
  cmp <- components(d$a, d$b); g <- cmp[d$a]; d[g == names(which.max(table(g))), ]
}

build_units <- function() {
  units <- list(); meta <- list()
  add <- function(name, d, family, type, min_games = 1, min_agents = 4) {
    d <- d[!is.na(d$y) & d$a != d$b, ]
    if (!nrow(d)) return(invisible())
    d <- trim(d, min_games)
    if (length(unique(c(d$a, d$b))) < min_agents) return(invisible())
    if (is.null(d$off)) d$off <- 0
    rownames(d) <- NULL
    units[[name]] <<- d[, intersect(c("a", "b", "y", "home", "off"), names(d))]
    meta[[name]] <<- data.frame(unit = name, family = family, type = type)
  }
  mk <- function(x, home = NULL) data.frame(a = as.character(x$agent_a), b = as.character(x$agent_b), y = yval(x$winner),
    home = if (is.null(home)) ifelse(!is.na(x$homefield) & x$homefield == "agent_a", 1, 0) else home)
  add_split <- function(prefix, x, family) {
    d <- mk(x); d <- d[!is.na(d$y), ]; if (!nrow(d)) return(invisible())
    cmp <- components(d$a, d$b); g <- cmp[d$a]; ks <- names(sort(table(g), decreasing = TRUE))
    for (j in seq_along(ks)) add(paste0(prefix, if (length(ks) > 1) paste0(" div", j) else ""), d[g == ks[j], ], family, "team")
  }
  # rows in a stable order (Redivis returns them in no particular order)
  ord <- function(x) x[order(x$date, x$agent_a, x$agent_b)]

  # ---- team sports: one unit per league-season ----
  mlb <- ord(fetch("mlb_through2023")); mlb <- mlb[winner != "draw"]   # MLB "draws" are suspended games
  for (s in sort(unique(mlb$season))) add_split(paste("mlb", s), mlb[season == s], "MLB")
  nba <- ord(fetch("fivethirtyeight_nba_1946_2023")); nba <- nba[is.na(playoff) | playoff == ""]
  for (s in sort(unique(nba$season))) add_split(paste("nba", s), nba[season == s], "NBA")
  nhl <- ord(fetch("fivethirtyeight_nhl_1917_2023")); nhl <- nhl[playoff == 0]
  for (s in sort(unique(nhl$season))) add_split(paste("nhl", s), nhl[season == s], "NHL")
  # NFL: 538 to 1998, nflverse from 1999 (the two overlap)
  nfl <- ord(fetch("fivethirtyeight_nfl_1920_2023")); nfl <- nfl[(is.na(playoff) | playoff == "") & season < 1999]
  for (s in sort(unique(nfl$season))) add_split(paste("nfl", s), nfl[season == s], "NFL")
  nv <- ord(fetch("nflverse_nfl_1999_2025")); nv <- nv[game_type == "REG"]
  for (s in sort(unique(nv$season))) add_split(paste("nfl", s), nv[season == s], "NFL")
  for (lg in c("england", "italy")) { f <- ord(fetch(paste0("footbayes_", lg)))
    for (s in sort(unique(f$season))) add_split(paste(lg, s), f[season == s], "Football: England and Italy") }
  # eufootball: Spain, Germany and France only (England and Italy are in footbayes)
  eu <- ord(fetch("eufootball_2010-2020")); eu$season <- season_of(eu$date, 7)
  engita <- c("Arsenal", "Chelsea", "Liverpool", "ManUnited", "Juventus", "Inter", "Milan", "Roma")
  for (s in sort(unique(eu$season))) { x <- eu[season == s]; d <- mk(x); cmp <- components(d$a, d$b); g <- cmp[d$a]
    for (k in unique(g)) { tm <- unique(c(d$a[g == k], d$b[g == k])); if (any(tm %in% engita)) next
      nm <- if ("Barcelona" %in% tm) "spain" else if ("Paris SG" %in% tm || "Marseille" %in% tm) "france" else if (any(grepl("Bayern|Dortmund", tm))) "germany" else paste0("eu", k)
      add(paste(nm, s), d[g == k, ], "Football: other leagues", "team") } }
  # 538 soccer, minus league-seasons covered by footbayes (to 2021) or eufootball (to 2019)
  sp <- ord(fetch("fivethirtyeight_soccer_2016_2023"))
  fb_cov <- c("Barclays Premier League", "English League Championship", "English League One", "English League Two", "Italy Serie A")
  eu_cov <- c("German Bundesliga", "Spanish Primera Division", "French Ligue 1")
  sp <- sp[!((league %in% fb_cov & season <= 2021) | (league %in% eu_cov & season <= 2019))]
  for (lg in sort(unique(sp$league))) for (s in sort(unique(sp[league == lg]$season)))
    add_split(paste(lg, s), sp[league == lg & season == s], "Football: other leagues")
  mls <- ord(fetch("openfootball_mls_2005_2025")); mls <- mls[!grepl("Play|Final|Conference|Round|Semi|Quarter|Knockout", stage)]
  for (s in sort(unique(mls$season))) add_split(paste("mls", s), mls[season == s], "Football: other leagues")
  for (cup in c("uefacl_2011_2026", "libertadores_2012_2026", "sudamericana_2012_2025", "concacafcl_2010_2025"))
    add(paste("cup", cup, "(all seasons)"), mk(ord(fetch(paste0("openfootball_", cup)))), "Football: club cups and internationals", "team", min_games = 5)
  for (sx in c("men_1872-2026", "women_1956-2026")) { x <- ord(fetch(paste0("intlfootball_", sx))); x$dec <- floor(year_of(x$date) / 10) * 10
    for (dc in sort(unique(x$dec))) add(paste0("intl ", sub("_.*", "", sx), " ", dc, "s"), mk(x[dec == dc]), "Football: club cups and internationals", "team", min_games = 5) }
  cf <- ord(fetch("collegefb_2021and2022")); cf$season <- season_of(cf$date, 7)
  for (s in sort(unique(cf$season))) add(paste("collegefb", s), mk(cf[season == s]), "US college football", "team", min_games = 5)
  for (f in c("domestic_men_2008_2026", "domestic_women_2015_2026")) { x <- ord(fetch(paste0("cricsheet_", f)))
    for (cp in sort(unique(x$competition))) for (s in sort(unique(x[competition == cp]$season)))
      add(paste("cricket", cp, s), mk(x[competition == cp & season == s]), "Cricket", "team") }
  for (f in c("intl_men_2001_2026", "intl_women_2003_2026")) { x <- ord(fetch(paste0("cricsheet_", f)))
    x$per <- floor((year_of(x$date) - 2001) / 5) * 5 + 2001
    for (mt in sort(unique(x$match_type))) for (p in sort(unique(x[match_type == mt]$per)))
      add(paste("cricket", sub("_.*", "", sub("intl_", "intl ", f)), mt, p), mk(x[match_type == mt & per == p]), "Cricket", "team", min_games = 5) }

  # ---- one-on-one: a dense core of the 80 most active agents (core() is in the helpers) ----
  # Lichess 2013: one unit per month x time control; the pre-game rating difference
  # (Glicko-2 on the Elo 400-point scale) is a fixed offset, so strengths are
  # residual strengths beyond the rating and a player improving during the month
  # cannot pass for a cycle
  for (tc in c("bullet", "blitz", "classical")) {
    x <- fetch(paste0("lichess_2013_", tc))
    x$month <- format(as.POSIXct(x$date, origin = "1970-01-01", tz = "UTC") + 6 * 3600, "%m")   # Dec 31 evening games are filed under January
    for (m in sort(unique(x$month))) { y <- x[month == m & !is.na(white_elo) & !is.na(black_elo)]
      d <- data.frame(a = y$agent_a, b = y$agent_b, y = yval(y$winner), home = 1, date = y$date,
                      off = (as.numeric(y$white_elo) - as.numeric(y$black_elo)) * log(10) / 400)
      add(sprintf("lichess 2013-%s %s", m, tc), core(d), sprintf("Lichess 2013 %s", tc), "1v1") }
    rm(x); gc() }
  tc <- fetch("tcec_engines_2010_2026")
  add("tcec engines (all seasons)", core(data.frame(a = tc$agent_a, b = tc$agent_b, y = yval(tc$winner), home = 1, date = tc$date)), "Engines and bots", "1v1")
  kag <- grep("^metakaggle_", irw_list_tables(source = "comp")$name, value = TRUE)
  for (nm in sort(kag)) { x <- fetch(nm)
    d <- data.frame(a = as.character(x$agent_a), b = as.character(x$agent_b), y = yval(x$winner),
                    home = as.numeric(!is.na(x$homefield) & x$homefield == "agent_a"), date = as.numeric(x$date)); rm(x); gc()
    add(sub("metakaggle_", "kaggle ", nm), core(d[!is.na(d$y), ]), "Engines and bots", "1v1")
    unlink(file.path(work, "tables", paste0(nm, ".rds"))) }   # the big ones are GB; the core is all we keep
  ufc <- fetch("ufc_1993_2026"); d <- mk(ufc, home = 0); d$date <- ufc$date
  add("ufc (all years)", core(d[!is.na(d$y), ], min_meet = 1), "UFC and padel", "1v1")
  pd <- fetch("costa_gine_2023_wpt_matches"); pd <- pd[!grepl("NULL", agent_a) & !grepl("NULL", agent_b)]
  for (ct in sort(unique(pd$cov_category))) { x <- pd[cov_category == ct]
    # this table's winner column holds "a"/"b"
    d <- data.frame(a = x$agent_a, b = x$agent_b, y = ifelse(x$winner == "a", 1, ifelse(x$winner == "b", 0, NA)), home = 0, date = x$date)
    add(paste("padel WPT", ct), core(d[!is.na(d$y), ], min_meet = 1), "UFC and padel", "1v1") }

  # ---- judgments: each table whole; order carries no information ----
  judg <- function(name) mk(fetch(name), home = 0)
  add("cems (6 schools, pooled)", judg("bradleyterry2_cems"), "Judgments", "judg")
  add("elochoice_physical", judg("elochoice_physical"), "Judgments", "judg")
  for (k in c("incidence", "harm", "longterm", "disaster", "worry", "fairness", "appropriate", "priority"))
    add(paste("friedman", k), judg(paste0("friedman2019_risk_", k)), "Judgments", "judg")
  for (k in c("immigration", "movie_reviews", "treebank", "movie50", "wiscads", "human_rights"))
    add(paste("carlson", k), judg(paste0("carlson_2017_", k)), "Judgments", "judg")
  add("zucco2019_portfoliosalience", judg("zucco2019_portfoliosalience"), "Judgments", "judg")
  add("guinaudeau2024_largechambers", judg("guinaudeau2024_largechambers"), "Judgments", "judg")

  meta <- do.call(rbind, meta)
  meta$agents <- sapply(units, function(d) length(unique(c(d$a, d$b))))
  meta$games <- sapply(units, nrow)
  meta$games_per_pair <- round(meta$games / sapply(units, function(d) length(unique(paste(pmin(d$a, d$b), pmax(d$a, d$b))))), 1)
  rownames(meta) <- NULL
  list(units = units, meta = meta)
}

f_units <- file.path(work, "units.rds")
if (!file.exists(f_units)) { logf("building units"); saveRDS(build_units(), f_units) }
U <- readRDS(f_units); units <- U$units; meta <- U$meta; rm(U)
logf(nrow(meta), "units,", sum(meta$games), "games")

# ============================================================================
# 2. Tests
# ============================================================================
# LR test on every unit (B = 50 bootstrap draws for team-seasons, 100 otherwise);
# triad statistics (B = 200) on units with at most 150 agents. Small units run in
# parallel across units, big ones one at a time with the bootstrap in parallel.
test_one <- function(u) {
  d <- units[[u]]
  f <- file.path(work, "lr", key(u))
  if (!file.exists(f)) {
    B <- if (meta$type[meta$unit == u] == "team") 50 else 100
    saveRDS(cbind(unit = u, B = B, lr_test(d, B = B, off = d$off)), f)
  }
  f <- file.path(work, "triads", key(u))
  if (!file.exists(f) && length(unique(c(d$a, d$b))) <= 150) {
    # an ordinal fit can be singular in a few sparse units; they get no triad statistics
    tri <- tryCatch(run_unit(u, d[, c("a", "b", "y", "home")]), error = function(e) data.frame(unit = u))
    saveRDS(tri, f)
  }
  invisible(NULL)
}
big <- meta$unit[meta$agents > 40 | meta$games > 3000]; small <- setdiff(meta$unit, big)
todo <- function(us) us[!file.exists(file.path(work, "lr", key(us)))]
options(lr_cores = 1)
invisible(parallel::mclapply(todo(small), function(u) tryCatch(test_one(u), error = function(e) logf(u, conditionMessage(e))),
                             mc.cores = CORES, mc.preschedule = FALSE))
options(lr_cores = CORES)
for (u in todo(big)) { logf("test", u); tryCatch(test_one(u), error = function(e) logf(u, conditionMessage(e))) }
for (u in big[!file.exists(file.path(work, "triads", key(big)))]) tryCatch(test_one(u), error = function(e) NULL)
options(lr_cores = 1)

# ============================================================================
# 3. Calibration
# ============================================================================
calib <- function(name, expr) {
  f <- file.path(work, "calib", paste0(name, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  logf("calibration", name); r <- expr; saveRDS(r, f); r
}
null_rep <- function(d, i) {
  # one true-null replicate on the schedule of d: transitive BT with the shrunk
  # strengths as the truth
  ag <- sort(unique(c(d$a, d$b))); f <- fit_model(d, ag)
  set.seed(100 + i); d$y <- simulate_y(d, f); d
}
# (a) sparse schedules: 40 cricket leagues with about one game per pair and 20 NFL
# seasons; each null replicate is tested twice, with the null simulated from the
# fitted strengths (p_fitted) and from the shrunk ones (lr_p)
size_sparse <- calib("size_sparse", {
  pick <- meta$unit[grepl("^cricket", meta$unit) & meta$agents >= 6 & meta$games_per_pair <= 1.6]
  set.seed(7); pick <- sample(pick, min(40, length(pick)))
  nfl <- meta$unit[grepl("^nfl", meta$unit) & meta$games_per_pair < 2]; set.seed(8); pick <- c(pick, sample(nfl, 20))
  rbindlist(parallel::mclapply(seq_along(pick), function(i) tryCatch({
    d <- null_rep(units[[pick[i]]][, c("a", "b", "y", "home")], i)
    m <- meta[meta$unit == pick[i], ]
    data.frame(unit = pick[i], agents = m$agents, games_per_pair = m$games_per_pair,
               p_fitted = lr_test(d, B = 50, shrink = FALSE)$lr_p, lr_test(d, B = 50)) }, error = function(e) NULL), mc.cores = CORES), fill = TRUE)
})
# (b) the 12 Lichess bullet months, same games and rating offsets
size_offset <- calib("size_offset", {
  pick <- grep("^lichess .* bullet$", meta$unit, value = TRUE)
  rbindlist(parallel::mclapply(seq_along(pick), function(i) tryCatch({
    d <- units[[pick[i]]]; ag <- sort(unique(c(d$a, d$b))); f <- fit_model(d, ag)
    set.seed(200 + i); eta <- d$off + f$h * d$home + f$theta_sim[d$a] - f$theta_sim[d$b]
    y <- as.numeric(runif(nrow(d)) < plogis(eta)); y[runif(nrow(d)) < mean(d$y == 0.5)] <- 0.5; d$y <- y
    data.frame(unit = pick[i], lr_test(d, B = 50, off = d$off)) }, error = function(e) NULL), mc.cores = CORES), fill = TRUE)
})
# (c) simulated leagues: 30 teams, 1,230 games (an NBA season), home advantage 0.4,
# strengths N(0, 0.7^2), plus a rank-2 cyclic term of scale c (u, v ~ N(0, 1)).
# Size at c = 0, power at c = 0.3 and 0.5, for the LR test and the triad statistics.
power <- calib("power", {
  one <- function(cc, seed) { set.seed(seed)
    n <- 30; th <- rnorm(n, 0, 0.7); u <- rnorm(n); v <- rnorm(n); ag <- sprintf("t%02d", 1:n)
    pr <- t(combn(n, 2)); idx <- sample(rep(seq_len(nrow(pr)), length.out = 1230)); flip <- runif(1230) < .5
    a <- ifelse(flip, pr[idx, 2], pr[idx, 1]); b <- ifelse(flip, pr[idx, 1], pr[idx, 2])
    eta <- 0.4 + th[a] - th[b] + cc * (u[a] * v[b] - u[b] * v[a])
    d <- data.frame(a = ag[a], b = ag[b], y = as.numeric(runif(1230) < plogis(eta)), home = 1)
    tri <- run_unit("sim", d)
    data.frame(c = cc, seed = seed, lr_p = lr_test(d, B = 50, seed = seed)$lr_p, wst_p = tri$wst_p, sst_p = tri$sst_p, curl_p = tri$curl_p)
  }
  grid <- expand.grid(seed = 1:40, c = c(0, 0.3, 0.5))
  rbindlist(parallel::mclapply(seq_len(nrow(grid)), function(i) one(grid$c[i], grid$seed[i]), mc.cores = CORES, mc.preschedule = FALSE))
})

# ============================================================================
# 4. Extras
# ============================================================================
# CEMS: each of 303 students compared all 15 pairs of 6 schools, so each student's
# own judgments form a complete tournament. Circular triads per student vs a null
# in which every student draws from one shared (pooled ordinal BT) preference.
cems <- calib("cems_students", {
  x <- fetch("bradleyterry2_cems"); x$y <- yval(x$winner)
  ag <- sort(unique(c(x$agent_a, x$agent_b)))
  per_rater <- function(dd) {
    A <- matrix(0, 6, 6); ia <- match(dd$agent_a, ag); ib <- match(dd$agent_b, ag)
    for (r in seq_len(nrow(dd))) if (!is.na(dd$y[r]) && dd$y[r] != .5) {
      w <- if (dd$y[r] == 1) ia[r] else ib[r]; l <- if (dd$y[r] == 1) ib[r] else ia[r]; A[w, l] <- 1 }
    sum(diag(A %*% A %*% A)) / 3
  }
  obs <- sapply(split(x, x$rater), per_rater)
  ok <- !is.na(x$y)
  f <- fit_model(data.frame(a = x$agent_a[ok], b = x$agent_b[ok], y = x$y[ok], home = 0), ag); f$theta_sim <- f$theta
  set.seed(303)
  sims <- replicate(100, { xx <- x; xx$y[ok] <- simulate_y(data.frame(a = x$agent_a[ok], b = x$agent_b[ok], home = 0), f)
                           sapply(split(xx, xx$rater), per_rater) })
  list(obs = obs, sims = sims, theta = f$theta)
})
# Example cycles, for illustration only (the tests do not rest on any one of them).
#  - Lichess bullet months and Kaggle competitions: the observed cycle A > B > C > A
#    whose three head-to-head records are most lopsided (the smallest of the three
#    winning shares is largest), among pairs with at least 5 games. Agents are
#    anonymised in the page.
#  - The two risk-judgment tables that reject: triads that the fitted rank-2 model
#    makes cyclic and that are also cyclic in the raw judgments (at least 3 per pair),
#    the 3 with the most lopsided model probabilities.
rec <- function(u, A, B, C, W, N, i, j, k, m = NA_real_)
  data.table(unit = u, A = A, B = B, C = C, ab_w = W[i, j], ab_n = N[i, j], bc_w = W[j, k], bc_n = N[j, k],
             ca_w = W[k, i], ca_n = N[k, i], model_min = m)
best_cycle <- function(u, d, min_n = 5) {
  ag <- sort(unique(c(d$a, d$b))); pm <- pair_mats(d, d$y, ag); W <- pm$W; N <- pm$N
  P <- ifelse(N >= min_n, W / N, NA)
  best <- NULL; sc <- -1
  for (i in seq_along(ag)) for (j in which(P[i, ] > .5)) for (k in which(P[j, ] > .5)) {
    if (is.na(P[k, i]) || P[k, i] <= .5) next
    s <- min(P[i, j], P[j, k], P[k, i]); if (s > sc) { sc <- s; best <- c(i, j, k) } }
  if (is.null(best)) return(NULL)
  rec(u, ag[best[1]], ag[best[2]], ag[best[3]], W, N, best[1], best[2], best[3])
}
model_cycles <- function(u, d, top = 3, min_n = 3) {
  ag <- sort(unique(c(d$a, d$b))); n <- length(ag); ia <- match(d$a, ag); ib <- match(d$b, ag)
  set.seed(1); f <- fit_ll(ia, ib, d$y, d$home, n, TRUE, starts = 5)
  th <- f$par[2:(n + 1)]; uu <- f$par[(n + 2):(2 * n + 1)]; vv <- f$par[(2 * n + 2):(3 * n + 1)]
  Pm <- plogis(outer(th, th, "-") + outer(uu, vv) - outer(vv, uu))
  pm <- pair_mats(d, d$y, ag); W <- pm$W; N <- pm$N
  out <- list()
  for (i in 1:n) for (j in which(Pm[i, ] > .5)) for (k in which(Pm[j, ] > .5)) if (Pm[k, i] > .5 && i < j && i < k) {
    if (min(N[i, j], N[j, k], N[k, i]) < min_n) next
    if (!(W[i, j] / N[i, j] > .5 && W[j, k] / N[j, k] > .5 && W[k, i] / N[k, i] > .5)) next
    out[[length(out) + 1]] <- rec(u, ag[i], ag[j], ag[k], W, N, i, j, k, min(Pm[i, j], Pm[j, k], Pm[k, i]))
  }
  r <- rbindlist(out); if (!nrow(r)) return(NULL)
  r[order(-model_min)][seq_len(min(top, nrow(r)))]
}
examples <- calib("examples", {
  ex <- list()
  for (u in c(grep("^lichess .* bullet$", meta$unit, value = TRUE), grep("^kaggle ", meta$unit, value = TRUE)))
    ex[[u]] <- best_cycle(u, units[[u]])
  for (u in c("friedman harm", "friedman incidence")) ex[[u]] <- model_cycles(u, units[[u]])
  rbindlist(ex)
})

# ============================================================================
# 5. Assemble
# ============================================================================
lr <- rbindlist(lapply(file.path(work, "lr", key(meta$unit)), function(f) if (file.exists(f)) readRDS(f)), fill = TRUE)
tri <- rbindlist(lapply(file.path(work, "triads", key(meta$unit)), function(f) if (file.exists(f)) readRDS(f)), fill = TRUE)
res <- merge(as.data.table(meta), lr, by = "unit", all.x = TRUE)
res <- merge(res, tri, by = "unit", all.x = TRUE)
tables <- sort(unique(irw_list_tables(source = "comp")$name))
saveRDS(list(units = res, size_sparse = size_sparse, size_offset = size_offset, power = power,
             cems = cems, examples = examples, tables = tables, date_run = Sys.Date()),
        file.path(out_dir, "results.rds"))
logf("wrote", file.path(out_dir, "results.rds"))
