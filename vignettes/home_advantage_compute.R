#!/usr/bin/env Rscript
#
# home_advantage_compute.R
#
# How large is home advantage, how has it changed over a century and a half of
# competition, and did it shrink in the 2020-21 games played without crowds?
#
# Every comp table with a home side codes it the same way: homefield is "agent_a"
# when agent_a is at home and NA at a neutral venue. For each unit (a league-season,
# or a decade of internationals) we fit a Bradley-Terry model with a home term,
#
#   logit P(a beats b) = h * home + theta_a - theta_b,      home = 1, -1 or 0,
#
# as a binary logit, or as an ordinal (cumulative-logit) Bradley-Terry when the unit
# has draws, so that a draw is half-way between a win and a loss on the same latent
# scale. h is the home advantage in logits, net of team strength: a strong team that
# happens to play more home games does not inflate it.
#
# The no-crowd test fits one model per league over the 2016-2023 seasons, with a
# strength for every team-season and a second home term for games in the no-crowd
# window:
#
#   logit P(a beats b) = (h + delta * nocrowd) * home + theta_{a,s} - theta_{b,s}
#
# delta is the change in home advantage when the stands were empty.
#
# Output: home_advantage_data/home_advantage_results.rds   (read by the page)
#         home_advantage_data/references.bib
# Resume caches (gitignored): home_advantage_data/fits/
#
# Usage (from the site root):
#   nice -n 10 Rscript vignettes/home_advantage_compute.R
# Serial by design; the full run takes a few minutes, most of it the first fetch.

Sys.setenv(OMP_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)
suppressPackageStartupMessages({
  library(irw)
  library(data.table)
  library(MASS)
  library(bit64)  # some tables store date as integer64; as.numeric() on it is garbage without bit64
})

set.seed(20261002)

out_dir   <- "vignettes/home_advantage_data"
fits_dir  <- file.path(out_dir, "fits")
table_dir <- file.path(fits_dir, "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
bib_file  <- file.path(out_dir, "references.bib")

PILOT     <- FALSE  # TRUE: England, MLB, NBA and the 538 soccer leagues only
MIN_GAMES <- 50     # a unit with fewer games gives an h too noisy to plot
# No-crowd window. Leagues stopped in mid-March 2020; by June 2021 most had let
# crowds back in some form. One window for every league is an approximation (see
# the page's Limitations); MLB is also checked against its own attendance column.
NOCROWD_FROM <- as.Date("2020-03-13")
NOCROWD_TO   <- as.Date("2021-05-31")

logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

# ==============================================================================
# 1. Tables
# ==============================================================================
# Chosen from the 107 comp tables (October 2026): every table with a real home
# side and enough seasons to say something. Excluded, with the reason, in
# `excluded` below so the page can list them.
tables_long <- c(
  "retrosheet_mlb_1871_2025", "fivethirtyeight_nba_1946_2023",
  "fivethirtyeight_nhl_1917_2023", "fivethirtyeight_nfl_1920_2023",
  "nflverse_nfl_1999_2025", "footbayes_england", "footbayes_italy",
  "fivethirtyeight_soccer_2016_2023", "rugby_union_tier1_1871_2026",
  "intlfootball_men_1872-2026", "intlfootball_women_1956-2026"
)
excluded <- data.frame(
  table = c("mlb_through2023", "eufootball_2010-2020", "openfootball_* (9 tables)",
            "collegefb_2021and2022", "cricsheet_* (5 tables)",
            "lichess_2013_*, tcec_engines_2010_2026",
            "UEFA cups in fivethirtyeight_soccer_2016_2023"),
  reason = c("superseded by retrosheet_mlb_1871_2025 (same games, plus 2024-25 and attendance)",
             "same leagues and seasons as footbayes and the 538 soccer table",
             "the leagues duplicate the 538 soccer table; the club cups are too small per season",
             "two seasons, too short for a trend and no 2020",
             "the 'home' side is ambiguous (toss, venue, host board)",
             "white's first-move edge is not a home advantage",
             "group stages and two-legged ties, not leagues; many sides win or lose every game")
)
all_tables <- tables_long
if (PILOT) tables_long <- c("retrosheet_mlb_1871_2025", "fivethirtyeight_nba_1946_2023",
                            "footbayes_england", "fivethirtyeight_soccer_2016_2023")

fetch <- function(name) {
  f <- file.path(table_dir, paste0(name, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  logf("fetch", name)
  x <- as.data.table(irw_fetch(name, source = "comp"))
  saveRDS(x, f); x
}
as_date <- function(d) as.Date(as.POSIXct(as.numeric(d), origin = "1970-01-01", tz = "UTC"))

# One row per game: a, b, y (a's result: 1, 0.5 draw, 0), home (1 if a is at home,
# 0 at a neutral venue), date.
# (data.table's fifelse/fcase keep integer64 intact; as.numeric() via bit64 converts.)
games <- function(x) {
  data.table(a = as.character(x$agent_a), b = as.character(x$agent_b),
             y = fcase(x$winner == "agent_a", 1, x$winner == "agent_b", 0, x$winner == "draw", 0.5),
             home = as.numeric(!is.na(x$homefield) & x$homefield == "agent_a"),
             date = as_date(x$date))
}

# ==============================================================================
# 2. The model
# ==============================================================================
# Connected components of the schedule: divisions that never meet in a season, or
# the separate seasons of a pooled fit. Strengths are identified only within one,
# so one reference agent per component is dropped from the design.
components <- function(a, b) {
  ag <- sort(unique(c(a, b))); lab <- setNames(seq_along(ag), ag)
  repeat {
    old <- lab
    m <- pmin(lab[a], lab[b])
    for (i in seq_along(a)) { lab[a[i]] <- min(lab[a[i]], m[i]); lab[b[i]] <- min(lab[b[i]], m[i]) }
    lab <- lab[lab]; names(lab) <- ag
    if (identical(old, lab)) break
  }
  lab
}

# Fit h (and delta, when `nocrowd` is given). Agents may carry a season suffix, so
# that a pooled fit has one strength per team-season.
#
# In a league every game has a home side, so with a always the home team the home
# column would be constant and, in the ordinal fit, absorbed by the cutpoints. Every
# second game is therefore written from the away side's point of view (a and b
# swapped, y = 1 - y, home = -1). This changes nothing in the likelihood except
# that h now has its own column.
#
# `mods`, a matrix of 0/1 indicator columns, adds one home term per column (the NBA
# bubble fit uses two); their estimates come back in out$mods.
fit_h <- function(a, b, y, home, nocrowd = NULL, mods = NULL) {
  flip <- seq_along(a) %% 2 == 0
  a2 <- ifelse(flip, b, a); b2 <- ifelse(flip, a, b)
  y <- ifelse(flip, 1 - y, y); home <- ifelse(flip, -home, home)
  agents <- sort(unique(c(a2, b2)))
  cmp <- components(a2, b2)
  ref <- tapply(names(cmp), cmp, `[`, 1)            # one reference agent per component
  keep <- setdiff(agents, ref)
  X <- matrix(0, length(a2), length(keep))
  ia <- match(a2, keep); ib <- match(b2, keep)
  X[cbind(which(!is.na(ia)), ia[!is.na(ia)])] <- 1
  X[cbind(which(!is.na(ib)), ib[!is.na(ib)])] <- -1
  H <- cbind(home = home)
  if (!is.null(nocrowd)) H <- cbind(H, home_nocrowd = home * nocrowd)
  if (!is.null(mods)) H <- cbind(H, home * mods)
  Z <- cbind(H, X)
  draws <- any(y == 0.5)
  if (draws) {
    m <- polr(factor(y, levels = c(0, 0.5, 1)) ~ Z, Hess = TRUE, method = "logistic")
    cf <- coef(m); V <- solve(m$Hessian)[seq_along(cf), seq_along(cf)]
    names(cf) <- colnames(Z)
    method <- "ordinal"
    # cutpoints: P(loss) = plogis(cut1 - eta), P(win) = 1 - plogis(cut2 - eta), so for
    # two equal teams (eta = h) the draw rate is plogis(cut2 - h) - plogis(cut1 - h)
    cuts <- unname(m$zeta)
  } else {
    m <- glm(y ~ Z - 1, family = binomial)
    cf <- coef(m); V <- vcov(m); names(cf) <- colnames(Z)
    method <- "binary"
    cuts <- c(NA_real_, NA_real_)
  }
  se <- sqrt(diag(V))[seq_len(ncol(H))]
  out <- list(h = unname(cf["home"]), h_se = unname(se[1]), method = method,
              n_games = length(a), n_agents = length(agents), n_components = length(ref),
              draw_rate = mean(y == 0.5), home_share = mean(home != 0),
              cut1 = cuts[1], cut2 = cuts[2])
  if (!is.null(nocrowd)) {
    out$delta <- unname(cf["home_nocrowd"]); out$delta_se <- unname(se[2])
    out$n_nocrowd <- sum(nocrowd & home != 0)
  }
  if (!is.null(mods)) {
    j <- match(colnames(mods), colnames(H))
    out$mods <- data.frame(term = colnames(mods), delta = unname(cf[colnames(mods)]), delta_se = unname(se[j]),
                           n_games = colSums(mods[, , drop = FALSE] * (home != 0)))
  }
  out
}

# Drop agents with fewer than MIN_AGENT_GAMES games, repeatedly (dropping one can
# push another below the line). An agent seen once or twice -- a cup side knocked
# out in the first round, a team with one fixture in the window -- adds a strength
# the data cannot pin down and, in the pooled fits, makes the design singular.
# Agents with a perfect record (no points dropped, or none won) are dropped the same
# way: their strength is infinite, which makes the fit singular. This matters only
# for the internationals, where minnows lose every game, often by double figures.
MIN_AGENT_GAMES <- 5
trim <- function(d) {
  repeat {
    g <- table(c(d$a, d$b)); keep <- names(g)[g >= MIN_AGENT_GAMES]
    pts <- tapply(c(d$y, 1 - d$y), c(d$a, d$b), sum)
    keep <- intersect(keep, names(pts)[pts > 0 & pts < g[names(pts)]])
    d2 <- d[a %in% keep & b %in% keep]
    if (nrow(d2) == nrow(d)) return(d)
    d <- d2
  }
}

# ==============================================================================
# 3. Units
# ==============================================================================
# Each unit: sport, source table, period (a season, or a decade for internationals),
# and its games. Playoff games are left out: they are few, and home-field order in
# them is earned, not scheduled.
build_units <- function() {
  u <- list()
  add <- function(sport, table, period, d, league = sport) {
    d <- trim(d[!is.na(y) & a != b])
    if (nrow(d) < MIN_GAMES) return(invisible())
    u[[length(u) + 1]] <<- list(sport = sport, league = league, table = table, period = period, d = d)
  }
  per_season <- function(sport, table, x, league = sport) {
    for (s in sort(unique(x$season))) add(sport, table, s, games(x[season == s]), league)
  }
  have <- function(t) t %in% tables_long

  if (have("retrosheet_mlb_1871_2025")) {
    x <- fetch("retrosheet_mlb_1871_2025")
    # MLB "draws" are suspended and called games, not results
    x <- x[game_type == "regular" & winner != "draw"]
    per_season("Baseball (MLB)", "retrosheet_mlb_1871_2025", x)
  }
  if (have("fivethirtyeight_nba_1946_2023")) {
    x <- fetch("fivethirtyeight_nba_1946_2023")[is.na(playoff) | playoff == ""]
    per_season("Basketball (NBA)", "fivethirtyeight_nba_1946_2023", x)
  }
  if (have("fivethirtyeight_nhl_1917_2023")) {
    x <- fetch("fivethirtyeight_nhl_1917_2023")[playoff == 0]
    per_season("Ice hockey (NHL)", "fivethirtyeight_nhl_1917_2023", x)
  }
  # NFL: 538 to 1998, nflverse from 1999 (the two tables overlap from 1999)
  if (have("fivethirtyeight_nfl_1920_2023")) {
    x <- fetch("fivethirtyeight_nfl_1920_2023")[(is.na(playoff) | playoff == "") & season < 1999]
    per_season("American football (NFL)", "fivethirtyeight_nfl_1920_2023", x)
  }
  if (have("nflverse_nfl_1999_2025")) {
    x <- fetch("nflverse_nfl_1999_2025")[game_type == "REG"]
    per_season("American football (NFL)", "nflverse_nfl_1999_2025", x)
  }
  # England and Italy: all divisions of a season in one fit (the divisions are
  # separate components, each with its own reference team)
  for (lg in c("england", "italy")) {
    t <- paste0("footbayes_", lg)
    if (have(t)) per_season("Football (England and Italy)", t, fetch(t),
                            league = if (lg == "england") "England (all divisions)" else "Italy (Serie A)")
  }
  if (have("fivethirtyeight_soccer_2016_2023")) {
    # the three UEFA club cups are not leagues: group stages and two-legged ties,
    # where a side often wins every game it plays and its strength is not finite
    x <- fetch("fivethirtyeight_soccer_2016_2023")[!grepl("^UEFA", league)]
    for (lg in sort(unique(x$league))) per_season("Football (538 leagues)", "fivethirtyeight_soccer_2016_2023",
                                                   x[league == lg], league = lg)
  }
  # Internationals and rugby: too few games per year for a season fit, so decades.
  # Strengths are held fixed within a decade, which they are not; h is still net of
  # the average strength of whoever hosted.
  decades <- function(sport, table, league = sport) {
    if (!have(table)) return(invisible())
    g <- games(fetch(table)); g[, dec := floor(year(date) / 10) * 10]
    for (dc in sort(unique(g$dec))) add(sport, table, dc, g[dec == dc], league)
  }
  decades("Rugby union (tier-1 tests)", "rugby_union_tier1_1871_2026")
  decades("Football (internationals)", "intlfootball_men_1872-2026", "Men's internationals")
  decades("Football (internationals)", "intlfootball_women_1956-2026", "Women's internationals")
  u
}

# ==============================================================================
# 4. Fit, caching each unit
# ==============================================================================
safe_fit <- function(key, expr) {
  f <- file.path(fits_dir, paste0(gsub("[^A-Za-z0-9]+", "_", key), ".rds"))
  if (file.exists(f)) return(readRDS(f))
  r <- tryCatch(expr, error = function(e) { logf("  fit failed:", key, conditionMessage(e)); NULL })
  if (!is.null(r)) saveRDS(r, f)
  r
}

units <- build_units()
logf(length(units), "units")

by_season <- rbindlist(lapply(units, function(un) {
  key <- paste("season", un$league, un$table, un$period)
  r <- safe_fit(key, { d <- un$d; fit_h(d$a, d$b, d$y, d$home) })
  if (is.null(r)) return(NULL)
  data.table(sport = un$sport, league = un$league, table = un$table, period = un$period,
             first_date = min(un$d$date), as.data.table(r))
}), fill = TRUE)

# ==============================================================================
# 5. No-crowd test: one pooled fit per league, seasons 2016-2023
# ==============================================================================
in_window <- function(d) d >= NOCROWD_FROM & d <= NOCROWD_TO
# England and Italy come from the 538 table here (league by league), not footbayes,
# so no game is counted twice; rugby and internationals have too few no-crowd games.
nocrowd_units <- Filter(function(un) is.numeric(un$period) && un$period %in% 2016:2023 &&
                          !un$sport %in% c("Rugby union (tier-1 tests)", "Football (internationals)",
                                           "Football (England and Italy)"), units)
leagues <- unique(vapply(nocrowd_units, function(un) un$league, ""))

pooled <- function(lg, define = c("window", "attendance")) {
  define <- match.arg(define)
  us <- Filter(function(un) un$league == lg, nocrowd_units)
  d <- rbindlist(lapply(us, function(un) cbind(un$d, season = un$period)))
  if (define == "window") d[, nocrowd := as.numeric(in_window(date))]
  d[, `:=`(a = paste(a, season), b = paste(b, season))]
  trim(d)
}
mlb_attendance <- function() {
  # the same MLB seasons, with "no crowd" read off the attendance column: 2020's
  # regular season has attendance missing (no paying crowd) in 938 of 951 games
  x <- fetch("retrosheet_mlb_1871_2025")[game_type == "regular" & winner != "draw" & season %in% 2016:2023]
  d <- games(x); d[, season := x$season]
  d[, nocrowd := as.numeric(season == 2020 & is.na(x$attendance))]
  d[, `:=`(a = paste(a, season), b = paste(b, season))]
  trim(d)
}

MIN_NOCROWD <- 50   # home games in the window; fewer gives a delta with no information
nocrowd <- rbindlist(lapply(leagues, function(lg) {
  d <- pooled(lg)
  if (sum(d$nocrowd & d$home != 0) < MIN_NOCROWD) { logf("  skip (too few no-crowd games):", lg); return(NULL) }
  r <- safe_fit(paste("nocrowd", lg), fit_h(d$a, d$b, d$y, d$home, d$nocrowd))
  if (is.null(r) || is.na(r$delta)) return(NULL)
  sport <- nocrowd_units[[which(vapply(nocrowd_units, function(un) un$league == lg, TRUE))[1]]]$sport
  data.table(league = lg, sport = sport, definition = "date window", as.data.table(r))
}), fill = TRUE)

if ("retrosheet_mlb_1871_2025" %in% tables_long) {
  r <- safe_fit("nocrowd MLB attendance", { d <- mlb_attendance(); fit_h(d$a, d$b, d$y, d$home, d$nocrowd) })
  if (!is.null(r)) nocrowd <- rbind(nocrowd, data.table(league = "Baseball (MLB)", sport = "Baseball (MLB)",
                                                         definition = "attendance column", as.data.table(r)), fill = TRUE)
}

# ==============================================================================
# 5b. NBA: crowd or travel?
# ==============================================================================
# The 2020 restart was played in a bubble at Walt Disney World: no crowd and no
# travel. The 2020-21 season that followed had travel but (almost) no crowd. 538
# flags the bubble games as neutral (homefield NA), but agent_a is still the
# designated home team -- in the 2020 Finals it is the Lakers in games 1, 2 and 5 and
# the Heat in 3, 4 and 6, as scheduled -- so here agent_a is taken as home for them.
# Seasons 2016-2023 with playoffs, so that the 172 bubble games (88 seeding games,
# 84 playoff games) are compared with the same mix of games in other seasons; the
# 2021 playoffs, when crowds were partly back, are left out.
BUBBLE_FROM <- as.Date("2020-07-30"); BUBBLE_TO <- as.Date("2020-10-12")
nba_bubble <- NULL
if ("fivethirtyeight_nba_1946_2023" %in% tables_long) {
  x <- fetch("fivethirtyeight_nba_1946_2023")[season %in% 2016:2023]
  d <- games(x); d[, season := x$season]
  bub <- d$date >= BUBBLE_FROM & d$date <= BUBBLE_TO
  d[bub, home := 1]
  po <- !(is.na(x$playoff) | x$playoff == "")
  d[, `:=`(bubble = as.numeric(bub), season_2021 = as.numeric(season == 2021 & !po))]
  d <- d[!(season == 2021 & po)]
  d[, `:=`(a = paste(a, season), b = paste(b, season))]
  d <- trim(d)
  r <- safe_fit("nba bubble", fit_h(d$a, d$b, d$y, d$home, mods = as.matrix(d[, .(bubble, season_2021)])))
  if (!is.null(r)) nba_bubble <- list(h = r$h, h_se = r$h_se, terms = r$mods, n_games = r$n_games)
}

# ==============================================================================
# 6. Save
# ==============================================================================
logf(nrow(by_season), "unit fits;", nrow(nocrowd), "no-crowd fits")
saveRDS(
  list(
    summary          = as.data.frame(by_season),
    nocrowd          = as.data.frame(nocrowd),
    nba_bubble       = nba_bubble,
    excluded         = excluded,
    nocrowd_window   = c(NOCROWD_FROM, NOCROWD_TO),
    min_games        = MIN_GAMES,
    candidate_tables = tables_long,
    n_all_candidates = length(all_tables),
    pilot            = PILOT,
    date_run         = Sys.Date(),
    session          = sessionInfo()
  ),
  file = file.path(out_dir, "home_advantage_results.rds")
)
logf("saved", file.path(out_dir, "home_advantage_results.rds"))

tryCatch(
  irw_save_bibtex(unique(by_season$table), output_file = bib_file, source = "comp"),
  error = function(e) logf("bibtex generation failed:", conditionMessage(e))
)
# Two tables can cite one source (footbayes_england and footbayes_italy both cite
# the footBayes package); keep one entry per distinct body so the page's
# reference list does not repeat it.
if (file.exists(bib_file)) {
  bl <- readLines(bib_file)
  starts <- grep("^@\\w+\\{", bl)
  ends <- c(starts[-1] - 1, length(bl))
  bodies <- vapply(seq_along(starts), function(i) paste(sub("^@\\w+\\{[^,]*,", "", bl[starts[i]:ends[i]]), collapse = "\n"), "")
  drop <- unlist(lapply(which(duplicated(bodies)), function(i) starts[i]:ends[i]))
  if (length(drop)) { writeLines(bl[-drop], bib_file); logf(length(which(duplicated(bodies))), "duplicate bibtex entries dropped") }
}

# Methods and literature cited on the page (checked against Crossref, 2026-10-02)
manual_entries <- c(
  "@article{bradley1952rank,
  title   = {Rank Analysis of Incomplete Block Designs: {I}. The Method of Paired Comparisons},
  author  = {Bradley, Ralph Allan and Terry, Milton E.},
  journal = {Biometrika},
  volume  = {39},
  number  = {3/4},
  pages   = {324--345},
  year    = {1952},
  doi     = {10.2307/2334029}
}",
  "@article{agresti1992analysis,
  title   = {Analysis of Ordinal Paired Comparison Data},
  author  = {Agresti, Alan},
  journal = {Applied Statistics},
  volume  = {41},
  number  = {2},
  pages   = {287--297},
  year    = {1992},
  doi     = {10.2307/2347562}
}",
  "@article{pollard1986home,
  title   = {Home advantage in soccer: A retrospective analysis},
  author  = {Pollard, Richard},
  journal = {Journal of Sports Sciences},
  volume  = {4},
  number  = {3},
  pages   = {237--248},
  year    = {1986},
  doi     = {10.1080/02640418608732122}
}",
  "@article{pollard2005long,
  title   = {Long-term trends in home advantage in professional team sports in {N}orth {A}merica and {E}ngland (1876--2003)},
  author  = {Pollard, Richard and Pollard, Gerald},
  journal = {Journal of Sports Sciences},
  volume  = {23},
  number  = {4},
  pages   = {337--350},
  year    = {2005},
  doi     = {10.1080/02640410400021559}
}",
  "@article{jamieson2010home,
  title   = {The Home Field Advantage in Athletics: A Meta-Analysis},
  author  = {Jamieson, Jeremy P.},
  journal = {Journal of Applied Social Psychology},
  volume  = {40},
  number  = {7},
  pages   = {1819--1848},
  year    = {2010},
  doi     = {10.1111/j.1559-1816.2010.00641.x}
}",
  "@article{fischer2021crowd,
  title   = {Does Crowd Support Drive the Home Advantage in Professional Football? Evidence from {G}erman Ghost Games during the {COVID-19} Pandemic},
  author  = {Fischer, Kai and Haucap, Justus},
  journal = {Journal of Sports Economics},
  volume  = {22},
  number  = {8},
  pages   = {982--1008},
  year    = {2021},
  doi     = {10.1177/15270025211026552}
}",
  "@article{mccarrick2021home,
  title   = {Home advantage during the {COVID-19} pandemic: Analyses of {E}uropean football leagues},
  author  = {McCarrick, Dane and Bilalic, Merim and Neave, Nick and Wolfson, Sandy},
  journal = {Psychology of Sport and Exercise},
  volume  = {56},
  pages   = {102013},
  year    = {2021},
  doi     = {10.1016/j.psychsport.2021.102013}
}",
  "@article{higgs2021bayesian,
  title   = {Bayesian analysis of home advantage in {N}orth {A}merican professional sports before and during {COVID-19}},
  author  = {Higgs, Nico and Stavness, Ian},
  journal = {Scientific Reports},
  volume  = {11},
  pages   = {14521},
  year    = {2021},
  doi     = {10.1038/s41598-021-93533-w}
}"
)

entry_key <- function(entry) sub("^@\\w+\\{([^,]+),.*$", "\\1", trimws(entry))
existing_keys <- if (file.exists(bib_file)) {
  key_lines <- grep("^@\\w+\\{", readLines(bib_file), value = TRUE)
  vapply(key_lines, entry_key, character(1), USE.NAMES = FALSE)
} else character(0)
new_entries <- manual_entries[!vapply(manual_entries, entry_key, character(1)) %in% existing_keys]
if (length(new_entries) > 0) {
  cat(paste0(new_entries, "\n"), file = bib_file, append = TRUE, sep = "\n")
  logf(length(new_entries), "manual citation(s) appended to", bib_file)
}
