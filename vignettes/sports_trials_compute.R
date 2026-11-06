#!/usr/bin/env Rscript
#
# sports_trials_compute.R
#
# Skill and selection in trial data where the respondent (or a coach, or an
# adaptive engine) picks the attempt: NBA shots, NFL field goals and extra
# points, NFL passes, soccer shots, and adaptive map practice. For each table we
# fit a ladder of Rasch-style glmer models, from person-only (M0) through an item
# effect (M1), continuous difficulty (M2) and the context of the attempt (M3),
# and record person SDs, rank changes, out-of-sample IMV, a selection diagnostic
# and split-half stability.
#
# The NBA data (DomSamangy/NBA_Shots_04_23, from NBA.com) carry no licence and
# are not in the IRW. They are read at compute time from a local copy or from
# GitHub; only aggregates are written to the cache (no shot rows, no player ids).
#
# Usage (from the site root): nice Rscript vignettes/sports_trials_compute.R
#        NBA_DIR=/path/to/NBA_Shots_04_24 uses a local copy of the NBA files;
#        otherwise the 2023-24 file is downloaded from GitHub.
# Writes vignettes/sports_trials_data/results.rds. About 40 minutes on one core.

suppressPackageStartupMessages({
  library(irw)
  library(lme4)
  library(splines)
})
options(digits = 7)
set.seed(20261001)
out_dir <- "vignettes/sports_trials_data"
dir.create(out_dir, showWarnings = FALSE)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

## ---- IMV (Domingue et al., 2024), as in the course's trials lesson ----------
coin <- function(ll) {
  if (ll <= log(0.5)) return(0.5)
  uniroot(function(w) w * log(w) + (1 - w) * log(1 - w) - ll,
          c(0.5, 1 - 1e-12), tol = 1e-12)$root
}
mean_ll <- function(y, p) {
  p <- pmin(pmax(p, 1e-9), 1 - 1e-9)
  mean(y * log(p) + (1 - y) * log(1 - p))
}
imv <- function(y, p0, p1) (coin(mean_ll(y, p1)) - coin(mean_ll(y, p0))) / coin(mean_ll(y, p0))

fit_fast <- function(f, d) glmer(f, d, binomial, nAGQ = 0,
                                 control = glmerControl(calc.derivs = FALSE))

## ---- helpers ------------------------------------------------------------------
z <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
# Spline basis built once on the full data, so predict() on held-out rows works.
add_ns <- function(d, var, df, prefix) {
  B <- ns(d[[var]], df = df)
  colnames(B) <- paste0(prefix, seq_len(df))
  cbind(d, B)
}
ns_terms <- function(prefix, df) paste(paste0(prefix, seq_len(df)), collapse = " + ")
keep_min <- function(d, n) {
  tab <- table(d$id)
  d[d$id %in% names(tab)[tab >= n], ]
}
person_re <- function(m) {
  r <- ranef(m)$id
  setNames(r[, 1], rownames(r))
}

# Out-of-sample: k-fold over attempts. Returns the IMV of each step over M0 and
# over the step before it, averaged over folds.
cv_ladder <- function(d, formulas, k = 5) {
  fold <- sample(rep(seq_len(k), length.out = nrow(d)))
  P <- sapply(formulas, function(f) {
    p <- numeric(nrow(d))
    for (i in seq_len(k)) {
      m <- fit_fast(f, d[fold != i, ])
      p[fold == i] <- predict(m, d[fold == i, ], type = "response", allow.new.levels = TRUE)
    }
    p
  })
  nm <- names(formulas)
  per_fold <- function(a, b) mean(sapply(seq_len(k), function(i)
    imv(d$resp[fold == i], P[fold == i, a], P[fold == i, b])))
  data.frame(model = nm[-1],
             imv_vs_m0 = sapply(nm[-1], function(b) per_fold(nm[1], b)),
             imv_vs_prev = sapply(seq_along(nm)[-1], function(j) per_fold(nm[j - 1], nm[j])),
             row.names = NULL)
}

# The whole analysis for one table. `formulas` is a named list M0..M3 (M3 may be
# absent); `diff_rhs` is the right-hand side of the attempt-difficulty model
# (no person term) used by the selection diagnostic.
analyse <- function(label, d, formulas, diff_rhs, split = c("order", "random")) {
  split <- match.arg(split)
  if (nzchar(Sys.getenv("SMOKE"))) d <- d[d$id %in% head(unique(d$id), 40), ]
  logf(label, ": ", nrow(d), " attempts, ", length(unique(d$id)), " persons")
  d$id <- factor(d$id)
  desc <- list(
    n_attempts = nrow(d), n_persons = nlevels(d$id),
    attempts_per_person = as.numeric(quantile(table(d$id), c(.1, .5, .9))),
    by_item = aggregate(cbind(n = 1, rate = resp) ~ item, d,
                        function(v) if (all(v == 1)) length(v) else mean(v)),
    rate = mean(d$resp)
  )
  desc$by_item$share <- desc$by_item$n / sum(desc$by_item$n)

  ## model ladder on the full data
  fits <- lapply(formulas, fit_fast, d = d)
  re <- lapply(fits, person_re)
  ids <- names(re[[1]])
  top10 <- function(r) names(sort(r[ids], decreasing = TRUE))[1:10]
  ladder <- data.frame(
    model = names(fits),
    person_sd = sapply(fits, function(m) attr(VarCorr(m)$id, "stddev")),
    spearman_m0 = sapply(re, function(r) cor(r[ids], re[[1]][ids], method = "spearman")),
    top10_kept = sapply(re, function(r) length(intersect(top10(r), top10(re[[1]])))),
    row.names = NULL)
  logf(label, ": ladder done")

  ## selection: does a person's skill (net of the attempt) go with the
  ## difficulty of the attempts they take?
  full <- names(fits)[length(fits)]
  # logit of the attempt's expected rate from a model with no person term
  df_ <- as.formula(paste("resp ~", diff_rhs))
  d$easiness <- if (grepl("|", diff_rhs, fixed = TRUE)) predict(fit_fast(df_, d), d)
                else predict(glm(df_, binomial, d), d)
  pe <- aggregate(cbind(easiness, resp) ~ id, d, mean)
  pe$skill <- re[[full]][as.character(pe$id)]
  pe$raw0 <- re[[1]][as.character(pe$id)]
  pe$decile <- cut(rank(pe$skill, ties.method = "first"), 10, labels = FALSE)
  selection <- list(
    cor_skill_easiness = cor(pe$skill, pe$easiness),
    cor_raw_easiness = cor(pe$raw0, pe$easiness),
    cor_skill_raw = cor(pe$skill, pe$raw0),
    by_decile = aggregate(cbind(easiness, rate = resp, skill) ~ decile, pe, mean)
  )
  logf(label, ": selection done")

  ## out-of-sample
  imv_tab <- cv_ladder(d, formulas)
  logf(label, ": cv done")

  ## split-half stability, M0 vs the full model
  if (split == "order") {
    d <- d[order(d$id, d$trial_number), ]
    d$half <- ave(seq_len(nrow(d)), d$id, FUN = function(v) seq_along(v) %% 2)
  } else {
    d$half <- sample(0:1, nrow(d), replace = TRUE)
  }
  sh <- sapply(c(names(formulas)[1], full), function(m) {
    a <- person_re(fit_fast(formulas[[m]], d[d$half == 0, ]))
    b <- person_re(fit_fast(formulas[[m]], d[d$half == 1, ]))
    common <- intersect(names(a), names(b))
    cor(a[common], b[common])
  })
  stability <- data.frame(model = names(sh), split_half_r = as.numeric(sh),
                          split = split, row.names = NULL)
  logf(label, ": stability done")

  list(label = label, formulas = sapply(formulas, function(f) paste(deparse(f), collapse = "")),
       desc = desc, ladder = ladder, selection = selection,
       imv = imv_tab, stability = stability)
}

results <- list()

## ---- NBA shots, 2023-24 (real, not in the IRW) --------------------------------
nba_dir <- Sys.getenv("NBA_DIR", "")
nba_file <- file.path(nba_dir, "NBA_2024_Shots.csv")
if (!file.exists(nba_file)) {
  zf <- tempfile(fileext = ".zip")
  download.file("https://github.com/DomSamangy/NBA_Shots_04_23/raw/main/NBA_2024_Shots.csv.zip",
                zf, mode = "wb")
  nba_file <- unzip(zf, exdir = tempdir())
}
x <- read.csv(nba_file)
x <- x[x$BASIC_ZONE != "Backcourt", ]
zone <- c("Restricted Area" = "restricted_area", "In The Paint (Non-RA)" = "paint",
          "Mid-Range" = "midrange", "Left Corner 3" = "corner_three",
          "Right Corner 3" = "corner_three", "Above the Break 3" = "above_break_three")
gd <- as.Date(x$GAME_DATE, "%m-%d-%Y")
clock <- 12 * (x$QUARTER - 1) + 12 - (x$MINS_LEFT + x$SECS_LEFT / 60)
nba <- data.frame(id = x$PLAYER_ID, item = unname(zone[x$BASIC_ZONE]),
                  resp = as.integer(x$EVENT_TYPE == "Made Shot"),
                  dist = x$SHOT_DISTANCE,
                  late = as.integer(x$MINS_LEFT < 2),
                  action = x$ACTION_TYPE,
                  ord = as.numeric(gd) * 1e3 + clock)
# HOME_TEAM / AWAY_TEAM are abbreviations and TEAM_ID is numeric: a team's
# abbreviation is the one that appears in all of its games.
team_abbr <- vapply(split(x, x$TEAM_ID), function(s) {
  tb <- table(c(s$HOME_TEAM, s$AWAY_TEAM)); names(tb)[which.max(tb)]
}, "")
nba$home <- as.integer(team_abbr[as.character(x$TEAM_ID)] == x$HOME_TEAM)
rm(x); invisible(gc())
nba <- keep_min(nba, 100)
rare <- names(which(table(nba$action) < 200))
nba$action[nba$action %in% rare] <- "other"
nba$trial_number <- ave(nba$ord, nba$id, FUN = rank)
nba <- add_ns(nba, "dist", 6, "d")
results$nba <- analyse("NBA shots 2023-24", nba, list(
  M0 = resp ~ 1 + (1 | id),
  M1 = resp ~ item + (1 | id),
  M2 = as.formula(paste("resp ~ item +", ns_terms("d", 6), "+ (1 | id)")),
  M3 = as.formula(paste("resp ~ item +", ns_terms("d", 6), "+ action + late + home + (1 | id)"))
), diff_rhs = paste("item +", ns_terms("d", 6)))
rm(nba); invisible(gc())

## ---- NFL field goals and extra points, 2015-2025 -------------------------------
k <- irw_fetch("nflverse_kicks")
results$kicks_rows_all <- nrow(k)
k <- k[k$trial_season >= 2015, ]
k$outdoor <- as.integer(k$trial_roof %in% c("outdoors", "open"))
k$wind0 <- ifelse(is.na(k$trial_wind) | k$outdoor == 0, 0, k$trial_wind)
k$wind_na <- as.integer(is.na(k$trial_wind) & k$outdoor == 1)
k$scorediff <- z(k$trial_scorediff)
k$playoff <- k$trial_playoff
k$home <- ifelse(is.na(k$trial_home), 0, k$trial_home)
k <- keep_min(k, 20)
k <- add_ns(k, "trial_dist", 4, "d")
results$kicks <- analyse("NFL kicks 2015-25", k, list(
  M0 = resp ~ 1 + (1 | id),
  M1 = resp ~ item + (1 | id),
  M2 = as.formula(paste("resp ~ item +", ns_terms("d", 4), "+ (1 | id)")),
  M3 = as.formula(paste("resp ~ item +", ns_terms("d", 4),
                        "+ outdoor + wind0 + wind_na + scorediff + playoff + home + (1 | id)"))
), diff_rhs = paste("item +", ns_terms("d", 4)))
rm(k); invisible(gc())

## ---- NFL passes, 2016-2025 -------------------------------------------------------
p <- irw_fetch("nflverse_passes")
results$passes_rows_all <- nrow(p)
p <- p[p$trial_season >= 2016, ]
p$air <- pmax(pmin(p$trial_airyards, 60), -10)
p$down <- factor(p$trial_down)
p$logtogo <- z(log1p(p$trial_togo))
p$yardline <- z(p$trial_yardline)
p$shotgun <- p$trial_shotgun
p$scorediff <- z(p$trial_scorediff)
p$home <- ifelse(is.na(p$trial_home), 0, p$trial_home)
p <- p[complete.cases(p[, c("down", "logtogo", "yardline", "shotgun", "scorediff")]), ]
p <- keep_min(p, 100)
p <- add_ns(p, "air", 5, "a")
results$passes <- analyse("NFL passes 2016-25", p, list(
  M0 = resp ~ 1 + (1 | id),
  M1 = resp ~ item + (1 | id),
  M2 = as.formula(paste("resp ~ item +", ns_terms("a", 5), "+ (1 | id)")),
  M3 = as.formula(paste("resp ~ item +", ns_terms("a", 5),
                        "+ down + logtogo + yardline + shotgun + scorediff + home + (1 | id)"))
), diff_rhs = paste("item +", ns_terms("a", 5)))
rm(p); invisible(gc())

## ---- Soccer shots (Wyscout, 2017-18 and two tournaments) ---------------------------
s <- irw_fetch("wyscout_shots")
results$shots_rows_all <- nrow(s)
s$counter <- s$trial_counter
s$late <- as.integer(s$trial_gameclock >= 75)
s$home <- ifelse(is.na(s$trial_home), 0, s$trial_home)
s$shootout <- s$trial_shootout
s <- keep_min(s, 20)
s <- add_ns(s, "trial_dist", 4, "d")
s <- add_ns(s, "trial_angle", 3, "g")
results$soccer <- analyse("Soccer shots", s, list(
  M0 = resp ~ 1 + (1 | id),
  M1 = resp ~ item + (1 | id),
  M2 = as.formula(paste("resp ~ item +", ns_terms("d", 4), "+", ns_terms("g", 3), "+ (1 | id)")),
  M3 = as.formula(paste("resp ~ item +", ns_terms("d", 4), "+", ns_terms("g", 3),
                        "+ counter + late + home + shootout + (1 | id)"))
), diff_rhs = paste("item +", ns_terms("d", 4), "+", ns_terms("g", 3)))
rm(s); invisible(gc())

## ---- Adaptive map practice (slepemapy.cz), a sample of 5,000 users ---------------
g <- irw_fetch("geography")
results$geo_rows_all <- nrow(g)
# item_complex is "<asked>__<other options, .-separated>__<type>": type 1 = find
# the named place on the map, 2 = name the highlighted place; no options = open.
parts <- strsplit(g$item_complex, "__", fixed = TRUE)
opts <- vapply(parts, `[`, "", 2)
g$type <- factor(vapply(parts, function(v) v[length(v)], ""))
n_other <- ifelse(is.na(opts) | opts == "", 0L, lengths(strsplit(opts, ".", fixed = TRUE)))
g$options <- factor(ifelse(n_other == 0, "open", as.character(n_other + 1)),
                    levels = c("2", "3", "4", "5", "6", "open"))
g <- g[!is.na(g$resp) & !is.na(g$options), c("id", "item", "resp", "type", "options")]
rm(parts, opts, n_other); invisible(gc())
g <- keep_min(g, 20)
users <- sample(unique(g$id), 5000)
g <- g[g$id %in% users, ]
invisible(gc())
results$geo <- analyse("Map practice", g, list(
  M0 = resp ~ 1 + (1 | id),
  M1 = resp ~ 1 + (1 | id) + (1 | item),
  M2 = resp ~ options + type + (1 | id) + (1 | item)
), diff_rhs = "options + type + (1 | item)", split = "random")
rm(g); invisible(gc())

if (nzchar(Sys.getenv("SMOKE"))) out_dir <- tempdir()
saveRDS(list(results = results, date_run = Sys.Date(), session = sessionInfo()),
        file.path(out_dir, "results.rds"))
logf("saved")
