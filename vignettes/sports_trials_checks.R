# Checks behind two numbers typed into sports_trials.qmd (not in the cache):
# the selection diagnostic on nbashots_sim, and player positions by skill decile
# for the soccer panel of Figure 1. Run from the site root.

## ---- nbashots_sim
suppressPackageStartupMessages({library(irw); library(lme4); library(splines)})
d <- irw_fetch("nbashots_sim", source = "sim")
d$id <- factor(d$id)
B <- ns(d$trial_dist, 6); colnames(B) <- paste0("d", 1:6); d <- cbind(d, B)
fit <- function(f) glmer(f, d, binomial, nAGQ = 0)
m0 <- fit(resp ~ 1 + (1 | id))
m3 <- fit(resp ~ item + d1 + d2 + d3 + d4 + d5 + d6 + trial_late + (1 | id))
d$easiness <- predict(glm(resp ~ item + d1 + d2 + d3 + d4 + d5 + d6, binomial, d), d)
pe <- aggregate(cbind(easiness, cov_true_theta) ~ id, d, mean)
pe$raw <- ranef(m0)$id[as.character(pe$id), 1]
pe$adj <- ranef(m3)$id[as.character(pe$id), 1]
round(c(raw_x_ease = cor(pe$raw, pe$easiness), adj_x_ease = cor(pe$adj, pe$easiness),
        true_x_ease = cor(pe$cov_true_theta, pe$easiness),
        raw_x_true = cor(pe$raw, pe$cov_true_theta), adj_x_true = cor(pe$adj, pe$cov_true_theta),
        sd_m0 = attr(VarCorr(m0)$id, "stddev"), sd_m3 = attr(VarCorr(m3)$id, "stddev")), 2)

## ---- soccer positions
suppressPackageStartupMessages({library(irw); library(lme4); library(splines)})
set.seed(20261001)
s <- irw_fetch("wyscout_shots")
s$counter <- s$trial_counter; s$late <- as.integer(s$trial_gameclock >= 75)
s$home <- ifelse(is.na(s$trial_home), 0, s$trial_home); s$shootout <- s$trial_shootout
tab <- table(s$id); s <- s[s$id %in% names(tab)[tab >= 20], ]
B <- ns(s$trial_dist, 4); colnames(B) <- paste0("d", 1:4)
G <- ns(s$trial_angle, 3); colnames(G) <- paste0("g", 1:3); s <- cbind(s, B, G)
s$id <- factor(s$id)
m3 <- glmer(resp ~ item + d1+d2+d3+d4 + g1+g2+g3 + counter + late + home + shootout + (1|id), s, binomial, nAGQ = 0)
s$easiness <- predict(glm(resp ~ item + d1+d2+d3+d4 + g1+g2+g3, binomial, s), s)
s$pen <- s$item == "penalty"; s$head <- s$item == "head"; s$fk <- s$item == "free_kick"
pe <- aggregate(cbind(easiness, resp, pen, head, fk, trial_dist, n = 1) ~ id, s, mean)
pe$n <- as.vector(table(s$id)[as.character(pe$id)])
pe$skill <- ranef(m3)$id[as.character(pe$id), 1]
pe$decile <- cut(rank(pe$skill, ties.method = "first"), 10, labels = FALSE)
print(round(aggregate(cbind(easiness, rate = resp, pen, head, fk, dist = trial_dist, n, skill) ~ decile, pe, mean), 3))
# easiness excluding penalties
s2 <- s[!s$pen, ]
pe2 <- aggregate(easiness ~ id, s2, mean); pe2$decile <- pe$decile[match(pe2$id, pe$id)]
print(round(aggregate(easiness ~ decile, pe2, mean), 3))
cat("cor(n, skill) =", round(cor(pe$n, pe$skill), 2), "\n")
pl <- jsonlite::fromJSON(path.expand("~/.cache/irw-sports/wyscout/players.json"))
pe$role <- pl$role$name[match(as.character(pe$id), as.character(pl$wyId))]
print(round(100 * prop.table(table(pe$decile, pe$role), 1)))
