# mental_rotation_helpers.R
# Shared model code for mental_rotation_compute.R (IRW tables) and
# mental_rotation_external.R (non-IRW data read from the original sites).
#
# fit_df(d, name): d has columns id, item, resp (0/1), rt (seconds) and
# angle (degrees, any range; folded here to angular disparity 0-180).
# RT: correct trials with 0.2 s <= rt <= 15 s, linear mixed model on RT in
# seconds with angle in units of 100 degrees:
#   rt ~ angle + (1 + angle | id) + (1 | item)
# The angle coefficient / 100 * 1000 is the slope in ms per degree.
# Accuracy: all answered trials, logistic GLMM with random intercepts for
# people and items (random slopes do not converge for several tables).
# Returns per-table summary, person-level slopes and observed medians by angle.

suppressPackageStartupMessages({ library(lme4); library(dplyr) })

fit_df <- function(d, name) {
  message("fitting ", name)
  d <- d[!is.na(d$resp) & !is.na(d$angle), ]
  d$angle <- d$angle %% 360
  d$angle <- ifelse(d$angle > 180, 360 - d$angle, d$angle)
  d$a100 <- d$angle / 100
  n_people <- length(unique(d$id))
  rtd <- d[d$resp == 1 & !is.na(d$rt) & d$rt >= 0.2 & d$rt <= 15, ]

  m_rt <- lmer(rt ~ a100 + (1 + a100 | id) + (1 | item), data = rtd, REML = TRUE,
               control = lmerControl(optimizer = "bobyqa"))
  co <- summary(m_rt)$coefficients
  vc <- as.data.frame(VarCorr(m_rt))
  sd_slope <- vc$sdcor[vc$grp == "id" & vc$var1 == "a100" & is.na(vc$var2)]
  r_int_slope <- vc$sdcor[vc$grp == "id" & vc$var1 == "(Intercept)" & !is.na(vc$var2) & vc$var2 == "a100"]

  m_acc <- glmer(resp ~ a100 + (1 | id) + (1 | item), data = d, family = binomial,
                 control = glmerControl(optimizer = "bobyqa"))
  ca <- summary(m_acc)$coefficients

  re <- ranef(m_rt)$id
  person <- data.frame(table = name, id = seq_len(nrow(re)),
                       intercept = co["(Intercept)", "Estimate"] + re[, "(Intercept)"],
                       slope_ms_deg = (co["a100", "Estimate"] + re[, "a100"]) * 10)

  by_angle <- rtd %>% group_by(angle) %>%
    summarise(median_rt = median(rt), mean_rt = mean(rt), n = n(), .groups = "drop") %>%
    left_join(d %>% group_by(angle) %>% summarise(accuracy = mean(resp), .groups = "drop"),
              by = "angle") %>%
    mutate(table = name)

  list(
    summary = data.frame(
      table = name, n_people = n_people, n_trials = nrow(d), n_rt_trials = nrow(rtd),
      angles = paste(sort(unique(d$angle)), collapse = "/"),
      intercept_s = co["(Intercept)", "Estimate"],
      slope_ms_deg = co["a100", "Estimate"] * 10, slope_se = co["a100", "Std. Error"] * 10,
      sd_person_slope_ms_deg = sd_slope * 10,
      r_intercept_slope = if (length(r_int_slope)) r_int_slope else NA,
      acc_logit_per100 = ca["a100", "Estimate"], acc_se = ca["a100", "Std. Error"],
      accuracy = mean(d$resp), singular_rt = isSingular(m_rt)),
    person = person, by_angle = by_angle)
}
