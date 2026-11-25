# mental_rotation_compute.R
#
# Precomputes the results loaded by mental_rotation.qmd: how response time and
# accuracy depend on rotation angle across the chronometric tables in the IRW
# `mental_rotation` collection (datapages/irw#460).
#
# Output: mental_rotation_data/mental_rotation_results.rds
#
# Usage (from vignettes/):
#   R_PROFILE_USER=/dev/null Rscript mental_rotation_compute.R
# Before the tables are released, set MR_LOCAL_DIR to a folder of the IRW
# CSVs to read them from disk instead of irw_fetch().

suppressPackageStartupMessages({
  library(irw); library(lme4); library(metafor); library(dplyr)
})

out_dir <- "mental_rotation_data"
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Tables: chronometric mental rotation with rt and a recorded angle
# ------------------------------------------------------------------------------
# angle_col: where the rotation angle lives in each table. Angles are folded
# to angular disparity in 0-180 (e.g. 270 -> 90).
tables <- tribble(
  ~table,                        ~angle_col,       ~stimuli,           ~sample,        ~format,
  "bauer_2021_mr_pupil",         "itemcov_angle",  "3D cube figures",  "Adults", "Same/mirror",
  "ebert_2024_animal_mr",        "itemcov_angle",  "Animal pictures",  "Children 4-6", "Same/mirror",
  "ganis_kievit_2015_mr",        "itemcov_angle",  "3D shapes (new)",  "Adults", "Same/mirror",
  "liu_lourenco_2026_mr",        "itemcov_angle",  "3D shapes",        "Adults", "Same/mirror",
  "jost_2023_mr_visualization",  "itemcov_angle",  "3D cube figures",  "Adults", "Pick the match (2)",
  "jost_2022_mcmr_e1",           "trial_angle",    "3D cube figures",  "Adults", "Pick the match (2)",
  "jost_2022_mcmr_e2",           "trial_angle",    "3D cube figures",  "Adults", "Pick the match (2)",
  "dd_rotation",                 "diffIRT_doc",    "3D shapes",        "Adults", "Same/different"
)
# dd_rotation (diffIRT's `rotation` data; Borst, Kievit, Thompson & Kosslyn,
# 2011) carries no angle column; the diffIRT documentation (?rotation) gives
# each item's angle, joined here.
dd_angles <- c(`1` = 150, `2` = 50, `3` = 100, `4` = 150, `5` = 50,
               `6` = 100, `7` = 150, `8` = 50, `9` = 150, `10` = 100)

local_dir <- Sys.getenv("MR_LOCAL_DIR", "")
get_table <- function(name) {
  if (nzchar(local_dir)) return(read.csv(file.path(local_dir, paste0(name, ".csv"))))
  as.data.frame(irw_fetch(name))
}

# ------------------------------------------------------------------------------
# 2. Per-table models
# ------------------------------------------------------------------------------
# RT: correct trials with 0.2 s <= rt <= 15 s (a common, loose trim; the
# depositors' own outlier rules differ by study, so one rule is applied to
# all). Linear mixed model on RT in seconds with angle in units of 100 degrees:
#   rt ~ angle + (1 + angle | id) + (1 | item)
# The angle coefficient / 100 * 1000 is the slope in ms per degree.
# Accuracy: all answered trials, logistic GLMM with the same structure but a
# random intercept only for people (random slopes do not converge for several
# tables).
fit_one <- function(name, angle_col) {
  message("fitting ", name)
  d <- get_table(name)
  d$angle <- if (angle_col == "diffIRT_doc") unname(dd_angles[as.character(d$item)]) else d[[angle_col]]
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

  # Person-level slopes for plotting (BLUPs: fixed + random)
  re <- ranef(m_rt)$id
  person <- data.frame(table = name, id = rownames(re),
                       intercept = co["(Intercept)", "Estimate"] + re[, "(Intercept)"],
                       slope_ms_deg = (co["a100", "Estimate"] + re[, "a100"]) * 10)

  # Observed medians by angle for plotting
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

res <- Map(fit_one, tables$table, tables$angle_col)
summ <- bind_rows(lapply(res, `[[`, "summary")) %>% left_join(tables, by = "table")
person <- bind_rows(lapply(res, `[[`, "person"))
by_angle <- bind_rows(lapply(res, `[[`, "by_angle"))

# ------------------------------------------------------------------------------
# 3. Pooling the slopes
# ------------------------------------------------------------------------------
meta_rt <- rma(yi = slope_ms_deg, sei = slope_se, data = summ, method = "REML")
meta_acc <- rma(yi = acc_logit_per100, sei = acc_se, data = summ, method = "REML")
# Between-study SD (tau) against the typical within-study SD of person slopes
spread <- data.frame(
  tau_between_studies_ms_deg = sqrt(meta_rt$tau2),
  median_sd_between_people_ms_deg = median(summ$sd_person_slope_ms_deg))

saveRDS(list(summary = summ, person = person, by_angle = by_angle,
             meta_rt = meta_rt, meta_acc = meta_acc, spread = spread,
             computed_at = Sys.time(),
             source = if (nzchar(local_dir)) "local CSVs (pre-release)" else "irw_fetch"),
        file.path(out_dir, "mental_rotation_results.rds"))
print(summ[, c("table", "n_people", "angles", "intercept_s", "slope_ms_deg", "slope_se",
               "sd_person_slope_ms_deg", "r_intercept_slope", "acc_logit_per100", "singular_rt")],
      digits = 3)
print(meta_rt); print(spread)
