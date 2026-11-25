# mental_rotation_external.R
#
# Results from chronometric mental rotation data that the IRW does NOT host.
# Each file is downloaded at run time from the depositor's own repository into
# a temporary directory, the same model as for the IRW tables is fitted
# (fit_df() in mental_rotation_helpers.R), and only summary results are kept:
# the slope and accuracy estimates, medians and accuracy by angle, and
# shrunken person slopes with no identifiers. The downloaded files are deleted
# when the script ends; nothing from them is committed to this repository.
#
# Output: mental_rotation_data/mental_rotation_external.rds
#
# Usage (from vignettes/):
#   R_PROFILE_USER=/dev/null Rscript mental_rotation_external.R

source("mental_rotation_helpers.R")

tmp <- tempfile("mr_external_")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
fetch <- function(url, name) {
  f <- file.path(tmp, name)
  download.file(url, f, mode = "wb", quiet = TRUE)
  f
}

sources <- list()
age_bands <- list()
levante_sites <- list()

# Slope by age band: the same RT model fitted separately within each band
# (bands with fewer than 30 people are skipped). Only the band estimates are
# kept.
fit_bands <- function(d, label, breaks) {
  d <- d[which(!is.na(d$age) & d$age >= min(breaks) & d$age < max(breaks) &
               d$resp == 1 & !is.na(d$rt) & d$rt >= 0.2 & d$rt <= 15), ]
  a <- d$angle %% 360; d$a100 <- ifelse(a > 180, 360 - a, a) / 100
  d$band <- cut(d$age, breaks, right = FALSE)
  out <- list()
  for (b in levels(droplevels(d$band))) {
    s <- d[d$band == b, ]
    if (length(unique(s$id)) < 30) next
    m <- lme4::lmer(rt ~ a100 + (1 + a100 | id) + (1 | item), data = s,
                    control = lme4::lmerControl(optimizer = "bobyqa"))
    co <- summary(m)$coefficients
    out[[b]] <- data.frame(study = label, band = b, lo = as.numeric(sub("\\[([0-9.]+),.*", "\\1", b)),
                           n_people = length(unique(s$id)),
                           slope_ms_deg = co["a100", "Estimate"] * 10, slope_se = co["a100", "Std. Error"] * 10)
  }
  dplyr::bind_rows(out)
}

# ------------------------------------------------------------------------------
# 1. MRTX cross-cultural study (Ruginski, Barhorst-Cates et al.)
#    OSF https://osf.io/bmuq5, "Manuscript Data/Original submission/
#    crosscultural_MRT_longformclean.csv". CC BY 4.0 on OSF, but the
#    participant column holds names, which is why the IRW does not host it;
#    names are used here only to tell people apart in memory.
#    Body figures (image) at 0-300 degrees, left/right judgement; six field
#    sites; ages 5-84.
# ------------------------------------------------------------------------------
f <- fetch("https://osf.io/download/fcra7/", "mrtx.csv")
x <- read.csv(f, fileEncoding = "latin1", stringsAsFactors = FALSE)
x <- x[x$mr_angle %in% c(0, 60, 120, 180, 240, 300), ]   # drops 12 rows coded 6
sources$mrtx <- fit_df(data.frame(
  id = as.integer(factor(paste(x$fieldsite_id, x$participant_id))),
  item = paste(x$mr_image, x$mr_angle, x$mr_truth),
  resp = x$mr_acc, rt = x$mr_time, angle = x$mr_angle), "external_mrtx")
age_bands$mrtx <- fit_bands(data.frame(
  id = paste(x$fieldsite_id, x$participant_id), item = paste(x$mr_image, x$mr_angle, x$mr_truth),
  resp = x$mr_acc, rt = x$mr_time, angle = x$mr_angle, age = x$mr_age),
  "MRTX (body figures)", c(5, 9, 13, 18, 30, 50, 90))
rm(x)

# ------------------------------------------------------------------------------
# 2. Mental rotation in athletes and non-athletes ("Mental Rotation HDDM")
#    OSF https://osf.io/ev32p, Mental_Rotation_HDDM_Data.xlsx. No licence is
#    set on the project, so the IRW does not host it.
#    48 adults, angles 0/50/100/150, RT in ms.
# ------------------------------------------------------------------------------
f <- fetch("https://osf.io/download/c3e5q/", "hddm.xlsx")
h <- as.data.frame(readxl::read_excel(f))
sources$hddm <- fit_df(data.frame(
  id = h$Sbj_ID, item = paste(h$Angle), resp = h$testresp,
  rt = h$RT / 1000, angle = h$Angle), "external_hddm")
rm(h)

# ------------------------------------------------------------------------------
# 3. LEVANTE core task "mental-rotation" (children aged 5-12)
#    LEVANTE Data Repository, levante-data-pilots v4.0, doi:10.57761/fwqx-vx48,
#    on Redivis under the LEVANTE Data Use Agreement, which forbids
#    redistributing the data but allows publishing analyses that cite the
#    versioned release. Running this part needs a Redivis token
#    (REDIVIS_API_TOKEN) for an account approved under that agreement; without
#    one it is skipped. Only table-level estimates are kept for LEVANTE: no
#    person-level slopes are stored.
#    Items are animal silhouettes (2D: rabbit, duck) or 3D shapes; the item
#    name ends in the angular disparity (e.g. rabbit_160, shape_040). The task
#    was given in fixed and adaptive forms across task versions.
# ------------------------------------------------------------------------------
lev_ok <- nzchar(Sys.getenv("REDIVIS_API_TOKEN")) && requireNamespace("redivis", quietly = TRUE)
if (lev_ok) {
  lv <- tryCatch(redivis::redivis$query(paste(
    "SELECT t.dataset, t.user_id, t.item, t.item_original, t.item_group, t.correct, t.rt_numeric,",
    "t.timestamp, p.birth_year, p.birth_month",
    "FROM `levante.levante_data_pilots:68kn:v4_0.trials:ztnm` t",
    "LEFT JOIN `levante.levante_data_pilots:68kn:v4_0.participants:19x0` p USING (user_id)",
    "WHERE t.task_id = 'mental-rotation'"))$to_tibble(), error = function(e) {
      message("LEVANTE query failed (no access?): ", conditionMessage(e)); NULL })
  if (!is.null(lv)) {
    lv <- as.data.frame(lv)
    lv$angle <- as.numeric(sub(".*_([0-9]{3})$", "\\1", lv$item))
    # Age at the trial from LEVANTE's blurred birth month and year; values
    # outside 4-14 (some birth years are evidently wrong) are dropped for the
    # age bands only.
    bd <- as.Date(sprintf("%d-%02d-15", lv$birth_year, pmax(1, pmin(12, lv$birth_month))))
    lv$age <- as.numeric(as.Date(lv$timestamp) - bd) / 365.25
    for (g in c("2d", "3d")) {
      s <- lv[lv$item_group == g, ]
      r <- fit_df(data.frame(id = s$user_id, item = s$item_original,
                             resp = as.integer(s$correct), rt = s$rt_numeric / 1000,
                             angle = s$angle), paste0("external_levante_", g))
      r$person <- NULL      # LEVANTE: table-level estimates only
      sources[[paste0("levante_", g)]] <- r
      age_bands[[paste0("levante_", g)]] <- fit_bands(data.frame(
        id = s$user_id, item = s$item_original, resp = as.integer(s$correct),
        rt = s$rt_numeric / 1000, angle = s$angle, age = s$age),
        paste0("LEVANTE ", toupper(g), " (", ifelse(g == "2d", "animal silhouettes", "shapes"), ")"),
        c(4, 7, 9, 11, 14))
      # by pilot site (LEVANTE's own caveat: sites differ in sampling and
      # administration, so these are not estimates of country differences)
      for (site in sort(unique(s$dataset))) {
        ss <- s[s$dataset == site, ]
        r <- fit_df(data.frame(id = ss$user_id, item = ss$item_original,
                               resp = as.integer(ss$correct), rt = ss$rt_numeric / 1000,
                               angle = ss$angle), paste0("levante_", g, "_", site))
        levante_sites[[paste(g, site)]] <- cbind(r$summary, site = site, items = toupper(g))
      }
    }
    rm(lv, s)
  }
} else message("No Redivis token or redivis package: LEVANTE skipped")

info <- data.frame(
  table = c("external_mrtx", "external_hddm", "external_levante_2d", "external_levante_3d"),
  label = c("Ruginski et al. MRTX (6 field sites, ages 5-84)",
            "Athletes and non-athletes (OSF ev32p)",
            "LEVANTE mental rotation, 2D animals (ages 5-12)",
            "LEVANTE mental rotation, 3D shapes (ages 5-12)"),
  stimuli = c("Body figures", "3D shapes", "Animal silhouettes", "3D shapes"),
  sample = c("Mixed ages", "Adults", "Children 5-12", "Children 5-12"),
  format = c("Left/right", "Same/mirror", "Rotation judgement", "Rotation judgement"),
  source_url = c("https://osf.io/bmuq5", "https://osf.io/ev32p",
                 "https://doi.org/10.57761/fwqx-vx48", "https://doi.org/10.57761/fwqx-vx48"),
  why_not_hosted = c("participant column holds names", "no licence set",
                     "LEVANTE Data Use Agreement forbids redistribution",
                     "LEVANTE Data Use Agreement forbids redistribution"))

# Overall results per dataset (used for the curves and the dataset table)
overall <- bind_rows(lapply(sources, `[[`, "summary")) %>% left_join(info, by = "table")

# Rows for the forest plot and pooled estimate: MRTX and the athletes study
# as one row each; LEVANTE as one row per pilot site and item type (2D/3D),
# so each LEVANTE sample counts as its own study. The 2D and 3D rows of a
# site share children, so the pooled model is multilevel: rows nested in a
# cluster (the LEVANTE site; every other dataset is its own cluster).
site_names <- c(pilot_mpieva_de_main = "Leipzig (DE)", pilot_uniandes_co_bogota = "Bogotá (CO)",
                pilot_uniandes_co_rural = "Rural Colombia (CO)", pilot_western_ca_main = "London, Ontario (CA)")
lev_rows <- bind_rows(levante_sites)
if (nrow(lev_rows)) lev_rows <- lev_rows %>%
  mutate(label = paste0("LEVANTE ", items, ", ", site_names[site]), cluster = site,
         stimuli = ifelse(items == "2D", "Animal silhouettes", "3D shapes"),
         sample = "Children 4-13", format = "Rotation judgement",
         source_url = "https://doi.org/10.57761/fwqx-vx48",
         why_not_hosted = "LEVANTE Data Use Agreement forbids redistribution")
ext_summary <- bind_rows(
  overall %>% filter(!grepl("levante", table)) %>% mutate(cluster = table),
  lev_rows)

# Pooled slope with and without the external data (needs the IRW results
# from mental_rotation_compute.R)
irw_res <- readRDS("mental_rotation_data/mental_rotation_results.rds")
all_s <- bind_rows(irw_res$summary %>% mutate(cluster = table),
                   ext_summary)[, c("table", "cluster", "slope_ms_deg", "slope_se")]
meta_all <- metafor::rma.mv(yi = slope_ms_deg, V = slope_se^2, random = ~ 1 | cluster / table,
                            data = all_s, method = "REML")

saveRDS(list(
  summary = ext_summary, overall = overall, meta_all = meta_all, age_bands = bind_rows(age_bands),
  levante_sites = bind_rows(levante_sites),
  person = bind_rows(lapply(sources, `[[`, "person")),
  by_angle = bind_rows(lapply(sources, `[[`, "by_angle")),
  computed_at = Sys.time()),
  "mental_rotation_data/mental_rotation_external.rds")
print(bind_rows(lapply(sources, `[[`, "summary"))[, c("table", "n_people", "angles",
      "slope_ms_deg", "slope_se", "sd_person_slope_ms_deg", "acc_logit_per100")], digits = 3)
