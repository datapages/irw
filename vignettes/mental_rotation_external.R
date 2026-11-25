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

info <- data.frame(
  table = c("external_mrtx", "external_hddm"),
  label = c("Ruginski et al. MRTX (6 field sites, ages 5-84)",
            "Athletes and non-athletes (OSF ev32p)"),
  stimuli = c("Body figures", "3D shapes"),
  sample = c("Mixed ages", "Adults"),
  format = c("Left/right", "Same/mirror"),
  source_url = c("https://osf.io/bmuq5", "https://osf.io/ev32p"),
  why_not_hosted = c("participant column holds names", "no licence set"))

ext_summary <- bind_rows(lapply(sources, `[[`, "summary")) %>% left_join(info, by = "table")

# Pooled slope with and without the external data (needs the IRW results
# from mental_rotation_compute.R)
irw_res <- readRDS("mental_rotation_data/mental_rotation_results.rds")
all_s <- bind_rows(irw_res$summary[, c("table", "slope_ms_deg", "slope_se", "acc_logit_per100", "acc_se")],
                   ext_summary[, c("table", "slope_ms_deg", "slope_se", "acc_logit_per100", "acc_se")])
meta_all <- metafor::rma(yi = slope_ms_deg, sei = slope_se, data = all_s, method = "REML")

saveRDS(list(
  summary = ext_summary, meta_all = meta_all,
  person = bind_rows(lapply(sources, `[[`, "person")),
  by_angle = bind_rows(lapply(sources, `[[`, "by_angle")),
  computed_at = Sys.time()),
  "mental_rotation_data/mental_rotation_external.rds")
print(bind_rows(lapply(sources, `[[`, "summary"))[, c("table", "n_people", "angles",
      "slope_ms_deg", "slope_se", "sd_person_slope_ms_deg", "acc_logit_per100")], digits = 3)
