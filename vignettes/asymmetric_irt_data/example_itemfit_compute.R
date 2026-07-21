# example_itemfit_compute.R
#
# Empirical-bin data for the single worked example in asymmetric_irt.qmd
# (colombia_2023_politics_attitudes, item_p5261s7): refits the RH model for
# that one table and extracts the same theta-binned observed-vs-expected
# proportions mirt::itemfit(..., empirical.plot = <item>) plots internally,
# via the return.tables = TRUE / fit_stats = "X2" path (Yen's 1981 Q1-style
# grouping, 10 EAP-theta bins by default) -- not a custom binning scheme.
#
# Output: asymmetric_irt_data/example_itemfit.rds
#
# Usage (from the project root; requires REDIVIS_API_TOKEN):
#   Rscript vignettes/asymmetric_irt_data/example_itemfit_compute.R

library(irw)
source("vignettes/asymmetric_irt_helpers.R")
library(dplyr)
library(purrr)
library(tibble)

set.seed(20260720)

EXAMPLE_TABLE <- "colombia_2023_politics_attitudes"
EXAMPLE_ITEM  <- "item_p5261s7"
MAX_N         <- 10000
EM_CYCLES     <- 2000

df <- irw_fetch(EXAMPLE_TABLE)
unique_ids <- unique(df$id)
if (length(unique_ids) > MAX_N) {
  df <- df[df$id %in% sample(unique_ids, MAX_N), ]
}
resp <- irw_long2resp(df)
resp$id <- NULL
resp <- resp[, sapply(resp, function(x) length(unique(na.omit(x))) > 1), drop = FALSE]

fit_2pl <- mirt(resp, 1, itemtype = "2PL", verbose = FALSE,
                technical = list(NCYCLES = EM_CYCLES, customTheta = Theta_mat_ref, customPriorFun = prior_GH50))
ad_2pl    <- extract_ad(fit_2pl)
ad_start  <- convert_ad_logit_to_probit(ad_2pl, D = 1.702)

out <- fit_custom(resp, "RH", make_custom_item("RH"), ad_start = ad_start,
                   shape_init = 0, prior_sd = 1, em_cycles = EM_CYCLES)
stopifnot(has_valid_mod(out), isTRUE(out$mod@OptimInfo$converged))
fit_rh <- out$mod

item_idx <- which(colnames(resp) == EXAMPLE_ITEM)
stopifnot(length(item_idx) == 1)

fit_tables <- itemfit(fit_rh, which.items = item_idx, fit_stats = "X2",
                       group.bins = 10, return.tables = TRUE)

bins_df <- imap_dfr(fit_tables, function(tab, theta_name) {
  n_tot <- sum(tab$Observed)
  tibble(
    theta    = as.numeric(theta_name),
    n        = n_tot,
    obs_prop = tab$Observed[2] / n_tot,
    exp_prop = tab$Expected[2] / n_tot
  )
})

saveRDS(
  list(table = EXAMPLE_TABLE, item = EXAMPLE_ITEM, bins = bins_df,
       date_run = Sys.Date(), session = sessionInfo()),
  file = "vignettes/asymmetric_irt_data/example_itemfit.rds"
)

message("Saved to vignettes/asymmetric_irt_data/example_itemfit.rds")
