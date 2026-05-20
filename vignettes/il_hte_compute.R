# il_hte_compute.R
#
# Fits item-level HTE models across IRW RCT datasets (gilbert_meta_* tables).
# Produces two analyses:
#   1. Item-level treatment effects (Figure 1 logic from Gilbert et al., 2025, JPAM)
#   2. Identification problem: how IL-HTE biases person-HTE interaction estimates
#      (Figure 5 logic from Gilbert et al., 2025, JPAM; theory in Gilbert et al., 2025, JEBS)
#
# Usage: Rscript vignettes/il_hte_compute.R   # from project root

library(irw)
library(lme4)
library(dplyr)
library(purrr)
library(tibble)

set.seed(20240601)

out_dir  <- "vignettes/ilhtedata"
fits_dir <- file.path(out_dir, "fits")
dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

# Tables selected from the reference script (Gilbert et al., 2025, JPAM)
rct_tables <- paste0("gilbert_meta_", c(1, 9, 17, 20, 37, 41, 69, 74))

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# ------------------------------------------------------------------------------
# Per-dataset fitting
# ------------------------------------------------------------------------------

fit_one <- function(tab) {
  message("\n=== ", tab, " ===")
  out_file <- file.path(fits_dir, paste0(tab, ".rds"))
  if (file.exists(out_file)) { message("  cached"); return(readRDS(out_file)) }

  df <- tryCatch(irw_fetch(tab), error = function(e) {
    message("  fetch failed: ", conditionMessage(e)); NULL
  })
  if (is.null(df)) return(NULL)

  # Filter to wave 1 if wave column is present
  if ("wave" %in% names(df)) df <- df[df$wave == 1, ]

  # Require treat column
  if (!"treat" %in% names(df)) { message("  no treat column"); return(NULL) }

  # Find pretest covariate (cov_ prefix) — optional; needed for M_B / M_C only.
  # When multiple cov_ columns exist, pick the one most correlated with resp
  # in the control group, since the pretest score is typically the strongest predictor.
  cov_cols <- grep("^cov_", names(df), value = TRUE)
  has_cov  <- length(cov_cols) > 0
  if (has_cov) {
    if (length(cov_cols) == 1) {
      cov_col <- cov_cols[1]
    } else {
      ctrl_df <- df[df$treat == 0, ]
      cors <- sapply(cov_cols, function(cn) {
        v <- as.numeric(ctrl_df[[cn]])
        if (sd(v, na.rm = TRUE) == 0) return(0)
        abs(cor(v, ctrl_df$resp, use = "complete.obs"))
      })
      cov_col <- cov_cols[which.max(cors)]
    }
    message("  covariate: ", cov_col)
  } else {
    cov_col <- NULL
    message("  no cov_ column — skipping identification analysis")
  }

  df$resp  <- as.integer(as.numeric(df$resp))
  df$treat <- as.integer(df$treat)
  df$item  <- as.character(df$item)
  df$id    <- as.character(df$id)
  if (has_cov) df$cov <- as.numeric(df[[cov_col]])

  # Drop rows with NA in required columns (cov only if present)
  keep <- !is.na(df$resp) & !is.na(df$treat)
  if (has_cov) keep <- keep & !is.na(df$cov)
  df <- df[keep, ]

  # Downsample to at most 5000 unique IDs
  ids <- unique(df$id)
  if (length(ids) > 5000) {
    set.seed(42)
    ids <- sample(ids, 5000)
    df  <- df[df$id %in% ids, ]
  }

  # Keep only items with exactly 2 response categories
  dichot_items <- df |>
    group_by(item) |>
    summarise(n_vals = n_distinct(resp[!is.na(resp)]), .groups = "drop") |>
    filter(n_vals == 2) |>
    pull(item)

  if (length(dichot_items) < 3) { message("  < 3 dichotomous items"); return(NULL) }
  df <- df[df$item %in% dichot_items, ]

  # Standardize covariate to mean 0, SD 1 (only if present)
  if (has_cov) df$cov <- as.numeric(scale(df$cov))

  n_items   <- n_distinct(df$item)
  n_persons <- n_distinct(df$id)
  message("  N=", n_persons, "  J=", n_items, "  rows=", nrow(df))

  # M_0: baseline random intercepts — used only to extract sigma_theta for standardization
  # (eq 12 in Gilbert et al., 2025, JPAM)
  m0 <- tryCatch(
    glmer(resp ~ treat + (1|id) + (1|item), data = df, family = binomial, control = ctrl),
    error = function(e) { message("  m0 failed: ", e$message); NULL }
  )
  if (is.null(m0)) return(NULL)
  sigma_theta <- sqrt(as.numeric(VarCorr(m0)$id))

  # M_A: IL-HTE model — random slope for treat on items (eq 14 in JPAM)
  m_a <- tryCatch(
    glmer(resp ~ treat + (1|id) + (treat|item), data = df, family = binomial, control = ctrl),
    error = function(e) { message("  m_a failed: ", e$message); NULL }
  )
  if (is.null(m_a)) return(NULL)

  # M_B and M_C require a pretest covariate — skip if unavailable
  m_b <- NULL
  m_c <- NULL
  if (has_cov) {
    m_b <- tryCatch(
      glmer(resp ~ treat + cov + treat:cov + (1|item) + (1|id), data = df, family = binomial, control = ctrl),
      error = function(e) { message("  m_b failed: ", e$message); NULL }
    )
    m_c <- tryCatch(
      glmer(resp ~ treat + cov + treat:cov + (treat|item) + (1|id), data = df, family = binomial, control = ctrl),
      error = function(e) { message("  m_c failed: ", e$message); NULL }
    )
  }

  # --- Extract from M_A ---
  vc_a       <- VarCorr(m_a)$item
  sigma_b    <- sqrt(vc_a["(Intercept)", "(Intercept)"])
  sigma_zeta <- sqrt(vc_a["treat", "treat"])
  rho        <- if (sigma_b > 0 && sigma_zeta > 0)
                  vc_a["(Intercept)", "treat"] / (sigma_b * sigma_zeta)
                else NA_real_

  re_a    <- ranef(m_a)$item
  b_i     <- re_a[, "(Intercept)"]
  zeta_i  <- re_a[, "treat"]
  sigma_b_ctrl  <- sd(b_i)
  sigma_b_treat <- sd(b_i + zeta_i)
  ate_a   <- fixef(m_a)["treat"]

  # LRT for H0: sigma_zeta = 0; p-value halved (boundary correction)
  lrt_pval <- tryCatch({
    lr <- anova(m0, m_a)
    as.numeric(lr$`Pr(>Chisq)`[2]) / 2
  }, error = function(e) NA_real_)

  # Item-level effects tibble (standardized by sigma_theta from M_0)
  item_tbl <- tibble(
    tab        = tab,
    item       = rownames(re_a),
    b_std      = b_i / sigma_theta,
    zeta_std   = zeta_i / sigma_theta,
    total_std  = (ate_a + zeta_i) / sigma_theta
  )

  # --- Extract from M_B and M_C (NA when no covariate was available) ---
  beta3_b <- if (!is.null(m_b)) as.numeric(fixef(m_b)["treat:cov"]) else NA_real_
  beta3_c <- if (!is.null(m_c)) as.numeric(fixef(m_c)["treat:cov"]) else NA_real_

  result <- list(
    tab             = tab,
    n_items         = n_items,
    n_persons       = n_persons,
    sigma_theta     = sigma_theta,
    ate_std         = as.numeric(ate_a / sigma_theta),
    sigma_zeta_std  = sigma_zeta / sigma_theta,
    rho             = rho,
    sigma_b_ctrl    = sigma_b_ctrl,
    sigma_b_treat   = sigma_b_treat,
    sd_ratio        = sigma_b_treat / sigma_b_ctrl,
    lrt_pval        = lrt_pval,
    sig_il_hte      = !is.na(lrt_pval) && lrt_pval < 0.05,
    beta3_b         = as.numeric(beta3_b),
    beta3_c         = as.numeric(beta3_c),
    delta_beta3     = as.numeric(beta3_b - beta3_c),
    item_tbl        = item_tbl
  )

  saveRDS(result, out_file)
  result
}

# ------------------------------------------------------------------------------
# Run over all tables
# ------------------------------------------------------------------------------

raw <- map(rct_tables, safely(fit_one))
results_list <- map(raw, \(x) {
  if (!is.null(x$error)) message("ERROR: ", conditionMessage(x$error))
  x$result
}) |> compact()

summary_df <- map_dfr(results_list, \(r) tibble(
  tab             = r$tab,
  n_items         = r$n_items,
  n_persons       = r$n_persons,
  ate_std         = r$ate_std,
  sigma_zeta_std  = r$sigma_zeta_std,
  rho             = r$rho,
  sigma_b_ctrl    = r$sigma_b_ctrl,
  sigma_b_treat   = r$sigma_b_treat,
  sd_ratio        = r$sd_ratio,
  lrt_pval        = r$lrt_pval,
  sig_il_hte      = r$sig_il_hte,
  beta3_b         = r$beta3_b,
  beta3_c         = r$beta3_c,
  delta_beta3     = r$delta_beta3
))

item_df <- map_dfr(results_list, \(r) r$item_tbl)

message("\n=== Summary ===")
print(summary_df)

# ------------------------------------------------------------------------------
# BibTeX — paper citations + dataset citations
# ------------------------------------------------------------------------------

bib_file <- file.path(out_dir, "references.bib")

cat(
'@article{gilbert2025estimating,
  title={Estimating heterogeneous treatment effects with item-level outcome data: {Insights} from {Item Response Theory}},
  author={Gilbert, Joshua B and Himmelsbach, Zachary and Soland, James and Domingue, Benjamin W},
  journal={Journal of Policy Analysis and Management},
  volume={44},
  number={4},
  pages={1417--1449},
  year={2025},
  doi={10.1002/pam.70025}
}

@article{gilbert2024disentangling,
  title={Disentangling {P}erson-{D}ependent and {I}tem-{D}ependent {C}ausal {E}ffects: {A}pplications of {I}tem {R}esponse {T}heory to the {E}stimation of {T}reatment {E}ffect {H}eterogeneity},
  author={Gilbert, Joshua B and Miratrix, Luke W and Joshi, Mridul and Domingue, Benjamin W},
  journal={Journal of Educational and Behavioral Statistics},
  volume={50},
  number={1},
  pages={72--101},
  year={2025},
  doi={10.3102/10769986241240085}
}
',
  file = bib_file
)

for (tab in summary_df$tab) {
  tryCatch(
    irw_save_bibtex(tab, output_file = bib_file, append = TRUE),
    error = function(e) message("bibtex failed for ", tab, ": ", e$message)
  )
}

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------

saveRDS(
  list(
    summary  = summary_df,
    item_df  = item_df,
    date_run = Sys.Date(),
    session  = sessionInfo()
  ),
  file.path(out_dir, "il_hte_results.rds")
)

message("\nSaved to ", out_dir, "/il_hte_results.rds")
