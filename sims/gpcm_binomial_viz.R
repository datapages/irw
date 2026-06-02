# sims/gpcm_binomial_viz.R
#
# Visualize results from gpcm_binomial_sweep.R.
# x-axis: asym (continuous Uniform(1,16)); smoothed with GAM.
# All IMV = IMV(model, GPCM): positive = GPCM wins.
# Line labels placed at x~16 in the extended x region (axis ends at 16).
#
# Usage: Rscript sims/gpcm_binomial_viz.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(mgcv)
})

dir.create("sims/plots", showWarnings = FALSE)
res <- readRDS("sims/gpcm_binomial_results.rds")

# ── Labels & colours ───────────────────────────────────────────────────────────
model_labels <- c(
  gpcm              = "GPCM",
  pcm               = "PCM",
  grm               = "GRM",
  binom_2pl_logit   = "Binom 2PL (logit)",
  binom_1pl_logit   = "Binom 1PL (logit)",
  binom_1pl_scobit  = "Binom 1PL (scobit)",
  binom_1pl_cloglog = "Binom 1PL (cloglog)",
  binom_1pl_cauchit = "Binom 1PL (cauchit)"
)
model_colors <- c(
  "GPCM"               = "#1b7837",
  "PCM"                = "#762a83",
  "GRM"                = "#2166ac",
  "Binom 2PL (logit)"  = "#d6604d",
  "Binom 1PL (logit)"  = "#f4a582",
  "Binom 1PL (scobit)" = "#f1a340",
  "Binom 1PL (cloglog)"= "#c2a5cf",
  "Binom 1PL (cauchit)"= "#a6dba0"
)

res <- res |>
  mutate(
    model_label = factor(model_labels[as.character(model)], levels = model_labels),
    K_label     = paste0("K = ", K)
  )

# Label positions: anchor x at mean of top-10% asym; y from GAM prediction there
lpos_fn <- function(data, y_var) {
  grps <- split(data, list(as.character(data$model_label),
                           as.character(data$K_label)), drop = TRUE)
  do.call(rbind, lapply(grps, function(g) {
    x_anch <- mean(g$asym[g$asym >= quantile(g$asym, 0.9)])
    fit    <- tryCatch(
      mgcv::gam(as.formula(paste(y_var, "~ s(asym, bs='cs')")), data = g),
      error = function(e) NULL
    )
    y_anch <- if (!is.null(fit))
      as.numeric(predict(fit, newdata = data.frame(asym = x_anch)))
    else
      mean(g[[y_var]][g$asym >= quantile(g$asym, 0.9)], na.rm = TRUE)
    data.frame(model_label = g$model_label[1], K_label = g$K_label[1],
               asym = x_anch, y_val = y_anch, stringsAsFactors = FALSE)
  }))
}

theme_set(
  theme_bw(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "grey92"),
      legend.position  = "none",
      plot.margin      = margin(5, 5, 5, 5)
    )
)

make_plot <- function(data, y_var, y_lab, title_str, exclude_smooth = "GPCM") {
  lpos <- lpos_fn(data, y_var)
  ggplot(data, aes(x = asym, y = .data[[y_var]],
                   colour = model_label, group = model_label)) +
    annotate("segment", x = -log(8), xend = log(8), y = 0, yend = 0,
             linetype = "dashed", colour = "grey60") +
    geom_smooth(data = function(d) if (is.null(exclude_smooth)) d else filter(d, !model_label %in% exclude_smooth),
                method = "gam", formula = y ~ s(x, bs = "cs"),
                se = TRUE, linewidth = 0.8, alpha = 0.12) +
    annotate("rect", xmin = log(8), xmax = log(8) + 3, ymin = -Inf, ymax = Inf,
             fill = "white", colour = NA) +
    geom_label_repel(
      data          = lpos,
      aes(x = asym, y = y_val, label = model_label),
      size          = 2.6,
      hjust         = 0,
      nudge_x       = log(8) + 0.3 - lpos$asym,
      direction     = "y",
      segment.size  = 0.3,
      segment.color = "grey50",
      box.padding   = 0.2,
      label.padding = 0.15,
      show.legend   = FALSE,
      xlim          = c(log(8), log(8) + 3)
    ) +
    facet_wrap(~ K_label) +
    scale_colour_manual(values = model_colors) +
    scale_x_continuous(
      name   = "Gap asymmetry  (0 = equal gaps;  negative = wider lower;  positive = wider upper)",
      limits = c(-log(8), log(8) + 3),
      breaks = round(c(-log(8), -1, 0, 1, log(8)), 2),
      labels = c("-ln8\n(wider lower)", "-1", "0\n(symmetric)", "1",
                 "ln8\n(wider upper)"),
      expand = expansion(mult = c(0.02, 0))
    ) +
    coord_cartesian(clip = "off") +
    labs(y = y_lab, title = title_str)
}

# ── Plot 1: IMV_c ──────────────────────────────────────────────────────────────
p1 <- make_plot(res, "imv_c",
                "IMV_c(model, GPCM)  [positive = GPCM wins]",
                "IMV_c: each model vs. GPCM")
ggsave("sims/plots/imv_c.pdf", p1, width = 10, height = 7)
ggsave("sims/plots/imv_c.png", p1, width = 10, height = 7, dpi = 150)
message("Saved: imv_c")

# ── Plot 2: IMV_t ──────────────────────────────────────────────────────────────
p2 <- make_plot(res, "imv_t",
                "IMV_t(model, GPCM)  [positive = GPCM wins]",
                "IMV_t: each model vs. GPCM")
ggsave("sims/plots/imv_t.pdf", p2, width = 10, height = 7)
ggsave("sims/plots/imv_t.png", p2, width = 10, height = 7, dpi = 150)
message("Saved: imv_t")

# ── Plot 3: RMSE ───────────────────────────────────────────────────────────────
p3 <- make_plot(res, "rmse",
                "RMSE (expected score, 0-1 scale)",
                "RMSE by model, K, and discrimination")
ggsave("sims/plots/rmse.pdf", p3, width = 10, height = 7)
ggsave("sims/plots/rmse.png", p3, width = 10, height = 7, dpi = 150)
message("Saved: rmse")

# ── Plot 4: IMV_c vs. 1PL-logit ───────────────────────────────────────────────
# Exclude binom_1pl_logit (always 0). GPCM now has a real value.
res_1pl <- filter(res, model_label != "Binom 1PL (logit)")
p4 <- make_plot(res_1pl, "imv_c_1pl",
                "IMV_c(1PL-logit, model)  [positive = model wins]",
                "IMV_c: Binom 1PL (logit) vs. each model",
                exclude_smooth = NULL)
ggsave("sims/plots/imv_c_vs_1pl.pdf", p4, width = 10, height = 7)
ggsave("sims/plots/imv_c_vs_1pl.png", p4, width = 10, height = 7, dpi = 150)
message("Saved: imv_c_vs_1pl")

# ── Plot 5: IMV_t vs. 1PL-logit ───────────────────────────────────────────────
p5 <- make_plot(res_1pl, "imv_t_1pl",
                "IMV_t(1PL-logit, model)  [positive = model wins]",
                "IMV_t: Binom 1PL (logit) vs. each model",
                exclude_smooth = NULL)
ggsave("sims/plots/imv_t_vs_1pl.pdf", p5, width = 10, height = 7)
ggsave("sims/plots/imv_t_vs_1pl.png", p5, width = 10, height = 7, dpi = 150)
message("Saved: imv_t_vs_1pl")

message("\nAll plots written to sims/plots/")
