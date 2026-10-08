## Draw the stratified corpus for the Plenitude replication.
## Deterministic given the seed; writes analysis/plenitude_data/corpus.csv.
suppressPackageStartupMessages(library(dplyr))

SEED_SAMPLE <- 20260827L
N_TARGET    <- 120L
MAX_N_PART  <- 50000L   # download-cost restriction, following the scouting run

d <- read.csv("analysis/scout_eligible_tables.csv", stringsAsFactors = FALSE)
frame <- d %>% filter(n_participants <= MAX_N_PART)

frame <- frame %>%
  mutate(catband = cut(n_categories, c(3, 5, 7, 1e6),
                       labels = c("4-5", "6-7", "8+"), right = TRUE),
         stratum = paste(affect_cog, catband, sep = " | "))

## proportional allocation, largest-remainder, >=1 per non-empty stratum
tab   <- table(frame$stratum)
raw   <- N_TARGET * as.numeric(tab) / sum(tab)
alloc <- pmax(1, floor(raw))
rem   <- N_TARGET - sum(alloc)
if (rem > 0) {
  ord <- order(raw - floor(raw), decreasing = TRUE)
  alloc[ord[seq_len(rem)]] <- alloc[ord[seq_len(rem)]] + 1
} else if (rem < 0) {
  ord <- order(alloc, decreasing = TRUE)
  alloc[ord[seq_len(-rem)]] <- alloc[ord[seq_len(-rem)]] - 1
}
names(alloc) <- names(tab)

set.seed(SEED_SAMPLE)
corpus <- bind_rows(lapply(names(alloc), function(s) {
  pool <- frame[frame$stratum == s, ]
  pool[sample.int(nrow(pool), min(alloc[[s]], nrow(pool))), ]
})) %>% arrange(table)

## Post-draw exclusion, applied AFTER sampling so the remaining tables keep the
## identities they were drawn with. The two HEXACO tables are 40 items at the
## full N = 2,000 cap, which puts each of them at roughly 19 core-hours -- about
## a quarter of the entire run between them. Dropped on compute cost alone.
## They are NOT replaced: backfilling from the frame would systematically swap
## wide tables for narrow ones and bias the corpus toward small item counts,
## which is exactly the dimension the transportability section is about.
EXCLUDED <- c("hexaco_ashton_2014_c", "hexaco_ashton_2014_h")
corpus <- corpus[!corpus$table %in% EXCLUDED, ]

corpus$eff_N   <- pmin(corpus$n_participants, 2000L)
corpus$N_short <- corpus$n_participants < 2000L

dir.create("analysis/plenitude_data", showWarnings = FALSE, recursive = TRUE)
write.csv(corpus, "analysis/plenitude_data/corpus.csv", row.names = FALSE)

cat("drawn:", nrow(corpus), "tables from a frame of", nrow(frame),
    sprintf("(584 eligible less %d with n_participants > %d)\n",
            nrow(d) - nrow(frame), MAX_N_PART))
cat("\n-- realized stratification --\n")
print(table(corpus$affect_cog, corpus$catband))
cat("\n-- n_items --\n"); print(summary(corpus$n_items))
cat("\n-- n_participants --\n"); print(summary(corpus$n_participants))
cat("\ntables below N=2000 (all respondents used):", sum(corpus$N_short),
    sprintf("(%.0f%%)\n", 100 * mean(corpus$N_short)))
