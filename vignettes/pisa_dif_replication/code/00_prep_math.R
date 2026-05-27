# 00_prep_math.R
# Replicates math_cleaning.R from Ouyang et al. (2026) using IRW data.
# Produces three CSVs expected by estimation_math.R:
#   PISA_2022_math_sas.csv            — N x J binary response matrix
#   countries_2022_math_sas.csv       — N x 37 country indicator matrix
#   PISA_2022_math_samplingweights_sas.csv — length-N sampling weights
#
# Usage: Rscript code/00_prep_math.R   (from vignettes/pisa_dif_replication/)

library(irw)
library(dplyr)
library(tidyr)

out_dir <- "data"
dir.create(out_dir, showWarnings = FALSE)

oecd_countries <- c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
                    "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
                    "KOR","LVA","LTU","MEX","NLD","NZL","NOR","POL","PRT","SVK",
                    "SVN","ESP","SWE","CHE","TUR","GBR","USA")

message("Fetching pisa2022_math from IRW...")
df <- irw_fetch("pisa2022_math")

message("Rows: ", nrow(df), "  Cols: ", paste(names(df), collapse=", "))

# Filter to OECD countries
df <- df[df$CNT %in% oecd_countries, ]
message("After OECD filter: ", nrow(df), " rows, ", length(unique(df$id)), " students")

# Keep only binary items (resp in {0,1} only, no 2s)
binary_items <- df %>%
  group_by(item) %>%
  summarise(max_resp = max(resp, na.rm = TRUE), .groups = "drop") %>%
  filter(max_resp <= 1) %>%
  pull(item)

message("Binary items: ", length(binary_items))
df <- df[df$item %in% binary_items, ]

# Filter to paper's exact 169-item list (suffix stripped to match IRW naming)
paper_items <- readLines("data/math_item_names_paper.txt")
message("Paper item list: ", length(paper_items), " items")
df <- df[df$item %in% paper_items, ]

message("Final: ", length(unique(df$id)), " students, ", length(unique(df$item)), " items")

# Pivot to wide response matrix
items_ordered <- sort(unique(df$item))
students <- df %>%
  select(id, CNT, W_FSTUWT = rt) %>%  # IRW doesn't carry sampling weights; use W_FSTUWT if available
  distinct(id, .keep_all = TRUE)

# Check if sampling weights are available (IRW pisa2022 has no W_FSTUWT; use equal weights)
# The IRW table has: CNT, CNTSCHID, id, item, resp, num_actions, num_short_visits, num_visits, rt, time_to_first_action
# No sampling weight column — use equal weights as fallback
message("Columns available: ", paste(names(df), collapse=", "))

wide <- df %>%
  select(id, CNT, item, resp) %>%
  pivot_wider(names_from = item, values_from = resp, values_fn = first)

# Align student order
student_ids <- wide$id
cnt_vec     <- wide$CNT

response_mat <- wide %>%
  select(all_of(items_ordered)) %>%
  as.matrix()

message("Response matrix: ", nrow(response_mat), " x ", ncol(response_mat))

# Country indicator matrix (N x 37)
country_mat <- matrix(0, nrow = nrow(response_mat), ncol = 37)
colnames(country_mat) <- oecd_countries
for (i in seq_along(cnt_vec)) {
  k <- which(oecd_countries == cnt_vec[i])
  country_mat[i, k] <- 1
}

# Sampling weights — IRW doesn't include W_FSTUWT so we use equal weights
# (a limitation vs the paper which uses PISA's W_FSTUWT)
sampling_weights <- rep(1, nrow(response_mat))
message("Note: using equal sampling weights (IRW does not carry W_FSTUWT)")

# Write outputs
write.csv(response_mat,    file.path(out_dir, "PISA_2022_math_sas.csv"),             row.names = TRUE)
write.csv(country_mat,     file.path(out_dir, "countries_2022_math_sas.csv"),         row.names = TRUE)
write.csv(sampling_weights,file.path(out_dir, "PISA_2022_math_samplingweights_sas.csv"), row.names = FALSE)
write.csv(items_ordered,   file.path(out_dir, "math_item_names.csv"),                 row.names = FALSE)

message("Saved to ", out_dir, "/")
message("Response matrix: ", nrow(response_mat), " x ", ncol(response_mat))
message("Country matrix:  ", nrow(country_mat),  " x ", ncol(country_mat))
