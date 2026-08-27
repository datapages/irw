## Collate cached fits into tidy CSVs.
suppressPackageStartupMessages({ library(dplyr); library(purrr) })
OUT <- "analysis/plenitude_data"; FITDIR <- file.path(OUT, "fits")

fits <- list.files(FITDIR, pattern = "\\.rds$", full.names = TRUE) |>
  map(readRDS) |> keep(~ identical(.x$status, "ok"))
message("collating ", length(fits), " completed table x replicate fits")

key <- function(x) data.frame(table = x$table, replicate = x$replicate, seed = x$seed)

mf <- map_dfr(fits, ~ cbind(key(.x), n_items = .x$n_items, n_resp = .x$n_resp,
                            n_resp_full = .x$n_resp_full, N_capped = .x$N_capped,
                            .x$metafeatures))
pr <- map_dfr(fits, ~ cbind(key(.x)[rep(1, nrow(.x$predicted)), ], .x$predicted))
so <- map_dfr(fits, ~ cbind(key(.x)[rep(1, nrow(.x$solutions)), ],
                            n_items = .x$n_items, n_resp = .x$n_resp, .x$solutions))
va <- map_dfr(fits, ~ cbind(key(.x)[rep(1, nrow(.x$validity)), ], .x$validity))
rk <- map_dfr(fits, ~ cbind(key(.x)[rep(1, nrow(.x$reverse_key)), ], .x$reverse_key))

write.csv(mf, file.path(OUT, "metafeatures.csv"), row.names = FALSE)
write.csv(pr, file.path(OUT, "predicted_accuracy.csv"), row.names = FALSE)
write.csv(so, file.path(OUT, "class_solutions.csv"), row.names = FALSE)
write.csv(va, file.path(OUT, "validity_criteria.csv"), row.names = FALSE)
write.csv(rk, file.path(OUT, "reverse_key.csv"), row.names = FALSE)

saveRDS(list(metafeatures = mf, predicted = pr, solutions = so,
             validity = va, reverse_key = rk, n_fits = length(fits)),
        file.path(OUT, "plenitude_results.rds"))
message("wrote 5 CSVs + plenitude_results.rds")
