library(dplyr)
library(readr)
library(stringr)

files <- dir("irw_public/")

metadata <- files |>
  map_df(function(x) {
    load("irw_public", x)
    dataset_name <- str_replace(x,".Rdata","")
    write_csv(df, here("csvs", paste0(dataset_name, ".csv")))
    
    tibble(dataset_name = dataset_name,
                                 rows = nrow(df), 
                                 n_items = n_distinct(df$item), 
                                 n_participants = n_distinct(df$id), 
                                 dichotomous = all(df$resp %in% c(0,1)))
  })


write_csv(metadata, "csvs/metadata.csv")
