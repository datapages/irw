# install irw package if necessary
# devtools::install_github("itemresponsewarehouse/Rpkg")

library(irw)

# fetch data from {{n_tables}} specified tables
irw_tables <- irw_fetch({{tables}})