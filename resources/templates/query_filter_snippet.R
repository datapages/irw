# install irw package if necessary
# devtools::install_github("itemresponsewarehouse/Rpkg")

library(irw)

irw_tables <- irw_filter({{filter_str}})

irw_data <- irw_fetch(irw_tables)
