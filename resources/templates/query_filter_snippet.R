# install the irw package if necessary (CRAN), plus redivis, which it reads
# the data through and which is not on CRAN:
# install.packages("irw")
# install.packages("redivis", repos = c("https://redivis.r-universe.dev", getOption("repos")))
# (development version: remotes::install_github("itemresponsewarehouse/Rpkg"))

library(irw)

irw_tables <- irw_filter({{filter_str}})

irw_data <- irw_fetch(irw_tables)
