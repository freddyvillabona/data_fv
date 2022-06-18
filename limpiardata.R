
library(dplyr)
# Se carga la data
data <- read.csv("owid-covid-data.csv")

dat <- dplyr::filter(data, location %in% c("Colombia", 
                                           "Venezuela",
                                           "Peru",
                                           "Ecuador"))
