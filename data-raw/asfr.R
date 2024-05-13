
library(readr)
library(dplyr)

asfr <- read_table("data-raw/asfrRR.txt.zip",
                   skip = 2,
                   col_types = "cicd") %>%
  filter(Year %in% 1980:2000)

save(asfr, file = "data/asfr.rda")
  



