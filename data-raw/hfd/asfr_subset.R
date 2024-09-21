
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(command)

cmd_assign(.asfr = "hfd/asfrRR.txt.zip",
           .out = "../data/asfr_subset.rda")

asfr_subset <- read_table(.asfr,
                          skip = 2,
                          col_types = "cicd") |>
  filter(Year %in% 1980:2000)

save(asfr_subset, file = .out, compress = "bzip2")
  



