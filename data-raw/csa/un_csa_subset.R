
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(command)
})

cmd_assign(.csa = "sch/UNdata_Export_20251022_221132488.csv.zip",
           .out = "../data/un_csa_subset.rda")

csa <- read_csv(.csa,
                show_col_types = FALSE,
                n_max = 100000)

include <- c("Australia",
             "Mexico",
             "Bulgaria")

un_csa_subset <- csa |>
  filter(`Country or Area` %in% include)

save(un_csa_subset, file = .out, compress = "bzip2")


