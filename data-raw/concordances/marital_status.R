
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(command)
})

cmd_assign(.marital = "concordances/marital_status.csv",
           .out = "../data/marital_status.rda")

marital_status <- read_csv(.marital, col_types = "cll")

save(marital_status, file = .out, compress = "bzip2")


  
  
