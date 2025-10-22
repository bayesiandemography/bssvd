
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(command)
})

cmd_assign(.un_m48 = "concordances/un_m49.csv",
           .out = "../data/country_code.rda")

country_code <- read_csv(.un_m48,
                         col_names = c("country", "code"),
                         col_types = "cc-",
                         skip = 1)

save(country_code, file = .out, compress = "bzip2")


  
  
