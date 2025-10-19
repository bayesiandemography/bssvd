
library(dplyr)

country_code <- read.csv("codes/un_m49.csv") |>
  mutate(country = if_else(Country == "United Kingdom of Great Britain and Northern Ireland",
                           "United Kingdom",
                           Country)) |>
  mutate(code = sprintf("%03.0f", UN_M49)) |>
  select(country, code) |>
  tibble()

save(country_code, file = "../data/country_code.rda",
     compress = "bzip2")


  
  
