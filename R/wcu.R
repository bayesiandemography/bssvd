
## library(readxl)
## library(poputils)


## col_types <- c("text",
##                  "skip",
##                  "numeric",
##                  "skip",
##                  "text",
##                  "skip",
##                  rep("text", 2),
##                  "numeric",
##                  rep("skip", 10))

## col_names <- c("country", "time", "marital_status", "age",
##                "indicator", "value")

## marital_keep <- c("Married/In-union", "All women")

## indicator_keep <- c("Any method", "Modern method")

## age_keep <- age_labels(type = "five", min = 15, max = 50)
               

## d <- read_xlsx("data-raw/wcu/undesa_pd_2024_wcu_country_data_survey-based.xlsx",
##                sheet = "By marital status and age",
##                skip = 9,
##                col_types = col_types,
##                col_names = col_names,
##                na = "...") |>
##   mutate(age = gsub("\\[|\\]", "", age)) |>
##   filter(marital_status %in% marital_keep) |>
##   filter(indicator %in% indicator_keep) |>
##   filter(age %in% age_keep) |>
##   mutate(value = value / 100)               
