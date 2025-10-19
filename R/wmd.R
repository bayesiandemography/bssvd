
## library(readxl)
## library(poputils)


## col_types <- c("text",
##                "skip",
##                "numeric",
##                "skip",
##                rep("text",
##                "skip",
##                rep("text", 2),
##                "numeric",
##                rep("skip", 10))

## col_names <- c("country", "time", "marital_status", "age",
##                "indicator", "value")

## marital_keep <- c("Married/In-union", "All women")

## indicator_keep <- c("Any method", "Modern method")

## age_keep <- age_labels(type = "five", min = 15, max = 50)
               

## d <- read_xlsx("data-raw/wmd/undesa_pd_2019_wmd_marital_status.xlsx",
##                sheet = "MARITAL_STATUS_BY_AGE",
##                skip = 1) 



##                skip = 9,
##                col_types = col_types,
##                col_names = col_names,
##                na = "...") |>
##   mutate(age = gsub("\\[|\\]", "", age)) |>
##   filter(marital_status %in% marital_keep) |>
##   filter(indicator %in% indicator_keep) |>
##   filter(age %in% age_keep) |>
##   mutate(value = value / 100)               
