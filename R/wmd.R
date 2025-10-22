


tidy_wmd <- function(file,
                     year_min = NULL) {
  col_types <- c("text",
                 "skip",
                 "numeric",
                 "skip",
                 rep("text", 2),
                 rep("skip", 2),
                 "text",
                 rep("skip", 2),
                 "numeric",
                 rep("skip", 11))
  col_names <- c("country",
                 "year",
                 "sex", "marital_status",
                 "age",
                 "value")
  check_year_min(year_min)
  ans <- readxl::read_xlsx(file,
                           sheet = "MARITAL_STATUS_BY_AGE",
                           skip = 3,
                           col_types = col_types,
                           col_names = col_names,
                           na = "...")
  ans$age <- gsub("\\[|\\]", "", ans$age)
  ans <- merge(ans, marital_status, by = "marital_status")

  
  
##   filter(marital_status %in% marital_keep) |>
##   filter(indicator %in% indicator_keep) |>
##   filter(age %in% age_keep) |>
##   mutate(value = value / 100)               

## d <- read_xlsx("../bage/data-raw/ssvd_wmd/undesa_pd_2019_wmd_marital_status.xlsx",
##                sheet = "MARITAL_STATUS_BY_AGE",
##                skip = 1)


##                col_types = col_types,
##                col_names = col_names,
##                na = "...") |>
