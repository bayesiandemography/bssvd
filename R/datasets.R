
#' OECD data on Labour Force Participation
#'
#' Data on labour force participation rates
#' by age and sex, for
#' multiple countries, starting in 1960,
#' from the OECD Data Explorer database.
#'
#' The labour force participation rate is the
#' number of people in the labour force (ie the
#' number of people who are currently employed
#' or unemployed) as a proportion of the population.
#'
#' @format
#' A tibble with 12069 rows and the following
#' columns `TIME_PERIOD`, `REF_AREA`,
#' `MEASURE`,`UNIT_MEASURE`, `SEX`, `AGE`,
#' `LABOUR FORCE STATUS`, `obsValue`, `DECIMALS`,
#' `OBS_STATUS`.
#'
#' @source Downloaded from table
#' "Employment and unemployment by five-year
#' age group and sex - indicators" from
#' [OECD Data Explorer](https://data-explorer.oecd.org).
#' Accessed 21 April 2024.
#' Only a subset of the full dataset is included
#' in `oecd_lpr`.
"oecd_lfp"
