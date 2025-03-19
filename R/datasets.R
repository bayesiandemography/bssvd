
#' OECD Data on Labour Force Participation
#'
#' Data on labour force participation rates
#' by age and sex, for
#' multiple countries in 2010--2012,
#' extracted from the OECD Data Explorer database.
#'
#' The labour force participation rate is the
#' number of people in the labour force (ie the
#' number of people who are currently employed
#' or unemployed) as a proportion of the population.
#'
#' @format
#' A tibble with 12,069 rows and with
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
#' Only a subset of the data is included
#' in `oecd_lfp_subset`.
"oecd_lfp_subset"


#' Human Fertility Database Data on
#' Age-Specific Fertility Rates
#'
#' Fertility rates by single year of age,
#' by county, by calendar year,
#' from the Human Fertility Database.
#'
#' `asfr_subset` is for testing, rather than data analysis.
#' It is restricted to the period 1980-2000,
#' and is not kept up to date. Data for analysis
#' should be obtained from the HFD itself.
#'
#' @format
#' A tibble with 33,572 rows and with
#' columns `Code`, `Year`, `Age`,`ASFR`.
#'
#' @source Downloaded from the "Age-specific fertility rate"
#' row of the "By Statistic" table on the
#' [ZippedDataFiles](https://www.humanfertility.org/Data/ZippedDataFiles)
#' page of the Human Fertility Database.
#' Accessed 12 April 2024.
"asfr_subset"
