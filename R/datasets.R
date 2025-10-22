
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


#' Country Codes
#'
#' Three-digit country codes, and
#' country names,
#' from UN_M49 standard.
#'
#' @format
#' A tibble with 190 rows and with
#' columns `country` and `code`.
#'
#' @source United Nations Statistics Division. (2024).
#' Standard country or area codes for statistical
#' use (M49). United Nations, Department of Economic
#' and Social Affairs. Retrieved from
#' https://unstats.un.org/unsd/methodology/m49/
#' on 13 October 2025.
"country_code"


#' Marital Status
#'
#' Mapping between reported marital statuses
#' reported in the World Marriage Data database
#' and variables 'currently_married' and 'ever_married'.
#'
#' @format
#' A tibble with 35 rows and with
#' columns `marital_status` and `currently_married`,
#' and `ever_married`.
#'
#' @source Marital statuses derived from data in
#' file `undesa_pd_2019_wmd_marital_status.xlsx`
#' downloaded from the United Nations Population Division
#' website https://www.un.org/development/desa/pd/data/world-marriage-data
#' on 10 October 2025.
"marital_status"

