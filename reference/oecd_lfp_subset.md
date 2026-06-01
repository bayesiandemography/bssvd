# OECD Data on Labour Force Participation

Data on labour force participation rates by age and sex, for multiple
countries in 2010–2012, extracted from the OECD Data Explorer database.

## Usage

``` r
oecd_lfp_subset
```

## Format

A tibble with 12,069 rows and with columns `TIME_PERIOD`, `REF_AREA`,
`MEASURE`,`UNIT_MEASURE`, `SEX`, `AGE`, `LABOUR FORCE STATUS`,
`obsValue`, `DECIMALS`, `OBS_STATUS`.

## Source

Downloaded from table "Employment and unemployment by five-year age
group and sex - indicators" from [OECD Data
Explorer](https://data-explorer.oecd.org). Accessed 21 April 2024. Only
a subset of the data is included in `oecd_lfp_subset`.

## Details

The labour force participation rate is the number of people in the
labour force (ie the number of people who are currently employed or
unemployed) as a proportion of the population.
