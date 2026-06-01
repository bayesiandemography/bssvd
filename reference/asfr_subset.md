# Human Fertility Database Data on Age-Specific Fertility Rates

Fertility rates by single year of age, by county, by calendar year, from
the Human Fertility Database.

## Usage

``` r
asfr_subset
```

## Format

A tibble with 33,572 rows and with columns `Code`, `Year`, `Age`,`ASFR`.

## Source

Downloaded from the "Age-specific fertility rate" row of the "By
Statistic" table on the
[ZippedDataFiles](https://www.humanfertility.org/Data/ZippedDataFiles)
page of the Human Fertility Database. Accessed 12 April 2024.

## Details

`asfr_subset` is for testing, rather than data analysis. It is
restricted to the period 1980-2000, and is not kept up to date. Data for
analysis should be obtained from the HFD itself.
