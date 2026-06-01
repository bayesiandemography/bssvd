# Tidy World Marriage Data

Tidy data downloaded from the UN Population Division's [World Marriage
Data](https://www.un.org/development/desa/pd/data/world-marriage-data),
database.

## Usage

``` r
tidy_wmd(file, status = c("current", "ever"))
```

## Arguments

- file:

  Path to excel spreadsheet holding World Marriage Data

- status:

  `"current"` (the default) or `"ever"`. Whether to report results for
  the marriage staus "currently married" or "ever-married". Note that
  both statuses include consensual and de facto unions, in addition to
  legal marriages.

## Value

A tibble

## See also

- [`coef_wmd()`](https://bayesiandemography.github.io/bssvd/reference/coef_wmd.md)
  Obtain time series of SVD coefficients from World Marriage Data

- [`data_ssvd_wmd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_wmd.md)
  Put World Marriage Data into the format required by the `ssvd()`
  function in bage

## Examples

``` r
file <- system.file(
  "extdata",
  "undesa_pd_2019_wmd_marital_status_subset.xlsx",
  package = "bssvd"
)
tidy_wmd(file)
#> # A tibble: 250 × 5
#>    country      time sex    age    value
#>    <chr>       <dbl> <chr>  <chr>  <dbl>
#>  1 Afghanistan 1973  Female 15-19 0.497 
#>  2 Afghanistan 1979  Female 15-19 0.534 
#>  3 Albania     1989  Female 15-19 0.082 
#>  4 Albania     2000  Female 15-19 0.0752
#>  5 Albania     2001  Female 15-19 0.0947
#>  6 Albania     2002  Female 15-19 0.095 
#>  7 Albania     2005  Female 15-19 0.0495
#>  8 Albania     2008. Female 15-19 0.07  
#>  9 Afghanistan 2010  Female 15-19 0.173 
#> 10 Afghanistan 2010. Female 15-19 0.198 
#> # ℹ 240 more rows
```
