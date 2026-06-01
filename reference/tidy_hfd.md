# Tidy HFD Age-Specific Fertility Data

Tidy a data frame containing data from the ASFR row of the "By
statistic" table of
[ZippedDataFiles](https://www.humanfertility.org/Data/ZippedDataFiles).

## Usage

``` r
tidy_hfd(data)
```

## Arguments

- data:

  A data frame

## Value

A tibble.

## See also

- [`data_ssvd_hfd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_hfd.md)Put
  data from the Human Fertility Database into the format required by the
  `ssvd()` function in bage.

- [`coef_hfd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hfd.md)
  Obtain time series of SVD coefficients from Human Fertility Database
  data

## Examples

``` r
tidy_hfd(asfr_subset)
#> # A tibble: 33,572 × 4
#>    country  time   age   value
#>    <chr>   <int> <int>   <dbl>
#>  1 AUT      1980    12 0.00002
#>  2 AUT      1980    13 0.00006
#>  3 AUT      1980    14 0.00048
#>  4 AUT      1980    15 0.00206
#>  5 AUT      1980    16 0.0101 
#>  6 AUT      1980    17 0.0275 
#>  7 AUT      1980    18 0.0518 
#>  8 AUT      1980    19 0.0817 
#>  9 AUT      1980    20 0.102  
#> 10 AUT      1980    21 0.115  
#> # ℹ 33,562 more rows
```
