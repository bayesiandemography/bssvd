# Tidy World Marriage Data

Tidy data downloaded from the UN Population Division's [World Migration
Data](https://www.un.org/development/desa/pd/data/world-marriage-data),
database.

## Usage

``` r
tidy_csa(data)
```

## Arguments

- data:

  Data frame with attendance data. (Data from multiple files combined,
  but otherwise unprocessed).

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
tidy_csa(un_csa_subset)
#> # A tibble: 645 × 5
#>    country    time sex     age value
#>    <chr>     <dbl> <chr> <int> <dbl>
#>  1 Australia  2021 Total     5 0.966
#>  2 Australia  2021 Total     6 1    
#>  3 Australia  2021 Total     7 1    
#>  4 Australia  2021 Total     8 1    
#>  5 Australia  2021 Total     9 1    
#>  6 Australia  2021 Total    10 1    
#>  7 Australia  2021 Total    11 1    
#>  8 Australia  2021 Total    12 1    
#>  9 Australia  2021 Total    13 1    
#> 10 Australia  2021 Total    14 1    
#> # ℹ 635 more rows
```
