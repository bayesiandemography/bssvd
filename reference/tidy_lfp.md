# Tidy OCED Labour Force Participation Data

Put data on labour force participation data downloaded from the [OECD
Data Explorer](https://data-explorer.oecd.org) into a tidy data frame.

## Usage

``` r
tidy_lfp(data, year_min = NULL)
```

## Arguments

- data:

  A data frame containing OECD data.

- year_min:

  Only include data for `year_min` onwards. Ignored if `year_min` is
  `NULL` (the default).

## Value

A tibble

## See also

- [`data_ssvd_lfp()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_lfp.md)
  Put OECD labour force participation data into the format required by
  the `ssvd()` function in bage

- `tidy_lfp()` Format OECD labour force participation data into a tidy
  data frame.

- [`coef_lfp()`](https://bayesiandemography.github.io/bssvd/reference/coef_lfp.md)
  Obtain time series of SVD coefficients for OECD labour force
  participation data

## Examples

``` r
tidy_lfp(oecd_lfp_subset)
#> # A tibble: 7,318 × 5
#>    time  country sex   age     value
#>    <chr> <chr>   <chr> <chr>   <dbl>
#>  1 2010  FIN     Total 50-54 0.875  
#>  2 2010  CHL     Total 80+   0.0533 
#>  3 2011  CHL     Total 80+   0.0602 
#>  4 2012  CHL     Total 80+   0.0627 
#>  5 2010  COL     Total 80+   0.0870 
#>  6 2011  COL     Total 80+   0.0945 
#>  7 2012  COL     Total 80+   0.0964 
#>  8 2010  ITA     Total 80+   0.00474
#>  9 2011  ITA     Total 80+   0.00474
#> 10 2012  ITA     Total 80+   0.00504
#> # ℹ 7,308 more rows
```
