# Tidy HIMD Age-Specific Internal Migration Data

Put data on age-specific migration probabilities from a zipped file
downloaded from the [Human Internal Migration
Database](https://osf.io/vmrfk/) into a tidy data frame.

## Usage

``` r
tidy_himd(zipfile, time_interval = 1)
```

## Arguments

- zipfile:

  The name of a zipped file downloaded from the Human Internal Migration
  Database. A path name that is handled by
  [`utils::unzip()`](https://rdrr.io/r/utils/unzip.html).

- time_interval:

  Length of interval over which values are calculated. Choices are `1`
  or `5`. Default is `1`.

## Value

A tibble

## See also

- [`data_ssvd_himd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_himd.md)
  Put data from the Human Internal Migration Database into the format
  required by the `ssvd()` function in bage

- [`coef_hmd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hmd.md)
  Obtain time series of SVD coefficients for Human Internal Migration
  Database data

## Examples

``` r
zipfile <- system.file("extdata", "himd_20241023_subset.zip",
                       package = "bssvd")
tidy_himd(zipfile)
#> # A tibble: 1,221 × 4
#>    country_orig_dest                  time   age    value
#>    <chr>                             <int> <int>    <dbl>
#>  1 Trinidad and Tobago.780010.780020  1999     0 0.000742
#>  2 Trinidad and Tobago.780010.780080  1999     0 0.0591  
#>  3 Trinidad and Tobago.780010.780094  1999     0 0.00251 
#>  4 Trinidad and Tobago.780020.780010  1999     0 0.000778
#>  5 Trinidad and Tobago.780020.780080  1999     0 0.0470  
#>  6 Trinidad and Tobago.780020.780094  1999     0 0.00120 
#>  7 Trinidad and Tobago.780080.780010  1999     0 0.00246 
#>  8 Trinidad and Tobago.780080.780020  1999     0 0.00158 
#>  9 Trinidad and Tobago.780080.780094  1999     0 0.000863
#> 10 Trinidad and Tobago.780094.780010  1999     0 0.00121 
#> # ℹ 1,211 more rows
```
