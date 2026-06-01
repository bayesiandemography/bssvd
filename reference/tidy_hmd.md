# Tidy HMD Age-Specific Mortality Data

Put data on age-specific mortality rates from a zipped file downloaded
from the [Human Mortality
Database](https://www.mortality.org/Data/ZippedDataFiles) into a tidy
data frame.

## Usage

``` r
tidy_hmd(zipfile, date = c("2025-09-25", "2024-02-26"), year_min = NULL)
```

## Arguments

- zipfile:

  The name of a zipped file downloaded from the Human Mortality
  Database. A path name that is handled by
  [`utils::unzip()`](https://rdrr.io/r/utils/unzip.html).

- date:

  Date on which data produced. Choices are `"2025-09-25"`,
  `"2024-02-26"`. (Class is character, not Date.)

- year_min:

  Only include data for `year_min` onwards. Ignored if `year_min` is
  `NULL` (the default).

## Value

A tibble

## See also

- [`data_ssvd_hmd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_hmd.md)
  Put data from the Human Mortality Database into the format required by
  the `ssvd()` function in bage

- [`coef_hmd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hmd.md)
  Obtain time series of SVD coefficients for Human Mortality Database
  data

## Examples

``` r
zipfile <- system.file("extdata", "hmd_statistics_20250925_subset.zip",
                       package = "bssvd")
tidy_hmd(zipfile)
#> # A tibble: 40,905 × 7
#>    country  time type_age age        mx    Lx sex  
#>    <chr>   <int> <fct>    <chr>   <dbl> <int> <fct>
#>  1 AUS      1921 single   0     0.0684  95294 Total
#>  2 AUS      1921 single   1     0.0132  92864 Total
#>  3 AUS      1921 single   2     0.00592 91978 Total
#>  4 AUS      1921 single   3     0.00355 91543 Total
#>  5 AUS      1921 single   4     0.00325 91232 Total
#>  6 AUS      1921 single   5     0.00251 90969 Total
#>  7 AUS      1921 single   6     0.00235 90748 Total
#>  8 AUS      1921 single   7     0.00187 90557 Total
#>  9 AUS      1921 single   8     0.00165 90398 Total
#> 10 AUS      1921 single   9     0.0014  90260 Total
#> # ℹ 40,895 more rows
tidy_hmd(zipfile,
         date = "2025-09-25",
         year_min = 1950)
#> # A tibble: 29,160 × 7
#>    country  time type_age age        mx    Lx sex   
#>    <chr>   <int> <fct>    <chr>   <dbl> <int> <fct> 
#>  1 AUS      1950 single   43    0.00373 92874 Female
#>  2 AUS      1950 single   47    0.00462 91426 Female
#>  3 AUS      1950 single   44    0.00348 92539 Female
#>  4 AUS      1950 single   45    0.00382 92202 Female
#>  5 AUS      1950 single   46    0.00422 91832 Female
#>  6 AUS      1950 single   59    0.0116  83452 Female
#>  7 AUS      1950 single   56    0.00911 86090 Female
#>  8 AUS      1950 single   57    0.00954 85292 Female
#>  9 AUS      1950 single   58    0.0113  84409 Female
#> 10 AUS      1950 single   50    0.00642 89974 Female
#> # ℹ 29,150 more rows
```
