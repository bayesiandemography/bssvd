# Obtain Coefficients from Scaled SVD of HMD Data

Obtain time series of coefficients from a scaled SVD of data from the
[Human Mortality
Database](https://www.mortality.org/Data/ZippedDataFiles). The
coefficients are a scaled version of the \\U\\ matrix from the SVD.

## Usage

``` r
coef_hmd(
  zipfile,
  n_comp = 5,
  date = c("2025-09-25", "2024-02-26"),
  year_min = NULL,
  eps = 1e-05
)
```

## Arguments

- zipfile:

  The name of a zipped file downloaded from the Human Mortality
  Database. A path name that is handled by
  [`utils::unzip()`](https://rdrr.io/r/utils/unzip.html).

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- date:

  Date on which data produced. Choices are `"2025-09-25"`,
  `"2024-02-26"`. (Class is character, not Date.)

- year_min:

  Only include data for `year_min` onwards. Ignored if `year_min` is
  `NULL` (the default).

- eps:

  Floor for rates. Default is `0.00001`.

## Value

A tibble

## Truncation and transformation

Rates below `eps` are shifted to `eps`, and all rates are
log-transformed, before the SVD is applied.

## See also

- [`data_ssvd_hmd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_hmd.md)
  Put data from the Human Mortality Database into the format required by
  the `ssvd()` function in bage

- [`tidy_hmd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hmd.md)
  Format data from the Human Mortality Database into a tidy data frame

## Examples

``` r
zipfile <- system.file("extdata", "hmd_statistics_20250925_subset.zip",
                       package = "bssvd")
coef_hmd(zipfile)
#> # A tibble: 1,515 × 5
#>    sex   country  time component    coef
#>    <fct> <chr>   <int> <chr>       <dbl>
#>  1 Total AUS      1921 Component 1 -1.66
#>  2 Total AUS      1922 Component 1 -1.52
#>  3 Total AUS      1923 Component 1 -1.59
#>  4 Total AUS      1924 Component 1 -1.51
#>  5 Total AUS      1925 Component 1 -1.45
#>  6 Total AUS      1926 Component 1 -1.49
#>  7 Total AUS      1927 Component 1 -1.47
#>  8 Total AUS      1928 Component 1 -1.49
#>  9 Total AUS      1929 Component 1 -1.48
#> 10 Total AUS      1930 Component 1 -1.27
#> # ℹ 1,505 more rows
```
