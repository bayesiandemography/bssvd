# Obtain Coefficients from Scaled SVD of HIMD Data

Obtain time series of coefficients from a scaled SVD of data from the
[Human Internal Migration Database](https://osf.io/vmrfk/). The
coefficients are a scaled version of the \\U\\ matrix from the SVD.

## Usage

``` r
coef_himd(
  zipfile,
  time_interval = 1,
  measure_type = c("rate", "prob"),
  n_comp = 5,
  eps = 1e-05
)
```

## Arguments

- zipfile:

  The name of a zipped file downloaded from the Human Internal Migration
  Database. A path name that is handled by
  [`utils::unzip()`](https://rdrr.io/r/utils/unzip.html).

- time_interval:

  Length of interval over which values are calculated. Choices are `1`
  or `5`. Default is `1`.

- measure_type:

  `"prob"` or `"rate"`. Whether values are to be treated as
  probabilities or as rates. See
  [`data_ssvd_himd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_himd.md)
  for details. Default is `"rate"`.

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- eps:

  Parameter for truncating probabililities or rates. Default is
  `0.00001`.

## Value

A tibble

## Truncation and transformation

If `measure_type` is `"prob"`, then reported probabilities outside
`(eps, 1-eps)` are shifted to `eps` or `1-eps`, and all values are
logit-transformed, before the SVD is applied.

If `measure_type` is `"rate"`, then reported probabilities less than
`eps` are shifted to `eps`, and all values are log-transformed, before
the SVD is applied.

## See also

- [`data_ssvd_hmd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_hmd.md)
  Put data from the Human Internal Migration Database into the format
  required by the `ssvd()` function in bage

- [`tidy_hmd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hmd.md)
  Format data from the Human Internal Migration Database into a tidy
  data frame

## Examples

``` r
zipfile <- system.file("extdata", "himd_20241023_subset.zip",
                       package = "bssvd")
coef_himd(zipfile)
#> # A tibble: 55 × 4
#>    country_orig_dest                  time component     coef
#>    <chr>                             <int> <chr>        <dbl>
#>  1 Trinidad and Tobago.780010.780020  1999 Component 1  0.280
#>  2 Trinidad and Tobago.780010.780080  1999 Component 1 -1.92 
#>  3 Trinidad and Tobago.780010.780094  1999 Component 1  0.226
#>  4 Trinidad and Tobago.780020.780010  1999 Component 1  0.809
#>  5 Trinidad and Tobago.780020.780080  1999 Component 1 -1.79 
#>  6 Trinidad and Tobago.780020.780094  1999 Component 1  0.718
#>  7 Trinidad and Tobago.780080.780010  1999 Component 1  0.281
#>  8 Trinidad and Tobago.780080.780020  1999 Component 1  0.582
#>  9 Trinidad and Tobago.780080.780094  1999 Component 1  0.718
#> 10 Trinidad and Tobago.780094.780010  1999 Component 1  0.711
#> # ℹ 45 more rows
```
