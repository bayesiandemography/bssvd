# Obtain Coefficients from Scaled SVD of HFD Data

Obtain time series of SVD coefficients from a scaled SVD of data from
the [Human Fertility
Database](https://www.humanfertility.org/Home/Index). The coefficents
are a scaled version of the \\U\\ matrix from the SVD.

## Usage

``` r
coef_hfd(data, n_comp = 5, eps = 1e-05)
```

## Arguments

- data:

  A data frame containing HFD data.

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- eps:

  Floor for rates. Default is `0.00001`.

## Value

A tibble

## Truncation and transformation

Rates below `eps` are shifted to `eps`, and all rates are
log-transformed, before the SVD is applied.

## See also

- [`data_ssvd_hfd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_hfd.md)Put
  data from the Human Fertility Database into the format required by the
  `ssvd()` function in bage.

- [`tidy_hfd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hfd.md)
  Format data from the Human Fertility Database into a tidy data frame

## Examples

``` r
coef_hfd(asfr_subset)
#> # A tibble: 3,815 × 4
#>    country  time component      coef
#>    <chr>   <int> <chr>         <dbl>
#>  1 AUT      1980 Component 1 -0.355 
#>  2 AUT      1981 Component 1 -0.275 
#>  3 AUT      1982 Component 1  0.0416
#>  4 AUT      1983 Component 1  0.176 
#>  5 AUT      1984 Component 1  0.285 
#>  6 AUT      1985 Component 1  0.471 
#>  7 AUT      1986 Component 1  0.134 
#>  8 AUT      1987 Component 1  0.191 
#>  9 AUT      1988 Component 1  0.515 
#> 10 AUT      1989 Component 1  0.472 
#> # ℹ 3,805 more rows
```
