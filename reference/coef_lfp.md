# Obtain Coefficients from Scaled SVD of OCED Labour Force Participation Data

Obtain coefficients from a scaled SVD of OCED labour force participation
data downloaded from the [OECD Data
Explorer](https://data-explorer.oecd.org). The coefficients are a scaled
version of the \\U\\ matrix from the SVD.

## Usage

``` r
coef_lfp(data, n_comp = 5, year_min = NULL, eps = 1e-05)
```

## Arguments

- data:

  A data frame containing OECD data.

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- year_min:

  Only include data for `year_min` onwards. Ignored if `year_min` is
  `NULL` (the default).

- eps:

  Parameter for truncating probabililities or rates. Default is
  `0.00001`.

## Value

A tibble

## Truncation and transformation

Reported proportions outside `(eps, 1-eps)` are shifted to `eps` or
`1-eps`, and all values are logit-transformed, before the SVD is
applied.

## See also

- [`data_ssvd_lfp()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_lfp.md)
  Put OECD labour force participation data into the format required by
  the `ssvd()` function in bage

- [`tidy_lfp()`](https://bayesiandemography.github.io/bssvd/reference/tidy_lfp.md)
  Format OECD labour force participation data into a tidy data frame.

## Examples

``` r
coef_lfp(oecd_lfp_subset)
#> # A tibble: 2,235 × 5
#>    sex    country  time component     coef
#>    <chr>  <chr>   <int> <chr>        <dbl>
#>  1 Female AUS      2010 Component 1 -0.717
#>  2 Female AUS      2011 Component 1 -0.675
#>  3 Female AUS      2012 Component 1 -0.681
#>  4 Female AUT      2010 Component 1  0.410
#>  5 Female AUT      2011 Component 1  0.496
#>  6 Female AUT      2012 Component 1  0.593
#>  7 Female BEL      2010 Component 1  0.777
#>  8 Female BEL      2011 Component 1  0.537
#>  9 Female BEL      2012 Component 1  0.667
#> 10 Female BGR      2010 Component 1  0.808
#> # ℹ 2,225 more rows
```
