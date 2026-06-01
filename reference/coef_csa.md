# Obtain Coefficients from Scaled SVD of Census School Attendance Data

Obtain coefficients from a scaled SVD of school attendance data. The
attendance data is derived from population censuses by the United
Nations Statistics Division. The data is available in the "Population 5
to 24 years of age by school attendance, sex and urban/rural residence"
table in from the [Population Censuses'
Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets).
The coefficients are a scaled version of the \\U\\ matrix from the SVD.

## Usage

``` r
coef_csa(data, n_comp = 5, eps = 1e-05)
```

## Arguments

- data:

  Data frame with attendance data. (Data from multiple files combined,
  but otherwise unprocessed).

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- eps:

  Parameter for truncating probabililities. Default is `0.00001`.

## Value

A tibble

## Truncation and transformation

Reported proportions outside `(eps, 1-eps)` are shifted to `eps` or
`1-eps`, and all values are logit-transformed, before the SVD is
applied.

## See also

- [`data_ssvd_csa()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_csa.md)
  Put school attendance data into the format required by the `ssvd()`
  function in bage

- [`tidy_csa()`](https://bayesiandemography.github.io/bssvd/reference/tidy_csa.md)
  Format school attendance data into a tidy data frame.

## Examples

``` r
coef_csa(un_csa_subset)
#> # A tibble: 150 × 5
#>    sex    country    time component     coef
#>    <chr>  <chr>     <int> <chr>        <dbl>
#>  1 Female Australia  2001 Component 1 -1.07 
#>  2 Female Australia  2006 Component 1 -1.10 
#>  3 Female Australia  2011 Component 1 -1.21 
#>  4 Female Australia  2021 Component 1 -1.23 
#>  5 Female Bulgaria   2001 Component 1  0.931
#>  6 Female Bulgaria   2011 Component 1  0.903
#>  7 Female Bulgaria   2021 Component 1  0.459
#>  8 Female Mexico     2000 Component 1  0.839
#>  9 Female Mexico     2005 Component 1  0.710
#> 10 Female Mexico     2020 Component 1  0.761
#> # ℹ 140 more rows
```
