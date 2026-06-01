# Obtain Coefficients from Scaled SVD of World Marriage Data

Obtain coefficients from a scaled SVD of marriage data from the UN
Population Division's [World Marriage
Data](https://www.un.org/development/desa/pd/data/world-marriage-data).
The coefficients are a scaled version of the \\U\\ matrix from the SVD.

## Usage

``` r
coef_wmd(file, n_comp = 5, status = c("current", "ever"), eps = 1e-05)
```

## Arguments

- file:

  Path to excel spreadsheet holding World Marriage Data

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- status:

  `"current"` (the default) or `"ever"`. Whether to report results for
  the marriage staus "currently married" or "ever-married". Note that
  both statuses include consensual and de facto unions, in addition to
  legal marriages.

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

- [`data_ssvd_wmd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_wmd.md)
  Put World Marriage Data into the format required by the `ssvd()`
  function in bage

- [`tidy_wmd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_wmd.md)
  Format World Marriage Data into a tidy data frame.

## Examples

``` r
file <- system.file(
  "extdata",
  "undesa_pd_2019_wmd_marital_status_subset.xlsx",
  package = "bssvd"
)
coef_wmd(file)
#> # A tibble: 60 × 5
#>    sex    country           time component      coef
#>    <chr>  <chr>            <int> <chr>         <dbl>
#>  1 Female Afghanistan.2015     5 Component 1  1.03  
#>  2 Female Albania           2000 Component 1  1.09  
#>  3 Female Albania           2001 Component 1 -0.189 
#>  4 Female Albania.2008         5 Component 1  0.339 
#>  5 Female Albania           2011 Component 1 -1.22  
#>  6 Female Albania.2017         5 Component 1 -1.05  
#>  7 Female Afghanistan.2015     5 Component 2  1.88  
#>  8 Female Albania           2000 Component 2 -0.0981
#>  9 Female Albania           2001 Component 2  0.185 
#> 10 Female Albania.2008         5 Component 2 -0.356 
#> # ℹ 50 more rows
```
