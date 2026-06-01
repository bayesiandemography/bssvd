# Prepare Data from the Human Fertility Database

Process age-specific fertility data from the the [Human Fertility
Database](https://www.humanfertility.org/Home/Index) so that it is ready
to create a scaled SVD.

## Usage

``` r
data_ssvd_hfd(
  data,
  age_min_max = 15,
  age_max_min = 50,
  n_comp = 5,
  eps = 1e-05
)
```

## Arguments

- data:

  A data frame containing HFD data.

- age_min_max, age_max_min:

  Every age classification must at least span the range
  `[age_min_max, age_max_min)`. Defaults are `15` and `50`.

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- eps:

  Floor for rates. Default is `0.00001`.

## Value

A tibble

## Usage

**Step 1: Download data**

Register or log in at the Human Fertility Database, and go to page
[ZippedDataFiles](https://www.humanfertility.org/Data/ZippedDataFiles).
Go to the "By statistic" table, and download the file from the
"Age-specific fertility rate" row.

**Step 2: Extract `asfrRR.txt`**

Extract the file `asfrRR.txt` from the downloaded data, and read in the
contents,

    asfr <- readr::read_table("asfrRR.txt", skip = 2)

**Step 3: Call function `data_ssvd_hfd()`**

    hfd_data <- data_ssvd_hfd(asfr)

## Lowest and highest ages

The original HFD data contains age groups `"12-"` (ie 12 and younger)
and `"55+"` (ie 55 and older). We treat these as closed intervals `"12"`
and `"55"`.

If an age classification in the return value covers a narrower range
than the data itself, then the youngest age group in the return value
includes ASFRs from younger age groups in the data, and the oldest age
group in the return value includes ASFRs from older age groups in the
data. For instance, if the classification in the return value starts at
age 15, but the data starts at age 12, the ASFR for age 15 includes
ASFRs from ages 12, 13, and 14. This shifting of ASFRs is common in
analyses of fertility.

## Truncation and transformation

Rates below `eps` are shifted to `eps`, and all rates are
log-transformed, before the SVD is applied.

## See also

- [`coef_hfd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hfd.md)
  Obtain time series of SVD coefficients from Human Fertility Database
  data

- [`tidy_hfd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hfd.md)
  Format data from the Human Fertility Database into a tidy data frame

## Examples

``` r
data <- data_ssvd_hfd(asfr_subset)
#> Tidying data...
#> Carrying out SVD for 'total'...
data
#> # A tibble: 34 × 5
#>    type  labels_age labels_sexgender matrix         offset    
#>    <chr> <list>     <list>           <list>         <list>    
#>  1 total <chr [38]> <NULL>           <dgCMatrx[,5]> <dbl [38]>
#>  2 total <chr [39]> <NULL>           <dgCMatrx[,5]> <dbl [39]>
#>  3 total <chr [40]> <NULL>           <dgCMatrx[,5]> <dbl [40]>
#>  4 total <chr [41]> <NULL>           <dgCMatrx[,5]> <dbl [41]>
#>  5 total <chr [42]> <NULL>           <dgCMatrx[,5]> <dbl [42]>
#>  6 total <chr [43]> <NULL>           <dgCMatrx[,5]> <dbl [43]>
#>  7 total <chr [44]> <NULL>           <dgCMatrx[,5]> <dbl [44]>
#>  8 total <chr [37]> <NULL>           <dgCMatrx[,5]> <dbl [37]>
#>  9 total <chr [38]> <NULL>           <dgCMatrx[,5]> <dbl [38]>
#> 10 total <chr [39]> <NULL>           <dgCMatrx[,5]> <dbl [39]>
#> # ℹ 24 more rows
```
