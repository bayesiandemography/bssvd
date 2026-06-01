# Prepare Data from Human Internal Migration Database

Process data on age-specific migration probabilities from the Human
Internal Migration Database.

## Usage

``` r
data_ssvd_himd(
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
  probabilities or as rates. See `data_ssvd_himd()` for details. Default
  is `"rate"`.

- n_comp:

  Number of SVD components to include in result. Default is `5`.

- eps:

  Parameter for truncating probabililities or rates. Default is
  `0.00001`.

## Value

A tibble

## Usage

**Step 1: Download data**

Download the data from Dyrting, S. (2024, October 23). *Data from:
Estimating Complete Migration Probabilities from Grouped Data* at
https://osf.io/vmrfk/. The data is in the "Migration" folder under the
"Files" tab. The facility for downloading all the files at once does not
appear to be working, but CSV files can be downloaded one by one. Zip
the folder containing these CSV files.

\*\*Step 2: Call function `data_ssvd_himd()`

Create three datasets:

    himd_data_rate <- data_svd_himd(zipfile = "himd.zip",
                                    time_interval = 1,
                                    measure_type = "rate")
    himd_data_prob1 <- data_svd_himd(zipfile = "himd.zip",
                                     time_interval = 1,
                                     measure_type = "prob")
    himd_data_prob5 <- data_svd_himd(zipfile = "himd.zip",
                                     time_interval = 5,
                                     measure_type = "prob")

## Rates or probabilities

The original data are probabilities. However, `data_ssvd_himd()` (and
[`coef_himd()`](https://bayesiandemography.github.io/bssvd/reference/coef_himd.md))
allow values where `time_interval` is 1 to be to treated as rates. If
more than one change in residence in a year is unusual, then one-year
probabilities and should in fact be very similar.

## coef_himd Truncation and transformation

## See also

- [`coef_hfd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hfd.md)
  Obtain time series of SVD coefficients for Human Internal Migration
  Database data

- [`tidy_hmd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hmd.md)
  Format data from the Human Internal Migration Database into a tidy
  data frame

## Examples

``` r
zipfile <- system.file("extdata", "himd_20241023_subset.zip",
                       package = "bssvd")
data_ssvd_himd(zipfile)
#> Unzipping file...
#> Creating five-year age groups...
#> Assembling datasets for alternative open age groups...
#> Carrying out SVD...
#> # A tibble: 22 × 5
#>    type  labels_age  labels_sexgender matrix         offset     
#>    <chr> <list>      <list>           <list>         <list>     
#>  1 total <chr [61]>  <NULL>           <dgCMatrx[,5]> <dbl [61]> 
#>  2 total <chr [66]>  <NULL>           <dgCMatrx[,5]> <dbl [66]> 
#>  3 total <chr [71]>  <NULL>           <dgCMatrx[,5]> <dbl [71]> 
#>  4 total <chr [76]>  <NULL>           <dgCMatrx[,5]> <dbl [76]> 
#>  5 total <chr [81]>  <NULL>           <dgCMatrx[,5]> <dbl [81]> 
#>  6 total <chr [86]>  <NULL>           <dgCMatrx[,5]> <dbl [86]> 
#>  7 total <chr [91]>  <NULL>           <dgCMatrx[,5]> <dbl [91]> 
#>  8 total <chr [96]>  <NULL>           <dgCMatrx[,5]> <dbl [96]> 
#>  9 total <chr [101]> <NULL>           <dgCMatrx[,5]> <dbl [101]>
#> 10 total <chr [106]> <NULL>           <dgCMatrx[,5]> <dbl [106]>
#> # ℹ 12 more rows
```
