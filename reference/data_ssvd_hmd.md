# Prepare Data from Human Mortality Database

Process data on age-specific mortality rates from a zipped file
downloaded from the [Human Mortality
Database](https://www.mortality.org/Data/ZippedDataFiles) so that it is
ready to created a scaled SVD.

## Usage

``` r
data_ssvd_hmd(
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

## Usage

**Step 1: Download data**

Register or log in at the Human Mortality Database, and go to page
[Downloading the HMD in zipped data
files](https://www.mortality.org/Data/ZippedDataFiles). Go to the
"Previous Versions" table at the bottom of the page, and download a file
from the "Statistics" column, eg file

https://www.mortality.org/File/Download/hmd.v6/zip/all_hmd/hmd_statistics_20240226.zip

**Step 2: Call function `data_ssvd_hmd()`**

Put the file into the working directory, and supply the name of the file
to function `data_ssvd_hmd()`, eg

    hmd_data <- data_ssvd_hmd("hmd_statistcs_20240226")

## Truncation and transformation

Rates below `eps` are shifted to `eps`, and all rates are
log-transformed, before the SVD is applied.

## See also

- [`coef_hfd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hfd.md)
  Obtain time series of SVD coefficients for Human Mortality Database
  data

- [`tidy_hmd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hmd.md)
  Format data from the Human Mortality Database into a tidy data frame

## Examples

``` r
zipfile <- system.file("extdata", "hmd_statistics_20250925_subset.zip",
                       package = "bssvd")
data <- data_ssvd_hmd(zipfile)
#> Unzipping file...
#> Tidying data...
#> Creating five-year age groups...
#> Assembling datasets for alternative open age groups...
#> Carrying out SVD for 'total'...
#> Carrying out SVD for 'indep'...
#> Carrying out SVD for 'joint'...
#> Combining results...
data
#> # A tibble: 99 × 5
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
#> # ℹ 89 more rows
```
