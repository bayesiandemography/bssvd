# Prepare OECD Data on Labour Force Participation Rates

Process labour force participation data downloaded from the [OECD Data
Explorer](https://data-explorer.oecd.org) so it is ready to create a
scaled SVD.

## Usage

``` r
data_ssvd_lfp(data, age_max = 75, n_comp = 5, year_min = NULL, eps = 1e-05)
```

## Arguments

- data:

  A data frame containing OECD data.

- age_max:

  The upper limit of the oldest closed age group. Default is `75`.

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

## Usage

**Step 1: Download data**

    library(rsdmx)
    url <- paste("https://sdmx.oecd.org/public/rest/data",
                 "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
                 "all?dimensionAtObservation=AllDimensions",
                 sep = "/")
    lfp_sdmx <- rsdmx::readSDMX(url)    ## can be slow
    lfp_df <- as.data.frame(lfp_sdmx)   ## can also be slow

**Step 2: Call function `data_ssvd_lfp()`**

    lfp_data <- data_ssvd_lfp(data)

## coef_lfp Truncation and transformation

## See also

- [`tidy_lfp()`](https://bayesiandemography.github.io/bssvd/reference/tidy_lfp.md)
  Format OECD labour force participation data into a tidy data frame.

- [`coef_lfp()`](https://bayesiandemography.github.io/bssvd/reference/coef_lfp.md)
  Obtain time series of SVD coefficients for OECD labour force
  participation data

## Examples

``` r
data <- data_ssvd_lfp(oecd_lfp_subset)
#> Tidying data...
#> Carrying out SVD for 'total'...
#> Carrying out SVD for 'indep'...
#> Carrying out SVD for 'joint'...
#> Combining results...
data
#> # A tibble: 18 × 5
#>    type  labels_age labels_sexgender matrix          offset    
#>    <chr> <list>     <list>           <list>          <list>    
#>  1 total <chr [10]> <NULL>           <dgCMatrx[,5]>  <dbl [10]>
#>  2 total <chr [11]> <NULL>           <dgCMatrx[,5]>  <dbl [11]>
#>  3 total <chr [12]> <NULL>           <dgCMatrx[,5]>  <dbl [12]>
#>  4 total <chr [11]> <NULL>           <dgCMatrx[,5]>  <dbl [11]>
#>  5 total <chr [12]> <NULL>           <dgCMatrx[,5]>  <dbl [12]>
#>  6 total <chr [13]> <NULL>           <dgCMatrx[,5]>  <dbl [13]>
#>  7 indep <chr [20]> <chr [20]>       <dgTMatrx[,10]> <dbl [20]>
#>  8 indep <chr [22]> <chr [22]>       <dgTMatrx[,10]> <dbl [22]>
#>  9 indep <chr [24]> <chr [24]>       <dgTMatrx[,10]> <dbl [24]>
#> 10 indep <chr [22]> <chr [22]>       <dgTMatrx[,10]> <dbl [22]>
#> 11 indep <chr [24]> <chr [24]>       <dgTMatrx[,10]> <dbl [24]>
#> 12 indep <chr [26]> <chr [26]>       <dgTMatrx[,10]> <dbl [26]>
#> 13 joint <chr [20]> <chr [20]>       <dgCMatrx[,5]>  <dbl [20]>
#> 14 joint <chr [22]> <chr [22]>       <dgCMatrx[,5]>  <dbl [22]>
#> 15 joint <chr [24]> <chr [24]>       <dgCMatrx[,5]>  <dbl [24]>
#> 16 joint <chr [22]> <chr [22]>       <dgCMatrx[,5]>  <dbl [22]>
#> 17 joint <chr [24]> <chr [24]>       <dgCMatrx[,5]>  <dbl [24]>
#> 18 joint <chr [26]> <chr [26]>       <dgCMatrx[,5]>  <dbl [26]>
```
