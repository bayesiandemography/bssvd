# Prepare Census School Attendance Data

Process school attendance data, so it is ready to create a scaled SVD.

## Usage

``` r
data_ssvd_csa(data, n_comp = 5, eps = 1e-05)
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

## Usage

**Step 1: Download data**

Go to to the [Population Censuses'
Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets)
database, then to the table "Population 15 years of age and over by
educational attainment, age and sex". Go to the download tab, and
download. Only 100,000 rows can be downloaded at one time so this needs
to be done in multiple steps.

**Step 2: Combine datasets**

Read in the downloadeded files, and combine into a single data frame.

**Step 3: Call function `data_ssvd_csa()`**

    csa_data <- data_ssvd_ssvd(data)

## coef_csa Truncation and transformation

## See also

- [`coef_csa()`](https://bayesiandemography.github.io/bssvd/reference/coef_csa.md)
  Obtain time series of SVD coefficients from census cchool attendance
  data

- [`tidy_csa()`](https://bayesiandemography.github.io/bssvd/reference/tidy_csa.md)
  Format census school attendance data into a tidy data frame.

## Examples

``` r
data_ssvd_csa(un_csa_subset)
#> Tidying data...
#> Carrying out SVD for 'total'...
#> Carrying out SVD for 'indep'...
#> Carrying out SVD for 'joint'...
#> Combining results...
#> # A tibble: 33 × 5
#>    type  labels_age labels_sexgender matrix         offset    
#>    <chr> <list>     <list>           <list>         <list>    
#>  1 total <int [10]> <NULL>           <dgCMatrx[,5]> <dbl [10]>
#>  2 total <int [11]> <NULL>           <dgCMatrx[,5]> <dbl [11]>
#>  3 total <int [12]> <NULL>           <dgCMatrx[,5]> <dbl [12]>
#>  4 total <int [13]> <NULL>           <dgCMatrx[,5]> <dbl [13]>
#>  5 total <int [14]> <NULL>           <dgCMatrx[,5]> <dbl [14]>
#>  6 total <int [15]> <NULL>           <dgCMatrx[,5]> <dbl [15]>
#>  7 total <int [16]> <NULL>           <dgCMatrx[,5]> <dbl [16]>
#>  8 total <int [17]> <NULL>           <dgCMatrx[,5]> <dbl [17]>
#>  9 total <int [18]> <NULL>           <dgCMatrx[,5]> <dbl [18]>
#> 10 total <int [19]> <NULL>           <dgCMatrx[,5]> <dbl [19]>
#> # ℹ 23 more rows
```
