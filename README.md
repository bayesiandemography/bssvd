
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bssvd

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/bssvd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/bssvd/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Use data from international demographic databases to create objects that
capture regularities in demographic rates.

**bssvd** supports package
[bage](https://github.com/bayesiandemography/bage), and is not intended
to be used directly for data analysis.

## Installation

You can install the development version of bssvd from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bayesiandemography/bssvd")
```

## Example

``` r
library(bssvd)
library(rsdmx)
url <- paste("https://sdmx.oecd.org/public/rest/data",
             "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
             "all?dimensionAtObservation=AllDimensions",
             sep = "/")
lfp_sdmx <- rsdmx::readSDMX(url)
lfp_df <- as.data.frame(lfp_sdmx)
LFP <- svd_lfp(data)
```
