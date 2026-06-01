# bssvd

Use data from international demographic databases to create objects that
describe regularities in demographic rates.

**bssvd** supports package
[bage](https://github.com/bayesiandemography/bage), and is not intended
to be used directly for data analysis.

## Installation

``` r

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
lfp <- data_ssvd_lfp(lfp_df)
```
