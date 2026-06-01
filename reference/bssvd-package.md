# Functions to Create Objects Describing Regularities in Demographic Rates

Functions for preparing data for objects that describe regularities in
demographic rates. The descriptions are obtained by applying singular
value decompositions to data in international databases. These packages
support package **bage**. They are designed for use by developers, not
data analysts.

## Human Fertility Database

Sample data:

- [`asfr_subset()`](https://bayesiandemography.github.io/bssvd/reference/asfr_subset.md)
  Subset of HFD data

Functions:

- [`data_ssvd_hfd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_hfd.md)
  Prepare HFD data to created scaled SVD object

- [`tidy_hfd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hfd.md)
  Initial tidying of HFD data

- [`coef_hfd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hfd.md)
  Extract coef from scaled SVD of HFD data

## Human Mortality Database

Functions:

- [`data_ssvd_hmd()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_hmd.md)
  Prepare HMD data to created scaled SVD object

- [`tidy_hmd()`](https://bayesiandemography.github.io/bssvd/reference/tidy_hmd.md)
  Initial tidying of HMD data

- [`coef_hmd()`](https://bayesiandemography.github.io/bssvd/reference/coef_hmd.md)
  Extract coef from scaled SVD of HMD data

## OECD Labour Force Participation data

Sample data:

- [`oecd_lfp_subset()`](https://bayesiandemography.github.io/bssvd/reference/oecd_lfp_subset.md)
  Subset of labour force data

Functions:

- [`data_ssvd_lfp()`](https://bayesiandemography.github.io/bssvd/reference/data_ssvd_lfp.md)
  Prepare labour force data to created scaled-SVD object

- [`tidy_lfp()`](https://bayesiandemography.github.io/bssvd/reference/tidy_lfp.md)
  Initial tidying of LFP data

- [`coef_lfp()`](https://bayesiandemography.github.io/bssvd/reference/coef_lfp.md)
  Extract coef from scaled SVD of labour force data

## General purpose functions

- [`make_matrix_and_offset()`](https://bayesiandemography.github.io/bssvd/reference/make_matrix_and_offset.md)
  Create components for scaled SVD object

## See also

Useful links:

- <https://bayesiandemography.github.io/bssvd/>

## Author

**Maintainer**: John Bryant <john@bayesiandemography.com>

Authors:

- John Bryant <john@bayesiandemography.com>
