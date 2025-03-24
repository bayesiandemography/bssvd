
#' Functions to Create Objects Describing Regularities in
#' Demographic Rates
#'
#' Functions for preparing data for objects that describe
#' regularities in demographic rates. The descriptions
#' are obtained by applying singular value decompositions
#' to data in international databases. These packages
#' support package **bage**. They are designed for use
#' by developers, not data analysts.
#'
#' @section Human Fertility Database:
#'
#' - [data_ssvd_hfd()] Prepare HFD data to created scaled SVD object
#' - [hfd_tidy()] Initial tidying of HFD data
#' - [coef_hfd()] Extract coef from scaled SVD of HFD data
#' - [asfr_subset()] Subset of HFD data
#'
#' @section Human Mortality Database:
#'
#' - [data_ssvd_hmd()] Prepare HMD data to created scaled SVD object
#' - [coef_hmd()] Extract coef from scaled SVD of HMD data
#'
#' @section OECD Labour Force Participation data:
#'
#' - [data_ssvd_lfp()] Prepare labour force data to created scaled-SVD object
#' - [coef_lfp()] Extract coef from scaled SVD of labour force data
#' - [oecd_lfp_subset()] Subset of labour force data
#'
#' @section General purpose functions:
#'
#' - [make_matrix_and_offset()] Create components for scaled SVD object
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
