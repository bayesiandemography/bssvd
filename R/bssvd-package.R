
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
#' Sample data:
#' 
#' - [asfr_subset()] Subset of HFD data
#'
#' Functions:
#' 
#' - [data_ssvd_hfd()] Prepare HFD data to created scaled SVD object
#' - [tidy_hfd()] Initial tidying of HFD data
#' - [coef_hfd()] Extract coef from scaled SVD of HFD data
#'
#' @section Human Mortality Database:
#'
#' Functions:
#' 
#' - [data_ssvd_hmd()] Prepare HMD data to created scaled SVD object
#' - [tidy_hmd()] Initial tidying of HMD data
#' - [coef_hmd()] Extract coef from scaled SVD of HMD data
#'
#' @section OECD Labour Force Participation data:
#'
#' Sample data:
#'
#' - [oecd_lfp_subset()] Subset of labour force data
#'
#' Functions:
#' 
#' - [data_ssvd_lfp()] Prepare labour force data to created scaled-SVD object
#' - [tidy_lfp()] Initial tidying of LFP data
#' - [coef_lfp()] Extract coef from scaled SVD of labour force data
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
