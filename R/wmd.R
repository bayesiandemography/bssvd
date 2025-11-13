
## User-visible ---------------------------------------------------------------

## HAS_TESTS
#' Obtain Coefficients from Scaled SVD of
#' World Marriage Data
#' 
#' Obtain coefficients from a
#' scaled SVD of marriage data from the
#' UN Population Division's
#' [World Marriage Data](https://www.un.org/development/desa/pd/data/world-marriage-data).
#' The coefficients are a 
#' scaled version of the \eqn{U} matrix
#' from the SVD.
#'
#' @section Truncation and transformation:
#' 
#' Reported proportions outside `(eps, 1-eps)`
#' are shifted to `eps` or `1-eps`, and
#' all values are logit-transformed,
#' before the SVD is applied.
#'
#' @param file Path to excel spreadsheet
#' holding World Marriage Data
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#' @param status `"current"` (the default)
#' or `"ever"`. Whether to report results for
#' the marriage staus "currently married"
#' or "ever-married". Note that both statuses
#' include consensual and de facto unions,
#' in addition to legal marriages.
#' @param eps Parameter for truncating
#' probabililities or rates.
#' Default is `0.00001`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_wmd()] Put World Marriage Data
#'   into the format required by
#'   the `ssvd()` function in \pkg{bage}
#' - [tidy_wmd()] Format World Marriage Data
#'   into a tidy data frame.
#' 
#' @examples
#' file <- system.file(
#'   "extdata",
#'   "undesa_pd_2019_wmd_marital_status_subset.xlsx",
#'   package = "bssvd"
#' )
#' coef_wmd(file)
#' @export
coef_wmd <- function(file,
                     n_comp = 5,
                     status = c("current", "ever"),
                     eps = 0.00001) {
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 6L,
                    divisible_by = NULL)
  check_eps(eps)
  n_comp <- as.integer(n_comp)
  status <- match.arg(status)
  data <- tidy_wmd(file = file,
                   status = status)
  wmd_calculate_coef(data = data,
                     n_comp = n_comp,
                     eps = eps)
}


## HAS_TESTS
#' Process World Marriage Data
#' 
#' Process marriage data downloaded from
#' the UN Population Division's
#' [World Marriage Data](https://www.un.org/development/desa/pd/data/world-marriage-data),
#' so it is ready to create a scaled SVD.
#'
#' @section Usage:
#' **Step 1: Download data**
#'
#' Go to to page [World Marriage Data](https://www.un.org/development/desa/pd/data/world-marriage-data).
#' Go to the data tab on the right. Download the most
#' recent excel spreadsheet (which as of October 2025 is called
#' `undesa_pd_2019_wmd_marital_status.xlsx`).
#'
#' **Step 2: Call function `data_ssvd_wmd()`**
#'
#' ```
#' wmd_data <- data_ssvd_wmd("undesa_pd_2019_wmd_marital_status.xlsx")
#' ```
#'
#' @section coef_wmd Truncation and transformation
#'
#' @inheritParams coef_wmd
#'
#' @returns A tibble
#'
#' @seealso
#' - [coef_wmd()] Obtain time series
#'   of SVD coefficients
#'   from World Marriage Data
#' - [tidy_wmd()] Format World Marriage Data
#'   into a tidy data frame.
#' 
#' @examples
#' file <- system.file(
#'   "extdata",
#'   "undesa_pd_2019_wmd_marital_status_subset.xlsx",
#'   package = "bssvd"
#' )
#' coef_wmd(file)
#' @export
data_ssvd_wmd <- function(file,
                          n_comp = 5,
                          status = c("current", "ever"),
                          eps = 0.00001) {
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 10L,
                    divisible_by = NULL)
  check_eps(eps)
  n_comp <- as.integer(n_comp)
  status <- match.arg(status)
  cli::cli_progress_message("Tidying data...")
  data <- tidy_wmd(file = file, status = status)
  labels_age <- wmd_make_labels_age()
  cli::cli_progress_message("Carrying out SVD for 'indep'...")
  indep <- wmd_indep(data = data,
                     labels_age = labels_age,
                     n_comp = n_comp,
                     eps = eps)
  cli::cli_progress_message("Carrying out SVD for 'joint'...")
  joint <- wmd_joint(data = data,
                     labels_age = labels_age,
                     n_comp = n_comp,
                     eps = eps)
  cli::cli_progress_message("Combining results...")
  data <- vctrs::vec_rbind(indep, joint)
  data <- tibble::as_tibble(data)
  data
}


## HAS_TESTS
#' Tidy World Marriage Data
#' 
#' Tidy data downloaded from
#' the UN Population Division's
#' [World Marriage Data](https://www.un.org/development/desa/pd/data/world-marriage-data),
#' database.
#'
#' @inheritParams coef_wmd
#'
#' @returns A tibble
#'
#' @seealso
#' - [coef_wmd()] Obtain time series
#'   of SVD coefficients
#'   from World Marriage Data
#' - [data_ssvd_wmd()] Put World Marriage Data
#'   into the format required by
#'   the `ssvd()` function in \pkg{bage}
#' 
#' @examples
#' file <- system.file(
#'   "extdata",
#'   "undesa_pd_2019_wmd_marital_status_subset.xlsx",
#'   package = "bssvd"
#' )
#' tidy_wmd(file)
#' @export
tidy_wmd <- function(file, status = c("current", "ever")) {
  col_types <- c("text",
                 "skip",
                 rep("numeric", 2),
                 rep("text", 2),
                 rep("skip", 2),
                 "numeric",
                 rep("skip", 8))
  col_names <- c("country",
                 "time_start",
                 "time_end",
                 "sex",
                 "age",
                 "value")
  age_keep <- poputils::age_labels(type = "five",
                                   min = 15,
                                   max = 75,
                                   open = TRUE)
  status <- match.arg(status)
  if (status == "current")
    sheet <- "CURRENTLY MARRIED"
  else
    sheet <- "EVER_MARRIED"
  ans <- readxl::read_xlsx(file,
                           sheet = sheet,
                           col_names = col_names,
                           col_types = col_types,
                           skip = 3)
  ans$age <- gsub("\\[|\\]", "", ans$age)
  ans <- ans[ans$age %in% age_keep, , drop = FALSE]
  ans$sex[ans$sex == "Men"] <- "Male"
  ans$sex[ans$sex == "Women"] <- "Female"
  ans$time <- 0.5 * (ans$time_start + ans$time_end)
  ans$value <- ans$value / 100
  ans <- stats::aggregate(ans["value"],
                          ans[c("country",
                                "time",
                                "sex",
                                "age")],
                          mean)
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Obtain the Scaled 'U' Matrix From an SVD of
#' World Marriage Data 
#'
#' @param data A data frame, typically produced by 'tidy_wmd'.
#' @param n_comp Number of components.
#' @param eps Truncation parameter
#'
#' @returns A tibble
#' 
#' @noRd
wmd_calculate_coef <- function(data, n_comp, eps) {
  calculate_coef(data = data,
                 n_comp = n_comp,
                 transform = "logit",
                 eps = eps)
}


## HAS_TESTS
#' Prepare Inputs for "indep" Type, ie Female and Male Separate
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#' @param eps Truncation parameter
#'
#' @returns A data frame
#' 
#' @noRd
wmd_indep <- function(data, labels_age, n_comp, eps) {
  make_indep(data = data,
             labels_age = labels_age,
             n_comp = n_comp,
             eps = eps)
}

## HAS_TESTS
#' Prepare Inputs for "joint" Type, ie Female and Male Concatenated
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#' @param eps Truncation parameter
#'
#' @returns A data frame
#'
#' @noRd
wmd_joint <- function(data, labels_age, n_comp, eps) {
  make_joint(data = data,
             labels_age = labels_age,
             n_comp = n_comp,
             eps = eps)
}


## HAS_TESTS
#' Make Age Labels Used by WMD
#'
#' @returns List of character vectors
#'
#' @noRd
wmd_make_labels_age <- function() {
  labels_all <- poputils::age_labels(type = "five",
                                     min = 15,
                                     max = 75,
                                     open = TRUE)
  lapply(7:13, function(i) labels_all[1:i])
}
