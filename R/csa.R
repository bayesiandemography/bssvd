
## User-visible ---------------------------------------------------------------

## HAS_TESTS
#' Obtain Coefficients from Scaled SVD of
#' Census School Attendance Data
#' 
#' Obtain coefficients from a
#' scaled SVD of school attendance data.
#' The attendance data is derived from
#' population censuses by the United Nations Statistics Division.
#' The data is available in the
#' "Population 5 to 24 years of age by school
#' attendance, sex and urban/rural residence" table in
#' from the
#' [Population Censuses' Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets). 
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
#' @param data Data frame with attendance data.
#' (Data from multiple files combined, but otherwise
#' unprocessed).
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_csa()] Put school attendance data
#'   into the format required by
#'   the `ssvd()` function in \pkg{bage}
#' - [tidy_csa()] Format school attendance data
#'   into a tidy data frame.
#' 
#' @examples
#' coef_csa(un_csa_subset)
#' @export
coef_csa <- function(data,
                     n_comp = 5,
                     eps = 0.00001) {
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 6L,
                    divisible_by = NULL)
  n_comp <- as.integer(n_comp)
  check_eps(eps)
  data <- tidy_csa(data)
  sch_calculate_coef(data = data,
                     n_comp = n_comp,
                     eps = eps)
}


## HAS_TESTS
#' Prepare World Marriage Data
#' 
#' Process marriage data downloaded from
#' the UN Statistics Division's
#' [Population Censuses' Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets),
#' collection so it is ready to create a scaled SVD.
#'
#' @section Usage:
#' **Step 1: Download data**
#'
#' Go to to page
#' [Population Censuses' Datasets](https://unstats.un.org/unsd/demographic-social/products/dyb/index.cshtml#censusdatasets),
#' Go to the table "Population 15 years of age and over by educational attainment, age and sex".
#' Go to the download tab, and download. Only 100,000 rows can be downloaded at one time
#' so this needs to be done in two steps.
#'
#' **Step 2: Combine datasets**
#'
#' Read in the downloadeded files, and combine into a single
#' data frame.
#'
#' **Step 3: Call function `data_ssvd_csa()`**
#'
#' ```
#' csa_data <- data_ssvd_ssvd(data)
#' ```
#'
#' @section coef_csa Truncation and transformation
#'
#' @inheritParams coef_csa
#'
#' @returns A tibble
#'
#' @seealso
#' - [coef_csa()] Obtain time series
#'   of SVD coefficients
#'   from World Marriage Data
#' - [tidy_csa()] Format World Marriage Data
#'   into a tidy data frame.
#' 
#' @examples
#' data_ssvd_csa(un_csa_subset)
#' @export
data_ssvd_csa <- function(data,
                          n_comp = 5,
                          eps = 0.00001) {
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 10L,
                    divisible_by = NULL)
  n_comp <- as.integer(n_comp)
  check_eps(eps)
  cli::cli_progress_message("Tidying data...")
  data <- tidy_csa(data)
  labels_age <- csa_make_labels_age()
  cli::cli_progress_message("Carrying out SVD for 'indep'...")
  indep <- wmd_indep(data = data, labels_age = labels_age, n_comp = n_comp)
  cli::cli_progress_message("Carrying out SVD for 'joint'...")
  joint <- wmd_joint(data = data, labels_age = labels_age, n_comp = n_comp)
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
#' [World Migration Data](https://www.un.org/development/desa/pd/data/world-marriage-data),
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
#' tidy_wmd(un_csa_subset)
#' @export
tidy_csa <- function(data) {
  data <- data[data$Area == "Total", , drop = FALSE]
  data <- data[data$Age %in% 5:24, ]
  names(data)[match("Country or Area", names(data))] <- "country"
  names(data)[match("Age", names(data))] <- "age"
  names(data)[match("Sex", names(data))] <- "sex"
  names(data)[match("Year", names(data))] <- "time"
  names(data)[match("School attendance", names(data))] <- "school"
  data$age <- as.integer(data$age)
  data$sex[data$sex == "Both Sexes"] <- "Total"
  data <- data[c("country", "time", "sex", "age", "school", "Value")]
  data <- data[data$country != "Niue", , drop = FALSE] ## numbers too small
  data <- data[data$country != "Tokelau", , drop = FALSE] ## numbers too small
  data <- data[!(data$country == "Montenegro" &
                   data$time == 2003), , drop = FALSE] ## data error
  data <- data[data$country != "Canada", , drop = FALSE] ## data error
  data <- data[data$country != "Liechtenstein", , drop = FALSE] ## data error
  data <- data[!(data$country == "French Polynesia" &
                   data$time == 2017), , drop = FALSE] ## data error
  data <- data[!(data$country == "Algeria" &
                   data$sex == "Total"), , drop = FALSE] ## data error
  data$school <- gsub(" ", ".", data$school)
  data$school <- tolower(data$school)
  data <- as.data.frame(data)
  data <- stats::reshape(data,
                         idvar = c("country", "time", "sex", "age"),
                         timevar = "school",
                         direction = "wide")
  data <- tibble::tibble(data)
  data$value <- data$Value.attending.school /
    (data$Value.attending.school +
       data$Value.not.attending.school)
  data$value <- ifelse(is.na(data$value),
                        data$Value.attending.school / data$Value.total,
                        data$value)
  data <- data[c("country", "time", "sex", "age", "value")]
  data
}

## HAS_TESTS
#' Make Age Labels Used by CSA
#'
#' @returns List of character vectors
#'
#' @noRd
csa_make_labels_age <- function() {
  lapply(15:24, function(i) seq.int(from = 5L, to = i))
}


## HAS_TESTS
#' Prepare Inputs for "total" Type, ie Female and Male Combined
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#' @param eps Truncation of probabilities
#'
#' @returns A data frame
#'
#' @noRd
csa_total <- function(data, labels_age, n_comp, eps) {
  data <- data[data$sex == "Total", ]
  data_split <- .mapply(get_data_one,
                        dots = list(labels_age = labels_age),
                        MoreArgs = list(data = data))
  x_split <- .mapply(poputils::to_matrix,
                     dots = list(x = data_split),
                     MoreArgs = list(rows = "age",
                                     cols = c("country", "time"),
                                     measure = "value"))
  x_split <- lapply(x_split, remove_cols_with_na, n_comp = n_comp)
  ssvd_split <- lapply(x_split,
                       make_matrix_and_offset,
                       transform = "logit",
                       n_comp = n_comp,
                       eps = eps)
  matrix <- lapply(ssvd_split, function(x) x$matrix)
  offset <- lapply(ssvd_split, function(x) x$offset)
  tibble::tibble(type = "total",
                 labels_age = labels_age,
                 labels_sexgender = list(NULL),
                 matrix = matrix,
                 offset = offset)
}
