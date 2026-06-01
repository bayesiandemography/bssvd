
## User-visible ---------------------------------------------------------------

## HAS_TESTS
#' Obtain Coefficients from Scaled SVD of HFD Data
#'
#' Obtain time series of SVD coefficients from a
#' scaled SVD of data from the
#' [Human Fertility Database](https://www.humanfertility.org/Home/Index).
#' The coefficents are a
#' scaled version of the \eqn{U} matrix
#' from the SVD.
#'
#' @section Truncation and transformation:
#'
#' Rates below `eps` are shifted to `eps`,
#' and all rates are log-transformed,
#' before the SVD is applied. 
#'
#' @param data A data frame containing HFD data.
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#' @param eps Floor for rates.
#' Default is `0.00001`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_hfd()]Put data
#'   from the Human Fertility Database
#'   into the format required by the
#'   `ssvd()` function in \pkg{bage}.
#' - [tidy_hfd()] Format data from the
#'   Human Fertility Database into
#'   a tidy data frame
#'
#' @examples
#' coef_hfd(asfr_subset)
#' @export
coef_hfd <- function(data,
                     n_comp = 5,
                     eps = 0.00001) {
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 6L,
                    divisible_by = NULL)
  check_eps(eps)
  n_comp <- as.integer(n_comp)
  data <- tidy_hfd(data)
  hfd_calculate_coef(data = data,
                     n_comp = n_comp,
                     eps = eps)
}


## HAS_TESTS
#' Prepare Data from the Human Fertility Database
#'
#' Process age-specific fertility data from the 
#' the [Human Fertility Database](https://www.humanfertility.org/Home/Index)
#' so that it is ready to create a scaled SVD.
#'
#' @section Usage:
#' **Step 1: Download data**
#'
#' Register or log in at the Human Fertility Database,
#' and go to page
#' [ZippedDataFiles](https://www.humanfertility.org/Data/ZippedDataFiles).
#' Go to the "By statistic" table, and
#' download the file from the "Age-specific fertility rate" row.
#'
#' **Step 2: Extract `asfrRR.txt`**
#'
#' Extract the file `asfrRR.txt` from the downloaded data,
#' and read in the contents,
#' ```
#' asfr <- readr::read_table("asfrRR.txt", skip = 2)
#' ```
#'
#' **Step 3: Call function `data_ssvd_hfd()`**
#'
#' ```
#' hfd_data <- data_ssvd_hfd(asfr)
#' ```
#'
#' @section Lowest and highest ages:
#'
#' The original HFD data contains age groups `"12-"` (ie 12 and younger)
#' and `"55+"` (ie 55 and older). We treat these as closed intervals
#' `"12"` and `"55"`. 
#' 
#' If an age classification in the return value covers a narrower
#' range than the data itself, then the youngest age
#' group in the return value includes ASFRs from younger age
#' groups in the data, and the oldest age group in the return
#' value includes ASFRs from older age groups in the data.
#' For instance, if the classification in the return value
#' starts at age 15, but the data starts at age 12,
#' the ASFR for age 15 includes ASFRs from ages 12, 13, and 14.
#' This shifting of ASFRs is common in analyses of fertility.
#'
#' @inheritSection coef_hfd Truncation and transformation
#'
#' @inheritParams coef_hfd
#' @param age_min_max,age_max_min Every age classification
#' must at least span the range `[age_min_max, age_max_min)`.
#' Defaults are `15` and `50`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [coef_hfd()] Obtain time series
#'   of SVD coefficients
#'   from Human Fertility Database data
#' - [tidy_hfd()] Format data from the
#'   Human Fertility Database into
#'   a tidy data frame
#' 
#' @examples
#' data <- data_ssvd_hfd(asfr_subset)
#' data
#' @export
data_ssvd_hfd <- function(data,
                          age_min_max = 15,
                          age_max_min = 50,
                          n_comp = 5,
                          eps = 0.00001) {
  poputils::check_n(n = age_min_max,
                    nm_n = "age_min_max",
                    min = NULL,
                    max = 15L,
                    divisible_by = NULL)
  poputils::check_n(n = age_max_min,
                    nm_n = "age_max_min",
                    min = 45L,
                    max = NULL,
                    divisible_by = 5L)
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 6L,
                    divisible_by = NULL)
  check_eps(eps)
  cli::cli_progress_message("Tidying data...")
  data <- tidy_hfd(data)
  labels_age <- hfd_make_labels_age(data = data,
                                    age_min_max = age_min_max,
                                    age_max_min = age_max_min)
  cli::cli_progress_message("Carrying out SVD for 'total'...")
  hfd_total(data = data,
            labels_age = labels_age,
            n_comp = n_comp,
            eps = eps)
}


## HAS_TESTS
#' Tidy HFD Age-Specific Fertility Data
#'
#' Tidy a data frame containing data from
#' the ASFR row of the "By statistic" table of
#' [ZippedDataFiles](https://www.humanfertility.org/Data/ZippedDataFiles).
#'
#' @param data A data frame
#'
#' @returns A tibble.
#'
#' @seealso
#' - [data_ssvd_hfd()]Put data
#'   from the Human Fertility Database
#'   into the format required by the
#'   `ssvd()` function in \pkg{bage}.
#' - [coef_hfd()] Obtain time series
#'   of SVD coefficients
#'   from Human Fertility Database data
#'
#' @examples
#' tidy_hfd(asfr_subset)
#' @export
tidy_hfd <- function(data) {
  nms_required <- c("Code", "Year", "Age", "ASFR")
  nms_obtained <- names(data)
  for (nm in nms_required)
    if (!(nm %in% nms_obtained))
      cli::cli_abort("{.arg data} does not have a column called {.val {nm}}.")
  ans <- data[nms_required]
  names(ans) <- c("country", "time", "age", "value")
  ans$age <- sub("^([0-9]+)-$", "\\1", ans$age)
  ans$age <- sub("^([0-9]+)\\+$", "\\1", ans$age)
  ans$age <- as.integer(ans$age)
  ans <- tibble::tibble(ans)
  ans
}


## Internal -------------------------------------------------------------------

## HAS_TESTS
#' Obtain the Scaled 'U' Matrix From an SVD of HFD Data
#'
#' @param data A data frame, typically produced by 'tidy_hfd'.
#' @param n_comp Number of components.
#' @param eps Floor for rates.
#'
#' @returns A tibble
#' 
#' @noRd
hfd_calculate_coef <- function(data, n_comp, eps) {
  calculate_coef_nosex(data = data,
                       n_comp = n_comp,
                       transform = "log",
                       eps = eps)
}  


## HAS_TESTS
#' Get Data For One Set of Age Labels
#'
#' Aggregate when age labels are for five years.
#'
#' @param data A data frame
#' @param labels_age A character vector
#'
#' @returns A data frame
#'
#' @noRd
hfd_get_data_one <- function(data, labels_age) {
  age_lower_labels <- poputils::age_lower(labels_age)
  age_upper_labels <- poputils::age_upper(labels_age)
  is_open_labels <- any(is.infinite(age_upper_labels))
  if (is_open_labels)
    cli::cli_abort("Internal error: Age labels include open age group.")
  age_type <- poputils::age_group_type(labels_age)
  min_labels_age <- min(age_lower_labels)
  max_labels_age <- max(age_lower_labels)
  age_lower_data <- poputils::age_lower(data$age)
  data$age[age_lower_data < min_labels_age] <- min_labels_age
  data$age[age_lower_data > max_labels_age] <- max_labels_age
  if (age_type == "five")
    data$age <- poputils::combine_age(data$age, to = "five")
  ans <- stats::aggregate(data["value"], data[c("country", "time", "age")], sum)
  ans$age <- factor(ans$age, levels = labels_age)
  ord <- order(ans$age)
  ans <- ans[ord, , drop = FALSE]
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Create Age Group Labels
#'
#' Creates one-year and five-year age groups.
#' Creates versions where oldest age group open,
#' and where oldest age group closed.
#'
#' @param data Data frame
#' @param age_min_max Lower bound for youngest age group.
#' @param age_max_min Upper bound for oldest closed age group.
#' Must be divisible by 5.
#'
#' @returns A list of character vectors
#'
#' @noRd
hfd_make_labels_age <- function(data, age_min_max, age_max_min) {
  age_data <- data$age
  if (!is.integer(age_data))
    cli::cli_abort("Internal error: Age variable is not integer.")
  age_min_data <- min(age_data)
  age_min_data_five <- floor(age_min_data / 5L) * 5L
  age_max_data <- max(age_data) + 1L ## +1 because 'age_max' is upper limit
  age_max_data_five <- ceiling(age_max_data / 5L) * 5L
  if (age_min_max < age_min_data)
    cli::cli_abort(c("{.arg age_min_max} is below the youngest age group in the data.",
                     i = "{.arg age_min_max}: {.val {age_min_max}}.",
                     i = "Lower limit of youngest age group: {.val {age_min_data}}."))
  if (age_max_min > age_max_data)
    cli::cli_abort(c("{.arg age_max_min} is above the upper limit of the oldest age group in the data.",
                     i = "{.arg age_max_min}: {.val {age_max_min}}.",
                     i = "Upper limit of oldest age group: {.val {age_max_data}}."))
  ages_min_single <- seq.int(from = age_min_data, to = age_min_max)
  ages_min_five <- seq.int(from = age_min_data_five, to = age_min_max, by = 5L)
  ages_max_single <- seq.int(from = age_max_min, to = age_max_data) 
  ages_max_five <- seq.int(from = age_max_min, to = age_max_data_five, by = 5L)
  ages_min_single_rep <- rep(ages_min_single, each = length(ages_max_single))
  ages_max_single_rep <- rep(ages_max_single, times = length(ages_min_single))
  ages_min_five_rep <- rep(ages_min_five, each = length(ages_max_five))
  ages_max_five_rep <- rep(ages_max_five, times = length(ages_min_five))
  labels_single <- .mapply(poputils::age_labels,
                           dots = list(min = ages_min_single_rep,
                                       max = ages_max_single_rep),
                           MoreArgs = list(type = "single",
                                           open = FALSE))
  labels_five <- .mapply(poputils::age_labels,
                         dots = list(min = ages_min_five_rep,
                                     max = ages_max_five_rep),
                         MoreArgs = list(type = "five",
                                         open = FALSE))
  c(labels_single,
    labels_five)
}




## HAS_TESTS
#' Prepare Inputs for "total" Type, ie No Sex/Gender
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#' @param eps Floor for rates
#'
#' @returns A data frame
#'
#' @noRd
hfd_total <- function(data, labels_age, n_comp, eps) {
  data_split <- .mapply(hfd_get_data_one,
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
                       transform = "log",
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
