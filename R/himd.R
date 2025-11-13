
## User-visible ---------------------------------------------------------------

## HAS_TESTS
#' Obtain Coefficients from Scaled SVD of HIMD Data
#'
#' Obtain time series of coefficients from a
#' scaled SVD of data from the
#' [Human Internal Migration Database](https://osf.io/vmrfk/).
#' The coefficients are a 
#' scaled version of the \eqn{U} matrix
#' from the SVD.
#'
#' @section Truncation and transformation:
#' 
#' If `measure_type` is `"prob"`, then
#' reported probabilities outside `(eps, 1-eps)`
#' are shifted to `eps` or `1-eps`, and
#' all values are logit-transformed,
#' before the SVD is applied.
#'
#' If `measure_type` is `"rate"`, then
#' reported probabilities less than `eps`
#' are shifted to `eps`,
#' and all values are log-transformed,
#' before the SVD is applied.
#'
#' @param zipfile The name of a zipped file downloaded
#' from the Human Internal Migration Database.
#' A path name that is handled by [utils::unzip()].
#' @param time_interval Length of interval over
#' which values are calculated. Choices are
#' `1` or `5`. Default is `1`.
#' @param measure_type `"prob"` or `"rate"`.
#' Whether values are to be
#' treated as probabilities or
#' as rates. See [data_ssvd_himd()] for details.
#' Default is `"rate"`.
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#' @param eps Parameter for truncating
#' probabililities or rates.
#' Default is `0.00001`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_hmd()] Put data
#'   from the Human Internal Migration Database
#'   into the format required by
#'   the `ssvd()` function in \pkg{bage}
#' - [tidy_hmd()] Format data from the
#'   Human Internal Migration Database into
#'   a tidy data frame
#'
#' @examples
#' zipfile <- system.file("extdata", "himd_20241023_subset.zip",
#'                        package = "bssvd")
#' coef_himd(zipfile)
#' @export
coef_himd <- function(zipfile,
                      time_interval = 1,
                      measure_type = c("rate", "prob"),
                      n_comp = 5,
                      eps = 0.00001) {
  if (!(time_interval %in% c(1, 5)))
    cli::cli_abort("Invalid time interval.")
  measure_type <- match.arg(measure_type)
  if ((measure_type == "rate") && (time_interval == 5))
    cli::cli_abort(paste("{.arg measure_type} can only be {.val {'rate'}} if",
                         "{.arg time_interval} is {.val {1}}."))
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 10L,
                    divisible_by = NULL)
  n_comp <- as.integer(n_comp)
  check_eps(eps)
  data <- himd_unzip(zipfile = zipfile,
                     time_interval = time_interval)
  himd_calculate_coef(data = data,
                      measure_type = measure_type,
                      n_comp = n_comp,
                      eps = eps)
}

## HAS_TESTS
#' Prepare Data from Human Internal Migration Database
#'
#' Process data on age-specific migration probabilities
#' from the Human Internal Migration Database.
#'
#' # Usage
#'
#' **Step 1: Download data**
#'
#' Download the data from 
#' Dyrting, S. (2024, October 23). *Data from: Estimating
#' Complete Migration Probabilities from Grouped Data*
#' at https://osf.io/vmrfk/.
#' The data is in the "Migration"
#' folder under the "Files" tab.
#' The facility for downloading all the files at once
#' does not appear to be working, but CSV files can
#' be downloaded one by one. Zip the folder
#' containing these CSV files.
#'
#' **Step 2: Call function `data_ssvd_himd()`
#'
#' Create three datasets:
#' ```
#' himd_data_rate <- data_svd_himd(zipfile = "himd.zip",
#'                                 time_interval = 1,
#'                                 measure_type = "rate")
#' himd_data_prob1 <- data_svd_himd(zipfile = "himd.zip",
#'                                  time_interval = 1,
#'                                  measure_type = "prob")
#' himd_data_prob5 <- data_svd_himd(zipfile = "himd.zip",
#'                                  time_interval = 5,
#'                                  measure_type = "prob")
#' ```
#'
#' # Rates or probabilities
#'
#' The original data are probabilities.
#' However, `data_ssvd_himd()` (and [coef_himd()])
#' allow values where `time_interval` is
#' 1 to be to treated as rates. If more than
#' one change in residence in a year is unusual,
#' then one-year probabilities and should
#' in fact be very similar.
#'
#' @section coef_himd Truncation and transformation
#'
#' @inheritParams coef_himd
#'
#' @returns A tibble
#'
#' @seealso
#' - [coef_hfd()] Obtain time series
#'   of SVD coefficients
#'   for Human Internal Migration Database data
#' - [tidy_hmd()] Format data from the
#'   Human Internal Migration Database into
#'   a tidy data frame
#' 
#' @examples
#' zipfile <- system.file("extdata", "himd_20241023_subset.zip",
#'                        package = "bssvd")
#' data_ssvd_himd(zipfile)
#' @export
data_ssvd_himd <- function(zipfile,
                           time_interval = 1,
                           measure_type = c("rate", "prob"),
                           n_comp = 5,
                           eps = 0.00001) {
  if (!(time_interval %in% c(1, 5)))
    cli::cli_abort("Invalid time interval.")
  measure_type <- match.arg(measure_type)
  if ((measure_type == "rate") && (time_interval == 5))
    cli::cli_abort(paste("{.arg measure_type} can only be {.val rate} if",
                         "{.arg time_interval} is {.val 1}."))
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 10L,
                    divisible_by = NULL)
  n_comp <- as.integer(n_comp)
  check_eps(eps)
  cli::cli_progress_message("Unzipping file...")
  data <- himd_unzip(zipfile = zipfile, time_interval = time_interval)
  cli::cli_progress_message("Creating five-year age groups...")
  data <- himd_add_age_five(data)
  cli::cli_progress_message("Assembling datasets for alternative open age groups...")
  data <- himd_vary_age_open(data)
  cli::cli_progress_message("Carrying out SVD...")
  transform <- if (measure_type == "rate") "log" else "logit"
  himd_total(data = data,
             n_comp = n_comp,
             transform = transform,
             eps = eps)
}


## HAS_TESTS
#' Tidy HIMD Age-Specific Internal Migration Data
#'
#' Put data on age-specific migration probabilities
#' from a zipped file downloaded from the
#' [Human Internal Migration Database](https://osf.io/vmrfk/)
#' into a tidy data frame.
#'
#' @inheritParams coef_himd
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_himd()] Put data
#'   from the Human Internal Migration Database
#'   into the format required by
#'   the `ssvd()` function in \pkg{bage}
#' - [coef_hmd()] Obtain time series
#'   of SVD coefficients
#'   for Human Internal Migration Database data
#'
#' @examples
#' zipfile <- system.file("extdata", "himd_20241023_subset.zip",
#'                        package = "bssvd")
#' tidy_himd(zipfile)
#' @export
tidy_himd <- function(zipfile, time_interval = 1) {
  if (!(time_interval %in% c(1, 5)))
    cli::cli_abort("Invalid time interval.")
  himd_unzip(zipfile, time_interval = time_interval)
}


## Internal -------------------------------------------------------------------

## HAS_TESTS
#' Create Age Group "five" and Add to 'x'
#'
#' @param x A data frame
#'
#' @returns Modifed version of 'x'
#'
#' @noRd
himd_add_age_five <- function(x) {
  five <- x
  five$age <- (five$age %/% 5) * 5L
  five <- stats::aggregate(five["value"],
                           five[c("country_orig_dest",
                                  "time",
                                  "age")],
                           mean)
  five$age <- ifelse(five$age == 110,
                     "110+",
                     paste(five$age, five$age + 4, sep = "-"))
  five$type_age = "five"
  x$age <- ifelse(x$age == 110, "110+", x$age)
  x$type_age <- "single"
  ans <- vctrs::vec_rbind(x, five)
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Obtain the Scaled 'U' Matrix From an SVD of HIMD Data
#'
#' @param data A data frame, typically produced by 'himd_unzip'
#' @param measure_type "rate" or "prob"
#' @param n_comp Number of components.
#' @param eps Floor for rates and probabilities. 1 - eps is ceiling
#' for probabilities.
#'
#' @returns A tibble
#' 
#' @noRd
himd_calculate_coef <- function(data, measure_type, n_comp, eps) {
  ord <- with(data, order(country_orig_dest, time, age))
  data <- data[ord, , drop = FALSE]
  ans <- poputils::to_matrix(data,
                             rows = "age",
                             cols = c("country_orig_dest", "time"),
                             measure = "value")
  ans <- remove_cols_with_na(ans, n_comp = n_comp)
  if (measure_type == "rate") {
    ans <- replace_zeros(ans, eps = eps)
    ans <- -log1p(-ans)
    ans <- log(ans)
  }
  else {
    ans <- replace_zeros_ones(ans, eps = eps)
    ans <- poputils::logit(ans)
  }
  country_orig_dest_time <- colnames(ans)
  ans <- svd(ans, nu = 0L, nv = n_comp)
  ans <- ans$v
  ans <- scale(ans, center = TRUE, scale = TRUE)
  dimnames(ans) <- list(country_orig_dest_time = country_orig_dest_time,
                        component = paste("Component", seq_len(n_comp)))
  ans <- as.data.frame.table(ans, responseName = "coef", stringsAsFactors = FALSE)
  p <- "^(.*)\\.([^.]*)$"
  ans$country_orig_dest <- sub(p, "\\1", ans$country_orig_dest_time)
  ans$time <- as.integer(sub(p, "\\2", ans$country_orig_dest_time))
  ans <- ans[c("country_orig_dest", "time", "component", "coef")]
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Prepare Inputs for "total" Type, ie Female and Male Combined
#'
#' @param x A data frame
#' @param n_comp Number of SVD components
#' to include in result.
#' @param transform to apply
#' @param eps Parameter for truncation of of rates/probabilities
#'
#' @returns A data frame
#'
#' @noRd
himd_total <- function(data, n_comp, transform, eps) {
  data <- vctrs::vec_split(x = data[c("age",
                                      "country_orig_dest",
                                      "time",
                                      "value")],
                           by = data[c("type_age",
                                       "age_open")])
  format_age <- function(x) {
    x$age <- poputils::reformat_age(x$age)
    x <- x[order(x$age), ]
    x
  }
  val <- lapply(data$val, format_age)
  val <- lapply(val,
                poputils::to_matrix,
                rows = "age",
                cols = c("country_orig_dest", "time"),
                measure = "value")
  is_rate <- transform == "log"
  if (is_rate)
    val <- lapply(val, function(x) -log1p(-x))
  l <- lapply(val,
              make_matrix_and_offset,
              transform = transform,
              n_comp = n_comp,
              eps = eps)
  matrix <- lapply(l, function(x) x$matrix)
  offset <- lapply(l, function(x) x$offset)
  labels_age <- lapply(matrix, rownames)
  tibble::tibble(type = "total",
                 labels_age = labels_age,
                 labels_sexgender = list(NULL),
                 matrix = matrix,
                 offset = offset)
}


## HAS_TESTS
#' Extract Values from Zipfile Holding HIMD Data
#'
#' @param zipfile Name of zipfile
#' @param time_interval Length of
#' interval (in years) over which
#' migration occurs. 1 or 5.
#'
#' @returns A tibble
#'
#' @noRd
himd_unzip <- function(zipfile, time_interval) {
  if (!(time_interval %in% c(1, 5)))
    cli::cli_abort("Invalid value for {.arg time_interval}.")
  tmp_dir <- tempfile("himd_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  utils::unzip(zipfile, exdir = tmp_dir)
  fnames <- list.files(tmp_dir, full.names = TRUE, recursive = TRUE)
  data <- lapply(fnames, utils::read.csv)
  data <- do.call(rbind, data)
  data <- data[data$Interval == time_interval, , drop = FALSE]
  data <- data[data$Dest != 0, , drop = FALSE]
  country <- sprintf("%03.0f", data$Country)
  country_code <- bssvd::country_code
  country <- country_code$country[match(country, country_code$code)]
  data$country_orig_dest <- paste(country,
                                  data$Orig,
                                  data$Dest,
                                  sep = ".")
  age <- seq.int(from = 0, to = 110)
  nms_value <- paste0("m", age)
  n_age <- length(age)
  n_data <- nrow(data)
  country_orig_dest <- rep(data$country_orig_dest, times = n_age)
  time <- rep(data$StartYear, times = n_age)
  age <- rep(age, each = n_data)
  value <- data[nms_value]
  value <- unlist(value, use.names = FALSE)
  tibble::tibble(country_orig_dest = country_orig_dest,
                 time = time,
                 age = age,
                 value = value)
}


## HAS_TESTS
#' Create Multiple Versions of Data, Each with Different
#' Open Age Group
#'
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @noRd
himd_vary_age_open <- function(data) {
  age_open <- seq.int(from = 60L, to = 110L, by = 5L)
  make_data_age_open <- function(age_open, X) {
    X$age <- as.character(X$age)
    X$age <- poputils::set_age_open(X$age, lower = age_open)
    X <- stats::aggregate(X["value"],
                          X[c("country_orig_dest",
                              "time",
                              "age",
                              "type_age")],
                          mean)
    X$age_open <- age_open
    X
  }
  data_single <- data[data$type_age == "single", , drop = FALSE]
  data_five <- data[data$type_age == "five", , drop = FALSE]
  ans_single <- lapply(age_open,
                       function(a) make_data_age_open(a, X = data_single))
  ans_five <- lapply(age_open,
                     function(a) make_data_age_open(a, X = data_five))
  ans_single <- vctrs::vec_rbind(!!!ans_single)
  ans_five <- vctrs::vec_rbind(!!!ans_five)
  ans <- vctrs::vec_rbind(ans_single, ans_five)  
  ans
}
