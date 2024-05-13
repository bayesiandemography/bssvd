
## HAS_TESTS
#' Create a Scaled SVD Object from HFD Age-Specific Fertility Data
#'
#' Create an object of class [`"bage_ssvd"`][bage::ssvd()]
#' from age-specific fertility data from the 
#' the [Human Fertility Database](https://www.humanfertility.org/Home/Index).
#'
#' @param data A data frame containing HFD data.
#' @param age_min_max, age_max_min Every age classification
#' must at least span the range `[age_min_max, age_max_min)`.
#' Defaults are `15` and `45`.
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#'
#' @returns An object of class [`"bage_ssvd"`][ssvd()]
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
#' **Step 2: Call function 'ssvd_hfd'**
#'
#' Extract the file `asfrRR.txt` from the downloaded data,
#' and read in the contents,
#' ```
#' data <- readr::read_table("asfrRR.txt.zip", skip = 2)
#' ```
#'
#' **Step 3: Call function 'ssvd_hfd'**
#'
#' ```
#' HFD <- ssvd_hfd(data)
#' ```
#' @seealso
#' - [hfd_tidy()] Tidy raw data, but do not process into SVD object.
#' - [ssvd_lfp()] Created scaled SVD object from OECD labour force
#'   participation rate data.
#' - [bage::SVD()], [bage::SVDS()], [bage::ESVD()], [bage::ESVDS()]
#'   to use an object of class
#'   [`"bage_ssvd"`][bage::ssvd()] in a prior.
#'
#' @examples
#' HFD_small <- ssvd_hfd(asfr)
#' @export
ssvd_hfd <- function(data,
                     age_min_max = 15,
                     age_max_min = 45,
                     n_comp = 5) {
  check_n(n = age_min_max,
          nm_n = "age_min_max",
          min = NULL,
          max = 15L,
          divisible_by = 1L)
  check_n(n = age_max_min,
          nm_n = "age_max_min",
          min = 45L,
          max = NULL,
          divisible_by = 5L)
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 3L,
          max = 6L,
          divisible_by = 1L)
  cli::cli_progress_message("Tidying data...")
  data <- hfd_tidy(data)
  labels_age <- hfd_make_labels_age(data = data,
                                    age_min_max = age_min_max,
                                    age_max_min = age_max_min)
  cli::cli_progress_message("Carrying out SVD for 'total'...")
  total <- hfd_total(data = data, labels_age = labels_age, n_comp = n_comp)
  cli::cli_progress_message("Carrying out SVD for 'indep'...")
  indep <- hfd_indep(data = data, labels_age = labels_age, n_comp = n_comp)
  cli::cli_progress_message("Carrying out SVD for 'joint'...")
  joint <- hfd_joint(data = data, labels_age = labels_age, n_comp = n_comp)
  cli::cli_progress_message("Combining results...")
  data <- vctrs::vec_rbind(total, indep, joint)
  data <- tibble::as_tibble(data)
  bage::ssvd(data)
}


## HAS_TESTS
#' Tidy HFD Age-Specific Fertility Data
#'
#' Tidy data frame containing data downloaded from
#' the ASFR row of the "By statistic" table of
#' [ZippedDataFiles](https://www.humanfertility.org/Data/ZippedDataFiles).
#'
#' @param data A data frame
#'
#' @returns A tibble.
#'
#' @seealso
#' - [ssvd_hfd()] Tidy data, and convert it into
#'   and an SVD object.
#'
#' @examples
#' hfd_tidy(asfr)
#' @export
hfd_tidy <- function(data) {
  nms_required <- c("Code", "Year", "Age", "ASFR")
  nms_obtained <- names(data)
  for (nm in nms_required)
    if (!(nm %in% nms_obtained))
      cli::cli_abort("{.arg data} does not have a column called {.val {nm}}.")
  ans <- data[nms_required]
  names(ans) <- c("country", "time", "age", "value")
  ans$age <- sub("^([0-9]+)-$", "\\1", ans$age)
  ans <- tibble::tibble(ans)
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
lfp_make_labels_age <- function(data, age_min_max, age_max_min) {
  age_data <- data$age
  age_lower_data <- poputils::age_lower(age_data)
  age_upper_data <- poputils::age_upper(age_data)
  age_min_data <- min(age_lower_data)
  age_max_data <- max(age_upper_data[is.finite(age_upper_data)])
  remainder_min_data_five <- age_min_data %% 5L
  if (remainder_min_data_five == 0L)
    age_min_data_five <- age_min_data
  else
    age_min_data_five <- age_min_data - remainder_min_data_five + 5L
  if (age_min_max < age_min_data)
    cli::cli_abort(c("{.arg age_min_max} is below the youngest age group in the data.",
                     i = "{.arg age_min_max}: {.val {age_min_max}}.",
                     i = "Lower limit of youngest age group: {.val {age_min_data}}."))
  if (age_min_max < age_min_data_five)
    cli::cli_abort(c(paste("{.arg age_min_max} is below the youngest age group in the data",
                           "after collapsing data into 5-year age groups."),
                     i = "{.arg age_min_max}: {.val {age_min_max}}.",
                     i = "Lower limit of youngest age group after collapsing: {.val {age_min_data_five}}."))
  if (age_max_min > age_max_data)
    cli::cli_abort(c("{.arg age_max_min} is above the oldest closed age group in the data.",
                     i = "{.arg age_max_min}: {.val {age_max_min}}.",
                     i = "Upper limit of oldest closed age group: {.val {age_max_data}}."))
  ages_min_single <- seq.int(from = age_data_min, to = age_min_max)
  ages_min_five <- seq.int(from = age_data_min_five, to = age_min_max, by = 5L)
  ages_max_single <- seq.int(from = age_max_min, to = age_max_data)
  ages_max_five <- seq.int(from = age_max_min, to = age_max_data, by = 5L)
  ages_min_single <- rep(ages_min_single, each = length(ages_max_single))
  ages_max_single <- rep(ages_max_single, times = length(ages_min_single))
  ages_min_five <- rep(ages_min_five, each = length(ages_max_five))
  ages_max_five <- rep(ages_max_five, times = length(ages_min_five))
  labels_closed_single <- .mapply(poputils::age_labels,
                                  dots = list(min = ages_max_single,
                                              max = ages_max_single),
                                  MoreArgs = list(type = "single",
                                                  open = FALSE))
  labels_closed_five <- .mapply(poputils::age_labels,
                                dots = list(min = ages_max_five,
                                            max = ages_max_five),
                                MoreArgs = list(type = "five",
                                                open = FALSE))
  labels_open_single <- .mapply(poputils::age_labels,
                                dots = list(min = ages_max_single,
                                            max = ages_max_single),
                                MoreArgs = list(type = "single",
                                                open = TRUE))
  labels_open_five <- .mapply(poputils::age_labels,
                              dots = list(min = ages_max_five,
                                          max = ages_max_five),
                              MoreArgs = list(type = "five",
                                              open = TRUE))
  c(labels_closed_single,
    labels_closed_five,
    labels_open_single,
    labels_open_five)
}
