
## User-visible ---------------------------------------------------------------

## HAS_TESTS
#' Obtain Coefficients from Scaled SVD of OCED
#' Labour Force Participation Data
#'
#' Obtain coefficients from a
#' scaled SVD of OCED labour force participation
#' data downloaded from
#' the [OECD Data Explorer](https://data-explorer.oecd.org).
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
#' @param data A data frame containing OECD data.
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#' @param year_min Only include data for
#' `year_min` onwards. Ignored if
#' `year_min` is `NULL` (the default).
#' @param eps Parameter for truncating
#' probabililities or rates.
#' Default is `0.00001`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_lfp()] Put OECD labour
#'   force participation data
#'   into the format required by
#'   the `ssvd()` function in \pkg{bage}
#' - [tidy_lfp()] Format OECD labour force
#'   participation data into a tidy data frame.
#' 
#' @examples
#' coef_lfp(oecd_lfp_subset)
#' @export
coef_lfp <- function(data,
                     n_comp = 5,
                     year_min = NULL,
                     eps = 0.00001) {
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 6L,
                    divisible_by = NULL)
  n_comp <- as.integer(n_comp)
  check_eps(eps)
  check_year_min(year_min)
  data <- tidy_lfp(data = data,
                   year_min = year_min)
  age_labels <- poputils::age_labels(type = "five", min = 15, max = 65)
  data <- data[data$age %in% age_labels, , drop = FALSE]
  lfp_calculate_coef(data = data,
                     n_comp = n_comp,
                     eps = eps)
}


## HAS_TESTS
#' Prepare OECD Data on Labour Force Participation Rates
#'
#' Process labour force participation data downloaded from
#' the [OECD Data Explorer](https://data-explorer.oecd.org)
#' so it is ready to create a scaled SVD.
#'
#' @section Usage:
#' **Step 1: Download data**
#'
#' ```
#' library(rsdmx)
#' url <- paste("https://sdmx.oecd.org/public/rest/data",
#'              "OECD.ELS.SAE,DSD_LFS@DF_LFS_INDIC,1.1",
#'              "all?dimensionAtObservation=AllDimensions",
#'              sep = "/")
#' lfp_sdmx <- rsdmx::readSDMX(url)    ## can be slow
#' lfp_df <- as.data.frame(lfp_sdmx)   ## can also be slow
#' ```
#'
#' **Step 2: Call function `data_ssvd_lfp()`**
#'
#' ```
#' lfp_data <- data_ssvd_lfp(data)
#' ```
#'
#' @section coef_lfp Truncation and transformation
#'
#' @inheritParams coef_lfp
#' @param age_max The upper limit of the
#' oldest closed age group. Default is `75`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [tidy_lfp()] Format OECD labour force
#'   participation data into a tidy data frame.
#' - [coef_lfp()] Obtain time series
#'   of SVD coefficients for OECD labour force
#'   participation data
#' 
#' @examples
#' data <- data_ssvd_lfp(oecd_lfp_subset)
#' data
#' @export
data_ssvd_lfp <- function(data,
                          age_max = 75,
                          n_comp = 5,
                          year_min = NULL,
                          eps = 0.00001) {
  poputils::check_n(n = age_max,
                    nm_n = "age_max",
                    min = 20L,
                    max = 75L,
                    divisible_by = 5L)
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 10L,
                    divisible_by = NULL)
  check_eps(eps)
  age_max <- as.integer(age_max)
  n_comp <- as.integer(n_comp) 
  check_year_min(year_min)
  cli::cli_progress_message("Tidying data...")
  data <- tidy_lfp(data = data, year_min = year_min)
  labels_age <- lfp_make_labels_age(data = data, age_max = age_max)
  cli::cli_progress_message("Carrying out SVD for 'total'...")
  total <- lfp_total(data = data,
                     labels_age = labels_age,
                     n_comp = n_comp,
                     eps = eps)
  cli::cli_progress_message("Carrying out SVD for 'indep'...")
  indep <- lfp_indep(data = data,
                     labels_age = labels_age,
                     n_comp = n_comp,
                     eps = eps)
  cli::cli_progress_message("Carrying out SVD for 'joint'...")
  joint <- lfp_joint(data = data,
                     labels_age = labels_age,
                     n_comp = n_comp,
                     eps = eps)
  cli::cli_progress_message("Combining results...")
  data <- vctrs::vec_rbind(total, indep, joint)
  data <- tibble::as_tibble(data)
  data
}


## HAS_TESTS
#' Tidy OCED Labour Force Participation Data
#'
#' Put data on labour force participation
#' data downloaded from
#' the [OECD Data Explorer](https://data-explorer.oecd.org)
#' into a tidy data frame.
#'
#' @inheritParams coef_lfp
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_lfp()] Put OECD labour
#'   force participation data
#'   into the format required by
#'   the `ssvd()` function in \pkg{bage}
#' - [tidy_lfp()] Format OECD labour force
#'   participation data into a tidy data frame.
#' - [coef_lfp()] Obtain time series
#'   of SVD coefficients for OECD labour force
#'   participation data
#'
#' @examples
#' tidy_lfp(oecd_lfp_subset)
#' @export
tidy_lfp <- function(data,
                     year_min = NULL) {
  nms_required <- c("TIME_PERIOD",
                    "REF_AREA",
                    "SEX",
                    "AGE",
                    "MEASURE",
                    "obsValue")
  nms_obtained <- names(data)
  for (nm in nms_required)
    if (!(nm %in% nms_obtained))
      cli::cli_abort("{.arg data} does not have a column called {.val {nm}}.")
  ans <- data[nms_required]
  check_year_min(year_min)
  ans <- ans[ans$MEASURE == "LF_RATE", ]
  ans <- ans[-match("MEASURE", names(ans))]
  names(ans) <- c("time", "country", "sex", "age", "value")
  ans <- ans[!(ans$country %in% c("G7",
                                  "EU27",
                                  "E",
                                  "EU22OECD",
                                  "EU19OECD",
                                  "OECD",
                                  "OECD_REP")), ]
  if (!is.null(year_min))
    ans <- ans[ans$time >= year_min, , drop = FALSE]
  ans <- ans[!(ans$age %in% c("_U", "_T")), ]
  p_open <- "^Y_GE([0-9]+)$"
  p_closed <- "^Y([0-9]+)T([0-9]+)$"
  ans$age <- sub(p_open, "\\1+", ans$age)
  ans$age <- sub(p_closed, "\\1-\\2", ans$age)
  ans <- ans[!(ans$age %in% c("15-24",
                              "25-34",
                              "30-39",
                              "35-44",
                              "40-49",
                              "45-54",
                              "50-59",
                              "55-64",
                              "65-74",
                              "25-54",
                              "25-64",
                              "15-64",
                              "20-64",
                              "25-39")), ]
  ans$sex <- sub("^_T$", "Total", ans$sex)
  ans$sex <- sub("^F$", "Female", ans$sex)
  ans$sex <- sub("^M$", "Male", ans$sex)
  is_valid_sex <- ans$sex %in% c("Total", "Female", "Male")
  i_invalid_sex <- match(FALSE, is_valid_sex, nomatch = 0L)
  if (i_invalid_sex > 0L)
    cli::cli_abort(c("Invalid value for {.var sex}: {.val {ans$sex[[i_invalid_sex]]}}."))
  ans$value <- ans$value / 100
  ans <- tibble::tibble(ans)
  ans
}


## Internal -------------------------------------------------------------------

## HAS_TESTS
#' Obtain the Scaled 'U' Matrix From an SVD of OECD
#' Labour Force Participation Data
#'
#' @param data A data frame, typically produced by 'tidy_lfp'.
#' @param n_comp Number of components.
#' @param eps Truncation parameter
#'
#' @returns A tibble
#' 
#' @noRd
lfp_calculate_coef <- function(data, n_comp, eps) {
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
lfp_indep <- function(data,
                      labels_age,
                      n_comp,
                      eps) {
  make_indep(data = data,
             labels_age = labels_age,
             n_comp = n_comp,
             transform = "logit",
             eps = eps)
}


## HAS_TESTS
#' Prepare Inputs for "joint" Type, ie Female and Male Concatenated
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Number of SVD components
#' @param eps Truncation parameter
#'
#' @returns A data frame
#'
#' @noRd
lfp_joint <- function(data,
                      labels_age,
                      n_comp,
                      eps) {
  make_joint(data = data,
             labels_age = labels_age,
             n_comp = n_comp,
             transform = "logit",
             eps = eps)
}


## HAS_TESTS
#' Create 5-Year Age Groups, Starting at Age 15
#'
#' Creates versions where oldest age group open,
#' and where oldest age group closed.
#'
#' @param data Data frame
#' @param age_max Upper bound for oldest closed age group.
#'
#' @returns A list of character vectors
#'
#' @noRd
lfp_make_labels_age <- function(data, age_max) {
  age <- data$age
  ages_max <- get_ages_max(age = age,
                           age_max = age_max)
  labels_closed <- .mapply(poputils::age_labels,
                           dots = list(max = ages_max),
                           MoreArgs = list(type = "five",
                                           min = 15,
                                           open = FALSE))
  labels_open <- .mapply(poputils::age_labels,
                         dots = list(max = ages_max),
                         MoreArgs = list(type = "five",
                                         min = 15,
                                         open = TRUE))
  c(labels_closed, labels_open)
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
lfp_total <- function(data, labels_age, n_comp, eps) {
  make_total(data = data,
             labels_age = labels_age,
             n_comp = n_comp,
             transform = "logit",
             eps = eps)
}
