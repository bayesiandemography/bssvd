
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
#' @param data A data frame containing OECD data.
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_hmd()] Prepare data on fertility
#'   from the Human Mortality Database
#' - [coef_hfd()] Obtain coefficients
#'   for Human Fertility Database data
#' - [coef_hmd()] Obtain coefficients
#'   for Human Mortality Database data
#'
#' @examples
#' coef_lfp(oecd_lfp_subset)
#' @export
coef_lfp <- function(data, n_comp = 5) {
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 3L,
                    max = 6L,
                    divisible_by = NULL)
  n_comp <- as.integer(n_comp)
  cli::cli_progress_message("Tidying data...")
  data <- lfp_tidy(data)
  age_labels <- poputils::age_labels(type = "five", min = 15, max = 65)
  data <- data[data$age %in% age_labels, , drop = FALSE]
  cli::cli_progress_message("Calculating coefficients...")
  lfp_calculate_coef(data = data, n_comp = n_comp)
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
#' @inheritParams coef_lfp
#' @param age_max The upper limit of the
#' oldest closed age group. Default is `75`.
#'
#' @returns A tibble
#'
#' @seealso
#' - [data_ssvd_hfd()] Prepare data on fertility
#'   from the Human Fertility Database
#' - [data_ssvd_hmd()] Prepare data on mortality
#'   from the Human Mortality Database
#'
#' @examples
#' data <- data_ssvd_lfp(oecd_lfp_subset)
#' data
#' @export
data_ssvd_lfp <- function(data,
                          age_max = 75,
                          n_comp = 5) {
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
  age_max <- as.integer(age_max)
  n_comp <- as.integer(n_comp)
  cli::cli_progress_message("Tidying data...")
  data <- lfp_tidy(data)
  labels_age <- lfp_make_labels_age(data = data,
                                    age_max = age_max)
  cli::cli_progress_message("Carrying out SVD for 'total'...")
  total <- lfp_total(data = data, labels_age = labels_age, n_comp = n_comp)
  cli::cli_progress_message("Carrying out SVD for 'indep'...")
  indep <- lfp_indep(data = data, labels_age = labels_age, n_comp = n_comp)
  cli::cli_progress_message("Carrying out SVD for 'joint'...")
  joint <- lfp_joint(data = data, labels_age = labels_age, n_comp = n_comp)
  cli::cli_progress_message("Combining results...")
  data <- vctrs::vec_rbind(total, indep, joint)
  data <- tibble::as_tibble(data)
  data
}


## HAS_TESTS
#' Obtain the Scaled 'U' Matrix From an SVD of OECD
#' Labour Force Participation Data
#'
#' @param data A data frame, typically produced by 'hfd_tidy'.
#' @param n_comp Number of components.
#'
#' @returns A tibble
#' 
#' @noRd
lfp_calculate_coef <- function(data, n_comp) {
  data$age <- poputils::reformat_age(data$age)
  ord <- with(data, order(sex, country, time, age))
  data <- data[ord, , drop = FALSE]
  data <- vctrs::vec_split(data[c("country", "time", "age", "value")], data["sex"])
  ans <- lapply(data$val,
                poputils::to_matrix,
                rows = "age",
                cols = c("country", "time"),
                measure = "value")
  ans <- lapply(ans, remove_cols_with_na, n_comp = n_comp)
  ans <- lapply(ans, replace_zeros)
  ans <- lapply(ans, log)
  country_time <- lapply(ans, colnames)
  ans <- lapply(ans, function(x) svd(x, nu = 0L, nv = n_comp)$v)
  ans <- lapply(ans, scale, center = TRUE, scale = TRUE)
  for (i in seq_along(ans)) {
    dimnames(ans[[i]]) <- list(country_time = country_time[[i]],
                               component = paste("Component", seq_len(n_comp)))
    ans[[i]] <- as.data.frame.table(ans[[i]], responseName = "coef", stringsAsFactors = FALSE)
    ans[[i]]$sex <- data$key$sex[[i]]
  }
  ans <- vctrs::vec_rbind(!!!ans)
  p <- "^(.*)\\.(.*)$"
  ans$country <- sub(p, "\\1", ans$country_time)
  ans$time <- as.integer(sub(p, "\\2", ans$country_time))
  ans <- ans[c("sex", "country", "time", "component", "coef")]
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Get Data Associated with One Set of Age Labels
#'
#' Retrieve data, turn the age variable into a factor,
#' and use it to sort the data.
#'
#' @param data A data frame
#' @param labels_age A character vector of age labels
#'
#' @returns A data frame
#'
#' @noRd
lfp_get_data_one <- function(data, labels_age) {
  ans <- data[data$age %in% labels_age, ]
  ans$age <- factor(ans$age, levels = labels_age)
  ord <- order(ans$age)
  ans <- ans[ord, ]
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Prepare Inputs for "indep" Type, ie Female and Male Separate
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#'
#' @returns A data frame
#' 
#' @noRd
lfp_indep <- function(data, labels_age, n_comp) {
  data <- data[data$sex %in% c("Female", "Male"), ]
  data$sex <- poputils::reformat_sex(data$sex)
  data_split <- .mapply(lfp_get_data_one,
                        dots = list(labels_age = labels_age),
                        MoreArgs = list(data = data))
  data_split <- lapply(data_split,
                       function(x) split(x[c("country", "time", "age", "value")], x["sex"]))
  x_split <- lapply(data_split,
                    function(x)
                      .mapply(poputils::to_matrix,
                              dots = list(x = x),
                              MoreArgs = list(rows = "age",
                                              cols = c("country", "time"),
                                              measure = "value")))
  x_split <- lapply(x_split,
                    function(x_one_age_max)
                      lapply(x_one_age_max, remove_cols_with_na, n_comp = n_comp))
  ssvd_split <- lapply(x_split,
                       function(x_one_age_max)
                         lapply(x_one_age_max,
                                make_matrix_and_offset,
                                transform = "logit",
                                n_comp = n_comp))
  n_age <- lengths(labels_age)
  labels_sexgender <- .mapply(rep,
                              dots = list(each = n_age),
                              MoreArgs = list(x = c("Female", "Male")))
  labels_age <- lapply(labels_age, rep.int, times = 2L)
  matrix <- lapply(ssvd_split, function(x) lapply(x, function(y) y$matrix))
  offset <- lapply(ssvd_split, function(x) lapply(x, function(y) y$offset))
  matrix <- lapply(matrix, Matrix::.bdiag)
  offset <- lapply(offset, function(x) vctrs::vec_c(!!!x))
  for (i in seq_along(offset)) {
    nms <- paste(labels_sexgender[[i]], labels_age[[i]], sep = ".")
    names(offset[[i]]) <- nms
    rownames(matrix[[i]]) <- nms
  }
  tibble::tibble(type = "indep",
                 labels_age = labels_age,
                 labels_sexgender = labels_sexgender,
                 matrix = matrix,
                 offset = offset)
}


## HAS_TESTS
#' Prepare Inputs for "joint" Type, ie Female and Male Concatenated
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#'
#' @returns A data frame
#'
#' @noRd
lfp_joint <- function(data, labels_age, n_comp) {
  data <- data[data$sex %in% c("Female", "Male"), ]
  data$sex <- poputils::reformat_sex(data$sex)
  data_split <- .mapply(lfp_get_data_one,
                        dots = list(labels_age = labels_age),
                        MoreArgs = list(data = data))
  order_sexage <- function(x) x[order(x$sex, x$age), ]
  data_split <- lapply(data_split, order_sexage)
  x_split <- .mapply(poputils::to_matrix,
                     dots = list(x = data_split),
                     MoreArgs = list(rows = c("sex", "age"),
                                     cols = c("country", "time"),
                                     measure = "value"))
  x_split <- lapply(x_split, remove_cols_with_na, n_comp = n_comp)
  ssvd_split <- lapply(x_split,
                       make_matrix_and_offset,
                       transform = "logit",
                       n_comp = n_comp)
  n_age <- lengths(labels_age)
  labels_age <- lapply(labels_age, rep, times = 2L)
  labels_sexgender <- .mapply(rep,
                              dots = list(each = n_age),
                              MoreArgs = list(x = c("Female", "Male")))
  matrix <- lapply(ssvd_split, function(x) x$matrix)
  offset <- lapply(ssvd_split, function(x) x$offset)
  tibble::tibble(type = "joint",
                 labels_age = labels_age,
                 labels_sexgender = labels_sexgender,
                 matrix = matrix,
                 offset = offset)
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
#' Tidy OECD Labor Force Participation Data
#'
#' Initial tidying of data downloaded from
#' the Labour Force Indicators table of the
#' [OECD Data Explorer](https://data-explorer.oecd.org). 
#'
#' @param data A data frame
#'
#' @returns A tibble
#'
#' @noRd
lfp_tidy <- function(data) {
  nms_required <- c("TIME_PERIOD", "REF_AREA", "SEX", "AGE", "MEASURE", "obsValue")
  nms_obtained <- names(data)
  for (nm in nms_required)
    if (!(nm %in% nms_obtained))
      cli::cli_abort("{.arg data} does not have a column called {.val {nm}}.")
  ans <- data[nms_required]
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
  ans$value <- trim_01(ans$value)
  ans <- tibble::tibble(ans)
  ans
}


## HAS_TESTS
#' Prepare Inputs for "total" Type, ie Female and Male Combined
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#'
#' @returns A data frame
#'
#' @noRd
lfp_total <- function(data, labels_age, n_comp) {
  data <- data[data$sex == "Total", ]
  data_split <- .mapply(lfp_get_data_one,
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
                       n_comp = n_comp)
  matrix <- lapply(ssvd_split, function(x) x$matrix)
  offset <- lapply(ssvd_split, function(x) x$offset)
  tibble::tibble(type = "total",
                 labels_age = labels_age,
                 labels_sexgender = list(NULL),
                 matrix = matrix,
                 offset = offset)
}






