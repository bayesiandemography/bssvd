
## HAS_TESTS
#' Create a Scaled SVD Object from OECD Labour Force Participation Data
#'
#' Create an object of class [`"bage_ssvd"`][bage::ssvd()]
#' from labour force participation data downloaded from
#' the [OECD Data Explorer](https://data-explorer.oecd.org). 
#'
#' @param data A data frame containing OECD data.
#' @param age_max The upper limit of the
#' oldest closed age group. Default is `75`.
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#'
#' @returns An object of class [`"bage_ssvd"`][ssvd()]
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
#' data <- as.data.frame(lfp_sdmx)     ## can also be slow
#' ```
#'
#' **Step 2: Call function 'ssvd_lfp'**
#'
#' ```
#' LFP <- ssvd_lfp(data)
#' ```
#' @seealso
#' - [lfp_tidy()] Tidy raw data, but do not process into SVD object.
#' - [ssvd_hfd()] Created scaled SVD object from Human Fertility
#'   Database age-specific fertility rates
#' - [bage::SVD()], [bage::SVDS()], [bage::ESVD()], [bage::ESVDS()]
#'   to use an object of class
#'   [`"bage_ssvd"`][bage::ssvd()] in a prior.
#'
#' @examples
#' LFP_small <- ssvd_lfp(oecd_lfp)
#' @export
ssvd_lfp <- function(data, age_max = 75, n_comp = 5) {
  check_n(n = age_max,
          nm_n = "age_max",
          min = 20L,
          max = 75L,
          divisible_by = 5L)
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 5L,
          max = 10L,
          divisible_by = 1L)
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
  bage::ssvd(data)
}


## HAS_TESTS
#' Tidy OECD Labor Force Participation Data
#'
#' Tidy data frame containing data downloaded from
#' the Labour Force Indicators table of the
#' [OECD Data Explorer](https://data-explorer.oecd.org). 
#'
#' @param data A data frame
#'
#' @returns A tibble.
#'
#' @seealso
#' - [ssvd_lfp()] Tidy data, and convert it into
#'   and an SVD object.
#'
#' @examples
#' data_tidy <- lfp_tidy(oecd_lfp)
#' @export
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


## helper functions -----------------------------------------------------------

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
                                bage::ssvd_comp,
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
                       bage::ssvd_comp,
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
                       bage::ssvd_comp,
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






