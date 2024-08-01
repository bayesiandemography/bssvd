
## HAS_TESTS
#' Prepare Data frome Human Mortality Database
#'
#' Process data on age-specific mortality rates
#' from a zipped file downloaded from the
#' [Human Mortality Database](https://www.mortality.org/Data/ZippedDataFiles).
#'
#' @section Usage:
#' **Step 1: Download data**
#'
#' Register or log in at the Human Mortality Database, and go to page
#' [Downloading the HMD in zipped data files](https://www.mortality.org/Data/ZippedDataFiles).
#' Go to the "Previous Versions" table at the bottom of the page, and
#' download a file from the "Statistics" column, eg file
#'
#' https://www.mortality.org/File/Download/hmd.v6/zip/all_hmd/hmd_statistics_20240226.zip
#'
#' **Step 2: Call function 'svd_hmd'**
#'
#' Put the file into the working directory, and supply the name of the file
#' to function `data_ssvd_hmd()`, eg
#' ```
#' data_hmd <- data_ssvd_hmd("hmd_statistcs_20240226")
#' ```
#'
#' @param zipfile The name of a zipped file downloaded
#' from the Human Mortality Database.
#' A path name that is handled by [utils::unzip()].
#' @param n_comp Number of SVD components
#' to include in result. Default is `5`.
#'
#' @returns A tibble with the format required by
#' function `bage::ssvd()`.
#'
#' @seealso
#' - [data_ssvd_hfd()] Prepare data on age-specific fertility rates
#'   from Human Fertility Database
#' - [data_ssvd_lfs()] Prepare data on labour force participation
#'   rates from the OCED
#' - [bage::ssvd()] Create Scaled SVD object
#'
#' @export
data_ssvd_hmd <- function(zipfile, n_comp = 5) {
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 2L,
          max = 10L,
          divisible_by = 1L)
  n_comp <- as.integer(n_comp)
  cli::cli_progress_message("Unzipping file...")
  data <- hmd_unzip(zipfile)
  cli::cli_progress_message("Tidying data...")
  data <- hmd_tidy_data(data)
  cli::cli_progress_message("Creating five-year age groups...")
  data <- hmd_add_age_five(data)
  cli::cli_progress_message("Assembling datasets for alternative open age groups...")
  data <- hmd_vary_age_open(data)
  cli::cli_progress_message("Carrying out SVD for 'total'...")
  total <- hmd_total(data, n_comp = n_comp)
  cli::cli_progress_message("Carrying out SVD for 'indep'...")
  indep <- hmd_indep(data, n_comp = n_comp)
  cli::cli_progress_message("Carrying out SVD for 'joint'...")
  joint <- hmd_joint(data, n_comp = n_comp)
  cli::cli_progress_message("Combining results...")
  data <- vctrs::vec_rbind(total, indep, joint)
  data <- tibble::as_tibble(data)
  data
}  


## helper functions -----------------------------------------------------------

#' Create Age Group "five" and Add to 'data'
#'
#' Create age group "five" by combining
#' age groups 0 and 1-4 in "lt"
#'
#' @param data A data frame
#'
#' @returns Modifed version of 'data'
#'
#' @noRd
hmd_add_age_five <- function(data) {
  five <- data[data$type_age == "lt", ]
  five$type_age <- "five"
  is_04 <- five$age %in% c("0", "1-4")
  five_04 <- five[is_04, ]
  five_5pl <- five[!is_04, ]
  five_04$age <- "0-4"
  five_04 <- hmd_aggregate_mx_Lx(five_04)
  five <- vctrs::vec_rbind(five_04, five_5pl)
  vctrs::vec_rbind(data, five)
}


## HAS_TESTS
#' Take Weighted Mean of 'mx' and Add Up 'Lx'
#'
#' Aggregation happens within categories
#' defined by non-mx, non-Lx variables.
#'
#' Case where Lx all 0 or all NA dealt with.
#'
#' @param data A data frame that includes
#' columns 'mx' and 'Lx'
#'
#' @returns A data frame
#'
#' @noRd
hmd_aggregate_mx_Lx <- function(data) {
  nms <- names(data)
  nms_x <- c("mx", "Lx")
  nms_by <- setdiff(nms, nms_x)
  data <- vctrs::vec_split(x = data[nms_x], by = data[nms_by])
  Lx <- vapply(data$val, function(x) sum(x$Lx), 0)
  mx <- double(length = length(Lx))
  is_pos <- Lx > 0
  mx[is_pos] <- vapply(data$val[is_pos],
                       function(x) stats::weighted.mean(x = x$mx, w = x$Lx), 0)
  mx[!is_pos] <- vapply(data$val[!is_pos], function(x) mean(x$mx), 0)
  vctrs::vec_cbind(data$key, mx = mx, Lx = Lx)
}


## HAS_TESTS
#' Prepare Inputs for "indep" Type, ie Female and Male Modelled Separately
#'
#' @param data A data frame
#'
#' @returns A data frame
#'
#' @noRd
hmd_indep <- function(data, n_comp) {
  data <- data[data$sex != "Total", ]
  data <- vctrs::vec_split(x = data[c("age", "country", "time", "mx")],
                           by = data[c("type_age", "age_open", "sex")])
  format_age <- function(x) {
    x$age <- poputils::reformat_age(x$age)
    x <- x[order(x$age), ]
    x
  }
  data$val <- lapply(data$val, format_age)
  x <- lapply(data$val,
              poputils::to_matrix,
              rows = "age",
              cols = c("country", "time"),
              measure = "mx")
  l <- lapply(x,
              make_matrix_and_offset,
              transform = "log",
              n_comp = n_comp)
  matrix <- lapply(l, function(x) x$matrix)
  offset <- lapply(l, function(x) x$offset)
  labels_sexgender <- .mapply(rep.int,
                              dots = list(x = as.character(data$key$sex),
                                          times = lengths(offset)),
                              MoreArgs = list())
  labels_age <- lapply(offset, names)
  f <- data$key[c("type_age", "age_open")]
  matrix <- split(matrix, f = f)
  offset <- split(offset, f = f)
  matrix <- lapply(matrix, Matrix::.bdiag)
  concat_list <- function(x) do.call(c, x)
  offset <- lapply(offset, concat_list)
  labels_sexgender <- split(labels_sexgender, f = f)
  labels_age <- split(labels_age, f = f)
  labels_sexgender <- lapply(labels_sexgender, concat_list)
  labels_age <- lapply(labels_age, concat_list)
  for (i in seq_along(offset)) {
    nms <- paste(labels_sexgender[[i]], labels_age[[i]], sep = ".")
    names(offset[[i]]) <- nms
    rownames(matrix[[i]]) <- nms
  }
  ans <- tibble::tibble(type = "indep",
                 labels_age = labels_age,
                 labels_sexgender = labels_sexgender,
                 matrix = matrix,
                 offset = offset)
  ans[-1L] <- lapply(ans[-1L], unname)
  ans
}


## HAS_TESTS
#' Prepare Inputs for "joint" Type, ie Female and Male Modelled Jointly
#'
#' @param data A data frame
#' @param n_comp Number of SVD components
#' to include in result.
#'
#' @returns A data frame
#'
#' @noRd
hmd_joint <- function(data, n_comp) {
  data <- data[data$sex != "Total", ]
  data <- vctrs::vec_split(x = data[c("age", "sex", "country", "time", "mx")],
                           by = data[c("type_age", "age_open")])
  format_sexage <- function(x) {
    x$sex <- poputils::reformat_sex(x$sex)
    x$age <- poputils::reformat_age(x$age)
    x <- x[order(x$sex, x$age), ]
    x
  }
  data$val <- lapply(data$val, format_sexage)
  x <- lapply(data$val,
              poputils::to_matrix,
              rows = c("sex", "age"),
              cols = c("country", "time"),
              measure = "mx")
  l <- lapply(x,
              make_matrix_and_offset,
              transform = "log",
              n_comp = n_comp)
  matrix <- lapply(l, function(x) x$matrix)
  offset <- lapply(l, function(x) x$offset)
  rn <- lapply(matrix, rownames)
  p_rn <- "^(.*)\\.(.*)$"
  labels_age <- lapply(rn, function(x) sub(p_rn, "\\2", x))
  labels_sexgender <- lapply(rn, function(x) sub(p_rn, "\\1", x))
  tibble::tibble(type = "joint",
                 labels_age = labels_age,
                 labels_sexgender = labels_sexgender,
                 matrix = matrix,
                 offset = offset)
}


## HAS_TESTS
#' Tidy Raw HMD data
#'
#' Steps:
#' - rename "Age" to "age"
#' - rename "Year" to "time"
#' - change levels for 'sex' variable to "Total", "Female", "Male"
#' - exclude any country-time combinations that have
#'   NAs for 'mx' or 'Lx'
#'
#' @param data A data fame with raw HMD data
#'
#' @returns A cleaned data frame
#'
#' @noRd
hmd_tidy_data <- function(data) {
  names(data)[[match("Age", names(data))]] <- "age"
  names(data)[[match("Year", names(data))]] <- "time"
  data$sex <- factor(data$sex,
                     levels = c("both", "female", "male"),
                     labels = c("Total", "Female", "Male"))
  data$type_age <- factor(data$type_age,
                          levels = c("1x1", "five", "5x1"), ## "5x1" splits 0-4
                          labels = c("single", "five", "lt"))
  has_na <- stats::aggregate(x = data.frame(has_na = is.na(data$mx) | is.na(data$Lx)),
                             by = data[c("country", "time", "type_age")],
                             FUN = any)
  data <- merge(x = data,
                y = has_na,
                by = c("country", "time", "type_age"),
                sort = FALSE)
  data <- data[!data$has_na, ]
  data <- data[-match("has_na", names(data))]
  data
}


## HAS_TESTS
#' Prepare Inputs for "total" Type, ie Female and Male Combined
#'
#' @param data A data frame
#' @param n_comp Number of SVD components
#' to include in result.
#'
#' @returns A data frame
#'
#' @noRd
hmd_total <- function(data, n_comp) {
  data <- data[data$sex == "Total", ]
  data <- vctrs::vec_split(x = data[c("age", "country", "time", "mx")],
                           by = data[c("type_age", "age_open")])
  format_age <- function(x) {
    x$age <- poputils::reformat_age(x$age)
    x <- x[order(x$age), ]
    x
  }
  data$val <- lapply(data$val, format_age)
  x <- lapply(data$val,
              poputils::to_matrix,
              rows = "age",
              cols = c("country", "time"),
              measure = "mx")
  l <- lapply(x,
              make_matrix_and_offset,
              transform = "log",
              n_comp = n_comp)
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
#' Convert a Zipped HMD into a Combined Data Frame
#'
#' Zipped HMD file obtained from
#' https://www.mortality.org/Data/ZippedDataFiles
#' 
#' @param file Name of zipped HMD file
#'
#' @returns A tibble
#'
#' @noRd
hmd_unzip <- function(zipfile) {
  dirs <- c("lt_both/bltper_1x1",
            "lt_both/bltper_5x1",
            "lt_female/fltper_1x1",
            "lt_female/fltper_5x1",
            "lt_male/mltper_1x1",
            "lt_male/mltper_5x1")
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir, recursive = TRUE))
  utils::unzip(zipfile, exdir = tmp_dir)
  get_data <- function(dir) {
    country_code <- sub("([A-Z]+)\\..*", "\\1", list.files(dir))
    sex <- sub(".*lt_([a-z]+).*", "\\1", dir)
    type_age <- sub(".*ltper_([1x5]+).*", "\\1", dir)
    files <- list.files(dir, full.names = TRUE)
    ans <- lapply(files,
                  utils::read.table,
                  header = TRUE,
                  colClasses = c("integer",
                                 "character",
                                 "double",
                                 rep("NULL", 4L),
                                 "integer",
                                 rep("NULL", 2L)),
                  na.strings = ".",
                  skip = 1)
    add_nm <- function(x, nm) {
      x[["country"]] <- nm
      x <- x[c(length(x), seq_len(length(x) - 1L))]
      x
    }
    ans <- .mapply(add_nm,
                   dots = list(x = ans,
                               nm = country_code),
                   MoreArgs = list())
    ans <- do.call(rbind, ans)
    ans$sex <- sex
    ans$type_age <- type_age
    ans
  }
  base_zipfile <- sub("\\.zip$", "", basename(zipfile))
  if (base_zipfile %in% dir(tmp_dir)) {
    dirs_unpacked <- file.path(tmp_dir, base_zipfile, dirs)
  }
  else if (all(c("lt_both", "lt_female", "lt_male") %in% dir(tmp_dir))) {
    dirs_unpacked <- file.path(tmp_dir, dirs)
  }
  else {
    cli::cli_abort(c("Did not get expected files when unzipped {.arg zipfile}.",
                     i = "Contents of temporary directory after unzipping: {.val {dir(tmp_dir)}}.",
                     i = "Value of {.arg zipfile}: {.file {zipfile}}."))
  }
  ans <- lapply(dirs_unpacked, get_data)
  ans <- do.call(rbind, ans)
  ans
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
hmd_vary_age_open <- function(data) {
  data <- vctrs::vec_split(data, data["type_age"])
  ans <- lapply(data$val, hmd_vary_age_open_type_age)
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}


## HAS_TESTS
#' Create Multiple Versions of Data, Each with Different
#' Open Age Group - For a Single Value of 'type_age'
#'
#' @param x A data frame
#'
#' @returns A data frame
#'
#' @noRd
hmd_vary_age_open_type_age <- function(x) {
  age_open <- seq.int(from = 60L, to = 110L, by = 5L)
  make_data_age_open <- function(age_open, X) {
    X$age <- as.character(X$age)
    X$age <- poputils::set_age_open(X$age, lower = age_open)
    X <- hmd_aggregate_mx_Lx(X)
    X$age_open <- age_open
    X
  }
  ans <- lapply(age_open, function(a) make_data_age_open(a, X = x))
  ans <- vctrs::vec_rbind(!!!ans)
  ans
}
