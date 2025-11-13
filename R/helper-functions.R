
## User-visible ---------------------------------------------------------------

## HAS_TESTS
#' Create Components Needed by a Scaled SVD Object
#'
#' Use the [Singular Value Decomposition][base::svd()]
#' (SVD) to construct a parsimonious representation
#' of a set of rates, probabilities, means, or
#' other values. The construction proceeds as follows:
#' - transform values in matrix `x` (eg take logs)
#' - carry out a SVD on the transformed version of `x`
#' - centre and scale the results from the SVD to produce
#' matrix `matrix` and vector `offset`.
#'
#' If 
#' - \eqn{X} is a the matrix of transformed values
#' of `x`
#' - \eqn{F} is the matrix called `matrix`,
#' - \eqn{g} is the vector called `offset`, and
#' - \eqn{\alpha} is a vector of standard normal variates,
#'
#' and
#'
#' \deqn{\beta = F \alpha + g}
#'
#' then \eqn{\beta} should look like a randomly-selected
#' column from \eqn{X}.
#'
#' Matrix `x` typically has age along the rows,
#' and some combination of classification variables,
#' such as country and time, along the columns.
#' One exception is when the SVD is used to capture
#' the relationship between female and male rates,
#' in which case rows are formed by interacting
#' sex and age. See below for an example.
#'
#' When `transform` is `"log"` or `"logit"`,
#' `make_matrix_and_offset()` converts any
#' values in `x` that are less than `eps` to
#' `eps` before transforming.
#' 
#' When `transform` is `"logit"`,
#' `make_matrix_and_offset() converts any
#' values greater than `1-eps` to `1-eps`
#' before transforming.
#'
#' @param x A matrix with value such as rates,
#' probabilities, or means.
#' @param transform `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#' @param n_comp Number of components.
#' @param eps Quantity used in setting
#' floor and ceiling on value. See
#' below for details. Default is `0.00001`.
#' 
#'
#' @returns A named list with two elements:
#' - `matrix`, a numeric matrix
#' - `offset`, a numeric vector
#'
#' @examples
#' x <- matrix(rgamma(n = 150, shape = 1),
#'             nrow = 10,
#'             ncol = 15)
#' x
#' make_matrix_and_offset(x)
#' make_matrix_and_offset(x, transform = "none")
#' make_matrix_and_offset(x, n_comp = 2)
#' @export
make_matrix_and_offset <- function(x,
                                   transform = c("log", "logit", "none"),
                                   n_comp = 10,
                                   eps = 0.00001) {
  ## check 'n_comp'
  poputils::check_n(n = n_comp,
                    nm_n = "n_comp",
                    min = 1L,
                    max = NULL,
                    divisible_by = NULL)
  ## check 'x'
  check_is_matrix(x, nm_x = "x")
  check_numeric(x, nm_x = "x")
  if (nrow(x) < n_comp)
    cli::cli_abort(c("{.code nrow(x)} less than {.arg n_comp}.",
                     i = "{.code nrow(x)}: {.val {nrow(x)}}.",
                     i = "{.arg n_comp}: {.val {n_comp}}."))
  if (ncol(x) < n_comp)
    cli::cli_abort(c("{.code ncol(x)} less than {.arg n_comp}X.",
                     i = "{.code ncol(x)}: {.val {ncol(x)}}.",
                     i = "{.arg n_comp}: {.val {n_comp}}."))
  ## check 'transform'
  transform <- match.arg(transform)
  if (transform %in% c("log", "logit")) {
    n_neg <- sum(x < 0)
    if (n_neg > 0)
      cli::cli_abort(paste("{.arg transform} is {.val {transform}} but {.arg x} has",
                           "{n_neg} negative value{?s}."))
  }
  if (transform  == "logit") {
    n_gt_1 <- sum(x > 1)
    if (n_gt_1 > 0L)
      cli::cli_abort(paste("{.arg transform} is {.val {transform}} but {.arg x} has",
                           "{n_gt_1} value{?s} greater than 1."))
  }
  ## check 'eps'
  check_eps(eps)
  ## transform to log or logit scale if necessary
  if (transform == "log") {
    x <- replace_zeros(x, eps = eps)
    x <- log(x)
  }
  if (transform == "logit") {
    x <- replace_zeros_ones(x, eps = eps)
    x <- poputils::logit(x)
  }
  ## apply svd
  svd <- svd(x = x,
             nu = n_comp,
             nv = n_comp)
  U <- svd$u
  D <- diag(svd$d[seq_len(n_comp)])
  V <- svd$v
  ## standardise
  m <- colMeans(V)
  S <- diag(apply(V, MARGIN = 2L, FUN = stats::sd))
  matrix <- U %*% D %*% S
  offset <- as.numeric(U %*% D %*% m)
  ## add names
  dn <- dimnames(x)
  if (!is.null(dn[[1L]])) {
    dimnames(matrix) <- c(dn[1L], list(component = seq_len(n_comp)))
    names(offset) <- dn[[1L]]
  }
  ## convert matrix to sparse matrix
  matrix <- Matrix::sparseMatrix(i = row(matrix),
                                 j = col(matrix),
                                 x = as.double(matrix),
                                 dimnames = dimnames(matrix))
  ## return
  list(matrix = matrix,
       offset = offset)
}


## NO_TESTS
#' Calculate the Percent of Variance Explained
#' by Components in SVD
#'
#' Calculate what percent of variance in a dataset
#' can be explained by the first `n_comp` components
#' of a singular value decompositon. The dataset should
#' include age, a single sex or gender category,
#' and one or more classifying variables.
#'
#' The dataset is transformed
#' before the SVD is applied. If the transformation
#' is `"log"`, then zeros are moved upwards to the
#' lowest non-zero value appearing in the data
#' before the transformation is applied.
#' If the transformation is `"logit"`, then
#' zeros are moved upwards and ones are
#' moved downward.
#'
#' `data` must contain a variable called `"age"`.
#' It must also contain the variables named by
#' the `measure` and `cols` arguments. The age
#' and `cols` arguments must uniquely identify
#' all cells in the dataset.
#'
#' Before the log or logit transforms are applied
#' all values less than `eps` are changed to `eps`.
#' Before the logit transform is applied, all values
#' greater than `1 - eps` are changed to `1 - eps`.
#'
#' @param data A data frame
#' @param measure Name of the rate or proportion
#' being modelled.
#' @param cols Names of the categorical variables
#' used to distinguish different age profiles.
#' @param transform Transformation to apply
#' to the data: `"log"` (the default) or `"logit"`.
#' @param n_comp Number of components to
#' give percentages for. Default is `5`.
#' @param eps Parameter controlling truncation.
#' See below for details. Default is `0.00001`.
#'
#' @returns A numeric vector of length `n_comp`.
#'
#' @export
percent_variance <- function(data,
                             measure,
                             cols = c("country", "time"),
                             transform = c("log", "logit"),
                             n_comp = 5,
                             eps = 0.00001) {
  transform <- match.arg(transform)
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 1L,
          max = NULL,
          divisible_by = 1L)
  check_eps(eps)
  nms_required <- c(measure, cols, "age")
  nms_data <- names(data)
  is_in_data <- nms_required %in% nms_data
  i_not_in_data <- match(FALSE, is_in_data, nomatch = 0L)
  if (i_not_in_data > 0L) {
    nm_mis <- nms_required[[i_not_in_data]]
    cli::cli_abort("Variable {.var {nm_mis}} not found in {.arg data}.")
  }
  data$age <- poputils::reformat_age(data$age)
  cols_ord <- c("age", cols)
  data_cols_ord <- unname(as.list(data[cols_ord]))
  args <- c(data_cols_ord, list(decreasing = FALSE))
  ord <- do.call(order, args)
  data <- data[ord, , drop = FALSE]
  m <- poputils::to_matrix(x = data,
                           rows = "age",
                           cols = cols,
                           measure = measure)
  m <- remove_cols_with_na(x = m, n_comp = n_comp)
  if (transform == "log") {
    m <- replace_zeros(m, eps = eps)
    m <- log(m)
  }
  else if (transform == "logit") {
    m <- replace_zeros_ones(m, eps = eps)
    m <- poputils::logit(m)
  }
  else
    cli::cli_abort("Internal error: Unexpected value for transform") # nocov
  svd <- svd(m, nu = 0L, nv = 0L)
  print(svd$d)
  svd2 <- svd(m)
  print(svd2$d)
  sing_val <- svd$d
  n_sing_val <- length(sing_val)
  if (n_sing_val < n_comp)
    cli::cli_abort(paste("Number of singular values ({.val n_sing_val})",
                         "less than {.arg n_comp} ({.val {n_comp}})."))
  ans <- 100 * cumsum(sing_val^2) / sum(sing_val^2)
  ans <- ans[seq_len(n_comp)]
  ans
}                   



## Internal -------------------------------------------------------------------

## HAS_TESTS
#' Obtain the Scaled 'U' Matrix From an SVD of Rates or Probabilities
#'
#' Assumes that the dataset has a single set of ages.
#'
#' @param data A data frame, typically produced by 'tidy_lfp'.
#' @param n_comp Number of components.
#' @param transform log (for rates) or logit (for probabilities)
#' @param eps Value controlling truncation
#'
#' @returns A tibble
#' 
#' @noRd
calculate_coef <- function(data, n_comp, transform, eps) {
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
  if (transform == "log") {
    ans <- lapply(ans, replace_zeros, eps = eps)
    ans <- lapply(ans, log)
  }
  else if (transform == "logit") {
    ans <- lapply(ans, replace_zeros_ones, eps = eps)
    ans <- lapply(ans, poputils::logit)
  }
  else
    cli::cli_abort("Internal error: Invalid valud for 'transform'.")
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
#' Extract Lower Limits of Open Age Groups
#'
#' @param age Character vector
#' @param age_max Only retain values less than
#' or equal to this.
#'
#' @returns An integer vector
#'
#' @noRd
get_ages_max <- function(age, age_max) {
  p <- "^([0-9]+)(\\+)$"
  ans <- grep(p, age, value = TRUE)
  ans <- unique(ans)
  ans <- sub(p, "\\1", ans)
  ans <- as.integer(ans)
  ans <- ans[ans <= age_max]
  ans <- sort(ans)
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
get_data_one <- function(data, labels_age) {
  ans <- data[data$age %in% labels_age, ]
  ans$age <- factor(ans$age, levels = labels_age)
  ord <- order(ans$age)
  ans <- ans[ord, , drop = FALSE]
  rownames(ans) <- NULL
  ans
}


## HAS_TESTS
#' Prepare Inputs for "indep" Type, ie Female and Male Separate
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#' @param transform Log or logit
#' @param eps Truncation of rates or probabilities
#'
#' @returns A data frame
#' 
#' @noRd
make_indep <- function(data, labels_age, n_comp, transform, eps) {
  data <- data[data$sex %in% c("Female", "Male"), ]
  data$sex <- poputils::reformat_sex(data$sex)
  data_split <- .mapply(get_data_one,
                        dots = list(labels_age = labels_age),
                        MoreArgs = list(data = data))
  data_split <- lapply(data_split,
                       function(x)
                         split(x[c("country", "time", "age", "value")],
                               x["sex"]))
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
                                transform = transform,
                                n_comp = n_comp,
                                eps = eps))
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
#' @param transform Log or logit
#' @param eps Truncation of rates or probabilities
#'
#' @returns A data frame
#'
#' @noRd
make_joint <- function(data, labels_age, n_comp, transform, eps) {
  data <- data[data$sex %in% c("Female", "Male"), ]
  data$sex <- poputils::reformat_sex(data$sex)
  data_split <- .mapply(get_data_one,
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
                       transform = transform,
                       n_comp = n_comp,
                       eps = eps)
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
#' Prepare Inputs for "total" Type, ie Female and Male Combined
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#' @param transform Log or logit
#' @param eps Truncation of probabilities
#'
#' @returns A data frame
#'
#' @noRd
make_total <- function(data, labels_age, n_comp, transform, eps) {
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
                       transform = transform,
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



## HAS_TESTS
#' Remove Any Columns Containing NAs
#' from a Matrix
#'
#' @param x A matrix
#'
#' @returns A matrix, possibly with
#' fewer columns than 'x'
#'
#' @noRd
remove_cols_with_na <- function(x, n_comp) {
  has_na <- apply(x, 2L, anyNA)
  ans <- x[ , !has_na, drop = FALSE]
  if (ncol(ans) < n_comp)
    cli::cli_abort(c("After removing columns with NAs, matrix has too few columns to perform SVD.",
                     i = "Number of columns: {.val {ncol(ans)}}.",
                     i = "Number of components for SVD: {.val {n_comp}}."))
  ans
}


## HAS_TESTS
#' Replace zeros in a matrix of
#' estimated rates
#'
#' Based on a simple main effects model,
#' replace values below `eps` in `x`, a matrix of
#' estimated rates. The replacement values
#' should typically be near 0.
#'
#' Assume that `x` is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no infinite values.
#'
#' @param x A matrix of rates.
#' @param eps Floor on values.
#'
#' @return A modified version of
#' matrix `x`.
#'
#' @noRd
replace_zeros <- function(x, eps) {
  is_zero <- x < eps
  if (any(is_zero)) {
    row_means <- rowMeans(x)
    col_sums <- colSums(x)
    standardized_row_means <- proportions(row_means) 
    predicted <- outer(standardized_row_means, col_sums)
    x[is_zero] <- pmax(0.5 * predicted[is_zero], eps)
  }
  x
}


## HAS_TESTS
#' Replace zeros and ones in a matrix 'x' of
#' estimated probabilities
#'
#' Based on a simple main effects model,
#' replace values below `eps` and
#' above `1 - eps` in `x`,
#' a matrix of estimated probabilities.
#' The replacement values should typically be
#' near 0 or 1.
#'
#' Assume that \code{x} is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no values above one.
#'
#' @param x A matrix of probabilities.
#' @param eps Small quantity defining
#' floor and ceiling.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros_ones <- function(x, eps) {
  is_zero <- x < eps
  is_one <- x > (1 - eps)
  if (any(is_zero) || any(is_one)) {
    row_means <- rowMeans(x)
    col_sums <- colSums(x)
    standardized_row_means <- proportions(row_means) 
    predicted <- outer(standardized_row_means, col_sums)
    x[is_zero] <- pmax(0.5 * predicted[is_zero], eps)
    x[is_one] <- pmin(0.5 + 0.5 * predicted[is_one], 1 - eps)
  }
  x
}
