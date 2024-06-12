
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
#' Create components needed by a ssvd object
#'
#' Use the [Singular Value Decomposition][base::svd()]
#' (SVD) to construct a parsimonious representation
#' of a set of rates, probabilities, means, or
#' other values.
#'
#' `make_matrix_and_offset() typically proceeds as follows:
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
#' When `scale` is `"log"` or `"logit"`,
#' `make_matrix_and_offset()` converts any `0`s in
#' `x` to values just above `0` before
#' applying the log or logit function.
#' When `scale` is `"logit"`,
#' `make_matrix_and_offset() also converts any
#' `1`s to values just below `1`.
#'
#' @param x A matrix with value such as rates,
#' probabilities, or means.
#' @param transform `"log"`, `"logit"`, or `"none"`.
#' Defaults to `"log"`.
#' @param n_comp Number of components.
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
make_matrix_and_offset <- function(x, transform = c("log", "logit", "none"), n_comp = 10) {
  ## check 'n_comp'
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 1L,
          max = NULL,
          divisible_by = 1L)
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
  ## transform to log or logit scale if necessary
  if (transform == "log") {
    x <- replace_zeros(x)
    x <- log(x)
  }
  if (transform == "logit") {
    x <- replace_zeros_ones(x)
    x <- log(x / (1 - x))
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
#' replace zeros in `x`, a matrix of
#' estimated rates. The replacement values
#' should typically be near 0.
#'
#' Assume that `x` is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no infinite values.
#'
#' @param x A matrix of rates.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros <- function(x) {
    is_zero <- x == 0
    if (any(is_zero)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
    }
    x
}


## HAS_TESTS
#' Replace zeros and ones in a matrix 'x' of
#' estimated probabilities
#'
#' Based on a simple main effects model,
#' replace zeros and ones in \code{x},
#' a matrix of estimated probabilities.
#' The replacement values should typically be
#' near 0 or 1.
#'
#' Assume that \code{x} is a valid numeric
#' matrix of rates with no NAs, no negative values,
#' and no values abouve one.
#'
#' @param x A matrix of probabilities.
#'
#' @return A modified version of
#' matrix \code{x}.
#'
#' @noRd
replace_zeros_ones <- function(x) {
    is_zero <- x == 0
    is_one <- x == 1
    if (any(is_zero) || any(is_one)) {
        row_means <- rowMeans(x)
        col_sums <- colSums(x)
        standardized_row_means <- proportions(row_means) 
        predicted <- outer(standardized_row_means, col_sums)
        x[is_zero] <- 0.5 * predicted[is_zero]
        x[is_one] <- 0.5 + 0.5 * predicted[is_one]
    }
    x
}


#' Truncate Values to (0, 1) Interval
#'
#' Allows for NAs
#'
#' @param x A numeric vector
#'
#' @returns A truncated version of `x`
#'
#' @noRd
trim_01 <- function(x) {
  is_obs <- !is.na(x)
  if (!any(is_obs))
    return(x)
  is_too_low <- x <= 0
  is_too_high <- x >= 1
  is_valid <- is_obs & !is_too_low & !is_too_high
  need_to_trunc_but_cannot <- any(is_obs) && !any(is_valid)
  if (need_to_trunc_but_cannot) {
    n_outside <- sum(is_too_low, is_too_high, na.rm = TRUE)
    cli::cli_abort(c("Unable to calculate truncated values.",
                     i = paste("{.arg x} has {n_outside} value{?s} outside the interval (0, 1),",
                               "but no values inside the interval.")))
  }
  valid <- x[is_valid]
  min <- min(valid)
  max <- max(valid)
  is_increase <- is_obs & is_too_low
  is_reduce <- is_obs & is_too_high
  x[is_increase] <- min
  x[is_reduce] <- max
  x
}
