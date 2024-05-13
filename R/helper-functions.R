
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
