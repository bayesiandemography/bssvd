
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


