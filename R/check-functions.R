
## HAS_TESTS
#' Check that an Object is a Matrix
#'
#' @param x An object
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_is_matrix <- function(x, nm_x) {
  if (!is.matrix(x))
    cli::cli_abort(c("{.arg {nm_x}} is not a matrix.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check 'n' argument
#'
#' @param n A whole number
#' @param nm_n Name for 'n' to be used in error messages
#' @param min,max Minimum and maximum values 'n' can take
#' @param divisible_by 'n' must be divisible by this
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n <- function(n, nm_n, min, max, divisible_by) {
  if (!is.numeric(n))
    cli::cli_abort(c("{.arg {nm_n}} is non-numeric.",
                     i = "{.arg {nm_n}} has class {.cls {class(n)}}."))
  if (length(n) != 1L)
    cli::cli_abort(c("{.arg {nm_n}} does not have length 1.",
                     i = "{.arg {nm_n}} has length {length(n)}."))
  if (is.na(n))
    cli::cli_abort("{.arg {nm_n}} is {.val {NA}}.")
  if (is.infinite(n))
    cli::cli_abort("{.arg {nm_n}} is {.val {Inf}}.")
  if (!isTRUE(all.equal(round(n), n)))
    cli::cli_abort(c("{.arg {nm_n}} is not an integer.",
                     i = "{.arg {nm_n}} is {.val {n}}."))
  if (n < min)
    cli::cli_abort(c("{.arg {nm_n}} is less than {min}.",
                     i = "{.arg {nm_n}} is {.val {n}}."))
  if (!is.null(max) && (n > max))
    cli::cli_abort(c("{.arg {nm_n}} is greater than {max}.",
                     i = "{.arg {nm_n}} is {.val {n}}."))
  if (n %% divisible_by != 0L)
    cli::cli_abort(c("{.arg {nm_n}} is not divisible by {divisible_by}.",
                     i = "{.arg nm_n}: {.val {n}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Vector is Numeric, Non-NA, Finite, Non-Zero Length
#'
#' @param x A vector
#' @param nm_x Name to be used in error messages
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_numeric <- function(x, nm_x) {
  if (!is.numeric(x))
    cli::cli_abort(c("{.arg {nm_x}} is non-numeric.",
                     i = "{.arg {nm_x}} has class {.cls {class(x)}}."))
  if (length(x) == 0L)
    cli::cli_abort("{.arg {nm_x}} has length 0.")
  if (anyNA(x))
    cli::cli_abort("{.arg {nm_x}} has {.val {NA}}.")
  if (any(is.infinite(x)))
    cli::cli_abort("{.arg {nm_x}} has non-finite value.")
  invisible(TRUE)
}
