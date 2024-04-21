
## HAS_TESTS
#' Check Maximum Age Group
#'
#' @param age_max Lower or upper limit for age group
#' @param minimum value for 'age_max'
#' @param divisible_by 'age_max' must be divisible by this
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_age_max <- function(age_max, min, divisible_by) {
  check_n(n = age_max,
          nm_n = "age_max",
          min = min,
          max = NULL,
          null_ok = FALSE)
  if (age_max %% divisible_by != 0L)
    cli::cli_abort(c("{.arg age_max} is not divisible by {divisible_by}.",
                     i = "{.arg age_max}: {.val {age_max}}."))
  invisible(TRUE)
}

## HAS_TESTS
#' Check 'n' argument
#'
#' @param n A whole number
#' @param nm_n Name for 'n' to be used in error messages
#' @param min,max Minimum and maximum values 'n' can take
#' @param null_ok Whether passing NULL (and skipping tests) is allowed
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_n <- function(n, nm_n, min, max, null_ok) {
    if (null_ok && is.null(n)) 
        return(invisible(TRUE))
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
    invisible(TRUE)
}

