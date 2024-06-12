
## 'check_is_matrix' ----------------------------------------------------------

test_that("'check_is_matrix' works with valid inputs", {
  expect_true(check_is_matrix(x = matrix(), nm_x = "data"))
})

test_that("'check_is_matrix' throws correct error with non-matrix", {
  expect_error(check_is_matrix(x = "a", nm_x = "val"),
               "`val` is not a matrix.")
})


## 'check_n' ------------------------------------------------------------------

test_that("'check_n' returns TRUE with valid inputs", {
    expect_true(check_n(n = 4, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L))
})

test_that("'check_n' throws correct error with non-numeric", {
    expect_error(check_n(n = "4", nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is non-numeric")
})

test_that("'check_n' throws correct error with wrong length", {
    expect_error(check_n(n = integer(), nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` does not have length 1")
    expect_error(check_n(n = 10:11, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` does not have length 1")
})

test_that("'check_n' throws correct error with NA", {
    expect_error(check_n(n = NA_real_, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is NA")
})

test_that("'check_n' throws correct error with Inf", {
    expect_error(check_n(n = Inf, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is Inf")
})

test_that("'check_n' throws correct error with non-integer", {
    expect_error(check_n(n = 6.4, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is not an integer")
})

test_that("'check_n' throws correct error when less than min", {
    expect_error(check_n(n = 3, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is less than 4")
})

test_that("'check_n' throws correct error when greater than max", {
    expect_error(check_n(n = 60, nm_n = "n", min = 4, max = 10, divisible_by = 1L),
                 "`n` is greater than 10")
})

test_that("'check_n' throws correct error when not divisible by 'divisible_by'", {
    expect_error(check_n(n = 61, nm_n = "n", min = 4, max = Inf, divisible_by = 5L),
                 "`n` is not divisible by 5")
})



## 'check_numeric' ------------------------------------------------------------------

test_that("'check_numeric' returns TRUE with valid inputs", {
    expect_true(check_numeric(x = 1:4, nm_x = "x"))
    expect_true(check_numeric(x = c(-1, 0), nm_x = "x"))
})

test_that("'check_numeric' throws correct error with non-numeric", {
    expect_error(check_numeric(x = "4", nm_x = "x"),
                 "`x` is non-numeric")
})

test_that("'check_numeric' throws correct error with 0 length", {
    expect_error(check_numeric(x = integer(), nm_x = "x"), 
                 "`x` has length 0")
})

test_that("'check_numeric' throws correct error with NA", {
    expect_error(check_numeric(x = c(1, NA), nm_x = "x"),
                 "`x` has NA")
})

test_that("'check_numeric' throws correct error with Inf", {
    expect_error(check_numeric(x = c(100, -1, Inf), nm_x = "x"),
                 "`x` has non-finite value.")
})
