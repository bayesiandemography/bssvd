
## 'check_age_max' ------------------------------------------------------------

test_that("'check_age_max' returns true with valid values", {
  expect_true(check_age_max(60L, min = 15L, divisible_by = 5L))
  expect_true(check_age_max(100L, min = 15L, divisible_by = 5L))
  expect_true(check_age_max(73L, min = 73, divisible_by = 1L))
})

test_that("'check_age_max' throws expected error with invalid values", {
  expect_error(check_age_max(23, min = 15L, divisible_by = 5L),
               "`age_max` is not divisible by 5.")
})
  


## 'check_n' ------------------------------------------------------------------

test_that("'check_n' returns TRUE with valid inputs", {
    expect_true(check_n(n = 4, nm_n = "n", min = 4L, max = NULL, null_ok = FALSE))
    expect_true(check_n(n = NULL, nm_n = "n", min = 4L, max = NULL, null_ok = TRUE))
})

test_that("'check_n' throws correct error with non-numeric", {
    expect_error(check_n(n = "4", nm_n = "n", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is non-numeric")
})

test_that("'check_n' throws correct error with wrong length", {
    expect_error(check_n(n = integer(), nm_n = "n", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` does not have length 1")
    expect_error(check_n(n = 10:11, nm_n = "n", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` does not have length 1")
})

test_that("'check_n' throws correct error with NA", {
    expect_error(check_n(n = NA_real_, nm_n = "n", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is NA")
})

test_that("'check_n' throws correct error with Inf", {
    expect_error(check_n(n = Inf, nm_n = "n", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is Inf")
})

test_that("'check_n' throws correct error with non-integer", {
    expect_error(check_n(n = 6.4, nm_n = "n", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is not an integer")
})

test_that("'check_n' throws correct error when less than min", {
    expect_error(check_n(n = 3, nm_n = "n", min = 4L, max = NULL, null_ok = FALSE),
                 "`n` is less than 4")
})

test_that("'check_n' throws correct error when greater than max", {
    expect_error(check_n(n = 60, nm_n = "n", min = 4, max = 10, null_ok = FALSE),
                 "`n` is greater than 10")
})
