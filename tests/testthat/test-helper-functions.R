
## 'get_ages_max' -------------------------------------------------------------

test_that("'get_ages_max' works", {
  age <- c("10-14", "50+", "20+", "15-19", "125+", "50+")
  ans_obtained <- get_ages_max(age, age_max = 200)
  ans_expected <- c(20L, 50L, 125L)
  ans_obtained <- get_ages_max(age, age_max = 70)
  ans_expected <- c(20L, 50L)
  expect_identical(ans_obtained, ans_expected)
})


## 'remove_cols_with_na' -----------------------------------------------------

test_that("'remove_cols_with_na' works when NAs present", {
  x <- matrix(1, nr = 10, ncol = 12)
  x[2, c(3, 6)] <- NA
  ans_obtained <- remove_cols_with_na(x, n_comp = 10)
  ans_expected <- matrix(1, nrow = 10, ncol = 10)
  expect_identical(ans_obtained, ans_expected)
  expect_error(remove_cols_with_na(x, n_comp = 11),
               "After removing columns with NAs, matrix has too few columns to perform SVD.")
})


## 'trim_01' ------------------------------------------------------------------

test_that("'trim_01' works with valid inputs", {
  x <- c(0.5,  0, -0.1, NA, 1, Inf, 2, NaN, 0.4)
  ans_expected <- trim_01(x)
  ans_obtained <- c(0.5, 0.4, 0.4, NA, 0.5, 0.5, 0.5, NaN, 0.4)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'trim_01' works when x all NA", {
  x <- rep(NA_real_, 5)
  ans_expected <- trim_01(x)
  ans_obtained <- x
  expect_identical(ans_obtained, ans_expected)
})

test_that("'trim_01' works when x has length 0", {
  x <- numeric()
  ans_expected <- trim_01(x)
  ans_obtained <- x
  expect_identical(ans_obtained, ans_expected)
})

test_that("'trim_01' throws correct error needs to truncate but cannot", {
  x <- c(0, -0.1, NA, 1, Inf, 2, NaN)
  expect_error(trim_01(x),
               "Unable to calculate truncated values.")
  x <- c(0, NA)
  expect_error(trim_01(x),
               "Unable to calculate truncated values.")
})

