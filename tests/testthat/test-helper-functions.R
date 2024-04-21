
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
