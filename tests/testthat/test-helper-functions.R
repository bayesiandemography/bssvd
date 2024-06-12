
## 'get_ages_max' -------------------------------------------------------------

test_that("'get_ages_max' works", {
  age <- c("10-14", "50+", "20+", "15-19", "125+", "50+")
  ans_obtained <- get_ages_max(age, age_max = 200)
  ans_expected <- c(20L, 50L, 125L)
  ans_obtained <- get_ages_max(age, age_max = 70)
  ans_expected <- c(20L, 50L)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_and_offset' ---------------------------------------------------

test_that("'make_matrix_and_offset' performs correctly with valid inputs - log", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rgamma(nrow(x), shape = 0.2)
    x <- poputils::to_matrix(x,
                             rows = c(age, sex),
                             cols = c(country, year), measure = value)
    ans <- make_matrix_and_offset(x = x, transform = "log")
    expect_identical(names(ans), c("matrix", "offset"))
    expect_identical(dim(ans$matrix), c(18L, 10L))
    expect_identical(length(ans$offset), 18L)
    expect_false(any(vapply(ans, anyNA, FALSE)))
    expect_false(any(vapply(ans, function(x) any(is.infinite(x)), FALSE)))
})

test_that("'make_matrix_and_offset' performs correctly with valid inputs - logit", {
    set.seed(0)
    x <- expand.grid(age = 0:9,
                     sex = c("F", "M", "T"),
                     year = 2000:2005,
                     country = letters)
    x$value <- rbeta(nrow(x), shape1 = 2, shape2 = 3)
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    ans <- make_matrix_and_offset(x = x, transform = "logit")
    expect_false(any(sapply(ans, function(x) any(is.infinite(x)))))
    expect_false(any(sapply(ans, anyNA)))
})

test_that("'make_matrix_and_offset' performs correctly with valid inputs - none", {
    set.seed(0)
    x <- expand.grid(age = 0:9,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rnorm(nrow(x))
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    ans <- make_matrix_and_offset(x = x, transform = "none")
    expect_false(any(sapply(ans, function(x) any(is.infinite(x)))))
    expect_false(any(sapply(ans, anyNA)))
})

test_that("'make_matrix_and_offset' gives expected error when too few columns", {
    set.seed(0)
    x <- expand.grid(age = 0:10,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = c("a", "b"))
    x$value <- rnorm(nrow(x))
    x <- poputils::to_matrix(x, rows = c(age, sex), cols = c(year, country), measure = value)
    expect_error(make_matrix_and_offset(x = x, transform = "none"),
                 "`ncol\\(x\\)` less than `n_comp`.")
})

test_that("'make_matrix_and_offset' gives expected error when too few columns", {
    set.seed(0)
    x <- expand.grid(age = 0:2,
                     sex = c("F", "M", "T"),
                     year = 2000:2011,
                     country = c("a", "b"))
    x$value <- rnorm(nrow(x))
    x <- poputils::to_matrix(x, rows = c(age, sex), cols = c(year, country), measure = value)
    expect_error(make_matrix_and_offset(x = x, transform = "none"),
                 "`nrow\\(x\\)` less than `n_comp`.")
})

test_that("'make_matrix_and_offset' gives expected error when negative values", {
    x <- expand.grid(age = 0:10,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = c("a", "b"))
    x$value <- c(-2, -2, rep(0.1, times = nrow(x) - 2))
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    expect_error(make_matrix_and_offset(x = x, transform = "log"),
                 "`transform` is \"log\" but `x` has 2 negative values")
})

test_that("'make_matrix_and_offset' gives expected error when negative values", {
    x <- expand.grid(age = 0:10,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = c("a", "b"))
    x$value <- c(2, 2, rep(0.1, times = nrow(x) - 2))
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    expect_error(make_matrix_and_offset(x = x, transform = "logit"),
                 "`transform` is \"logit\" but `x` has 2 values greater than 1.")
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


## 'replace_zeros' ------------------------------------------------------------

test_that("'replace_zeros' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 2), nrow = 5)
    x[sample(100, 10)] <- 0
    ans_obtained <- replace_zeros(x)
    expect_true(all(ans_obtained > 0))
    expect_true(all(ans_obtained[x > 0] == x[x > 0]))
})


## 'replace_zeros_ones' -------------------------------------------------------

test_that("'replace_zeros_ones' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 1), nrow = 5)
    x[sample(100, 5)] <- 0
    x[sample(100, 5)] <- 1
    ans_obtained <- replace_zeros_ones(x)
    expect_true(all(ans_obtained > 0))
    expect_true(all(ans_obtained < 1))
    expect_true(all(ans_obtained[(x > 0) & (x < 1)] == x[(x > 0) & (x < 1)]))
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

