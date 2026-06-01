
## 'calculate_coef_nosex' -----------------------------------------------------

test_that("'calculate_coef_nosex' works with valid inputs - log", {
  data <- tidy_hfd(asfr_subset)
  suppressMessages(ans <- calculate_coef_nosex(data,
                                               n_comp = 5L,
                                               transform = "log",
                                               eps = 0.001))
  expect_true(tibble::is_tibble(ans))
  expect_setequal(names(ans), c("country", "time", "component", "coef"))
  expect_equal(mean(ans$coef), 0)
})

test_that("'calculate_coef_nosex' works with valid inputs - logit", {
  data <- tidy_hfd(asfr_subset)
  suppressMessages(ans <- calculate_coef_nosex(data,
                                               n_comp = 5L,
                                               transform = "logit",
                                               eps = 0.001))
  expect_true(tibble::is_tibble(ans))
  expect_setequal(names(ans), c("country", "time", "component", "coef"))
  expect_equal(mean(ans$coef), 0)
})

test_that("'calculate_coef_nosex' throws error withe invalid transform", {
  data <- tidy_hfd(asfr_subset)
  expect_error(calculate_coef_nosex(data,
                                    n_comp = 5L,
                                    transform = "wrong",
                                    eps = 0.001),
               "Internal error")
})


## 'calculate_coef_sex' -------------------------------------------------------

test_that("'calculate_coef_sex' works with valid inputs", {
  file <- system.file("extdata",
                      "undesa_pd_2019_wmd_marital_status_subset.xlsx",
                      package = "bssvd")
  data <- tidy_wmd(file)
  suppressMessages(ans <- calculate_coef_sex(data,
                                             n_comp = 3,
                                             transform = "logit",
                                             eps = 0.001))
  expect_true(tibble::is_tibble(ans))
  expect_setequal(names(ans),
                  c("sex", "country", "time", "component", "coef"))
  expect_equal(mean(ans$coef), 0)
})


## 'get_ages_max' -------------------------------------------------------------

test_that("'get_ages_max' works", {
  age <- c("10-14", "50+", "20+", "15-19", "125+", "50+")
  ans_obtained <- get_ages_max(age, age_max = 200)
  ans_expected <- c(20L, 50L, 125L)
  ans_obtained <- get_ages_max(age, age_max = 70)
  ans_expected <- c(20L, 50L)
  expect_identical(ans_obtained, ans_expected)
})


## 'get_data_one' -------------------------------------------------------------

test_that("'get_data_one' works", {
  data <- data.frame(age = c("25-29", "20-24", "30-34", "30+"),
                     val = 1:4)
  labels_age <- c("20-24", "25-29", "30+")
  ans_obtained <- get_data_one(data = data,
                               labels_age = labels_age)
  ans_expected <- data.frame(age = factor(c("20-24", "25-29", "30+")),
                             val = c(2L, 1L, 4L))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_indep' ---------------------------------------------------------------

test_that("'make_indep' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                   min = 15,
                                                   max = 65,
                                                   open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- lfp_make_labels_age(data = data, age_max = 65)
  ans_obtained <- make_indep(data, labels_age = labels_age, n_comp = 5,
                             transform = "logit", eps = 0.00001)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgTMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
})


## 'make_joint' ---------------------------------------------------------------

test_that("'make_joint' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 65,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- lfp_make_labels_age(data = data, age_max = 65)
  ans_obtained <- make_joint(data, labels_age = labels_age, n_comp = 5,
                             transform = "logit", eps = 0.00001)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
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

## 'make_total' ---------------------------------------------------------------

test_that("'make_total' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 65,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- lfp_make_labels_age(data = data, age_max = 65)
  ans_obtained <- make_total(data, labels_age = labels_age, n_comp = 5,
                             transform = "logit", eps = 0.00001)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_age[[1]],
                         sep = "."))
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
    ans_obtained <- replace_zeros(x, eps = 0.01)
    expect_true(all(ans_obtained > 0.01))
    expect_true(all(ans_obtained[x > 0.01] == x[x > 0.01]))
})


## 'replace_zeros_ones' -------------------------------------------------------

test_that("'replace_zeros_ones' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 1), nrow = 5)
    x[sample(100, 5)] <- 0
    x[sample(100, 5)] <- 1
    ans_obtained <- replace_zeros_ones(x, eps = 0.01)
    expect_true(all(ans_obtained > 0))
    expect_true(all(ans_obtained < 1))
    expect_true(all(ans_obtained[(x > 0.01) & (x < .99)] == x[(x > 0.01) & (x < 0.99)]))
})

