
## 'coef_himd' -----------------------------------------------------------------

test_that("'coef_himd' works with valid inputs", {
  fn <- system.file("extdata", "himd_20241023_subset.zip", package = "bssvd")
  suppressMessages(ans <- coef_himd(fn))
  expect_true(tibble::is_tibble(ans))
  expect_setequal(names(ans),
                  c("country_orig_dest", "time", "component", "coef"))
  expect_equal(mean(ans$coef), 0)
})


## 'data_ssvd_himd' -----------------------------------------------------------------

test_that("'data_ssvd_hmd' works with valid inputs - time_interval is 1, measure_type is 'rate'", {
  fn <- system.file("extdata", "himd_20241023_subset.zip", package = "bssvd")
  suppressMessages(data <- data_ssvd_himd(fn))
  expect_true(tibble::is_tibble(data))
})

test_that("'data_ssvd_hmd' works with valid inputs - time_interval is 1, measure_type is 'prob'", {
  fn <- system.file("extdata", "himd_20241023_subset.zip", package = "bssvd")
  suppressMessages(data <- data_ssvd_himd(fn, measure_type = "prob"))
  expect_true(tibble::is_tibble(data))
})

test_that("'data_ssvd_hmd' works with valid inputs - time_interval is 1, measure_type is 'prob'", {
  fn <- system.file("extdata", "himd_20241023_subset.zip", package = "bssvd")
  suppressMessages(data <- data_ssvd_himd(fn, time_interval = 5, measure_type = "prob"))
  expect_true(tibble::is_tibble(data))
})


## 'tidy_himd' ----------------------------------------------------------------

test_that("'tidy_hmd' works with valid inputs", {
  fn <- system.file("extdata", "himd_20241023_subset.zip", package = "bssvd")
  suppressMessages(data <- tidy_himd(fn))
  expect_true(tibble::is_tibble(data))
})


## 'himd_add_age_five' --------------------------------------------------------

test_that("'himd_add_age_five' works with valid inputs", {
  fn <- system.file("extdata", "himd_20241023_subset.zip", package = "bssvd")
  data <- himd_unzip(fn, time_interval = 1)
  ans <- himd_add_age_five(data)
  expect_setequal(names(ans),
                  c("country_orig_dest",
                    "time",
                    "age",
                    "type_age",
                    "value"))
})


## 'himd_calculate_coef' ------------------------------------------------------

test_that("'himd_calculate_coef' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "single", max = 45),
                      time = 2001:2005,
                      country_orig_dest = c("A.a", "B.b", "C.c"),
                      type_age = "single",
                      age_open = 45)
  data$value <- runif(n = nrow(data))
  ans_obtained <- himd_calculate_coef(data, measure_type = "rate", n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("country_orig_dest", "time", "component", "coef"))
  expect_equal(mean(ans_obtained$coef), 0)
})


## 'himd_total' ---------------------------------------------------------------

test_that("'himd_total' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 45),
                            time = 2001:2010,
                            country_orig_dest = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 45),
                expand.grid(age = poputils::age_labels(type = "five", max = 50),
                            time = 2001:2010,
                            country_orig_dest = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 50))
  data$value <- runif(n = nrow(data))
  ans_obtained <- himd_total(data, n_comp = 6, transform = "log")
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
})


## 'himd_unzip' ---------------------------------------------------------------

test_that("'himd_unzip' works with valid inputs", {
  fn <- system.file("extdata", "himd_20241023_subset.zip", package = "bssvd")
  ans1 <- himd_unzip(fn, time_interval = 1)
  expect_setequal(names(ans1),
                  c("country_orig_dest",
                    "time",
                    "age",
                    "value"))
  ans5 <- himd_unzip(fn, time_interval = 5)
  expect_setequal(names(ans5),
                  c("country_orig_dest",
                    "time",
                    "age",
                    "value"))
})


## 'himd_vary_age_open' -------------------------------------------------------

test_that("'himd_vary_age_open' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 110),
                            time = 2001,
                            country_orig_dest = "A",
                            type_age = "five"),
                expand.grid(age = poputils::age_labels(type = "single",
                                                       max = 110),
                            time = 2001,
                            country_orig_dest = "A",
                            type_age = "single"))
  data$value <- runif(n = nrow(data))
  ans_obtained <- himd_vary_age_open(data)
  expect_true(all(paste0(seq(from = 60, 110, by = 5), "+") %in% ans_obtained$age))
  expect_identical(unique(ans_obtained$age_open),
                   seq.int(from = 60L, to = 110L, by = 5L))
})
