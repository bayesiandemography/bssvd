
## 'coef_hfd' -----------------------------------------------------------------

test_that("'coef_hfd' works with valid inputs", {
  suppressMessages(ans <- coef_hfd(asfr_subset, n_comp = 5L))
  expect_true(tibble::is_tibble(ans))
  expect_setequal(names(ans), c("country", "time", "component", "coef"))
  expect_equal(mean(ans$coef), 0)
})


## 'data_ssvd_hfd' ------------------------------------------------------------

test_that("'data_ssvd_hfd' works with valid inputs", {
  suppressMessages(ans <- data_ssvd_hfd(asfr_subset, age_max = 50))
  expect_true(tibble::is_tibble(ans))
})


## 'hfd_calculate_coef' -------------------------------------------------------

test_that("'hfd_calculate_coef' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      age = 15:54,
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  ans <- hfd_calculate_coef(data, n_comp = 5)
  expect_setequal(names(ans), c("country", "time", "component", "coef"))
})


## 'hfd_get_data_one' ---------------------------------------------------------

test_that("'hfd_get_data_one' works", {
  data <- data.frame(age = 12:51,
                     country = "a",
                     time = 2000,
                     value = 1:40)
  labels_age <- c("20-24", "25-29", "30-34", "35-39")
  ans_obtained <- hfd_get_data_one(data = data,
                                   labels_age = labels_age)
  ans_expected <- data.frame(country = "a",
                             time = 2000,
                             age = factor(c("20-24", "25-29", "30-34", "35-39")),
                             value = c(sum(1:13),
                                       sum(14:18),
                                       sum(19:23),
                                       sum(24:40)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'hfd_get_data_one' throws appropriate error with open age group", {
  data <- data.frame(age = 12:51,
                     country = "a",
                     time = 2000,
                     value = 1:40)
  labels_age <- c("20-24", "25-29", "30-34", "35-39", "40+")
  expect_error(hfd_get_data_one(data = data,
                                labels_age = labels_age),
               "Internal error: Age labels include open age group.")
})



## 'hfd_make_labels_age' ------------------------------------------------------

test_that("'hfd_make_labels_age' works - age_min_max higher than data", {
  data <- data.frame(age = 12:19)
  ans_obtained <- hfd_make_labels_age(data = data,
                                      age_min_max = 15,
                                      age_max_min = 20)
  ans_expected <- list(as.character(12:19),
                       as.character(13:19),
                       as.character(14:19),
                       as.character(15:19),
                       c("10-14", "15-19"),
                       c("15-19"))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'hfd_make_labels_age' works - age_max_min lower than data", {
  data <- data.frame(age = 12:20)
  ans_obtained <- hfd_make_labels_age(data = data,
                                      age_min_max = 12,
                                      age_max_min = 20)
  ans_expected <- list(as.character(12:19),
                       as.character(12:20),
                       c("10-14", "15-19"),
                       c("10-14", "15-19", "20-24"))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'hfd_make_labels_age' works - age_min_max higher than data, age_max_min lower than data", {
  data <- data.frame(age = 12:20)
  ans_obtained <- hfd_make_labels_age(data = data,
                                      age_min_max = 13,
                                      age_max_min = 20)
  ans_expected <- list(as.character(12:19),
                       as.character(12:20),
                       as.character(13:19),
                       as.character(13:20),
                       c("10-14", "15-19"),
                       c("10-14", "15-19", "20-24"))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'hfd_make_labels_age' throws appropriate error when age_min_max below data", {
  data <- data.frame(age = 12:20)
  expect_error(hfd_make_labels_age(data = data,
                                   age_min_max = 10,
                                   age_max_min = 20),
               "`age_min_max` is below the youngest age group in the data.")
})

test_that("'hfd_make_labels_age' throws appropriate error when age_max_min above data", {
  data <- data.frame(age = 12:20)
  expect_error(hfd_make_labels_age(data = data,
                                   age_min_max = 15,
                                   age_max_min = 25),
               "`age_max_min` is above the upper limit of the oldest age group in the data.")
})

test_that("'hfd_make_labels_age' works - thows appropriate error when is factor", {
  data <- data.frame(age = factor(12:19))
  expect_error(hfd_make_labels_age(data = data,
                                      age_min_max = 15,
                                   age_max_min = 20),
               "Internal error: Age variable is not integer.")
})


## 'hfd_tidy' -----------------------------------------------------------------

test_that("'hfd_tidy' works with valid inputs", {
  ans <- hfd_tidy(asfr_subset)
  expect_setequal(names(ans),
                  c("country", "age", "time", "value"))
  expect_true(tibble::is_tibble(ans))
  expect_identical(min(ans$age), 12L)
  expect_identical(max(ans$age), 55L)
})

test_that("'hfd_tidy' throws correct error when variable missing", {
  data <- asfr_subset
  data <- data[-match("Age", names(data))]
  expect_error(hfd_tidy(data),
               "`data` does not have a column called \"Age\"")
})


## 'hfd_total' ----------------------------------------------------------------

test_that("'hfd_total' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      age = 15:54,
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- hfd_make_labels_age(data = data,
                                    age_min_max = 15,
                                    age_max_min = 45)
  ans_obtained <- hfd_total(data, labels_age = labels_age, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
})
