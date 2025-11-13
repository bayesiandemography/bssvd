
## 'coef_wmd' -----------------------------------------------------------------

test_that("'coef_wmd' works with valid inputs", {
  file <- system.file("extdata",
                      "undesa_pd_2019_wmd_marital_status_subset.xlsx",
                      package = "bssvd")
  suppressMessages(ans <- coef_wmd(file))
  expect_true(tibble::is_tibble(ans))
  expect_setequal(names(ans),
                  c("sex", "country", "time", "component", "coef"))
  expect_equal(mean(ans$coef), 0)
})


## 'data_ssvd_wmd' ------------------------------------------------------------

test_that("'ssvd_wmd' works with valid inputs", {
  file <- system.file("extdata",
                      "undesa_pd_2019_wmd_marital_status_subset.xlsx",
                      package = "bssvd")
  suppressMessages(data <- data_ssvd_wmd(file))
  expect_true(tibble::is_tibble(data))
  data$version <- "v1"
  ans <- bage::ssvd(data)
  expect_s3_class(ans, "bage_ssvd")
})


## 'tidy_wmd' --------------------------------------------------------

test_that("'tidy_wmd' works with valid inputs - no year_min", {
  file <- system.file("extdata",
                      "undesa_pd_2019_wmd_marital_status_subset.xlsx",
                      package = "bssvd")
  ans <- tidy_wmd(file)
  expect_setequal(names(ans),
                  c("country", "sex", "age", "time", "value"))
  expect_true(tibble::is_tibble(ans))
})

test_that("'tidy_wmd' works with valid inputs - status is ever", {
  file <- system.file("extdata",
                      "undesa_pd_2019_wmd_marital_status_subset.xlsx",
                      package = "bssvd")
  ans <- tidy_wmd(file, status = "ev")
  expect_setequal(names(ans),
                  c("country", "sex", "age", "time", "value"))
  expect_true(tibble::is_tibble(ans))
})


## 'wmd_calculate_coef' -------------------------------------------------------

test_that("'wmd_calculate_coef' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 75,
                                                 open = TRUE),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  ans_obtained <- wmd_calculate_coef(data, n_comp = 5, eps = 0.001)
  expect_setequal(names(ans_obtained),
                  c("sex", "country", "time", "component", "coef"))
})


## 'wmd_indep' ----------------------------------------------------------------

test_that("'wmd_indep' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male"),
                      age = poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 75,
                                                 open = TRUE),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- wmd_make_labels_age()
  ans_obtained <- wmd_indep(data, labels_age = labels_age, n_comp = 5,
                            eps = 0.001)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender",
                    "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgTMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
})


## 'wmd_joint' ----------------------------------------------------------------

test_that("'wmd_joint' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male"),
                      age = poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 75,
                                                 open = TRUE),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- wmd_make_labels_age()
  ans_obtained <- wmd_joint(data, labels_age = labels_age, n_comp = 5,
                            eps = 0.001)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
})


## 'wmd_make_labels_age' ------------------------------------------------------

test_that("'wmd_make_labels_age' works", {
  ans <- wmd_make_labels_age()
  expect_identical(ans[[1]],
                   c("15-19", "20-24",
                     "25-29", "30-34",
                     "35-39", "40-44",
                     "45-49"))
  expect_identical(ans[[7]],
                   c("15-19", "20-24",
                     "25-29", "30-34",
                     "35-39", "40-44",
                     "45-49", "50-54",
                     "55-59", "60-64",
                     "65-69", "70-74",
                     "75+"))
})

