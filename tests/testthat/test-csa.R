
## 'coef_csa' -----------------------------------------------------------------

test_that("'coef_csa' works with valid inputs", {
  suppressMessages(ans <- coef_csa(un_csa_subset))
  expect_true(tibble::is_tibble(ans))
  expect_setequal(names(ans),
                  c("sex", "country", "time", "component", "coef"))
  expect_equal(mean(ans$coef), 0)
})


## 'data_ssvd_csa' ------------------------------------------------------------

test_that("'ssvd_csa' works with valid inputs", {
  suppressMessages(data <- data_ssvd_csa(un_csa_subset))
  expect_true(tibble::is_tibble(data))
  data$version <- "v1"
  ans <- bage::ssvd(data)
  expect_s3_class(ans, "bage_ssvd")
})


## 'tidy_csa' --------------------------------------------------------

test_that("'tidy_csa' works with valid inputs", {
  ans <- tidy_csa(un_csa_subset)
  expect_setequal(names(ans),
                  c("country", "sex", "age", "time", "value"))
  expect_true(tibble::is_tibble(ans))
})


## 'csa_calculate_coef' -------------------------------------------------------

test_that("'csa_calculate_coef' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = 5:24,
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  ans_obtained <- csa_calculate_coef(data, n_comp = 5, eps = 0.001)
  expect_setequal(names(ans_obtained),
                  c("sex", "country", "time", "component", "coef"))
})


## 'csa_indep' ----------------------------------------------------------------

test_that("'csa_indep' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male"),
                      age = 5:24,
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- csa_make_labels_age()
  ans_obtained <- csa_indep(data,
                            labels_age = labels_age,
                            n_comp = 5,
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


## 'csa_joint' ----------------------------------------------------------------

test_that("'csa_joint' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male"),
                      age = 5:24,
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- csa_make_labels_age()
  ans_obtained <- csa_joint(data, labels_age = labels_age, n_comp = 5,
                            eps = 0.001)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
})


## 'csa_total' ----------------------------------------------------------------

test_that("'csa_total' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = 5:24,
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- csa_make_labels_age()
  ans_obtained <- csa_total(data, labels_age = labels_age, n_comp = 5,
                            eps = 0.001)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   as.character(ans_obtained$labels_age[[1]]))
})


## 'csa_make_labels_age' ------------------------------------------------------

test_that("'csa_make_labels_age' works", {
  ans <- csa_make_labels_age()
  expect_identical(ans[[1]],
                   5:14)
  expect_identical(ans[[11]],
                   5:24)
})

