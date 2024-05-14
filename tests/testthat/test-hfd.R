
## 'ssvd_hfd' -----------------------------------------------------------------

test_that("'ssvd_hfd' works with valid inputs", {
  suppressMessages(ans <- ssvd_hfd(oecd_hfd, age_max = 65))
  expect_s3_class(ans, "bage_ssvd")
})


## 'hfd_tidy' --------------------------------------------------------

test_that("'hfd_tidy' works with valid inputs", {
  ans <- hfd_tidy(asfr)
  expect_setequal(names(ans),
                  c("country", "age", "time", "value"))
  expect_true(tibble::is_tibble(ans))
  expect_identical(min(ans$age), 12L)
  expect_identical(max(ans$age), 55L)
})

test_that("'hfd_tidy' throws correct error when variable missing", {
  data <- asfr
  data <- data[-match("Age", names(data))]
  expect_error(hfd_tidy(data),
               "`data` does not have a column called \"Age\"")
})


## 'hfd_get_data_one' ---------------------------------------------------------

test_that("'hfd_get_data_one' works", {
  data <- data.frame(age = c("25-29", "20-24", "30-34", "30+"),
                     val = 1:4)
  labels_age <- c("20-24", "25-29", "30+")
  ans_obtained <- hfd_get_data_one(data = data,
                                   labels_age = labels_age)
  ans_expected <- data.frame(age = factor(c("20-24", "25-29", "30+")),
                             val = c(2L, 1L, 4L))
  expect_identical(ans_obtained, ans_expected)
})


## 'hfd_indep' ----------------------------------------------------------------

test_that("'hfd_indep' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 65,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- hfd_make_labels_age(data = data, age_max = 65)
  ans_obtained <- hfd_indep(data, labels_age = labels_age, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgTMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
})


## 'hfd_joint' ----------------------------------------------------------------

test_that("'hfd_joint' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 65,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- hfd_make_labels_age(data = data, age_max = 65)
  ans_obtained <- hfd_joint(data, labels_age = labels_age, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
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




## 'hfd_total' ----------------------------------------------------------------

test_that("'hfd_total' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 70,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$value <- runif(n = nrow(data))
  labels_age <- hfd_make_labels_age(data = data, age_max = 65)
  ans_obtained <- hfd_total(data, labels_age = labels_age, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
})
