
## 'ssvd_lfp' -----------------------------------------------------------------

test_that("'ssvd_lfp' works with valid inputs", {
  fn <- file.path("data_for_tests", "OECD.ELS.SAE,DSD_LFS@DF_LFS_COMP,1.1+all.csv")
  suppressMessages(ans <- ssvd_lfp(fn, age_open_max = 65))
  expect_s3_class(ans, "bage_ssvd")
})


## 'lfp_get_data_one' ---------------------------------------------------------

test_that("'lfp_get_data_one' works", {
  data <- data.frame(age = c("25-29", "20-24", "30-34", "30+"),
                     val = 1:4)
  labels_age <- c("20-24", "25-29", "30+")
  ans_obtained <- lfp_get_data_one(data = data,
                                   labels_age = labels_age)
  ans_expected <- data.frame(age = factor(c("20-24", "25-29", "30+")),
                             val = c(2L, 1L, 4L))
  expect_identical(ans_obtained, ans_expected)
})


## 'lfp_indep' ----------------------------------------------------------------
d
test_that("'lfp_indep' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 65,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$measure <- runif(n = nrow(data))
  labels_age <- lfp_make_labels_age(data = data, age_max = 65)
  ans_obtained <- lfp_indep(data, labels_age = labels_age, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgTMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
})


## 'lfp_joint' ----------------------------------------------------------------

test_that("'lfp_joint' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 65,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$measure <- runif(n = nrow(data))
  labels_age <- lfp_make_labels_age(data = data, age_max = 65)
  ans_obtained <- lfp_joint(data, labels_age = labels_age, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(rownames(ans_obtained$matrix[[1]]),
                   paste(ans_obtained$labels_sexgender[[1]],
                         ans_obtained$labels_age[[1]],
                         sep = "."))
})


## 'lfp_make_labels_age' ------------------------------------------------------

test_that("'lfp_make_labels_age' works", {
  data <- data.frame(age = c("15-19", "20-24", "20+", "25+", "25-29", "30+"))
  ans_obtained <- lfp_make_labels_age(data = data,
                                      age_max = 25)
  ans_expected <- list("15-19",
                       c("15-19", "20-24"),
                       c("15-19", "20+"),
                       c("15-19", "20-24", "25+"))
  expect_identical(ans_obtained, ans_expected)
})


## 'lfp_read_and_tidy' --------------------------------------------------------

test_that("'lfp_read_and_tidy' works with valid inputs", {
  fn <- file.path("data_for_tests", "OECD.ELS.SAE,DSD_LFS@DF_LFS_COMP,1.1+all.csv")
  ans <- lfp_read_and_tidy(fn)
  expect_setequal(names(ans),
                  c("country", "sex", "age", "time", "measure"))
  expect_true(tibble::is_tibble(ans))
})


## 'lfp_total' ----------------------------------------------------------------

test_that("'lfp_total' works with valid inputs", {
  data <- expand.grid(country = 1:2,
                      sex = c("Female", "Male", "Total"),
                      age = c(poputils::age_labels(type = "five",
                                                 min = 15,
                                                 max = 70,
                                                 open = TRUE),
                              "60+"),
                      time = 2001:2005)
  data$measure <- runif(n = nrow(data))
  labels_age <- lfp_make_labels_age(data = data, age_max = 65)
  ans_obtained <- lfp_total(data, labels_age = labels_age, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
})
