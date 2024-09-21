
## 'data_ssvd_hmd' -----------------------------------------------------------------

test_that("'data_ssvd_hmd' works with valid inputs", {
  fn <- system.file("extdata", "hmd_statistics_subset.zip", package = "bssvd")
  suppressMessages(data <- data_ssvd_hmd(fn))
  expect_true(tibble::is_tibble(data))
  ## ans <- bage::ssvd(data)
  ## expect_s3_class(ans, "bage_ssvd")
})


## 'hmd_add_age_five' ---------------------------------------------------------

test_that("'hmd_add_age_five' works with valid inputs", {
  data <- tibble::tribble(~age,  ~time, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                          "0",     2001,  "Total",   "a",      "lt",   0.1, 2,
                          "1-4", 2001,  "Total",   "a",      "lt",   0.2, 3,
                          "5-9", 2001,  "Total",   "a",      "lt",   0.4, 4)
  ans_obtained <- hmd_add_age_five(data)
  ans_expected <-
    tibble::tribble(~age,  ~time, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                    "0",   2001,  "Total",   "a",      "lt",   0.1, 2,
                    "1-4", 2001,  "Total",   "a",      "lt",   0.2, 3,
                    "5-9", 2001,  "Total",   "a",      "lt",   0.4, 4,
                    "0-4", 2001,  "Total",   "a",      "five", 0.4*0.1+0.6*0.2, 5,
                    "5-9", 2001,  "Total",   "a",      "five", 0.4, 4)
  expect_identical(ans_obtained, ans_expected)
})


## 'hmd_aggregate_mx_Lx' ------------------------------------------------------

test_that("'hmd_aggregate_mx_Lx' works with valid inputs", {
  data <- tibble::tribble(~age, ~time, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                          "0",  2001,  "Total",   "a",      "single",   0.1, 2,
                          "0",  2001,  "Total",   "a",      "single",   0.2, 3,
                          "1",  2001,  "Total",   "a",      "single",   0.1, 0,
                          "1",  2001,  "Total",   "a",      "single",   0.2, 0,
                          "0",  2001,  "Total",   "a",      "lt",       0.1, 2,
                          "0",  2001,  "Total",   "a",      "lt",       0.2, 3,
                          "1-4",2001,  "Total",   "a",      "lt",       0.1, 0,
                          "1-4",2001,  "Total",   "a",      "lt",       0.2, 0)
  ans_obtained <- hmd_aggregate_mx_Lx(data)
  ans_expected <-
    tibble::tribble(~age, ~time, ~sex,     ~country, ~type_age, ~mx,            ~Lx,
                    "0",  2001,  "Total",   "a",      "single", 0.4*0.1+0.6*0.2, 2+3,
                    "1",  2001,  "Total",   "a",      "single", 0.5*0.1 + 0.5*0.2, 0+0,
                    "0",  2001,  "Total",   "a",      "lt",     0.4*0.1+0.6*0.2, 2+3,
                    "1-4",2001,  "Total",   "a",      "lt",     0.5*0.1 + 0.5*0.2, 0+0)
  expect_identical(ans_obtained, ans_expected)
})


## 'hmd_indep' ---------------------------------------------------------

test_that("'hmd_indep' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 45),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 45),
                expand.grid(age = poputils::age_labels(type = "five", max = 50),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 50))
  data$mx <- runif(n = nrow(data))
  ans_obtained <- hmd_indep(data, n_comp = 5)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgTMatrix")))
  expect_identical(.mapply(paste,
                           dots = list(x = ans_obtained$labels_sexgender,
                                       y = ans_obtained$labels_age),
                           MoreArgs = list(sep = ".")),
                   lapply(ans_obtained$matrix, rownames))
})


## 'hmd_joint' ----------------------------------------------------------------

test_that("'hmd_joint' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 45),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 45),
                expand.grid(age = poputils::age_labels(type = "five", max = 50),
                            sex = c("Female", "Male"),
                            time = 2001:2005,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 50))
  data$mx <- runif(n = nrow(data))
  ans_obtained <- hmd_joint(data, n_comp = 10)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
  expect_identical(.mapply(paste,
                           dots = list(x = ans_obtained$labels_sexgender,
                                       y = ans_obtained$labels_age),
                           MoreArgs = list(sep = ".")),
                   lapply(ans_obtained$matrix, rownames))
})


## 'hmd_tidy_data' ------------------------------------------------------------

test_that("'hmd_tidy_data' works with valid inputs", {
  data <- tibble::tribble(~Age, ~Year, ~sex,     ~country, ~type_age, ~mx, ~Lx,
                          0,    2001,  "both",   "a",      "1x1",     0.1, 2,
                          1,    2002,  "female", "b",      "5x1",     0.1, 2,
                          2,    2003,  "male",   "c",      "1x1",     0.1, 2,
                          3,    2004,  "both",   "d",      "1x1",     NA,  0)
  ans_obtained <- hmd_tidy_data(data)
  expect_setequal(names(ans_obtained),
                  c("age", "time", "sex", "country", "type_age", "mx", "Lx"))
  expect_identical(nrow(ans_obtained), 3L)
  expect_identical(ans_obtained$sex,
                   factor(c("Total", "Female", "Male"),
                          levels = c("Total", "Female", "Male")))
  expect_identical(ans_obtained$type_age,
                   factor(c("single", "lt", "single"),
                          levels = c("single", "five", "lt")))
})


## 'hmd_total' ----------------------------------------------------------------

test_that("'hmd_total' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 45),
                            sex = "Total",
                            time = 2001:2010,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 45),
                expand.grid(age = poputils::age_labels(type = "five", max = 50),
                            sex = "Total",
                            time = 2001:2010,
                            country = c("A", "B", "C"),
                            type_age = c("single", "five"),
                            age_open = 50))
  data$mx <- runif(n = nrow(data))
  ans_obtained <- hmd_total(data, n_comp = 6)
  expect_setequal(names(ans_obtained),
                  c("type", "labels_age", "labels_sexgender", "matrix", "offset"))
  expect_true(all(sapply(ans_obtained$matrix, is, "dgCMatrix")))
})


## 'hmd_unzip' ----------------------------------------------------------------

test_that("'hmd_unzip' works with valid inputs - unzipped has hmd_statistics_test folder", {
  fn <- system.file("extdata", "hmd_statistics_subset.zip", package = "bssvd")
  ans <- hmd_unzip(fn)
  expect_setequal(names(ans),
                  c("country", "Year", "Age", "mx", "Lx", "sex", "type_age"))
})

test_that("'hmd_unzip' works with valid inputs - unzipped does not have hmd_statistics_test folder", {
  fn <- file.path("data_for_tests", "hmd_statistics_test_no_folder.zip")
  ans <- hmd_unzip(fn)
  expect_setequal(names(ans),
                  c("country", "Year", "Age", "mx", "Lx", "sex", "type_age"))
})

test_that("'hmd_unzip' gives expected error when zipfile missing files", {
  fn <- file.path("data_for_tests", "hmd_statistics_test_missing_files.zip")
  expect_error(hmd_unzip(fn),
               "Did not get expected files when unzipped `zipfile`.")
})


## 'hmd_vary_age_open' --------------------------------------------------------

test_that("'hmd_vary_age_open' works with valid inputs", {
  set.seed(0)
  data <- rbind(expand.grid(age = poputils::age_labels(type = "five", max = 110),
                      sex = c("F", "M"),
                      time = 2001,
                      country = "A",
                      type_age = "five"),
                expand.grid(age = poputils::age_labels(type = "lt", max = 110),
                      sex = c("F", "M"),
                      time = 2001,
                      country = "A",
                      type_age = "lt"))
  data$mx <- runif(n = nrow(data))
  data$Lx <- runif(n = nrow(data), max = 5)
  ans_obtained <- hmd_vary_age_open(data)
  expect_true(all(paste0(seq(from = 60, 110, by = 5), "+") %in% ans_obtained$age))
  expect_identical(unique(ans_obtained$age_open),
                   seq.int(from = 60L, to = 110L, by = 5L))
})


## 'hmd_vary_age_open_type_age' -----------------------------------------------

test_that("'hmd_vary_age_open_type_age' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "five", max = 110),
                      sex = c("F", "M"),
                      time = 2001,
                      country = "A",
                      type_age = "five")
  data$mx <- runif(n = nrow(data))
  data$Lx <- runif(n = nrow(data), max = 5)
  ans_obtained <- hmd_vary_age_open_type_age(data)
  expect_true(all(paste0(seq(from = 60, 110, by = 5), "+") %in% ans_obtained$age))
  expect_identical(unique(ans_obtained$age_open),
                   seq.int(from = 60L, to = 110L, by = 5L))
})
