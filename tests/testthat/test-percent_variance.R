
test_that("percent_variance returns named cumulative % variance", {
  df <- expand.grid(
    age = c("0", "1"),
    country = c("A", "B"),
    time = c(2000, 2001),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  df$value <- c(1.2, 0.8, 1.1, 0.9, 1.3, 0.7, 1.0, 0.95)
  ans <- percent_variance(
    data = df,
    transform = "log",
    measure = "value",
    cols = c("country", "time"),
    n_comp = 2,
    eps = 1e-5
  )
  expect_type(ans, "double")
  expect_length(ans, 2)
  expect_named(ans, c("Component 1", "Component 2"))
  expect_true(all(is.finite(ans)))
  expect_true(all(diff(unname(ans)) >= 0))      # cumulative
  expect_lte(max(unname(ans)), 100 + 1e-8)      # numerical tolerance
  expect_gte(min(unname(ans)), 0)
})

test_that("percent_variance is insensitive to row order", {
  df <- expand.grid(
    age = c("1", "0"),  # deliberately out of order
    country = c("A", "B"),
    time = c(2001, 2000),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  df$value <- seq_len(nrow(df)) / 10
  ans1 <- percent_variance(df, transform = "log", n_comp = 2)
  ans2 <- percent_variance(df[sample.int(nrow(df)), , drop = FALSE],
                           transform = "log", n_comp = 2)
  expect_equal(unname(ans1), unname(ans2))
})

test_that("percent_variance errors when required columns are missing", {
  df <- data.frame(country = "A", time = 2000, value = 1)  # no age
  expect_error(
    percent_variance(df, transform = "log"),
    "`age` not found in `data`."
  )
})

test_that("percent_variance rejects invalid transform", {
  skip_if_not_installed("poputils")
  df <- data.frame(age = "0", country = "A", time = 2000, value = 1)
  expect_error(
    percent_variance(df, transform = "bad"),
    "should be one of"
  )
})

test_that("percent_variance errors if n_comp exceeds number of singular values", {
  # 2 ages => at most 2 singular values
  df <- expand.grid(
    age = c("0", "1"),
    country = c("A", "B"),
    time = c(2000, 2001),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  df$value <- runif(nrow(df), 0.5, 1.5)
  expect_error(
    percent_variance(df, transform = "log", n_comp = 3),
    "Number of singular values \\(2\\) less than `n_comp` \\(3\\)."
  )
})

test_that("percent_variance works with logit transform on proportions", {
  df <- expand.grid(
    age = c("0", "1"),
    country = c("A", "B"),
    time = c(2000, 2001),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  df$value <- runif(nrow(df), 0.1, 0.9)  # valid for logit
  ans <- percent_variance(df, transform = "logit", n_comp = 2)
  expect_type(ans, "double")
  expect_length(ans, 2)
  expect_true(all(is.finite(ans)))
})

test_that("percent_variance works with no transform", {
  df <- expand.grid(
    age = c("0", "1"),
    country = c("A", "B"),
    time = c(2000, 2001),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  df$value <- rnorm(nrow(df))
  ans <- percent_variance(df, transform = "none", n_comp = 2)
  expect_type(ans, "double")
  expect_length(ans, 2)
  expect_true(all(is.finite(ans)))
})

