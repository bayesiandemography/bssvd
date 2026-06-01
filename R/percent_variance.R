
## NO_TESTS
#' Calculate the Percent of Variance Explained
#' by Components in SVD
#'
#' Calculate the percent of variance in a dataset
#' that can be explained by each component
#' of a singular value decomposition. The dataset should
#' include age, a single sex or gender category,
#' and one or more classifying variables.
#'
#' `data` must contain a variable called `"age"`.
#' It must also contain the variables named by
#' the `measure` and `cols` arguments. The age
#' and `cols` arguments must uniquely identify
#' all cells in the dataset.
#'
#' When `transform` is `"log"` or `"logit"`,
#' `percent_variance()` converts any
#' values for the measure variable
#' that are less than `eps` to
#' values greater than or equal to `eps` before
#' transforming. The converted value is based on
#' on a main effects model.
#'
#' When `transform` is `"logit"`,
#' `precent_variange() converts any
#' values greater than `1-eps` to values
#' less than or equal to `1-eps`
#' before transforming. The converted value
#' is based on main effects model.
#'
#' @param data A data frame
#' @param transform Transformation to apply
#' to the data: `"log"`, `"logit"`, or `"none"`.
#' @param measure Name of the rate or proportion
#' being modelled. Default is `"value"`.
#' @param cols Names of the categorical variables
#' used to distinguish different age profiles.
#' @param n_comp Number of components to
#' give percentages for. Default is `5`.
#' @param eps Parameter controlling truncation.
#' See below for details. Default is `0.00001`.
#'
#' @returns A numeric vector of length `n_comp`.
#'
#' @examples
#' asfr_subset |>
#'   tidy_hfd() |>
#'   percent_variance(transform = "log")
#' @export
percent_variance <- function(data,
                             transform,
                             measure = "value",
                             cols = c("country", "time"),
                             n_comp = 5,
                             eps = 0.00001) {
  transform <- match.arg(transform,
                         choices = c("log", "logit", "none"))
  check_n(n = n_comp,
          nm_n = "n_comp",
          min = 1L,
          max = NULL,
          divisible_by = 1L)
  check_eps(eps)
  nms_required <- c(measure, cols, "age")
  nms_data <- names(data)
  is_in_data <- nms_required %in% nms_data
  i_not_in_data <- match(FALSE, is_in_data, nomatch = 0L)
  if (i_not_in_data > 0L) {
    nm_mis <- nms_required[[i_not_in_data]]
    cli::cli_abort("Variable {.var {nm_mis}} not found in {.arg data}.")
  }
  data$age <- poputils::reformat_age(data$age)
  cols_ord <- c("age", cols)
  data_cols_ord <- unname(as.list(data[cols_ord]))
  args <- c(data_cols_ord, list(decreasing = FALSE))
  ord <- do.call(order, args)
  data <- data[ord, , drop = FALSE]
  m <- poputils::to_matrix(x = data,
                           rows = "age",
                           cols = cols,
                           measure = measure)
  m <- remove_cols_with_na(x = m, n_comp = n_comp)
  if (transform == "log") {
    m <- replace_zeros(m, eps = eps)
    m <- log(m)
  }
  else if (transform == "logit") {
    m <- replace_zeros_ones(m, eps = eps)
    m <- poputils::logit(m)
  }
  else if (transform == "none") {
    NULL
  }
  else
    cli::cli_abort("Internal error: Unexpected value for transform") # nocov
  svd <- svd(m, nu = 0L, nv = 0L)
  sing_val <- svd$d
  n_sing_val <- length(sing_val)
  if (n_sing_val < n_comp)
    cli::cli_abort(paste("Number of singular values ({.val {n_sing_val}})",
                         "less than {.arg n_comp} ({.val {n_comp}})."))
  ans <- 100 * cumsum(sing_val^2) / sum(sing_val^2)
  s <- seq_len(n_comp)
  ans <- ans[s]
  names(ans) <- paste("Component", s)
  ans
}                   

