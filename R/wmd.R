
file <- "data-raw/wmd/undesa_pd_2019_wmd_marital_status_subset.xlsx"
file <- "../bage/data-raw/ssvd_wmd/undesa_pd_2019_wmd_marital_status.xlsx"

tidy_wmd <- function(file, status = c("current", "ever")) {
  col_types <- c("text",
                 "skip",
                 "numeric",
                 "skip",
                 rep("text", 2),
                 rep("skip", 2),
                 "numeric",
                 rep("skip", 8))
  col_names <- c("country",
                 "year",
                 "sex",
                 "age",
                 "value")
  age_keep <- poputils::age_labels(type = "five",
                                   min = 15,
                                   max = 75,
                                   open = TRUE)
  status <- match.arg(status)
  if (status == "current")
    sheet <- "CURRENTLY MARRIED"
  else
    sheet <- "EVER_MARRIED"
  ans <- readxl::read_xlsx(file,
                           sheet = sheet,
                           col_names = col_names,
                           col_types = col_types,
                           skip = 3)
  ans$age <- gsub("\\[|\\]", "", ans$age)
  ans <- ans[ans$age %in% age_keep, , drop = FALSE]
  ans$sex[ans$sex == "Men"] <- "Male"
  ans$sex[ans$sex == "Women"] <- "Female"
  ans$value <- ans$value / 100
  ans$value <- trim_01(ans$value)
  ans <- ans[c("country",
               "year",
               "sex",
               "age",
               "value")]
  ans
}

wmd_get_data_one <- function(data, labels_age) {
  ans <- data[data$age %in% labels_age, ]
  ans$age <- factor(ans$age, levels = labels_age)
  ord <- order(ans$age)
  ans <- ans[ord, , drop = FALSE]
  rownames(ans) <- NULL
  ans
}



  

## HAS_TESTS
#' Prepare Inputs for "indep" Type, ie Female and Male Separate
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#'
#' @returns A data frame
#' 
#' @noRd
wmd_indep <- function(data, labels_age, n_comp) {
  make_indep(data = data,
             labels_age = labels_age,
             n_comp = n_comp)
}

## HAS_TESTS
#' Prepare Inputs for "joint" Type, ie Female and Male Concatenated
#'
#' @param data A data frame
#' @param labels_age List of character vectors
#' @param n_comp Numbers of SVD components
#'
#' @returns A data frame
#'
#' @noRd
wmd_joint <- function(data, labels_age, n_comp) {
  make_joint(data = data,
             labels_age = labels_age,
             n_comp = n_comp)
}


## NO_TESTS
#' Make Age Labels Used by WMD
#' 
wmd_make_labels_age <- function() {
  labels_all <- poputils::age_labels(type = "five",
                                     min = 15,
                                     max = 75,
                                     open = TRUE)
  lapply(7:13, function(i) labels_all[1:i])
}
