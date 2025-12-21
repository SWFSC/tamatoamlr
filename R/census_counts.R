#' Functions for processing census counts
#'
#' Functions for processing census counts
#'
#' @param x integer vector
#' @param census.df data frame, likely of census data.
#'   Must have at least one integer column with the suffix '_count'.
#'   All columns with the suffix '_count' must be integer vectors
#' @param na.rm see \code{\link[base:sum]{sum}}. Default is \code{TRUE}
#'
#' @details
#' These functions were designed to be used in tidyverse piped workflows.
#' They handle common situations when working with AMLR Pinniped census data
#'
#' @return
#' \code{sum_count} returns an integer vector of the \code{\link[base:sum]{sum}}
#' of \code{x}. The exception is if all elements of \code{x} are \code{NA},
#' in which case \code{sum_count} returns \code{\link[base:NA]{NA_integer_}}.
#'
#' \code{total_count} returns the data frame \code{census.df},
#' with a new column 'total_count' that is the rowwise sum of all columns that
#' satisfy \code{\link[tidyselect:starts_with]{ends_with}("_count")}
#'
#' @examples
#' sum_count(as.integer(c(NA, NA, NA)))
#' sum_count(as.integer(c(NA, 5, 2)))
#' sum_count(as.integer(c(NA, 5, 2)), na.rm = FALSE)
#'
#' census.df <- data.frame(
#'   ID = 1:4, first_count = as.integer(c(2, NA, 3, 9)),
#'   second_count = as.integer(c(70, 4, 15, 7))
#' )
#' total_count(census.df)
#' total_count(census.df, na.rm = FALSE)
#'
#' @name census_counts
#' @export
sum_count <- function(x, na.rm = TRUE) {
  stopifnot(is.integer(x))
  if_else(all(is.na(x)), NA_integer_, sum(x, na.rm = na.rm))
}


#' @name census_counts
#' @export
total_count <- function(census.df, na.rm = TRUE) {
  count.df <- census.df %>% select(ends_with("_count"))
  if (ncol(count.df) == 0) {
    stop("'census.df' must include at least one column that ends with '_count'")
  } else {
    if (!all(vapply(count.df, is.integer, TRUE)))
      stop("All columns that end in '_count' must be integers")
  }

  census.df %>%
    rowwise() %>%
    mutate(total_count = sum(c_across(dplyr::ends_with("_count")),
                             na.rm = na.rm)) %>%
    ungroup()
}
