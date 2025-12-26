#' Intuitively sort by tag
#'
#' Intuitively sort a data frame by a given column of tag numbers
#'
#' @param data A data frame, with a tag column
#' @param col <tidy-select> (link todo) Column of tag numbers;
#'   used to generate the tag sort column
#' @param ... Passed directly to `...` argument of [dplyr::arrange()],
#'   before the tag sort column. Ignored if `.arrange` is `FALSE`
#' @param .arrange If `TRUE`, then sort the data frame by the tag sort column
#' @param .drop If `TRUE`, then drop the tag sort column before returning the
#'   data frame
#' @param x character vector of tag numbers
#'
#' @details
#' Pinniped tags may contain a mix of letters and numbers.
#' Specifically, tags may have a capital letter as the first symbol,
#' followed by some number of 0s.
#' Because of this, tags must be treated as characters,
#' and thus will not properly sort as-is
#' (e.g., A100 will sort in front of A21).
#' This function takes a column of tag numbers, and creates a tag sort column
#' with the tag values padded with 0s
#' in the relevant places to provide intuitive sorting.
#'
#' Specifically, for the tag sort column, all tag numbers will be padded with
#' zeros to a length of 10. For tags with all numeric values, all the padding
#' will be the left of the tag number.
#' For tag numbers with a leading capital letter, zeros will be inserted after
#' the letter and before the rest of the tag number.
#' For instance, "345" become "0000000345", while "A21" becomes "A000000021".
#' Thus, tag numbers of all numeric values will be sorted before
#' tags with a letter for the first symbol.
#'
#' The arguments `.arrange` and `.drop` allow the user to specify if
#' the data frame should be sorted by the tag sort column, and if the tag sort
#' column should be dropped from the output, respectively.
#' If `.arrange` is `TRUE`, then any variables passed via `...` are passed directly
#' to [dplyr::arrange()] before the tag sort column.
#'
#' If you're working with a vector rather than a full data frame,
#' use [vec_tag_sort()].
#'
#' @examples
#' df <- data.frame(
#'   tag_primary = c("10", "A100", "A20", "5"),
#'   tag = c("C99", "A100", "A20", "5"),
#'   d = c(1, 2, 1, 2)
#' )
#'
#' tag_sort(df, tag)
#' tag_sort(df, tag_primary, .arrange = FALSE, .drop = FALSE)
#'
#' vec_tag_sort(df[["tag"]])
#'
#' @name tag_sort
#' @export
tag_sort <- function(data, col, ..., .arrange = TRUE, .drop = TRUE) {
  stopifnot(
    is_bool(.arrange),
    is_bool(.drop),
    inherits(data, "data.frame")
  )

  col.enquo <- enquo(col)

  # Get the column name
  # based on tidyr::build_longer_spec, via tidyr::pivot_longer
  col <- tidyselect::eval_select(
    expr = col.enquo, data = data, allow_rename = FALSE
  )
  if (length(col) != 1) {
    cli::cli_abort("{.arg col} must select exactly one column.")
  }
  col <- names(col)
  col.sort <- glue("{col}_sort")


  # Create the sortby column
  LETTERS.regex <- paste0("[", paste0(LETTERS, collapse = ""), "]")

  out <- data %>%
    mutate(col_sort = vec_tag_sort(!!col.enquo),
           .after = last_col())

  # Arrange and/or drop, as specified
  if (.arrange) out <- out %>% arrange(..., col_sort)

  if (.drop) {
    out %>% select(-col_sort)
  } else {
    # Can use this logic because of `.after = last_col()`
    names(out)[length(names(out))] <- col.sort
    out
  }
}


#' @name tag_sort
#' @export
vec_tag_sort <- function(x) {
  LETTERS.regex <- paste0("[", paste0(LETTERS, collapse = ""), "]")
  if (!is_character(x))
    stop("The vector of tag numbers must be a character vector")

  case_when(
    str_detect(x, LETTERS.regex) ~ #& !non_amlr_tag_primary ~
      paste0(
        str_sub(x, 1, 1),
        str_pad(
          str_split_i(x, LETTERS.regex, 2),
          width = 9, side = "left", pad = "0")
      ),
    .default = str_pad(x, width = 10, side = "left", pad = "0")
  )
}
