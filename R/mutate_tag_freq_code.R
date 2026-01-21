#' Create tag/freq/code ID column
#'
#' A 'wrapper' around mutate that creates a new freq/code or  tag/freq/code ID
#' column
#'
#' @param x data frame; must contain required subset of 'tag', 'freq', and
#'   'code' columns
#'
#' @details
#' `mutate_tag_freq_code()` is a wrapper around [dplyr::mutate()] that pastes
#' the tag, freq, and code values together to create a single identifying
#' string.
#'
#' `mutate_freq_code()` is the same principle, but only pastes together the freq
#' and code values.
#'
#' These functions are typically used in [mod_dcc_pinniped_server()], or other
#' Pinniped DCC data processing scripts.
#'
#' @return `x`, with a new column, either 'tag_freq_code' calculated as
#'   `paste(tag, freq, code, sep = " | ")` for `mutate_tag_freq_code()`,
#'   or 'freq_code' calculated as
#'   `paste(freq, code, sep = " | ")` for `mutate_freq_code()`
#'
#' @examples
#' x <- data.frame(tag = "A21", freq = 164.105, code = 12)
#' mutate_tag_freq_code(x)
#'
#' mutate_freq_code(x)
#'
#' @name mutate_tag_freq_code
#' @export
mutate_tag_freq_code <- function(x) {
  stopifnot(
    inherits(x, "data.frame"),
    c("tag", "freq", "code") %in% names(x)
  )

  x %>% mutate(tag_freq_code = paste(tag, freq, code, sep = " | "))
}

#' @name mutate_tag_freq_code
#' @export
mutate_freq_code <- function(x) {
  stopifnot(
    inherits(x, "data.frame"),
    c("freq", "code") %in% names(x)
  )

  x %>% mutate(freq_code = paste(freq, code, sep = " | "))
}
