#' Multiple season date filter
#'
#' Filter for records near a month/day across multiple seasons
#'
#' @param df data frame of all records, to filter
#' @param date.col column in `x` with record date info
#' @param fs list with season/filter info. Intended to be the output of
#'   [mod_filter_season_server()]
#' @param vals if `NULL` (default), then ignored. If not `NULL`, then must be a
#'   [shiny::reactiveValues()] with a named object `warning_mult_date_filter`,
#'   to which to write associated warning messages
#'
#' @details
#' This function is used to filter pinniped records, typically census values,
#' for the records that are closes to a given month/day. This is done for each
#' season in the data frame; only one set of records is kept per season.
#'
#' The date and the max gap are found in the fs object. Records are only kept if
#' they are within a max gap (`fs$max_gap()`) in days from the provided date
#' (`fs$mult_date()`).
#'
#' If `vals` is not `NULL`, then a warning message is written to
#' `vals$warning_mult_date_filter` if `df` has records from a season, but no
#' records within the max gap from the filter date
#'
#' @return The filtered data frame.
#'
#' @export
mult_date_filter <- function(df, date.col, fs, vals = NULL) {
  date.col.enquo <- enquo(date.col)

  m <- month(req(fs$mult_date()))
  m.abb <- month.abb[m]
  d <- day(fs$mult_date())
  max.gap <- req(fs$mult_max_gap())

  fs.date.df <- data.frame(
    season_name = req(fs$season()),
    m = m,
    d = d
  )

  df.fs <- df %>%
    left_join(fs.date.df, by = "season_name") %>%
    mutate(season_date = amlr_date_from_season(season_name, m, d),
           days_diff = as.numeric(
             difftime(!!date.col.enquo, season_date, units = "days")),
           days_diff = if_else(days_diff < 0, abs(days_diff)-0.5, days_diff)) %>%
    group_by(season_name) %>%
    filter(days_diff == min(days_diff))

  df.filtered <- df.fs %>%
    filter(days_diff <= max.gap) %>%
    select(-c(m, d, season_date)) %>%
    ungroup()

  # if (n_distinct(census.df.ds$season_name) !=
  #     n_distinct(census.df.ds$census_phocid_header_id)) {
  #   validate(
  #     paste("Error in census fs_mult_date summaries -",
  #           "please contact the database manager")
  #   )
  # }


  validate(
    need(nrow(df.filtered) > 0,
         glue("There are no records for the",
              "selected season(s) within {max.gap}",
              "days of the provided date of {d} {m.abb}"))
  )

  # Warning message for seasons w/o census record close enough\
  if (!is.null(vals)) {
    nrow.diff <- nrow(df.fs) - nrow(df.filtered)
    if (nrow.diff != 0) {
      seasons.rmd <- df.fs %>%
        filter(!(season_name %in% unique(df.filtered$season_name))) %>%
        distinct(season_name) %>%
        arrange(season_name) %>%
        select(season_name) %>%
        unlist()

      vals.warning <- paste(
        "The following season(s) have records, but none within",
        glue("{max.gap} days of the provided date of {d} {m.abb}:"),
        paste(seasons.rmd, collapse = ", ")
      )
    } else {
      vals.warning <- NULL
    }
    vals$warning_mult_date_filter <- vals.warning
  }

  df.filtered
}
