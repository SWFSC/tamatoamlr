#' Filter timing functions
#'
#' Functions used in filtering based on summary timing
#'
#' @param df input data frame
#' @param date.col <tidy-select> (link todo) column in `df` with date info
#' @param fs list with season/filter info. Intended to be the output of
#'   [mod_filter_season_server()]
#' @param date.sel Date; the user-selected date; see Details for more info
#' @param max.gap numeric; the maximum allowable gap between `date.sel` and the
#'   given record date
#' @param summary.timing character; intended to be `input$summary_timing`
#'   value from whatever module function is called from
#' @param reactive.val if `NULL` (default), then ignored. If not `NULL`, then
#'   must be a [shiny::reactiveVal()] to which to write associated warning
#'   messages
#'
#' @details
#' `filter_timing` is a function used by most [tamatoa()] modules. Specifically,
#' it provides a standardized way for modules that use the 'summary_timing'
#' shiny widget to do their filtering of the collected dataframe.
#'
#' `filter_timing` requires a dataframe, the column in that dataframe that
#' contains the record date, the output of [mod_filter_season_server()] to
#' access the seasons and other season-associated info specified by the user,
#' and the summary timing value specified by the user. It also accepts a
#' [shiny::reactiveVal()] object, for writing warning messages to be displayed
#' in the shiny app.
#'
#' `filter_mult_date` a specific filter timing subfunction. It is used to filter
#' records, typically census values, for the records that are closes to a given
#' month/day. Specifically, for each season, only the set of records that are
#' closest to the month and day of the user-specified date is kept.
#'
#' When called by `filter_timing`, the date and the max gap are found in the
#' `fs` object (`fs$max_gap()` and `fs$mult_date()`, respectively). However,
#' `filter_mult_date` is designed to not depend on any reactive values, so that
#' is could be run by non-AMLR users if useful (e.g., users of the CS_PHOC
#' dataset).
#'
#' If `reactive.val` is not `NULL`, then a warning message is written to it if
#' `df` has records from a season, but no records within the max gap from the
#' filter date.
#'
#' @returns `filter_timing` returns a (filtered) data frame. `filter_mult_date`
#'   returns a list of a filtered data frame, and warning text (described in
#'   details). The warning text is `NULL` if there are no warnings.
#'
#' @name filter_timing
#' @export
filter_timing <- function(df, date.col, fs, summary.timing, reactive.val = NULL) {
  stopifnot(
    inherits(df, "data.frame"),
    is.list(fs),
    is.reactive(fs$season),
    is.reactive(fs$date_range),
    is_string(summary.timing)
  )

  if (!is.null(reactive.val)) reactive.val(NULL)

  #----------------------------------------------
  season.curr <- req(fs$season())

  out <- if (summary.timing %in% .summary.timing.multiple) {
    df %>%
      filter(season_name %in% season.curr)
  } else if (summary.timing %in% .summary.timing.single) {
    req(length(season.curr) == 1, fs$date_range())
    df %>%
      filter(season_name == season.curr,
             between({{ date.col }}, fs$date_range()[1], fs$date_range()[2]))
  } else {
    .validate_else(summary.timing)
  }

  #----------------------------------------------
  # Do additional date single filtering, if necessary
  if (summary.timing == "fs_mult_date") {
    req(fs$mult_date())
    out.list <- filter_mult_date(
      out, {{ date.col }}, req(fs$mult_date()), req(fs$mult_max_gap())
    )

    out <- out.list[[1]]
    reactive.val(out.list[[2]])
  }

  #----------------------------------------------
  # Check, and output
  validate(
    need(nrow(out) > 0,
         "There are no data for the given season timing filter(s)")
  )

  out
}


#' @name filter_timing
#' @export
filter_mult_date <- function(df, date.col, date.sel, max.gap) {
  stopifnot(
    # is.list(fs),
    lubridate::is.Date(date.sel),
    is.numeric(max.gap)
  )

  # m <- month(req(fs$mult_date()))
  # m.abb <- month.abb[m]
  # d <- day(fs$mult_date())
  m <- month(date.sel)
  m.abb <- month.abb[m]
  d <- day(date.sel)
  # max.gap <- req(fs$mult_max_gap())

  # fs.date.df <- data.frame(
  #   season_name = req(fs$season()),
  #   m = m,
  #   d = d
  # )

  df.fs <- df %>%
    # left_join(fs.date.df, by = "season_name") %>%
    mutate(season_date = amlr_date_from_season(season_name, m, d),
           days_diff = as.numeric(
             difftime({{ date.col }}, season_date, units = "days")),
           days_diff = if_else(days_diff < 0, abs(days_diff)-0.5, days_diff)) %>%
    # group_by(season_name) %>%
    filter(days_diff == min(days_diff),
           .by = season_name)

  df.filtered <- df.fs %>%
    filter(days_diff <= max.gap) %>%
    select(-c(season_date))

  validate(
    need(nrow(df.filtered) > 0,
         glue("There are no records for the ",
              "selected season(s) within {max.gap} ",
              "days of the provided date of {d} {m.abb}"))
  )

  # Warning message for seasons w/o census record close enough
  # if (!is.null(vals)) {
  nrow.diff <- nrow(df.fs) - nrow(df.filtered)
  txt.warning <- if (nrow.diff != 0) {
    seasons.rmd <- df.fs %>%
      filter(!(season_name %in% unique(df.filtered$season_name))) %>%
      distinct(season_name) %>%
      arrange(season_name) %>%
      select(season_name) %>%
      unlist()

    # vals$warning_mult_date_filter <- paste(
    txt.warning <- paste(
      "The following season(s) have records in the selected season(s),",
      "but none within",
      glue("{max.gap} days of the provided date of {d} {m.abb}:"),
      paste(seasons.rmd, collapse = ", ")
    )
  } else {
    NULL
  }
  # }

  list(df.filtered, txt.warning)

}

