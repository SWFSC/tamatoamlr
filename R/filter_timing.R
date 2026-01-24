#' Filter timing functions
#'
#' Functions used in filtering based on summary timing
#'
#' @param df input data frame
#' @param date.col <tidy-select> (link todo) column in `df` with date info
#' @param fs list with season/filter info. Intended to be the output of
#'   [mod_filter_season_server()]
#' @param summary.timing character; intended to be `input$summary_timing`
#'   value from whatever module function is called from
#' @param vals if `NULL` (default), then ignored. If not `NULL`, then must be a
#'   [shiny::reactiveValues()] with a named object `warning_mult_date_filter`,
#'   to which to write associated warning messages
#'
#' @details
#' `filter_timing` TODO
#'
#' `filter_mult_date` a specific filter timing subfunction.
#' It is used to filter pinniped records, typically census values,
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
#' @returns Both functions return a (filtered) data frame.
#'
#' @name filter_timing
#' @export
filter_timing <- function(df, date.col, fs, summary.timing, vals = NULL) {
  stopifnot(
    inherits(df, "data.frame"),
    is.list(fs),
    is.reactive(fs$season),
    is.reactive(fs$date_range),
    is_string(summary.timing)
  )

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
    out <- filter_mult_date(out, {{ date.col }}, fs, vals)
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
filter_mult_date <- function(df, date.col, fs, vals = NULL) {
  stopifnot(
    is.list(fs)
  )

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
             difftime({{ date.col }}, season_date, units = "days")),
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
         glue("There are no records for the ",
              "selected season(s) within {max.gap} ",
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

