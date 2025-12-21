#' Summarize AFS Capewide Pup Census Data
#'
#' Summarize AFS Capewide Pup (CWP) Census Data
#'
#' @param x data frame; output of [tbl_vCensus_AFS_Capewide_Pup()] or
#'   equivalent. The exception is if `x.byloc == TRUE`; in this case then `x`
#'   must be the output of `cwp_total_by_loc()`
#' @param loc.agg logical; indicates if `cwp_review()` should aggregate records
#'   across location (by observer) before summarizing (`TRUE`), or if records
#'   should be summarized by season/location as-is (`FALSE`; default).
#' @param x.byloc logical; indicates if `x` is the output of
#'   `cwp_total_by_loc()`. This argument is intended to minimize work if a user
#'   needs both the output of `cwp_total_by_loc()` and `cwp_total()`
#'
#' @details Function-specific details:
#'  * `cwp_loc_agg()`: Aggregate pup counts and related info across
#'   locations, and by season/observer. This function is intended for situations
#'   where a user has filtered CWP data for a specific subset of locations and
#'   wants to summarize the counts across all of these locations, effectively
#'   treating all of the locations as one location.
#'
#'   * `cwp_review()`: Summarize pup counts and ancillary information for review
#'   after grouping by season_name and location. `loc.agg == TRUE` then all
#'   counts will be summed by observer across locations, i.e., `x` will be
#'   passed to `cwp_loc_agg()`, before being summarized. The output data frame
#'   is various pasted conglomerations that are intended for review to help
#'   determine if recounts need to happen.
#'
#'   * `cwp_total_by_loc()`: group by season_name and location; return count mean,
#'   variance, SD, and min date. Calculating the variance and SD follow Cape
#'   Shirref/AMLR-specific rules about when it must be NA based on historical
#'   knowledge about past survey data. See the 'assumptions' section for
#'   details. census_date_min is the earliest census date for that
#'   season/location group. Records with
#'
#'   * `cwp_total()`: after running cwp_total_by_loc, group by season name and
#'   return counts and standard deviations
#'
#'   Note that none of these functions do any rounding. To round values, for
#'   instance to get a whole number for pup counts, users may use functions such
#'   as [base::round()] or [amlrian::round_logical()]
#'
#'   The cwp functions make several assumptions specific to U.S. AMLR Pinniped
#'   Data:
#'   * The research program values are unique when considered by season
#'   * Study beaches (locations: Copihue, Maderas, Cachorros, Chungungo)
#'   censused between 2008-07-01 and 2011-07-01 are single counts and thus do
#'   not have variance values.
#'   * All counts before July 2008 (i.e., before the 2008/09 season)
#'   do not have variance values.
#'
#' @return An ungrouped object of the same tpye as `x`, with the summarized AFS
#'   Capewide Pup Census data. The summary options are described in 'Details'.
#'
#'   For all functions except `cwp_total()`: if 'census_afs_capewide_pup_sort'
#'   exists as a column in `x`, then the output data frame will also be sorted
#'   by 'census_afs_capewide_pup_sort' (after being sorted first by the column
#'   'season_name').
#'
#' @examplesIf FALSE
#'   # Not run; examples only will run if `con` is a valid database connection
#'   con <- odbc::dbConnect(odbc(), filedsn = "amlr-pinniped-db-prod.dsn")
#'   x <- tbl_vCensus_AFS_Capewide_Pup(con)
#'
#'   x.201617 <- x[x$season_name == "2016/17", ]
#'   cwp_review(x.201617)
#'
#'   x.201617.ballena <- x.201617[grepl("Ballena", x.201617$location), ]
#'   cwp_loc_agg(x.201617.ballena)
#'   cwp_review(x.201617.ballena, loc.agg = TRUE)
#'
#'   cwp_total(x)
#'   cwp_total(x.201617.ballena, loc.agg = TRUE)
#'
#'   x.byloc <- cwp_total_by_loc(x)
#'   cwp_total(x.byloc, x.byloc = TRUE)
#'
#' @name afs_capewide_pup
#' @export
cwp_loc_agg <- function(x) {
  columns.names <- c(
    "season_name", "location", "census_date",
    "observer", "census_notes", "exclude_count",
    "pup_count", "pup_live_count", "pup_dead_count"
  )
  stopifnot(
    all(c(columns.names %in% names(x)))
  )

  sort.syms <- syms(intersect(c("census_afs_capewide_pup_sort"), names(x)))

  x <- cwp_exclude_count(x)

  x.out <- x %>%
    arrange(!!!sort.syms) %>%
    group_by(season_name, observer) %>%
    summarise(across(c(pup_count, pup_live_count, pup_dead_count),
                     \(x) sum(x, na.rm = TRUE),
                     .names = "{.col}"),
              location = paste(unique(location), collapse = ", "),
              census_date_min = min(census_date),
              exclude_count = unique(exclude_count),
              census_notes = if_else(
                all(is.na(census_notes)),
                NA_character_,
                paste(as.character(na.omit(census_notes)), collapse = "; ")),
              research_program = unique(research_program),
              .groups = "drop")

  if (length(sort.syms) > 0) {
    x.out %>%
      mutate(census_afs_capewide_pup_sort = 1, .after = season_name)
  } else {
    x.out
  }
}



#' @name afs_capewide_pup
#' @export
cwp_review <- function(x, loc.agg = FALSE) {
  columns.names <- c(
    "season_name","location", "census_date",
    "observer", "census_notes", "exclude_count",
    "pup_count", "pup_live_count", "pup_dead_count"
  )
  stopifnot(
    all(c(columns.names %in% names(x)))
  )

  sort.syms <- syms(intersect(c("census_afs_capewide_pup_sort"), names(x)))

  # Aggregate across locations, if specified
  if (loc.agg) x <- cwp_loc_agg(x) %>% rename(census_date = census_date_min)

  x %>%
    group_by(season_name, !!!sort.syms, location) %>%
    arrange(observer) %>% #so that collapsed data are always in the same order
    summarise(n_records = n(),
              count_mean = mean(pup_count),
              count_range = diff(range(pup_count)),
              count_range_perc_diff = if_else(
                count_mean == 0, 0, count_range / count_mean * 100),
              observers = paste(observer, collapse = "; "),
              counts = paste(pup_count, collapse = "; "),
              exclude_count = paste(as.integer(exclude_count), collapse = "; "),
              notes = paste(census_notes, collapse = "; "),
              counts_live = paste(pup_live_count, collapse = "; "),
              counts_dead = paste(pup_dead_count, collapse = "; "),
              .groups = "drop") %>%
    relocate(!!!sort.syms, .after = last_col())
}

#' @name afs_capewide_pup
#' @export
cwp_total_by_loc <- function(x, loc.agg = FALSE) {
  columns.names <- c(
    "season_name", "location",
    "census_date", "pup_count", "research_program", "exclude_count"
  )
  stopifnot(all(c(columns.names %in% names(x))))

  sort.syms <- syms(intersect(c("census_afs_capewide_pup_sort"), names(x)))

  x <- cwp_exclude_count(x)

  # Aggregate across locations, if specified
  if (loc.agg) x <- cwp_loc_agg(x) %>% rename(census_date = census_date_min)

  x %>%
    group_by(season_name, !!!sort.syms, location) %>%
    summarise(num_records = n(),
              count_loc_mean = mean(pup_count),
              count_loc_var = var(pup_count),
              census_date_min = min(census_date),
              research_program = unique(research_program),
              .groups = "drop") %>%
    mutate(study_beach_count = between(census_date_min,
                                       ymd("2008-07-01"), ymd("2011-07-01")) &
             location %in% c("Copihue", "Maderas", "Cachorros", "Chungungo"),
           count_loc_var = case_when(
             census_date_min < as.Date("2008-07-01") ~ NA_real_,
             study_beach_count ~ NA_real_,
             .default = count_loc_var
           ),
           count_loc_sd = sqrt(count_loc_var)) %>%
    relocate(count_loc_sd, .before = count_loc_var) %>%
    select(-c(study_beach_count))
}

#' @name afs_capewide_pup
#' @export
cwp_total <- function(x, x.byloc = FALSE, loc.agg = FALSE) {
  x.df.byloc <- if (x.byloc) {
    loc.columns.names <- c(
      "season_name", "location",
      "num_records", "count_loc_mean", "count_loc_sd", "count_loc_var",
      "census_date_min", "research_program"
    )
    stopifnot(all(loc.columns.names %in% names(x)))

    x
  } else {
    cwp_total_by_loc(x, loc.agg = loc.agg)
  }

  x.df.byloc %>%
    group_by(season_name) %>%
    summarise(count_mean = sum(count_loc_mean),
              count_var = if_else(min(census_date_min) < as.Date("2011-07-01"),
                                  NA_real_, sum(count_loc_var, na.rm = TRUE)),
              count_sd = sqrt(count_var),
              research_program = unique(research_program),
              .groups = "drop") %>%
    select(-count_var)
}



# Verbosely remove any records with an exclude_count flag
cwp_exclude_count <- function(x) {
  if (any(x$exclude_count)) {
    warning("x contains ", sum(x$exclude_count),
            " record(s) with exclude_count == TRUE; ",
            "note that these records will be filtered out ",
            "for the specified processing",
            immediate. = TRUE)

    x %>% filter(exclude_count == 0)

  } else {
    x
  }
}
