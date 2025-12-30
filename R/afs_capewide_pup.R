#' Summarize AFS CWP Census Data
#'
#' Summarize AFS Cape-wide Pup (CWP) Census Data
#'
#' @param x data frame; output of [tbl_vCensus_AFS_Capewide_Pup()] or
#'   equivalent. The exception is if `x.byloc == TRUE`; in this case then `x`
#'   must be the output of `cwp_total_by_loc()`
#' @param loc.agg logical; if `TRUE`, then records will be aggregated by season
#'   and observer across all locations in `x` before doing further summarizing.
#'   This option is intended for situations where e.g. puppies moved between
#'   beaches between observer counts, and the user wants to aggregate counts
#'   before reviewing to see if recounts are needed
#' @param x.byloc logical; indicates if `x` is the output of
#'   `cwp_total_by_loc()`. This argument is intended for times where the user
#'   needs both the output of `cwp_total_by_loc()` and `cwp_total()`
#'   Specifically, it allows the user to pass the output of `cwp_total_by_loc()`
#'   directly to `cwp_total()` as `x`, rather than `cwp_total()` needing to run
#'   `cwp_total_by_loc()` again. is intended to minimize work if a user needs
#'   both the output of `cwp_total_by_loc()` and `cwp_total()`
#' @param verbose logical; should a message be printed stating how many records
#'   with `exclude_count == TRUE` were removed from `x` before processing?
#'
#' @details Function-specific details:
#'
#'   * `cwp_loc_agg()`: Aggregate pup counts and related info across
#' locations, grouping by season and observer. This function is intended for
#' situations where a user has filtered CWP data for a specific subset of
#' locations and wants to work with the counts across all of these locations,
#' effectively treating all of the selected locations as one location.
#'
#'   * `cwp_review()`: Summarize pup counts and ancillary information for review
#' after grouping by season_name and location. If `loc.agg == TRUE` then all
#' counts will be summed by observer across locations. Specifically, `x` will be
#' passed to `cwp_loc_agg()`, before being summarized. The output data frame is
#' various pasted conglomerations that are intended for within-season review to
#' help determine if recounts are needed.
#'
#'   * `cwp_total_by_loc()`: group by season_name and location, and return count mean,
#' variance, SD, and min census date (across locations). Calculating the
#' variance and SD follow Cape Shirreff/AMLR-specific rules about when it must
#' be `NA`. These rules are based on historical knowledge about past survey
#' data. See the 'assumptions' section below for further details. The min census
#' date is the earliest census date for that season/location group.
#'
#'   * `cwp_exclude_count()` removes any records with an 'exclude_count' flag,
#' i.e., with `exclude_count == TRUE` for that record. If `verbose` is `TRUE`
#' (default), then a message will be printed via [base::message()].
#'
#'   * `cwp_total()`: after running `cwp_total_by_loc()`, group by season name and
#' return counts and standard deviations.
#'
#' Note that none of these functions do any rounding. To round values, for
#' instance to get a whole number for pup counts, use functions such as
#' [base::round()] or [amlrian::round_logical()]
#'
#' Also note that the functions `cwp_loc_agg()` and `cwp_total_by_loc()`
#' always remove any records with an 'exclude_count' flag before processing.
#'
#' Assumptions: The 'cwp' functions make several assumptions specific to U.S. AMLR Pinniped
#' Data:
#'   * The research program values are unique when considered by season
#'   * Study beaches (locations: Copihue, Maderas, Cachorros, Chungungo)
#' censused between 1 July 2008 and 1 July 2011 were recorded as single counts,
#' and thus do not have variance values.
#'   * All counts before July 2008 (i.e., before the 2008/09 season)
#' do not have valid variance values.
#'
#' @return An ungrouped object of the same type as `x`, with the summarized AFS
#'   Capewide Pup Census data. The summary options are described in 'Details'.
#'
#'   For all functions except `cwp_total()`: if 'census_afs_capewide_pup_sort'
#'   exists as a column in `x`, then the output data frame will also be sorted
#'   by 'census_afs_capewide_pup_sort' (after being sorted first by the column
#'   'season_name').
#'
#' @examplesIf FALSE
#'   # Not run; examples only will run if 'con' is a valid database connection
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
#'   # All included locations are aggregated by observer,
#'   # before summarizing across single agregated location
#'   cwp_total(x.201617.ballena, loc.agg = TRUE)
#'   # Each location is summarized, before combining
#'   cwp_total(x.201617.ballena, loc.agg = FALSE)
#'
#'   x.byloc <- cwp_total_by_loc(x)
#'   cwp_total(x.byloc, x.byloc = TRUE)
#'
#' @name afs_capewide_pup
#' @export
cwp_loc_agg <- function(x, verbose = TRUE) {
  columns.names <- c(
    "season_name", "location", "census_date",
    "observer", "census_notes", "exclude_count",
    "pup_count", "pup_live_count", "pup_dead_count"
  )
  stopifnot(
    all(c(columns.names %in% names(x)))
  )

  sort.syms <- syms(intersect(c("census_afs_capewide_pup_sort"), names(x)))

  x <- cwp_exclude_count(x, verbose)

  locations.all <- paste(sort(unique(x$location)), collapse = ", ")

  x.out <- x %>%
    arrange(!!!sort.syms) %>%
    group_by(season_name, observer) %>%
    summarise(across(c(pup_count, pup_live_count, pup_dead_count),
                     \(x) sum(x, na.rm = TRUE),
                     .names = "{.col}"),
              location = locations.all, #needed to properly group later
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
cwp_total_by_loc <- function(x, loc.agg = FALSE, verbose = TRUE) {
  columns.names <- c(
    "season_name", "location",
    "census_date", "pup_count", "research_program", "exclude_count"
  )
  stopifnot(all(c(columns.names %in% names(x))))

  sort.syms <- syms(intersect(c("census_afs_capewide_pup_sort"), names(x)))

  x <- cwp_exclude_count(x, verbose)

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
cwp_total <- function(x, x.byloc = FALSE, loc.agg = FALSE, verbose = TRUE) {
  x.df.byloc <- if (x.byloc) {
    loc.columns.names <- c(
      "season_name", "location",
      "num_records", "count_loc_mean", "count_loc_sd", "count_loc_var",
      "census_date_min", "research_program"
    )
    stopifnot(all(loc.columns.names %in% names(x)))

    x
  } else {
    cwp_total_by_loc(x, loc.agg = loc.agg, verbose)
  }

  x.df.byloc %>%
    group_by(season_name) %>%
    summarise(count_mean = sum(count_loc_mean),
              count_var = if_else(min(census_date_min) < as.Date("2011-07-01"),
                                  NA_real_,
                                  sum(count_loc_var, na.rm = TRUE)),
              count_sd = sqrt(count_var),
              research_program = unique(research_program),
              .groups = "drop") %>%
    select(-count_var)
}



# Verbosely remove any records with an exclude_count flag
cwp_exclude_count <- function(x, verbose) {
  if (any(x$exclude_count)) {
    if (verbose) {
      message(glue("x contains {sum(x$exclude_count)} record(s) with ",
                   "'exclude_count == TRUE'; ",
                   "note that these records will be removed ",
                   "for the specified processing"))
    }
    x %>% filter(exclude_count == 0)

  } else {
    x
  }
}
