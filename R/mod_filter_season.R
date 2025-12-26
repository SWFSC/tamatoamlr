#' Season filter selection
#'
#' Module for filtering Pinniped data by multiple and single season options
#'
#' @name mod_filter_season
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
mod_filter_season_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      uiOutput(ns("season_uiOut_select")),
      uiOutput(ns("date_range_uiOut_dateRange"))
    ),
    fluidRow(
      column(4, uiOutput(ns("month_uiOut_select"))),
      column(4, uiOutput(ns("day_uiOut_select"))),
      column(4, uiOutput(ns("today_uiOut_action")))
    )

    # fluidRow(
    #   uiOutput(ns("season_uiOut_select")),
    #   uiOutput(ns("date_range_uiOut_dateRange"))
    # ),
    # fluidRow(
    #   column(6, uiOutput(ns("week_uiOut_select")))
    # )
  )
}


#' @name mod_filter_season
#'
#' @param summ.level a reactive of the 'summary level one' selection. Value must
#'   be one of: TODO
#' @param season.df reactive; the season info data frame. Intended to be the
#'   first element (\code{season.df}) of the (list) output of
#'   \code{\link{mod_season_info_server}}
#'
#' @return A list with following components:
#' \itemize{
#'   \item{season: reactive character, the name(s) of the selected season(s)}
#'   \item{date_range: reactive Date vector of length 2; the date range for a single season}
#'   \item{week: reactive character; the week to select for, across multiple seasons}
#' }
#'
#' @export
mod_filter_season_server <- function(id, summ.level, season.df) {
  stopifnot(
    is.reactive(summ.level),
    is.reactive(season.df)
  )

  moduleServer(
    id,
    function(input, output, session) {
      #------------------------------------------------------------------------
      ### Generate season list to use in reactive
      season_list <- reactive({
        as.list(req(season.df())$season_name)
      })

      # season_month_day_min <- reactive({
      #   season.df <- req(season.df())
      #
      #   open.min <- season.df$season_open_date[which.min(lubridate::yday(season.df$season_open_date))]
      #   close.max <- season.df$season_close_date[which.max(lubridate::yday(season.df$season_close_date))]
      #
      #   list()
      # })

      #------------------------------------------------------------------------
      ### Select season dropdown - could combine with min season, but left separate for now
      output$season_uiOut_select <- renderUI({
        validate(
          need(summ.level() %in% .summary.timing.choices,
               paste("Invalid summ.level value in mod_filter_season_server -",
                     "please contact thedatabase manager"))
        )

        if (summ.level() == "fs_single") {
          multi <- FALSE
          choices.sel <- max(unlist(season_list()))
          column.width = 6
          # } else if (summ.level() == "fs_mult_date") {
          #   multi <- TRUE
          #   choices.sel <- utils::head(unlist(season_list()), 6)
          #   column.width = 12
        } else {
          multi <- TRUE
          choices.sel <- unlist(season_list())
          column.width = 12
        }

        column(
          width = column.width,
          selectInput(
            session$ns("season"), tags$h5("Select season"),
            choices = season_list(), selected = choices.sel,
            multiple = multi, selectize = TRUE
          )
        )
      })


      ### Date range - for single season only
      output$date_range_uiOut_dateRange <- renderUI({
        req(summ.level() == "fs_single", season.df(), input$season)
        season.curr <- season.df() %>%
          filter(season_name == input$season)

        validate(
          need(nrow(season.curr) == 1,
               "Error in mod_season_range_server.date_range_uiOut_dateRange")
        )

        start <- min <- season.curr[["season_open_date"]]
        end <- max <- if (is.na(season.curr[["season_close_date"]])) {
          Sys.Date()
        } else {
          season.curr[["season_close_date"]]
        }

        column(
          width = 6,
          dateRangeInput(
            session$ns("date_range"), tags$h5("Date range"),
            start = start, end = end, min = min, max = max
          )
        )
      })


      ### Month dropdown
      output$month_uiOut_select <- renderUI({
        req(summ.level() == "fs_mult_date", season.df())

        months.named <- month.abb
        names(months.named) <- month.name

        selectInput(
          session$ns("month"), tags$h5("Select month"),
          choices = months.named[c(10:12, 1:3)], selected = NULL
        )
      })

      ### Day dropdown - depends on month value
      output$day_uiOut_select <- renderUI({
        req(summ.level() == "fs_mult_date", season.df())
        date.tmp <- ymd(paste("2000", req(input$month), "01", sep = "-"))
        selectInput(
          session$ns("day"), tags$h5("Select day"),
          choices = 1:days_in_month(date.tmp), selected = NULL
        )
      })

      ### Action button to select today
      output$today_uiOut_action <- renderUI({
        req(summ.level() == "fs_mult_date", season.df())
        actionButton(session$ns("today"), "Select today (todo)")
      })


      # # Week dropdown - for multiple season by week only
      # output$week_uiOut_select <- renderUI({
      #   req(summ.level() == "fs_week", input$season)
      #
      #   selectInput(
      #     session$ns("week"), tags$h5("Select week (calendar year)"),
      #     choices = 1:53, selected = 1, multiple = FALSE,
      #   )
      # })


      #------------------------------------------------------------------------
      ### Return values
      list(
        season = reactive(input$season),
        date_range = reactive(input$date_range),
        # week = reactive(input$week),
        month = reactive(input$month),
        day = reactive(input$day)
      )
    }
  )
}
