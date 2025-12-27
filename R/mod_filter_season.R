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
    uiOutput(ns("season_uiOut_buttons")),
    fluidRow(
      # dateInput(ns("mult_date"), tags$h5("Select date"))

      uiOutput(ns("mult_date_uiOut"))
      # column(4, uiOutput(ns("month_uiOut_select"))),
      # column(4, uiOutput(ns("day_uiOut_select"))),
      # column(4, uiOutput(ns("today_uiOut_action")))
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
      ### Select season dropdown
      amlr_seasons <- reactive({
        choices.all <- unlist(season_list())
        # TODO: temporary workaround to not select NSF seasons
        choices.all[!str_detect(choices.all, "NSF")]
      })

      output$season_uiOut_select <- renderUI({
        validate(
          need(summ.level() %in% .summary.timing.choices,
               paste("Invalid summ.level value in mod_filter_season_server -",
                     "please contact the database manager"))
        )

        choices.all <- unlist(season_list())
        choices.sel <- amlr_seasons()

        if (summ.level() == "fs_single") {
          multi <- FALSE
          choices.sel <- max(choices.all)
          column.width <- 6
        } else if (summ.level() == "fs_mult_date") {
          multi <- TRUE
          choices.sel <- utils::head(choices.sel, 5)
          column.width = 12
        } else {
          multi <- TRUE
          # choices.sel <- choices.all[!str_detect(choices.all, "NSF")]
          column.width <- 12
        }

        column(
          width = column.width,
          selectInput(
            session$ns("season"), tags$h5("Select season"),
            choices = choices.all, selected = choices.sel,
            multiple = multi, selectize = TRUE
          )
        )
      })

      output$season_uiOut_buttons <- renderUI({
        req(summ.level() != "fs_single")

        tagList(
          actionButton(session$ns("season_all"), "Select all AMLR seasons"),
          actionButton(session$ns("season_none"), "Clear selected"),
          actionButton(session$ns("season_recent"), "Select recent")
        )
      })


      ### Date range - for single season only
      output$date_range_uiOut_dateRange <- renderUI({
        req(summ.level() == "fs_single", season.df(), input$season)
        req(length(input$season) == 1)

        season.curr <- season.df() %>%
          filter(season_name == input$season)

        # browser()
        validate(
          need(nrow(season.curr) == 1, "Error in date_range_uiOut_dateRange")
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


      ### Mult Date selector
      output$mult_date_uiOut <- renderUI({
        req(summ.level() == "fs_mult_date")
        # date.lbl <- tags$h5(
        #   "Select date"
        # )

        tagList(
          column(6, dateInput(session$ns("mult_date"), tags$h5("Select date"))),
          # column(8, helpText("The year of the date selected does not matter;",
          #                    "only the month and day will be used")),
          column(6, numericInput(session$ns("mult_max_gap"), tags$h5("Max gap (days)"),
                                 value = 7, min = 1, step = 1)),
          column(12, helpText("Find the records closest to the selected date",
                              "for all selected seasons.",
                              "The year of the date does not matter;",
                              "only the month and day will be used.",
                              "The max gap is the maximum number of days",
                              "between the record and the selected date",
                              "for the record to be considered valid"))
        )
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
      observeEvent(input$season_all, {
        updateSelectInput(session, "season", selected = amlr_seasons())
      }, ignoreInit = TRUE)

      observeEvent(input$season_recent, {
        selected <- utils::head(amlr_seasons(), 5)
        updateSelectInput(session, "season", selected = selected)
      }, ignoreInit = TRUE)

      observeEvent(input$season_none, {
        updateSelectInput(session, "season", selected = list())
      }, ignoreInit = TRUE)


      #------------------------------------------------------------------------
      out_season <- reactive({
        validate(
          need(input$season, "Please select at least one season")
        )

        input$season
      })

      ### Return values
      list(
        season = out_season,
        date_range = reactive(input$date_range),
        # week = reactive(input$week),
        mult_date = reactive(input$mult_date),
        mult_max_gap = reactive(input$mult_max_gap)
      )
    }
  )
}

