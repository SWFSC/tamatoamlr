#' @name shiny_modules
#' @export
mod_takes_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        mod_filter_season_ui(ns("filter_season")),
        checkboxInput(ns("pinniped_species"), "Filter takes by pinniped species",
                      value = FALSE),
        conditionalPanel(
          condition = "input.pinniped_species == true", ns = ns,
          helpText("Note this filter will remove any 'other species' dead animals"),
          selectInput(ns("species"), tags$h5("Species"),
                      choices = tamatoamlr::pinniped.sp,
                      selected = tamatoamlr::pinniped.sp,
                      multiple = TRUE, selectize = TRUE)
        )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        helpText("This tab allows you to generate and view different summaries",
                 "of pinniped takes, for reporting for the",
                 "Marine Mammal Protection Act (MMPA) permit. "),
        fluidRow(
          column(6, .summaryTimingUI(ns, c("fs_single"))),
          column(6, radioButtons(ns("summary_type"), tags$h5("Summary type"),
                                 choices = c("By species" = "species",
                                             "By individual" = "individual",
                                             "By database table" = "table",
                                             "All takes" = "all"))
          )
        )
      )
    ),
    mod_output_ui(ns("out"))
  )
}



#' @name shiny_modules
#' @export
mod_takes_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General

      ### Season
      output$season <- renderUI({
        selectInput(session$ns("season"), tags$h5("Season"),
                    choices = req(season.df())$season_name)
      })


      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })


      #-------------------------------------------------------------------------
      ### Finish constructing query, and collect data
      takes_filter <- reactive({
        req(src())
        fs <- filter_season()

        validate(
          need(input$summary_timing == "fs_single",
               "Only single season summaries are currently available")
        )

        # Generate SQL query, and collect
        takes.sql <- tbl_vTakes(src()) %>%
          filter(season_name == !!req(fs$season()),
                 between(take_date,
                         !!req(fs$date_range())[1],
                         !!req(fs$date_range())[2]))

        if (input$pinniped_species) {
          takes.sql <- takes.sql %>%
            filter(species %in% !!input$species)
        }

        takes.collect <- takes.sql %>% collect()

        # Generate validate messages
        validate(
          need(nrow(takes.collect) > 0,
               "No take data to view based on given filters")
        )

        takes.collect
      })


      ### Summarize take data, as specified by user
      takes <- reactive({
        takes <- takes_filter()

        takes_summary <- function(x, ...) {
          x %>%
            group_by(...) %>%
            summarise(n_takes = n(), .groups = "drop")
        }

        if (input$summary_type == "all") {
          takes
        } else if (input$summary_type == "species") {
          takes %>% takes_summary(season_name, species, age_class)
        } else if (input$summary_type == "individual") {
          takes %>%
            filter(!is.na(individual_id)) %>%
            takes_summary(season_name, species, age_class,
                          individual_identifier, individual_id,
                          individual_id_source) %>%
            relocate(n_takes, .after = individual_identifier)
        } else if (input$summary_type == "table") {
          takes %>% takes_summary(season_name, table_name, species, age_class)
        } else {
          validate("Invalid summary_type - please contact the database manager")
        }
      })


      #-------------------------------------------------------------------------
      tbl_output <- reactive({
        takes()
      })

      plot_output <- reactive({
        NULL
        validate("There are currently no plots for MMPA takes")
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
