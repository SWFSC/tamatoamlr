#' @name shiny_modules
#' @export
mod_views_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        # mod_filter_season_ui(ns("filter_season"))
        fluidRow(
          column(6, uiOutput(ns("season")))
        )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to view and download certain Pinniped views"),
        fluidRow(
          column(
            width = 6,
            selectInput(ns("view"), tags$h5("View to display"),
                        # choices = c("Sample inventory" = "sample_inventory",
                        choices = c("Attendance pup weights" = "apw"),
                        selected = "sample_inventory")
          )
          # column(
          #   width = 6,
          #   conditionalPanel(
          #     condition = "input.view == 'sample_inventory'", ns = ns,
          #     radioButtons(ns("summary_type"), tags$h5("Summarize by"),
          #                  choices = c("All samples" = "all",
          #                              "Sample type" = "sample_type",
          #                              "Sample type group" = "sample_type_group"),
          #                  selected = "all")
          #   )
          # )
          # column(4, .summaryTimingUI(ns, c("fs_single"))),
          # column(4, .summaryLocationUI(ns, c("by_capewide", "by_beach"), "by_capewide")),
          # column(4, .summarySpAgeSexUI(ns, c("by_sp", "by_sp_age_sex"), "by_sp"))
        )
      )
    ),
    mod_output_ui(ns("out"))
  )
}



#' @name shiny_modules
#' @export
mod_views_server <- function(id, src, season.df, tab) {
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


      # ### Get filter_season values
      # filter_season <- reactive({
      #   mod_filter_season_server(
      #     "filter_season",  reactive(input$summary_timing), season.df
      #   )
      # })


      # #-------------------------------------------------------------------------
      # # Sample inventory view
      #
      # sample_inventory_collect <- reactive({
      #   sample.inventory <- try(
      #     tbl(src(), "vSample_Inventory") %>% collect(),
      #     silent = TRUE
      #   )
      #   validate(
      #     need(sample.inventory,
      #          "Unable to find and load vSample_Inventory from specified database")
      #   )
      #   sample.inventory
      # })
      #
      # sample_inventory <- reactive({
      #   x <- sample_inventory_collect() %>%
      #     filter(season_name == req(input$season))
      #
      #   if (input$summary_type == "all") {
      #     x
      #
      #   } else {
      #     x <- x %>%
      #       # Make single column with 'most unique' ID
      #       # case_when rolls through order of priority
      #       mutate(on_the_fly_unique = if_else(!is.na(unk_group_id),
      #                                          unk_group_id, on_the_fly_id),
      #              id_unique = case_when(
      #                !is.na(pinniped_id) ~ pinniped_id,
      #                !is.na(pup_afs_id) ~ pup_afs_id,
      #                !is.na(on_the_fly_unique) ~ on_the_fly_unique,
      #                .default = NA_integer_
      #              ))
      #
      #     x.grouped <- if (input$summary_type == "sample_type") {
      #       x %>% group_by(species, sample_type)
      #     } else if (input$summary_type == "sample_type_group") {
      #       x %>% group_by(species, sample_type_group)
      #     } else {
      #       validate("invalid summary_type - please contact the database manager")
      #     }
      #
      #     x.grouped %>%
      #       summarise(package_count = n(),
      #                 individual_seals_count = n_distinct(id_unique),
      #                 # n_pinniped_id = n_distinct(pinniped_id, na.rm = TRUE),
      #                 # n_on_the_fly = n_distinct(on_the_fly_unique, na.rm = TRUE),
      #                 # n_pup_afs_id = n_distinct(pup_afs_id, na.rm = TRUE),
      #                 # individual_seals_count =
      #                 #   (n_pinniped_id+n_on_the_fly+n_pup_afs_id),
      #                 n_adults_juveniles = sum(age_class %in% c("Adult", "Adult/Juvenile", "Juvenile")),
      #                 n_pups = sum(age_class %in% c("Pup")),
      #                 .groups = "drop") %>%
      #       relocate(individual_seals_count, n_adults_juveniles, n_pups,
      #                .after = package_count)
      #   }
      # })


      #-------------------------------------------------------------------------
      # Attendance pup weights

      apw_collect <- reactive({
        apw <- try(
          tbl(src(), "vAttendance_Pup_Weights") %>% collect(),
          silent = TRUE
        )
        validate(
          need(apw,
               "Unable to find and load vAttendance_Pup_Weights from specified database")
        )
        apw
      })


      apw <- reactive({
        apw_collect() %>% filter(season_name == req(input$season))
      })

      #-------------------------------------------------------------------------
      tbl_output <- reactive({
        # tbl.df <- if (input$view == "sample_inventory") {
        #   sample_inventory()
        # } else
        if (input$view == "apw") {
          apw()
        } else {
          validate("invalid view - please contact the database manager")
        }
      })

      plot_output <- reactive({
        validate("There are no plots for the views")
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
