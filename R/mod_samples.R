#' @name shiny_modules
#' @export
mod_samples_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      amlr_box(
        title = "Filters", width = 6,
        # mod_filter_season_ui(ns("filter_season"))
        fluidRow(
          column(6, uiOutput(ns("season")))
        )
      ),
      amlr_box(
        title = "Summary options", width = 6,
        helpText("Samples..."),
        fluidRow(
          column(
            width = 6,
            selectInput(ns("summary"), .lbl("Summary"),
                        choices = c("Sample inventory" = "inventory",
                                    "Sample type" = "type"),
                        selected = "inventory")
          ),
          # column(6, uiOutput(ns("summary_type_uiOut_radio")))
          column(
            width = 6,
            conditionalPanel(
              condition = "input.summary == 'inventory'", ns = ns,
              radioButtons(ns("inventory_summ"), .lbl("Summarize by"),
                           choices = c("All samples" = "all",
                                       "Sample type" = "sample_type",
                                       "Sample type group" = "sample_type_group"),
                           selected = "all")
            ),
            conditionalPanel(
              condition = "input.summary == 'type'", ns = ns,
              radioButtons(ns("type_summ"), .lbl("Summarize by"),
                           choices = c("DNA - Ethanol" = "dna_etoh",
                                       "DNA - RNALater" = "dna_rnalater"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.summary == 'inventory'", ns = ns,
          checkboxInput(ns("scats"), "Include scats in the sample inventory",
                        value = FALSE)
        )
      )
    ),
    mod_output_ui(ns("out"))
  )
}



#' @name shiny_modules
#' @export
mod_samples_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General

      ### Season
      output$season <- renderUI({
        selectInput(session$ns("season"), .lbl("Season"),
                    choices = req(season.df())$season_name)
      })


      # ### Get filter_season values
      # filter_season <- reactive({
      #   mod_filter_season_server(
      #     "filter_season",  reactive(input$summary_timing), season.df
      #   )
      # })


      # output$summary_type_uiOut_radio <- renderUI({
      #   choices <- if (input$summary == "inventory") {
      #     choices <- c(
      #       "All samples" = "all",
      #       "Sample type" = "sample_type",
      #       "Sample type group" = "sample_type_group"
      #     )
      #   } else if (input$summary == "type") {
      #     label <-
      #     choices <- c(
      #       "DNA - Ethanol" = "dna_etoh",
      #       "DNA - RNALater" = "dna_rnalater"
      #     )
      #   } else {
      #     validate("Invalid input$summary value - contact the database manager")
      #   }
      #
      #   radioButtons(
      #     session$ns("summary_type"), .lbl("Summarize by:"),
      #     choices = choices
      #   )
      # })


      sample_inventory_collect <- reactive({
        sample.inventory <- try(
          tbl(src(), "vSample_Inventory") %>% collect(),
          silent = TRUE
        )
        validate(
          need(sample.inventory,
               "Unable to find and load vSample_Inventory from specified database")
        )
        sample.inventory
      })


      #-------------------------------------------------------------------------
      # Scats
      diets_collect <- reactive({
        diets <- try(
          tbl_vDiets(src()) %>% collect(),
          silent = TRUE
        )
        validate(
          need(diets,
               "Unable to find and load vDiets from specified database")
        )
        diets
      })


      diets_inventory <- reactive({
        x <- diets_collect() %>%
          filter(season_name == req(input$season)) %>%
          mutate(sample_source = "Diets",
                 sample_type_group = "Diets",
                 sample_type = str_to_sentence(sample_type),
                 quantity = 1,
                 units = "count")

        if (req(input$inventory_summ) == "all") {
          x %>%
            select(diets_id, sample_num, sample_source, season_name,
                   sample_date = collection_date,
                   species, sample_type, sample_type_group)
        } else {
          x.grouped <- if (input$inventory_summ == "sample_type") {
            x %>% group_by(species, sample_type, sample_type_group)
          } else if (input$inventory_summ == "sample_type_group") {
            x %>% group_by(species, sample_type_group)
          } else {
            .validate_else("inventory_summ")
          }

          x.grouped %>%
            summarise(n_packages = n())
        }
      })


      #-------------------------------------------------------------------------
      # Sample inventory

      sample_inventory <- reactive({
        x <- sample_inventory_collect() %>%
          filter(season_name == req(input$season))

        if (req(input$inventory_summ) == "all") {
          x
        } else {
          x <- x %>%
            # Make single column with 'most unique' ID
            # case_when rolls through order of priority
            mutate(unk_group_id = unk_group_id,
                   on_the_fly_unique = if_else(
                     !is.na(unk_group_id), unk_group_id, on_the_fly_id),
                   id_unique = case_when(
                     !is.na(pinniped_id) ~ pinniped_id,
                     !is.na(pup_afs_id) ~ pup_afs_id,
                     !is.na(on_the_fly_unique) ~ on_the_fly_unique,
                     .default = NA_integer_
                   ))

          x.grouped <- if (input$inventory_summ == "sample_type") {
            x %>% group_by(species, sample_type, sample_type_group)
          } else if (input$inventory_summ == "sample_type_group") {
            x %>% group_by(species, sample_type_group)
          } else {
            .validate_else("inventory_summ")
          }

          x.grouped %>%
            summarise(n_packages = n(),
                      n_individual_animals = n_distinct(id_unique),
                      # n_pinniped_id = n_distinct(pinniped_id, na.rm = TRUE),
                      # n_on_the_fly = n_distinct(on_the_fly_unique, na.rm = TRUE),
                      # n_pup_afs_id = n_distinct(pup_afs_id, na.rm = TRUE),
                      # individual_seals_count =
                      #   (n_pinniped_id+n_on_the_fly+n_pup_afs_id),
                      n_adults_juveniles = sum(age_class %in% c("Adult", "Adult/Juvenile", "Juvenile")),
                      n_pups = sum(age_class %in% c("Pup")),
                      .groups = "drop") %>%
            relocate(n_individual_animals, n_adults_juveniles, n_pups,
                     .after = n_packages)
        }
      })


      #-------------------------------------------------------------------------
      # Sample types
      sample_type <- reactive({
        sample_inventory_collect()
      })



      #-------------------------------------------------------------------------
      tbl_output <- reactive({
        tbl.df <- if (input$summary == "inventory") {
          if (input$scats) {
            bind_rows(sample_inventory(), diets_inventory())
          } else {
            sample_inventory()
          }
        } else if (input$summary == "type") {
          sample_type()
        } else {
          .validate_else("summary")
        }
      })

      plot_output <- reactive({
        validate("There are no plots for the samples")
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
