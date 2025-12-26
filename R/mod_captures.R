#' @name shiny_modules
#' @export
mod_captures_ui <- function(id) {
  pinniped.sp.captures <- tamatoamlr::pinniped.sp[
    c("Fur seal", "Elephant seal", "Leopard seal", "Weddell seal")
  ]

  ns = NS(id)
  tagList(
    box(
      title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
      mod_filter_season_ui(ns("filter_season")),
      checkboxGroupInput(ns("species"), label = tags$h5("Species"), inline = TRUE,
                         choices = pinniped.sp.captures, selected = NULL),
      # uiOutput(ns("species_uiOut_checkbox")),
      helpText("TODO: individual captures dropdown")
    ),
    box(
      title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
      fluidRow(
        column(
          width = 4,
          .summaryTimingUI(ns, c("fs_mult_total", "fs_single", #"fs_capture_ind",
                                 "fs_raw"))
        ),
        column(
          width = 4,
          conditionalPanel(
            ns = ns,
            condition = "input.summary_timing != 'fs_raw'",
            radioButtons(ns("data_type"), tags$h5("Data type"),
                         choices = c("Body condition" = "body",
                                     "Capture times" = "times",
                                     "Captures by individual" = "individual",
                                     "Capture summaries" = "summary",
                                     "Drug records" = "drugs",
                                     "Sample records" = "samples"))
          ),
        ),
        column(
          width = 4,
          conditionalPanel(
            ns = ns,
            condition = "input.data_type == 'times'",
            checkboxGroupInput(ns("time_type"), tags$h5("Time type"),
                               choices = c("Capture to release" = "capture_release",
                                           "Time on gas" = "on_gas",
                                           "Capture to reunion" = "capture_reunion",
                                           "Recovery in box" = "in_box"),
                               selected = "capture_release")
          )
        )
      ),
      # column(
      #   width = 5,
      #   radioButtons(ns("summary"), tags$h5("Choose Summary Option"),
      #                choices = c("Summary 1: Captures in one season",
      #                            "Summary 2: Captures in multiple seasons",
      #                            "Summary 3: Captures by Individual",
      #                            "raw data"))),
      # column(
      #   width = 5,
      #   conditionalPanel(
      #     ns = ns,
      #     condition = "input.summary != 'Summary 3: Captures by Individual' && input.summary != 'raw data'",
      #     radioButtons(ns("data_type"), tags$h5("Choose Data Type Option"),
      #                  choices = c("Masses(kg)",
      #                              "Body Conditions (Mass/Length)",
      #                              "Capture to Release Times",
      #                              "Times on Gas (Fur Seals only)",
      #                              "Capture to Reunion Times (Fur Seals only)",
      #                              "Recovery in Box Times (Fur Seals only)",
      #                              "Number of Captures"),
      #                  selected = "Number of Captures"))
      # )
    ),
    mod_output_ui(
      ns("out"),
      tags$br(), uiOutput(ns("warning_na_records"))
      # conditionalPanel(
      #   condition = "input.summary == 'Summary 2: Captures in multiple seasons' &
      #                                                            input.data_type != 'Number of Captures'",
      #   ns = ns,
      #   checkboxInput(ns("faceting"), "Facet By Season")
      # )
    )
  )
}


#' @name shiny_modules
#' @export
mod_captures_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  moduleServer(
    id,
    function(input, output, session) {
      # captures <- reactive({
      #   collect(tbl(req(con()), "vCaptures")) #vCaptures
      # })
      # season_info <- reactive({
      #   tbl(req(con()), "season_info") %>%
      #     select(-ts) %>%
      #     collect()
      # })
      # pinnipeds <- reactive({
      #   tbl(req(con()), "pinnipeds") %>%
      #     select(-ts) %>%
      #     collect()
      # })
      # beaches <- reactive({
      #   collect(tbl(req(con()), "beaches"))
      # })
      # tags <- reactive({
      #   collect(tbl(req(con()), "tags"))
      # })
      # output$datatbl <- renderDT({
      #   season_info()
      # }, options = list(lengthChange = FALSE)
      # )

      ##########################################################################
      # General

      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })

      # ### Get location column
      # loc_column <- reactive({
      #   if_else(input$location_aggregate, "location_group", "location")
      # })
      #
      # ### Date column
      # census.date <- "census_date_start"
      #
      ### Captures-specific common values
      vals <- reactiveValues(
        warning_na_records = NULL
      )


      ##########################################################################
      # Observe events

      # ### Store the selected beaches and column names
      # # observe(vals$beaches_selected <- input$location)
      # observe(vals$census_tbl_columns_selected <- input$age_sex)
      #
      # observeEvent(input$tabs, {
      #   # vals$beaches_selected  <- NULL
      #   vals$census_tbl_columns_selected <- NULL
      # })


      ##########################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      # ### Species checkboxes
      # output$species_uiOut_checkbox <- renderUI({
      #   req() # TODO
      #
      #   pinniped.sp.captures <- tamatoamlr::pinniped.sp[
      #     c("Fur seal", "Elephant seal", "Leopard seal", "Weddell seal")
      #   ]
      #
      #   # TODO: if statement, sometimes display 'AFS only'
      #   checkboxGroupInput(
      #     session$ns("species"), label = tags$h5("Species"), inline = TRUE,
      #     choices = pinniped.sp.captures, selected = NULL
      #   )
      # })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all capture data - one time run, then all data is collected
      captures_df_collect <- reactive({
        req(src(), tab() == .id.list$captures)
        vals$warning_na_records <- NULL

        # TODO: split these into their own reactive functions
        captures.df.collect <- if (input$data_type == "drugs") {
          captures.drugs <- try(tbl_vCaptures_Drugs(src()), silent = TRUE)
          validate(
            need(captures.drugs,
                 "Unable to find and load vCaptures_Drugs from specified database")
          )
          captures.drugs

        } else if (input$data_type == "samples") {
          captures.samples <- try(tbl_vCaptures_Samples(src()), silent = TRUE)
          validate(
            need(captures.samples,
                 "Unable to find and load vCaptures_Samples from specified database")
          )
          captures.samples

        } else {
          captures <- try(tbl_vCaptures(src()), silent = TRUE)
          validate(
            need(captures,
                 "Unable to find vCaptures on specified database")
          )
          captures
        }

        # captures.df.collect <- try(tbl_vCaptures(pool()), silent = TRUE)
        # validate(
        #   need(captures.df.collect,
        #        "Unable to find vCaptures on specified database")
        # )

        #----------------------------------------------
        # Filter records for non-NA values, verbosely as appropriate
        captures.df.nona <- captures.df.collect %>%
          filter(!is.na(season_name), !is.na(capture_date), !is.na(location),
                 !is.na(pinniped_id), !is.na(species))

        nrow.diff <- nrow(captures.df.collect) - nrow(captures.df.nona)
        vals$warning_na_records <- if (nrow.diff != 0) {
          paste(
            nrow.diff,
            ifelse(nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL season_name, capture date, location,",
            "pinniped ID, and/or species value.",
            "Please tell the database manager."
          )
        } else {
          NULL
        }

        validate(
          need(nrow(captures.df.nona) > 0,
               "No data to process after removing rows with NA values")
        )

        captures.df.nona
      })


      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by season/date
      captures_df_filter_season <- reactive({
        captures.df.orig <- captures_df_collect()

        #----------------------------------------------
        # Filter by season/date
        fs <- filter_season()

        captures.df <- if (input$summary_timing %in% .summary.timing.multiple) {
          captures.df.orig %>%
            filter(season_name %in% !!req(fs$season()))
        } else if (input$summary_timing %in% .summary.timing.single) {
          captures.df.orig %>%
            filter(season_name == !!req(fs$season()),
                   between(capture_date,
                           !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        } else {
          captures.df.orig
        }

        validate(
          need(nrow(captures.df) > 0,
               "There are no data for the given season filter(s)")
        )

        captures.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by species, if not AFS-specific times
      captures_df_filter_species <- reactive({
        captures.df.orig <- captures_df_filter_season()

        sp.check <- input$data_type == "times" &&
          any(input$time_type %in% c("on_gas", "capture_reunion", "in_box")) &&
          !identical(input$species, "Fur seal")

        validate(
          need(!sp.check,
               paste("At least one of the sleected time type(s) is",
                     "Fur seal-specific.",
                     "Please select only 'Fur seal' from the species list")
          )
        )

        captures.df <- captures_df_filter_season() %>%
          filter(species %in% !!input$species)

        validate(
          need(nrow(captures.df) > 0,
               "There are no data for the given species filter")
        )

        captures.df
      })




      # #Uses mod_filter_season to implement the season and date filters
      # summ_level <- reactive(
      #   if(input$summary != "Summary 1: Captures in one season"){
      #     "fs_multiple_total"
      #   } else {
      #     "fs_single"
      #   }
      # )
      # season_filter = reactive(mod_filter_season_server("season_filter", summ_level , season_info))
      #
      #
      # filtered_captures <- reactive({
      #   captures <- captures()
      #   if(input$data_type %in% c("Masses(kg)","Body Conditions (Mass/Length)", "Number of Captures", "Capture to Release Times")) {
      #     captures <- captures %>%
      #       filter(species %in% input$species)
      #   } else {
      #     captures <- captures %>%
      #       filter(species == "Fur seal")
      #   }
      #   if(input$summary == "Summary 2: Captures in multiple seasons" || input$summary == "Summary 3: Captures by Individual") {
      #     captures <- captures %>%
      #       filter(season_name %in% season_filter()$season_select())
      #   }
      #   if(input$summary == "Summary 1: Captures in one season") {
      #     captures <- captures %>%
      #       filter(capture_date > season_filter()$date_range()[[1]] & capture_date < season_filter()$date_range()[[2]])
      #   }
      #   return(captures)
      # })
      #
      # DTsummary1_and_2 <- reactive({
      #   if(input$data_type == "Number of Captures") {
      #     if(input$summary == "Summary 1: Captures in one season") {
      #       captures <- filtered_captures() %>%
      #         group_by(capture_date, species) %>%
      #         summarize(number_of_captures = n())
      #     }
      #     if(input$summary == "Summary 2: Captures in multiple seasons") {
      #       captures <- filtered_captures() %>%
      #         group_by(season_name, species) %>%
      #         summarize(number_of_captures = n())
      #     }
      #   }
      #   if(input$data_type == "Masses(kg)") {
      #     captures <- filtered_captures() %>%
      #       group_by(species) %>%
      #       mutate(Mass = mass_total_kg - tare_kg)
      #   }
      #   if(input$data_type == "Capture to Release Times") {
      #     captures <- filtered_captures() %>%
      #       group_by(species) %>%
      #       mutate(capt_to_release = as.numeric(difftime(hms::as_hms(release_time), hms::as_hms(capture_time), units = "mins")))
      #   }
      #   if(input$data_type == "Body Conditions (Mass/Length)") {
      #     captures <- filtered_captures() %>%
      #       group_by(species) %>%
      #       mutate(body_condition = (mass_total_kg - tare_kg)/std_length_cm)
      #   }
      #   if(input$data_type == "Capture to Reunion Times (Fur Seals only)") {
      #     captures <- filtered_captures() %>%
      #       mutate(capt_to_reunion = as.numeric(difftime(hms::as_hms(reunion_time), hms::as_hms(capture_time), units = "mins")))
      #   }
      #   if(input$data_type == "Times on Gas (Fur Seals only)") {
      #     captures <- filtered_captures() %>%
      #       mutate(gas_time = as.numeric(difftime(hms::as_hms(gas_off), hms::as_hms(gas_on), units = "mins")))
      #   }
      #   if(input$data_type == "Recovery in Box Times (Fur Seals only)") {
      #     captures <- filtered_captures() %>%
      #       mutate(box_time = as.numeric(difftime(hms::as_hms(release_time), hms::as_hms(in_box), units = "mins")))
      #   }
      #   return(captures)
      # })
      #
      # #reactive for creating summary 3 table
      # DTsummary3 <- reactive({
      #   captures <- filtered_captures()
      #   captures <- captures %>%
      #     group_by(pinniped_id, season_name) %>%
      #     summarize(number_of_captures = n())
      #   return(captures)
      # })
      #
      # #creates raw data table
      # DTrawdata <- reactive({
      #   req()
      #   captures <- captures()
      #   if(input$data_type %in% c("Masses(kg)", "Body Conditions (Mass/Length)", "Number of Captures", "Capture to Release Times")) {
      #     captures <- captures %>%
      #       filter(species %in% input$species)
      #   } else {
      #     captures <- captures %>%
      #       filter(species == "Fur seal")
      #   }
      #   if(input$summary == "Summary 3: Captures in some seasons" || input$summary == "Summary 2: Captures in one season" || input$summary == 'raw data'){
      #     captures <- captures %>%
      #       filter(season_name %in% season_filter()$season_select())
      #     #capture_date > season_filter()$date_range()[[1]] & capture_date < season_filter()$date_range()[[2]])
      #   }
      #   return(captures)
      # })
      #
      #
      # output$fur_seals_only <- renderText({
      #   "The selected summary is only available for fur seals"})
      #
      # #DT tables above render with the appropriate data
      # summary_table_reactive <- reactive({
      #   if(input$summary == "Summary 3: Captures by Individual") {
      #     pinnipeds <- pinnipeds() %>%
      #       rename(pinniped_id = ID)
      #     summary3_table <- DTsummary3() %>%
      #       pivot_wider(names_from = season_name, values_from = number_of_captures) %>%
      #       left_join(pinnipeds) %>%
      #       left_join(
      #         select(filter(
      #           tags(), primary_tag == "TRUE"),
      #           tag, tag_type, pinniped_id, tagging_date)
      #       )
      #     season_list <- intersect(rev(season_info()$season_name), colnames(summary3_table))
      #     summary3_table <- summary3_table %>%
      #       select(pinniped_id, species, tag, tag_type, season_list, sex, cohort)
      #     return(summary3_table)
      #   } else if(input$data_type == "Number of Captures") {
      #     if(input$summary == "Summary 1: Captures in one season" || input$summary == "Summary 2: Captures in multiple seasons") {
      #       return(DTsummary1_and_2())
      #     }
      #     #  else if(input$summary == "Summary 2: Captures in multiple seasons") {
      #     #   return(DTsummary2())
      #     # }
      #     else {
      #       return(filtered_captures())
      #     }
      #   } else {
      #     return(filtered_captures())
      #   }
      # })
      #
      # # output$summarydatatbl <- renderDT({
      # #   summary_table_reactive()}, options = list(lengthChange = FALSE)
      # # )
      #
      # #Creates the plots of captures by season and date
      # summary_plot_reactive <- reactive({
      #   #browser()
      #   if(input$summary == "Summary 1: Captures in one season") {
      #     table_for_plot <- DTsummary1_and_2()
      #     x_var <- table_for_plot$capture_date
      #   } else if(input$summary == "Summary 2: Captures in multiple seasons") {
      #     table_for_plot <- DTsummary1_and_2()
      #     x_var <- table_for_plot$season_name
      #   } else if(input$summary == "Summary 3: Captures by Individual") {
      #     #browser()
      #     return(ggplot(DTsummary3(), aes(x = number_of_captures)) +
      #              geom_histogram() +
      #              xlab("Number of Captures by Individual"))
      #   } else {
      #     validate(need(input$summary != "raw data", "no plot for raw data"))
      #   }
      #   if(input$data_type == "Number of Captures") {
      #     plot <- ggplot(table_for_plot, aes(x = x_var, y = number_of_captures, color = species, group = species)) +
      #       geom_point(position = "identity", stat = "identity") +
      #       geom_line(position = "identity", stat = "identity") +
      #       theme(axis.text.x = element_text(angle = 90)) +
      #       scale_color_manual(values = .colorsPresent(table_for_plot)) +
      #       labs(x = ifelse(input$summary == "Summary 1: Captures in all seasons", "Season", "Date"), y = "Number of Captures")
      #   }
      #   if(input$data_type == "Masses(kg)") {
      #     plot <- ggplot(table_for_plot, aes(x = Mass, fill = species, group = species)) +
      #       geom_histogram() +
      #       scale_fill_manual(values = .colorsPresent(table_for_plot)) +
      #       xlab("Mass(kg)")
      #   }
      #   if(input$data_type == "Body Conditions (Mass/Length)") {
      #     plot <- ggplot(table_for_plot, aes(x = body_condition, fill = species, group = species)) +
      #       geom_histogram() +
      #       scale_fill_manual(values = .colorsPresent(table_for_plot)) +
      #       xlab("Body Condition(Mass/length)")
      #   }
      #   if(input$data_type == "Capture to Release Times") {
      #     plot <- ggplot(table_for_plot, aes(x = capt_to_release, fill = species, group = species)) +
      #       geom_histogram() +
      #       scale_fill_manual(values = .colorsPresent(table_for_plot)) +
      #       xlab("Capture to Release Time (minutes)")
      #   }
      #   if(input$data_type == "Capture to Reunion Times (Fur Seals only)") {
      #     plot <- ggplot(table_for_plot, aes(x = capt_to_reunion, fill = species, group = species)) +
      #       geom_histogram() +
      #       scale_fill_manual(values = .colorsPresent(table_for_plot)) +
      #       xlab("Capture to Reunion Time (minutes)")
      #   }
      #   if(input$data_type == "Times on Gas (Fur Seals only)") {
      #     plot <- ggplot(table_for_plot, aes(x = gas_time, fill = species, group = species)) +
      #       geom_histogram() +
      #       scale_fill_manual(values = .colorsPresent(table_for_plot)) +
      #       xlab("Time on Gas")
      #   }
      #   if(input$data_type == "Recovery in Box Times (Fur Seals only)") {
      #     plot <- ggplot(table_for_plot, aes(x = box_time, fill = species, group = species)) +
      #       geom_histogram() +
      #       scale_fill_manual(values = .colorsPresent(table_for_plot)) +
      #       xlab("Time Recovering in Box (minutes)")
      #   }
      #   # Implements the faceting option
      #   if(input$summary == "Summary 2: Captures in multiple seasons" &&
      #      input$data_type != "Number of Captures" & input$faceting) {
      #     plot <- plot + facet_wrap(~season_name, ncol = 4)
      #   }
      #   return(plot)
      # })

      tbl_output <- reactive({
        if (input$data_type %in% c("drugs", "samples")) {
          captures_df_filter_species()

        } else {
          captures_df_filter_species()
        }
      })

      plot_output <- reactive({
        if (input$data_type %in% c("drugs", "samples")) {
          validate("There is no plot for 'Drug records' or 'Sample records'")
        } else {
          NULL
        }
      })

      #Uses mod_output to implement the graph and table produced above
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
