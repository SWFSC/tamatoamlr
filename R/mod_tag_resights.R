#' @name shiny_modules
#' @export
mod_tag_resights_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        mod_filter_season_ui(ns("filter_season")),
        fluidRow(
          column(6, selectInput(ns("species"), tags$h5("Species"), #inline = TRUE,
                                choices = tamatoamlr::pinniped.sp.study,
                                selected = tamatoamlr::pinniped.sp.study,
                                multiple = TRUE, selectize = TRUE)),
          column(
            width = 6,
            tags$br(), tags$br(),
            checkboxInput(ns("ka"), "Include only resights of known-age pinnipeds")
          )
        )
        # checkboxInput(ns("all_pinnipeds"),
        #               "Summarize all pinnipeds. Uncheck to select individuals",
        #               value = TRUE),
        # conditionalPanel(
        #   condition = "input.all_pinnipeds == false", ns = ns,
        #   uiOutput(ns("pinniped_uiOut_selectize"))
        # )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        helpText("This tab allows you to summarize and visualize tag resight data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        fluidRow(
          column(4, .summaryTimingUI(ns, c("fs_total", "fs_single"),
                                     "fs_single")),
          column(4,  radioButtons(ns("summary_type"), tags$h5("Summarize by"),
                                  choices = c("Pinniped" = "pinniped",
                                              "Resights by season" = "table",
                                              "Species" = "species"),
                                  selected = "species")),
          column(4, uiOutput(ns("table_season_uiOut_selectize")))
        )
      )
    ),
    mod_output_ui(
      ns("out"),
      tags$br(), uiOutput(ns("warning_na_records"))
      # uiOutput(ns("warning_date_single_filter"))
    )
  )
}



#' @name shiny_modules
#' @export
mod_tag_resights_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General

      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })

      ### Census-specific common values
      vals <- reactiveValues(
        warning_na_records = NULL,
        warning_date_single_filter = NULL
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

      # output$warning_date_single_filter <- renderUI({
      #   span(req(vals$warning_date_single_filter), style = "color:red;")
      # })


      # ### Summarize by selection
      # output$summary_type_uiOut_radio <- renderUI({
      #   req(input$summary_timing)
      #
      #   choices <- c("Pinniped" = "pinniped", "Species" = "species")
      #
      #   if (input$summary_timing %in% .summary.timing.multiple) {
      #     choices <- c(choices, "Resights by season" = "table")
      #   }
      #
      #   radioButtons(session$ns("summary_type"), tags$h5("Summarize by"),
      #                choices = choices, selected = "species")
      # })

      ### Pinniped selection dropdown
      output$pinniped_uiOut_selectize <- renderUI({
        req(input$summary_timing == "fs_single")

        x <- tr_df() %>%
          distinct(pinniped_id, species, tag_primary, tag_type_primary) %>%
          mutate(name = paste(species, paste(tag_primary, tag_type_primary),
                              sep = " | "))

        choices.list <- as.list(x$pinniped_id) %>% set_names(x$name)

        selectInput(
          session$ns("pinniped"),
          tags$h5("Pinnipeds to include (Species | Primary Tag"),
          choices = choices.list, selected = choices.list,
          multiple = TRUE, selectize = TRUE
        )
      })

      ### Multi-season table summary filter
      output$table_season_uiOut_selectize <- renderUI({
        req(input$summary_type == "table")
        # seasons <- tr_df() %>%
        #   distinct(season_name) %>%
        #   arrange(season_name) %>%
        #   pull(season_name)
        seasons <- filter_season()$season()

        selectInput(
          session$ns("table_season"),
          tags$h5("Only include pinnipeds with at least ",
                  "one resight in the selected season(s)"),
          choices = seasons, selected = seasons,
          multiple = TRUE, selectize = TRUE
        )
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all resight data - one time run, then all data is collected
      tr_df_collect <- reactive({
        req(src()) #, tab() == .id.list$resights)
        vals$warning_na_records <- NULL

        tr.sql.orig <- try(tbl_vTag_Resights(src()), silent = TRUE)

        validate(
          need(tr.sql.orig,
               "Unable to find vTag_Resights on the specified database")
        )

        #--------------------------------------------------
        ### TMP: bring in leops separately, for now
        leop.sql.orig <- try(tbl_vTag_Resights_Leopards(src()), silent = TRUE)
        validate(
          need(leop.sql.orig,
               "Unable to find vTag_Resights_Leopards from the specified database")
        )
        #--------------------------------------------------

        # NOTE: to use bind_rows, it must be using actual data frames.
        #   Cannot use union_all here because of arrange() calls, and
        #   because of different columns in different tables
        # So, resights have to stay with early collect until leops are
        #   integrated into the tag_resights table. Ugh.
        tr.df.collect <- bind_rows(collect(tr.sql.orig), collect(leop.sql.orig))

        #----------------------------------------------
        # Filter records for non-NA values, verbosely as appropriate
        # NOTE: no need to remove location NAs
        tr.df.nona <- tr.df.collect %>%
          filter(!is.na(season_name), !is.na(resight_date), !is.na(species))

        nrow.diff <- nrow(tr.df.collect) - nrow(tr.df.nona)
        vals$warning_na_records <- if (nrow.diff != 0) {
          paste(
            nrow.diff,
            ifelse(nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL season_name, species,",
            "and/or resight_date value.",
            "Please tell the database manager."
          )
        } else {
          NULL
        }

        validate(
          need(nrow(tr.df.nona) > 0,
               "No data to process after removing rows with NA values")
        )

        tr.df.nona
      })

      #------------------------------------------------------------------------
      ps_df_collect <- reactive({
        req(src(), tab() == .id.list$resights)
        ps.sql <- try(tbl_vPinniped_Season(src()), silent = TRUE)

        validate(
          need(ps.sql,
               "Unable to find vPinniped_Season on the specified database")
        )

        ps.sql %>% collect()
      })


      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by season/date
      tr_df_filter_season <- reactive({
        tr.df.orig <- tr_df_collect()

        #----------------------------------------------
        # Filter by season/date/week num
        fs <- filter_season()

        tr.df <- if (input$summary_timing %in% .summary.timing.multiple) {
          tr.df.orig %>%
            filter(season_name %in% !!req(fs$season()))
        } else if (input$summary_timing %in% .summary.timing.single) {
          tr.df.orig %>%
            filter(season_name == !!req(fs$season()),
                   between(resight_date,
                           !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        } else {
          validate("invalid input$summary_timing value")
        }

        validate(
          need(nrow(tr.df) > 0,
               "There are no data for the given season/date filter(s)")
        )

        tr.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by species
      tr_df_filter_species <- reactive({
        tr.df <- tr_df_filter_season() %>%
          filter(species %in% !!input$species)

        validate(
          need(nrow(tr.df) > 0,
               "There are no data for the given species filter")
        )

        tr.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by non-NA cohort
      tr_df_filter_ka <- reactive({
        tr.df <- tr_df_filter_species()
        if (input$ka) tr.df <- tr.df %>% filter(!is.na(cohort))

        validate(
          need(nrow(tr.df) > 0,
               "There are no data for the given known-age filter")
        )

        tr.df
      })


      # #-------------------------------------------------------------------------
      # ### Filter data by selected pinniped(s), if necessary
      # tr_df_filter_pinniped <- reactive({
      #   tr.df <- tr_df_filter_ka()
      #
      #   if (!input$all_pinnipeds){
      #     validate(
      #       need(input$pinniped, "Please select at least one pinniped")
      #     )
      #     tr.df <- tr.df %>% filter(pinniped_id %in% as.numeric(input$pinniped))
      #   }
      #
      #   validate(
      #     need(nrow(tr.df) > 0,
      #          "There are no data for the given pinniped selection(s)")
      #   )
      #
      #   tr.df
      # })

      tr_df <- reactive({
        req(input$summary_type)
        tr_df_filter_ka() %>%
          # collect() %>% #See NOTE above
          # tag_sort(tag.sort = TRUE, tag.sort.primary = TRUE) %>%
          tag_sort(col = tag_primary, .drop = FALSE) %>%
          mutate(species = str_to_sentence(species),
                 species = factor(species, levels = sort(unique(species))))
      })


      ##########################################################################

      #------------------------------------------------------------------------
      ### Summarize multiseason tag resight data, for display
      tr_summary_table <- reactive({
        req(input$summary_type == "table")

        # Generate pinniped_ids with 1+ resights in the selected season(s)
        p.keep <- tr_df() %>%
          filter(season_name %in% req(input$table_season)) %>%
          distinct(pinniped_id) %>%
          pull(pinniped_id)

        # Get the number of resights by season, and pivot wider
        tr_df() %>%
          filter(pinniped_id %in% p.keep) %>%
          arrange(species) %>%
          group_by(season_name, pinniped_id) %>%
          summarise(n_resights = n(),
                    species = unique(species),
                    tag_primary = unique(tag_primary),
                    tag_type_primary = unique(tag_type_primary),
                    tags = paste(unique(tag), collapse = "; "),
                    .groups = "drop") %>%
          mutate(id = paste(species, tag_primary, tag_type_primary,
                            sep = " | ")) %>%
          arrange(desc(season_name)) %>%
          pivot_wider(id_cols = c(id, species, tag_primary, tag_type_primary),
                      names_from = season_name, values_from = n_resights) %>%
          tag_sort(tag_primary) %>%
          arrange(species) %>%
          relocate(species, tag_primary, tag_type_primary, .after = last_col())
      })


      ### Summarize single season tag resight data, for display
      tr_summary_species <- reactive({
        req(input$summary_type == "species")
        tr_df() %>%
          arrange(species, resight_date) %>% #for tag list order
          mutate(species = as.character(species)) %>%
          group_by(season_name, species) %>%
          summarise(n_pinnipeds = n_distinct(pinniped_id),
                    n_pinnipeds_ka = n_distinct(pinniped_id[!is.na(cohort)]),
                    n_pinniped_amlr = n_distinct(pinniped_id[!non_amlr_tag_primary]),
                    primary_tags_list = list(unique(tag_primary)),
                    .groups = "drop")
      })

      tr_summary_pinniped <- reactive({
        req(input$summary_type == "pinniped")
        ps <- ps_df_collect() %>%
          select(season_info_id, pinniped_id, attendance_study,
                 arrival_date, parturition, parturition_date, twins,
                 pup_mortality, pup_mortality_date)

        tr_df() %>%
          mutate(species = as.character(species)) %>%
          group_by(season_info_id, season_name, pinniped_id, species,
                   tag_primary, tag_type_primary, sex = pinniped_sex, cohort,
                   tag_primary_sort) %>%
          summarise(n_resights = n(),
                    resight_date_first = min(resight_date),
                    resight_date_last = max(resight_date),
                    locations = paste(unique(location_group), collapse = ", "),
                    statuses = paste(unique(status), collapse = ", "),
                    amlr_tag_primary = !unique(non_amlr_tag_primary),
                    .groups = "drop") %>%
          mutate(age = pinniped_age(today(), cohort))  %>%
          left_join(ps, by = join_by(season_info_id, pinniped_id)) %>%
          select(-season_info_id) %>%
          relocate(pinniped_id, tag_primary_sort,
                   .after = last_col()) %>%
          relocate(age, amlr_tag_primary, .after = cohort) %>%
          arrange(species, tag_primary_sort)
      })


      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        switch(
          req(input$summary_type),
          species = tr_summary_species(),
          pinniped = tr_summary_pinniped(),
          table = tr_summary_table(),
          validate(paste("Invalid summary_type input -",
                         "please contact the database manager"))
        )
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        #--------------------------------------------------------
        fs <- filter_season()
        ggplot.out <- NULL
        validate("No plot for tag resights")

        # Output
        ggplot.out
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
