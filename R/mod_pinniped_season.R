#' @name shiny_modules
#' @export
mod_pinniped_season_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE,
        width = 6, collapsible = TRUE,
        mod_filter_season_ui(ns("filter_season")),
        fluidRow(
          column(6, radioButtons(ns("location_scope"),
                                 .lbl("Pinniped home location"),
                                 choices = c("Across all pinnipeds" = "all",
                                             "Study beaches" = "study",
                                             "Non-study beaches" = "non_study"),
                                 selected = "study")),
          column(6, radioButtons(ns("age"), .lbl("Ages to include"),
                                 choices = c("All pinnipeds" = "all",
                                             "Prime age" = "prime"),
                                 selected = "all"))
        ),
        helpText("'Pinniped location' refers to the fur seal's 'home' beach'.",
                 "Home beach is determined by the location of the",
                 "first resight of the season"),
        helpText("'Prime age' refers to known-age fur seals between ",
                 "the ages of 7 and 17 (inclusive)")
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to generate summaries of pinniped season",
                 "data"),
        fluidRow(
          column(6, .summaryTimingUI(ns, c("fs_mult_total", "fs_single"))),
          column(6, radioButtons(ns("summary_type"), .lbl("Summary type"),
                                 choices = c("Pup fate" = "fate",
                                             # "Average age" = "age",
                                             # "Natality rates" = "natality",
                                             # "Return rates" = "return",
                                             "Raw data" = "raw"),
                                 selected = "fate"))
        ),
        uiOutput(ns("pup_fate_choices_uiOut_selectize"))
      )
    ),
    mod_output_ui(ns("out"))
  )
}



#' @name shiny_modules
#' @export
mod_pinniped_season_server <- function(id, src, season.df, tab) {
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


      ### Pup mortality values to include
      output$pup_fate_choices_uiOut_selectize <- renderUI({
        req(input$summary_type == "fate")

        choices <- sort(unique(ps_df()$pup_mortality))

        selectInput(session$ns("pup_fate_choices"), .lbl("To display"),
                    choices = choices, selected = choices,
                    multiple = TRUE, selectize = TRUE)
      })


      #-------------------------------------------------------------------------
      # Generate SQL query, add filters, and collect

      ps_sql <- reactive({
        ps.sql <- try(tbl_vPinniped_Season(req(src())), silent = TRUE)

        validate(
          need(ps.sql,
               "Unable to find vPinniped_Season on the specified database")
        )

        ps.sql
      })

      ### Filter data by season/date
      ps_sql_filter_season <- reactive({
        ps.sql.orig <- ps_sql()
        fs <- filter_season()

        ps.sql <- if (input$summary_timing %in% .summary.timing.multiple) {
          ps.sql.orig %>%
            filter(season_name %in% !!req(fs$season()))
        } else if (input$summary_timing %in% .summary.timing.single) {
          ps.sql.orig %>%
            filter(season_name %in% !!req(fs$season()),
                   # https://stackoverflow.com/questions/55348039
                   between(ISNULL(pup_mortality_date, !!req(fs$date_range())[1]),
                           !!req(fs$date_range())[1],
                           !!req(fs$date_range())[2]))
        } else {
          validate("invalid input$summary_timing value")
        }

        ps.sql
      })


      ### Collect data, pre-location and age filtering
      ps_df_filter_season <- reactive({
        ps.df <- ps_sql_filter_season() %>%
          mutate(pup_mortality = ISNULL(pup_mortality, "Not dead yet"))%>%
          collect()

        validate(
          need(nrow(ps.df) > 0,
               "There are no data for the given season/date filter(s)")
        )

        ps.df
      })

      #----------------------------------------------------
      # NOTE: Query takes a very long time if the inner join between pinniped
      # season and first resight info happens in the database. So, we collect
      # these tables first, and then do the join/filtering in R

      ### Get 'Home and study beach' info
      tr_first_per_season <- reactive({
        beaches.study.sql <- tbl(src(), "vBeaches") %>%
          filter(!is.na(study_beach_season_start_id)) %>%
          select(location_name = name,
                 # study_beach_season_start_id,
                 # study_beach_season_start_name,
                 study_beach_season_start_open_date)

        tbl_vTag_Resights_First_Per_Season(src()) %>%
          filter(season_name %in% !!req(filter_season()$season())) %>%
          left_join(beaches.study.sql,
                    by = join_by(location_group == location_name)) %>%
          select(season_info_id, pinniped_id,
                 first_resight_date = resight_date,
                 location_home = location_group,
                 study_beach_season_start_open_date) %>%
          collect()
      })
      #----------------------------------------------------

      ### Filter by age
      ps_df_age <- reactive({
        if (input$age == "prime") validate("Unable to filter by prime age")
        ps_df_filter_season()
      })

      ### Filter by study beach/home location
      ps_df_location <- reactive({
        ps.df <- ps_df_age()

        switch(
          input$location_scope,
          study = ps.df %>%
            inner_join(tr_first_per_season(),
                       by = join_by(pinniped_id, season_info_id)) %>%
            filter(!is.na(study_beach_season_start_open_date),
                   first_resight_date >= study_beach_season_start_open_date),
          non_study = ps.df %>%
            inner_join(tr_first_per_season(),
                       by = join_by(pinniped_id, season_info_id)) %>%
            filter(is.na(study_beach_season_start_open_date)
                   | (first_resight_date < study_beach_season_start_open_date)),
          all = ps.df %>%
            left_join(tr_first_per_season(),
                      by = join_by(pinniped_id, season_info_id)),
          validate("invalid location_scope value")
        )
      })

      ### Reactive to be used by the rest of the functions
      ps_df <- reactive({
        ps.df <- ps_df_location() %>%
          relocate(pinniped_id, pinniped_season_id, season_info_id,
                   .after = last_col())

        validate(
          need(nrow(ps.df) > 0,
               "There are no data for the given age/location filter(s)")
        )

        ps.df
      })


      #-------------------------------------------------------------------------
      pup_fate <- reactive({
        x.totals <- ps_df() %>%
          filter(parturition) %>%
          group_by(season_name) %>%
          summarise(n_pups_total = n())

        x <- ps_df() %>%
          filter(parturition,
                 pup_mortality %in% req(input$pup_fate_choices))

        x %>%
          group_by(season_name, pup_mortality) %>%
          summarise(n_pups = n()) %>%
          left_join(x.totals, by = join_by(season_name)) %>%
          mutate(percent_pups = round_logical(n_pups / n_pups_total * 100, 2)) %>%
          relocate(percent_pups, .before = n_pups_total) %>%
          arrange(desc(season_name), pup_mortality)

      })

      #-------------------------------------------------------------------------
      tbl_output <- reactive({
        validate(need(input$summary_type, "Please select a summary type"))
        switch(input$summary_type,
               fate = pup_fate(),
               raw = ps_df() %>% tag_sort(tag_primary, desc(season_name)),
               .validate_else("summary_type"))
      })

      plot_output <- reactive({
        validate("plots todo")
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
