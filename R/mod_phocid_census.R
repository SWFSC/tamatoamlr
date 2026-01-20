#' @name shiny_modules
#' @export
mod_phocid_census_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        # fluidRow(
        #   column(9, mod_filter_season_ui(ns("filter_season"))),
        #   column(
        #     width = 3, #offset = 1,
        #     checkboxGroupInput(ns("species"), label = tags$h5("Species"),
        #                        choices = tamatoamlr::pinniped.phocid.sp,
        #                        selected = tamatoamlr::pinniped.phocid.sp)
        #   )
        mod_filter_season_ui(ns("filter_season")),
        # checkboxGroupInput(ns("species"), tags$h5("Species"), inline = TRUE,
        #                    choices = tamatoamlr::pinniped.phocid.sp,
        #                    selected = tamatoamlr::pinniped.phocid.sp),
        selectInput(ns("species"), tags$h5("Species"), #inline = TRUE,
                    choices = tamatoamlr::pinniped.phocid.sp,
                    selected = tamatoamlr::pinniped.phocid.sp,
                    multiple = TRUE, selectize = TRUE),

        uiOutput(ns("age_sex_uiOut_selectize")),
        uiOutput(ns("location_uiOut_selectize"))
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to summarize and visualize phocid census data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        fluidRow(
          column(5, .summaryTimingUI(ns, c("fs_mult_total", "fs_mult_date", "fs_single"))),
          column(3, .summaryLocationUI(ns, c("by_capewide", "by_beach"), "by_capewide")),
          column(4, .summarySpAgeSexUI(ns, c("by_sp", "by_sp_age_sex"), "by_sp"))
        ),
        conditionalPanel(
          condition = "input.summary_timing == 'fs_single'", ns = ns,
          checkboxInput(ns("plot_cumsum"), "Plot cumulative sum", value = FALSE),
          helpText("Note that the output plot and table will have",
                   "'census_date_start', the date of the start of the census,",
                   "rather than 'census_date'")
        )
      )
    ),
    mod_output_ui(
      ns("out"),
      tags$br(), uiOutput(ns("warning_na_records")),
      uiOutput(ns("warning_mult_date_filter"))
    )
  )
}



#' @name shiny_modules
#' @export
mod_phocid_census_server <- function(id, src, season.df, tab) {
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

      ### Get location column
      loc_column <- reactive({
        if_else(input$location_aggregate, "location_group", "location")
      })

      ### Date column
      census.date <- "census_date_start"

      ### Census-specific common values
      vals <- reactiveValues(
        warning_na_records = NULL,
        warning_mult_date_filter = NULL
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

      output$warning_mult_date_filter <- renderUI({
        span(req(vals$warning_mult_date_filter), style = "color:red;")
      })

      ### locations dropdown
      output$location_uiOut_selectize <- renderUI({
        req(input$summary_location == "by_beach")
        census.df <- census_df_filter_season()

        beaches.list <- if (input$location_aggregate){
          sort(unique(census.df$location_group))
        } else {
          sort(unique(census.df$location))
        }

        selectInput(
          session$ns("location"), tags$h5("Location(s)"),
          choices = beaches.list, multiple = TRUE
        )
      })

      ### Columns dropdown
      output$age_sex_uiOut_selectize <- renderUI({
        req(input$summary_sas == "by_sp_age_sex")

        selectInput(
          session$ns("age_sex"), tags$h5("Columns to plot"),
          choices = .census.cols.phocid, selected = .census.cols.phocid[[1]],
          multiple = TRUE, selectize = TRUE
        )
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all phocid census data - one time run, then all data is collected
      census_df_collect <- reactive({
        req(src(), tab() == .id.list$csphoc)
        vals$warning_na_records <- NULL

        validate(
          need(try(tbl(src(), "vCensus_Phocid"), silent = TRUE),
               "Unable to find vCensus_Phocid on specified database")
        )

        census.df.collect <- tbl_vCensus_Phocid(src()) %>%
          mutate(week_num = lubridate::week(census_date))
        # census_date = factor(census_date),
        # species = factor(species, levels = sort(unique(species)))),
        # species = str_to_sentence(species)

        #----------------------------------------------
        # Filter records for non-NA values, verbosely as appropriate
        census.df.nona <- census.df.collect %>%
          filter(!is.na(season_name), !is.na(location),
                 !is.na(census_date), !is.na(species))

        nrow.diff <- nrow(census.df.collect) - nrow(census.df.nona)
        vals$warning_na_records <- if (nrow.diff != 0) {
          paste(
            nrow.diff,
            ifelse(nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL season_name, species,",
            "location, and/or census_date value.",
            "Please tell the database manager."
          )
        } else {
          NULL
        }

        validate(
          need(census.date %in% names(census.df.nona),
               "Error with census date column")
        )

        validate(
          need(nrow(census.df.nona) > 0,
               "No data to process after removing rows with NA values")
        )

        census.df.nona
      })


      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by season/date
      census_df_filter_season <- reactive({
        census.df.orig <- census_df_collect()
        vals$warning_mult_date_filter <- NULL

        #----------------------------------------------
        # Filter by season/date/week num
        fs <- filter_season()
        season.curr <- req(fs$season())

        census.df <- if (input$summary_timing %in% .summary.timing.multiple) {
          census.df.orig %>%
            filter(season_name %in% season.curr)
        } else if (input$summary_timing %in% .summary.timing.single) {
          req(length(season.curr) == 1)
          census.df.orig %>%
            filter(season_name == season.curr,
                   between(!!sym(census.date),
                           !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        } else {
          validate("invalid input$summary_timing value")
        }

        #----------------------------------------------
        # Do additional date single filtering, if necessary
        if (input$summary_timing == "fs_mult_date") {
          req(fs$mult_date())
          census.df <- mult_date_filter(census.df, census_date_start, fs, vals)
        }

        # if (input$summary_timing == "fs_week") {
        #   census.df <- census.df %>%
        #     filter(week_num == as.numeric(!!req(fs$week())))
        # }

        #----------------------------------------------
        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given season filter(s)")
        )

        census.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by species
      census_df_filter_species <- reactive({
        census.df <- census_df_filter_season() %>%
          filter(species %in% !!input$species)

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given species filter")
        )

        census.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by location
      census_df_filter_location <- reactive({
        census.df <- census_df_filter_species()

        if (input$summary_location == "by_beach") {
          validate(need(input$location, "Please select at least one beach name"))
          census.df <- census.df %>% filter(!!sym(loc_column()) %in% input$location)
        }

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given location filter")
        )

        census.df
      })


      ##########################################################################
      # Process collected and filtered census data

      phocid.single <- c(.summary.timing.single, "fs_facet")
      phocid.multiple <- setdiff(.summary.timing.multiple, "fs_facet")

      ### Process data, part 1: group by/summarise/complete as desired
      #-------------------------------------------------------------------------
      census_df_summ <- reactive({
        vcs <- census_df_filter_location()

        #----------------------------------------------
        # Summarize as specified, and output
        vcs_summ_func <- function(y, ...) {
          y.summ <- y %>%
            group_by(...) %>%
            summarise(across(where(is.numeric), sum, na.rm = TRUE),
                      .groups = "drop") %>%
            complete(...) %>%
            mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
            arrange_season(season.df(), species)
          # if (!("season_name" %in% names(y.summ))) {
          #   y.summ <- y.summ %>%
          #     mutate(season_name = amlr_season_from_date(!!sym(census.date))) %>%
          #     select(season_name, everything())
          # }
          # y.summ %>% arrange_season(season.df(), species)
        }

        if (input$summary_location == "by_capewide" && input$summary_timing %in% phocid.single) {
          vcs_summ_func(vcs, season_name, !!sym(census.date), species)
        } else if (input$summary_location == "by_capewide" && input$summary_timing %in% phocid.multiple) {
          vcs_summ_func(vcs, season_name, species)
        } else if (input$summary_location == "by_beach" && input$summary_timing %in% phocid.single) {
          vcs_summ_func(vcs, season_name, !!sym(census.date), species, !!sym(loc_column()))
        } else if (input$summary_location == "by_beach" && input$summary_timing %in% phocid.multiple) {
          vcs_summ_func(vcs, season_name, species, !!sym(loc_column()))
        } else {
          validate("invalid input$summary_timing + input$summary_location combo")
        }
      })


      #------------------------------------------------------------------------
      ### Process data, part 2: make data long, and calculate other values if necessary
      census_df <- reactive({
        # Get the names of the applicable census columns, and then summarize
        grp.names.all <- c("season_name", census.date, "species", loc_column())
        # vcs.summ.orig <- census_df_summ() %>%
        #   arrange_season(season.df(), species)

        if (input$summary_sas == "by_sp") {
          # Summarize by species only
          vcs.summ <- census_df_summ() %>%
            select(where(is.character), where(is.Date), !!!.census.cols.phocid)

          grp.syms <- syms(intersect(grp.names.all, names(vcs.summ)))

          vcs.summ <- vcs.summ %>%
            pivot_longer(cols = where(is.numeric), names_to = "count_class",
                         values_to = "count_value") %>%
            group_by(!!!grp.syms) %>%
            summarise(count_value = sum(count_value),
                      .groups = "drop") %>%
            arrange_season(season.df(), !!!syms(intersect(grp.names.all[-1], names(vcs.summ))))

          # } else if (input$summary_sas == "by_sp_age") {
          #   validate("Cannot summarize by species+age (yet?)")

        } else if (input$summary_sas == "by_sp_age_sex") {
          # Summarize by species, sex, and age class
          req(all(input$age_sex %in% unlist(.census.cols.phocid)))

          vcs.summ <- census_df_summ() %>%
            select(where(is.character), where(is.Date), !!!as.list(input$age_sex)) %>%
            arrange_season(season.df(), !!!syms(dplyr::intersect(grp.names.all[-1], names(.))))

        } else {
          validate("Invalid input$summary_sas value")
        }


        # If necessary, calculate cumsums of census columns
        if (input$summary_timing == "fs_single" && input$plot_cumsum) {
          grp.names.all <- c("season_name", "species", loc_column())
          grp.syms <- syms(dplyr::intersect(grp.names.all, names(vcs.summ)))

          vcs.summ <- vcs.summ %>%
            group_by(!!!grp.syms) %>%
            mutate(across(where(is.numeric), cumsum)) %>%
            ungroup()
        }

        vcs.summ
      })


      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        df.out <- census_df() %>%
          mutate(species = str_to_sentence(species)) %>%
          nest(data_lc = where(is.numeric)) %>%
          mutate(flag0 = pmap_lgl(list(.data$data_lc), function(i) all(i == 0))) %>%
          filter(!.data$flag0) %>%
          unnest(cols = c(.data$data_lc)) %>%
          select(-.data$flag0) %>%
          arrange(if(census.date %in% names(.)) desc(!!sym(census.date)) else desc(season_name),
                  species)

        if (input$summary_timing == "fs_mult_total") {
          census_df_filter_location() %>%
            group_by(season_name) %>%
            summarise(n_header_records = n_distinct(census_phocid_header_id)) %>%
            right_join(df.out, by = "season_name") %>%
            select(season_name, .data$n_header_records, everything())
        } else if (input$summary_timing == "fs_mult_date") {
          census_df_filter_location() %>%
            group_by(season_name) %>%
            summarise(n_header_records = n_distinct(census_phocid_header_id),
                      census_date_start = unique(census_date_start),
                      census_date_end = unique(census_date_end)) %>%
            right_join(df.out, by = "season_name") %>%
            select(season_name, .data$n_header_records, everything())
        } else{
          df.out %>% select(season_name, !!sym(census.date), everything())
        }
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        #--------------------------------------------------------
        fs <- filter_season()

        census.df.orig <- census_df() %>%
          mutate_factor_species(levels = tamatoamlr::pinniped.phocid.sp)

        if (input$summary_sas == "by_sp_age_sex") {
          validate(
            need(between(length(input$age_sex), 1, 6),
                 "Please select between one and six 'columns to plot'")
          )
        }

        #--------------------------------------------------------
        # Set some plot variable depending on user selections
        if (input$summary_timing %in% phocid.single) {
          x.val <- as.name(census.date)
          x.lab <- "Date"
        } else if (input$summary_timing %in% phocid.multiple) {
          x.val <- as.name("season_name")
          x.lab <- "Season"
        } else {
          validate("census plot - invalid input$summary_timing value")
        }

        y.lab <- if (input$plot_cumsum) "Count (cumulative sum)" else "Count"

        md <- if (input$summary_timing == "fs_mult_date") {
          paste(day(fs$mult_date()), month.abb[month(fs$mult_date())])
        } else {
          ""
        }

        gg.title <- case_when(
          input$summary_timing == "fs_mult_total" ~
            "Phocid Census - Totals by Season",
          input$summary_timing == "fs_mult_date" ~
            paste("Phocid Census - Closest to", md),
          input$summary_timing == "fs_single" ~
            paste("Phocid Census -", fs$season()[1])
        )
        #[1] in fs_single is so that other options always return a vector of length 1

        lty_guide_legend <- if (input$summary_location == "by_beach") {
          guide_legend(title = "Location")
        } else {
          "none"
        }

        shape_guide_legend <- if (input$summary_sas == "by_sp") {
          "none"
        } else {
          guide_legend(title = "Sex + Age Class")
        }


        #--------------------------------------------------------
        # This processing is done here so that output$tbl is wide
        grp.names.all <- c("species", loc_column(), census.date)
        grp.syms <- syms(dplyr::intersect(grp.names.all, names(census.df.orig)))


        census.df <- if (input$summary_location == "by_beach") {
          census.df.orig %>% mutate(location_lty = factor(!!sym(loc_column())))
        } else {
          census.df.orig %>% mutate(location_lty = "1")
        }

        census.df <- if (input$summary_sas == "by_sp") {
          census.df %>%
            mutate(count_class = "1") %>%
            arrange_season(season.df(), !!!grp.syms)
        } else {
          census.df %>%
            pivot_longer(cols = where(is.numeric), names_to = "count_class",
                         values_to = "count_value") %>%
            arrange_season(season.df(), !!!grp.syms, count_class)
        }

        validate(need(nrow(census.df) > 0, "No data to plot"))

        #--------------------------------------------------------
        # Plotting
        # Always: Species is color, shape is age/sex class, linetype is beach
        ggplot.out <- ggplot(census.df, aes(x = !!x.val, y = count_value)) +
          geom_point(aes(shape = count_class, color = species)) +
          geom_line(aes(group = interaction(species, count_class, location_lty),
                        color = species, linetype = location_lty)) +
          scale_color_manual(values = tamatoamlr::pinniped.sp.colors[input$species],
                             drop = FALSE) +
          guides(color = guide_legend(title = "Species", order = 1),
                 linetype = lty_guide_legend,
                 shape = shape_guide_legend,
                 size = "none") +
          xlab(x.lab) +
          ylab(y.lab) +
          ggtitle(gg.title) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

        if (input$summary_timing == "fs_facet") {
          ggplot.out <- ggplot.out +
            facet_wrap(vars(season_name), scales = "free")
        }

        ggplot.out <- if (input$summary_timing %in% phocid.single) {
          ggplot.out +
            scale_x_date(breaks = sort(unique(census.df[[census.date]])),
                         date_labels = "%d %b %Y") +
            expand_limits(y = 0)
        } else if (input$summary_timing %in% phocid.multiple){
          ggplot.out +
            expand_limits(x = req(fs$season()), y = 0)
        }

        # Output
        ggplot.out
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
