#' @name shiny_modules
#' @export
mod_afs_sam_census_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      amlr_box(
        title = "Filters", width = 6,
        fluidRow(
          column(12, mod_filter_season_ui(ns("filter_season")))
        ),
        uiOutput(ns("age_sex_uiOut_selectize")),
        uiOutput(ns("location_uiOut_selectize"))
      ),
      amlr_box(
        title = "Summary options", width = 6,
        helpText("This tab allows you to summarize and visualize AFS SAM census data.",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply. "),
        fluidRow(
          column(6, .summaryTimingUI(ns, c("fs_mult_date", "fs_single", "fs_mult_raw"))),
          column(6, .summaryLocationUI(ns, c("by_capewide", "by_beach"),
                                       "by_capewide", FALSE)),
          # column(4, .summarySpAgeSexUI(ns, c("by_sp_age_sex"), "by_sp_age_sex"))
        ),
        conditionalPanel(
          "input.summary_timing != 'fs_mult_raw", ns = ns,
          helpText("Note that locations (i.e., the 'location' column in the",
                   "table output) are always grouped")
        )
      )
    ),
    mod_output_ui(
      ns("out"),
      tags$br(), uiOutput(ns("warning_na_records")),
      uiOutput(ns("warning_date_single_filter"))
    )
  )
}



#' @name shiny_modules
#' @export
mod_afs_sam_census_server <- function(id, src, season.df, tab) {
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

      # AFS SAM Census data is always filtered by location_group
      # ### Get location column
      # loc_column <- reactive({
      #   if_else(input$location_aggregate, "location_group", "location")
      # })


      ##########################################################################
      # Census-specific common values
      vals <- reactiveValues(
        warning_na_records = NULL,
        warning_date_single_filter = NULL
      )


      ##########################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      output$warning_date_single_filter <- renderUI({
        span(req(vals$warning_date_single_filter), style = "color:red;")
      })

      ### Locations dropdown
      output$location_uiOut_selectize <- renderUI({
        req(input$summary_location == "by_beach")
        census.df <- census_df_filter_season()
        beaches.list <- sort(unique(census.df$location_group))

        selectInput(
          session$ns("location"), .lbl("Location(s)"),
          choices = beaches.list, multiple = TRUE
        )
      })

      ### Columns dropdown
      output$age_sex_uiOut_selectize <- renderUI({
        req(src())
        census.names <- c("ad_male_count", "juv_male_count", "juv_unk_count")
        choices.names.cs <- census.names[
          grepl("_count", census.names) | grepl("_sum", census.names)]

        # validate(
        #   need(all(tamatoamlr::afs.study.beach.counts %in% census.names) &
        #          all(choices.names.cs %in% tamatoamlr::afs.study.beach.counts),
        #        paste("The column names from afs.study.beach.counts",
        #              "and names(census_df_collect()) are not identical -",
        #              "please adjust the afs.study.beach.counts variable"))
        # )

        selectInput(
          session$ns("age_sex"), .lbl("Columns to plot"),
          choices = census.names, selected = census.names,
          multiple = TRUE, selectize = TRUE
        )
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all census data - one time run, then all data is collected
      census_df_collect <- reactive({
        req(src(), tab() == .id.list$afs_sam)
        vals$warning_na_records <- NULL

        census.df.collect <- try(tbl_vCensus_AFS_SAM(req(src())),
                                 silent = TRUE)
        validate(
          need(census.df.collect,
               "Unable to find tbl_vCensus_AFS_SAM on specified database")
        )
        census.df.collect

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
          need(nrow(census.df.nona) > 0,
               "No data to process after removing rows with NA values")
        )

        census.df.nona
      })



      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by species, season/date, and remove NA values
      census_df_filter_season <- reactive({
        filter_timing(
          census_df_collect(), census_date, filter_season(),
          input$summary_timing
        )
        # census.df.orig <- census_df_collect()
        # vals$warning_mult_date_filter <- NULL
        #
        # #----------------------------------------------
        # # Filter by season/date/week num
        # fs <- filter_season()
        # season.curr <- req(fs$season())
        #
        # census.df <- if (input$summary_timing %in% .summary.timing.multiple) {
        #   census.df.orig %>%
        #     filter(season_name %in% season.curr)
        # } else if (input$summary_timing %in% .summary.timing.single) {
        #   req(length(season.curr) == 1)
        #   census.df.orig %>%
        #     filter(season_name == season.curr,
        #            between(census_date,
        #                    !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        # } else {
        #   validate("invalid input$summary_timing value")
        # }
        #
        # #----------------------------------------------
        # # Do additional date single filtering, if necessary
        # if (input$summary_timing == "fs_mult_date") {
        #   req(fs$mult_date())
        #   census.df <- mult_date_filter(census.df, census_date, fs, vals)
        # }
        #
        # #----------------------------------------------
        # validate(
        #   need(nrow(census.df) > 0,
        #        "There are no data for the given season filter(s)")
        # )
        #
        # census.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by location
      census_df_filter_location <- reactive({
        census.df <- census_df_filter_season()

        if (input$summary_location == "by_beach") {
          validate(need(input$location, "Please select at least one beach name"))
          census.df <- census.df %>% filter(location_group %in% input$location)
        }

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given location filter")
        )

        census.df
      })


      ##########################################################################
      # Process collected and filtered census data

      grp_names_chr <- reactive({
        if (req(input$summary_location) == "by_beach") {
          c("season_name", "census_date", "species", "location")
        } else {
          c("season_name", "census_date", "species")
        }
      })

      census_df <- reactive({
        census_df_filter_location() %>%
          mutate(location = location_group) %>%
          select(!!grp_names_chr(), !!!as.list(input$age_sex)) %>%
          group_by(!!!syms(grp_names_chr())) %>%
          summarise(n_records = n(),
                    across(where(is.numeric), sum), #na.rm = TRUE
                    .groups = "drop") %>%
          arrange_season(season.df(), desc(census_date))
      })



      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        if (input$summary_timing == "fs_mult_raw") {
          census_df_filter_location() %>%
            # select(-c(beach_id, census_created_dt)) %>%
            select(season_name, census_type, observer,
                   census_date, time_start, time_end, location, location_group,
                   species, !!!as.list(input$age_sex), census_notes, census_id)
        } else {
          census_df()
        }
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        validate(
          need(input$summary_timing != "fs_mult_raw", "No plot for raw data")
        )
        census.df.orig <- census_df()
        fs <- filter_season()

        # browser()

        #--------------------------------------------------------
        # Set some plot variable depending on user selections
        if (input$summary_timing %in% .summary.timing.single) {
          x.val <- as.name("census_date")
          x.lab <- "Date"
        } else if (input$summary_timing %in% .summary.timing.multiple) {
          x.val <- as.name("season_name")
          x.lab <- "Season"
        } else {
          validate("census plot - invalid input$summary_timing value")
        }

        y.lab <- "Count"

        md <- if (input$summary_timing == "fs_mult_date") {
          paste(day(fs$mult_date()), month.abb[month(fs$mult_date())])
        } else {
          ""
        }

        gg.title <- case_when(
          # input$summary_timing == "fs_total" ~
          #   "AFS SAM Census - Totals by Season",
          input$summary_timing == "fs_mult_date" ~
            paste("AFS SAM Census - Closest to", md),
          input$summary_timing == "fs_single" ~
            paste("AFS SAM Census -", fs$season()[1]),
          TRUE ~ "Title todo"
        )

        if (input$summary_location == "by_beach") {
          guide_legend_color <- guide_legend(title = "Location")
          guide_legend_shape <- guide_legend(title = "Sex / age class", order = 1)
          color.val <- as.name("location_fctr")
          shape.val <- as.name("count_class")
        } else {
          guide_legend_color <- guide_legend(title = "Sex / age class")
          guide_legend_shape <- "none"
          color.val <- as.name("count_class")
          shape.val <- as.name("location_fctr")
        }


        #--------------------------------------------------------
        # This processing is done here so that output$tbl is wide
        census.df <- if (input$summary_location == "by_beach") {
          census.df.orig %>% mutate(location_fctr = factor(location))
        } else {
          census.df.orig %>% mutate(location_fctr = "1")
        }

        census.df <- census.df %>%
          select(-n_records) %>%
          pivot_longer(cols = where(is.numeric), names_to = "count_class",
                       values_to = "count_value") %>%
          arrange_season(season.df(), !!!syms(grp_names_chr()), count_class)

        validate(need(nrow(census.df) > 0, "No data to plot"))


        #--------------------------------------------------------
        # Plotting
        # Always: color is sex/age class, shape is beach
        ggplot.out <- ggplot(census.df, aes(x = !!x.val, y = count_value)) +
          geom_point(aes(color = !!color.val, shape = !!shape.val)) +
          geom_line(aes(group = interaction(count_class, location_fctr),
                        color = !!color.val)) +
          # scale_color_manual(values = tamatoamlr::pinniped.sp.colors[input$species],
          #                    drop = FALSE) +
          guides(color = guide_legend_color, linetype = "none",
                 shape = guide_legend_shape, size = "none")+
          xlab(x.lab) +
          ylab(y.lab) +
          ggtitle(gg.title) +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

        ggplot.out <- if (input$summary_timing %in% .summary.timing.single) {
          ggplot.out +
            scale_x_date(breaks = sort(unique(census.df$census_date)),
                         date_labels = "%d %b %Y") +
            expand_limits(y = 0)
        } else if (input$summary_timing %in% .summary.timing.multiple){
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
