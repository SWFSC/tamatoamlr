#' @name shiny_modules
#' @export
mod_afs_capewide_pup_census_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      amlr_box(
        title = "Filters", width = 6,
        fluidRow(
          column(12, mod_filter_season_ui(ns("filter_season")))
        ),
        uiOutput(ns("location_uiOut_selectize"))
      ),
      amlr_box(
        title = "Summary options", width = 6,
        helpText("This tab allows you to summarize and visualize",
                 "AFS Capewide Pup Census data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        # TODO: add notes about CWP data from other years and eg SSI surveys
        fluidRow(
          column(6, .summaryTimingUI(ns, c("fs_mult_total", "fs_single", "fs_mult_raw"))),
          column(6, .summaryLocationUI(ns, c("by_capewide", "by_amlr", "by_beach"),
                                       "by_capewide", FALSE))
        ),
        conditionalPanel(
          condition = "input.summary_timing == 'fs_single'", ns = ns,
          checkboxInput(ns("exclude_count"),
                        "Remove data with the 'exclude_count' flag set to TRUE",
                        value = FALSE)
        ),
        conditionalPanel(
          condition = "(input.summary_location == 'by_beach' | input.summary_location == 'by_amlr') & input.summary_timing != 'fs_mult_raw'",
          ns = ns,
          checkboxInput(ns("loc_agg"), "Aggregate across selected locations",
                        value = FALSE)
        ),
        uiOutput(ns("exclude_count_uiOut_helptext"))
      )
    ),
    mod_output_ui(ns("out"), tags$br(), uiOutput(ns("warning_na_records")))
  )
}



#' @name shiny_modules
#' @export
mod_afs_capewide_pup_census_server <- function(id, src, season.df, tab) {
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
        warning_na_records = NULL
      )

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Locations dropdown
      output$location_uiOut_selectize <- renderUI({
        req(input$summary_location == "by_beach", src())
        # beaches.list <- tbl_beaches_capewide(src())$location
        beaches.list <-  sort(unique(census_df_collect()$location))

        selectInput(
          session$ns("location"), .lbl("Location(s)"),
          choices = beaches.list, multiple = TRUE
        )
      })

      ### Exclude count block
      output$exclude_count_uiOut_helptext <- renderUI({
        req(input$summary_timing)

        if (input$summary_timing == "fs_single") {
          helpText("The 'Single Season'",
                   "summary is for reviewing data for count consistency.",
                   "In the table below, the order of the data in the columns",
                   "with multiple data is is consistent across all columns.",
                   tags$br(), tags$br(),
                   "Note: If 'By beach' is selected and",
                   "'Aggregate across selected locations' is checked,",
                   "then data with the 'exclude_count' flag will always",
                   "be removed before aggregating by location")
        } else if (input$summary_timing == "fs_mult_total") {
          helpText("Note: Data with the 'exclude_count' flag will always",
                   "be removed when summarizing over multiple seasons")
        }  else if (input$summary_timing == "fs_mult_raw") {
          helpText("Note: All data, including entries where the 'exclude_count'",
                   " flag is TRUE, are included in the raw data output")
        } else {
          .validate_else("summary_timing")
        }
      })


      ###################################################################################
      ##########################################################################
      ##########################################################################
      # Collect all census data - one time run, then all data is collected
      census_df_collect <- reactive({
        req(src(), tab() == .id.list$afs_cwpc)
        vals$warning_na_records <- NULL

        census.df.collect <- try(tbl_vCensus_AFS_Capewide_Pup(src()),
                                 silent = TRUE)

        validate(
          need(census.df.collect,
               "Unable to find and load vCensus_AFS_Capewide_Pup from the specified database")
        )


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

      loc_agg <- reactive({
        summ.loc <- (
          input$summary_location == "by_beach" ||
            input$summary_location == "by_amlr")

        input$loc_agg && summ.loc &&input$summary_timing != "fs_mult_raw"
      })

      #-------------------------------------------------------------------------
      ### Filter data by species, season/date, and remove NA values
      census_df_filter_season <- reactive({
        census.df.orig <- census_df_collect()
        #----------------------------------------------
        # Filter by season/date/week num
        fs <- filter_season()

        census.df <- if (input$summary_timing %in% .summary.timing.multiple) {
          census.df.orig %>%
            filter(season_name %in% !!req(fs$season()))
        } else if (input$summary_timing %in% .summary.timing.single) {
          census.df.orig %>%
            filter(season_name == !!req(fs$season()),
                   between(census_date,
                           !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        } else {
          .validate_else("summary_timing")
        }

        # TODO: add warning message about time of year if 2021/22 data is included

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given season filter(s)")
        )

        census.df %>%
          mutate(season_name = factor(season_name))
      })


      #-------------------------------------------------------------------------
      ### Filter data that has been specified to be excluded
      census_df_filter_exclude <- reactive({
        census.df <- census_df_filter_season()
        vals$warning_na_records <- NULL

        if (input$summary_timing == "fs_mult_total") {
          census.df <- census.df %>% filter(!exclude_count)

        } else if (input$summary_timing == "fs_single") {
          if (loc_agg()) {
            validate(
              need(input$exclude_count,
                   paste("If locations are being aggregated, then the",
                         "'exclude_count' box must be checked")
              )
            )
          }

          if (input$exclude_count) {
            census.df.orig <- census.df
            census.df <- census.df.orig %>% filter(!exclude_count)

            nrow.diff <- nrow(census.df.orig) - nrow(census.df)
            nrow.diff.phrase <- ifelse(nrow.diff == 1, "row was", "rows were")
            vals$warning_na_records <- if (nrow.diff != 0) {
              glue(
                "Note: {nrow.diff} {nrow.diff.phrase} removed because of ",
                "an 'exclude_count' flag"
              )
            } else {
              NULL
            }
          }
        }

        validate(
          need(
            nrow(census.df) > 0,
            "No data to process after removing 'exclude_count' rows"
          )
        )

        census.df
      })


      #-------------------------------------------------------------------------
      ### Filter data by location
      census_df_filter_location <- reactive({
        census.df <- census_df_filter_exclude()

        if (input$summary_location == "by_beach") {
          validate(need(input$location, "Please select at least one beach name"))
          census.df <- census.df %>%
            filter(location %in% input$location) %>%
            mutate(location = factor(location, levels = input$location))
        } else if(input$summary_location == "by_amlr") {
          census.df <- census.df %>%
            filter(location %in% .amlr.beaches) %>%
            mutate(location = factor(location, levels = .amlr.beaches))
        }

        validate(
          need(nrow(census.df) > 0,
               "There are no data for the given location filter")
        )

        census.df
      })


      # ##########################################################################
      # Process data for plot / table

      #----------------------------------------------------
      ### Table and plot for fs_single summaries
      ## Table
      census_df_fs_single <- reactive({
        cwp_review(census_df_filter_location(), loc.agg = loc_agg()) %>%
          select(-census_afs_capewide_pup_sort) %>%
          mutate(count_mean = round_logical(count_mean, 0),
                 count_range_perc_diff = round_logical(count_range_perc_diff, 2))
      })

      ## Plot
      plot_fs_single <- reactive({
        x <- census_df_filter_location()

        g.title <- glue(
          "Cape Shirreff AFS Capewide Pup Census Individual Counts - ",
          "{unique(x$season_name)}"
        )

        # If aggregating across locations, run agg function and update title
        # Also update location name in df in case many locations were aggregated
        if (loc_agg()) {
          x.agg <- cwp_loc_agg(x)
          g.title <- paste(g.title, unique(x.agg$location), sep = " - ")
          x <- x.agg %>%
            mutate(location = "Aggregated locations")
        } else if (input$summary_location == "by_amlr") {
          g.title <- glue("{g.title} - AMLR Study Beaches")
        }

        x <- x %>%
          mutate(exclude_count = factor(exclude_count, levels = c("FALSE", "TRUE")))

        g.out <-  if (input$exclude_count) {
          ggplot(x) +
            geom_col(aes(location, pup_count, fill = observer),
                     position = "dodge")

        } else {
          ggplot(x) +
            geom_col(aes(location, pup_count, fill = observer,
                         alpha = exclude_count),
                     position = "dodge") +
            scale_alpha_manual(values = c(1, 0.3), drop = FALSE)
        }

        g.out +
          guides(fill = guide_legend(title = "Observer", order = 1)) +
          theme(axis.text.x = element_text(angle = 90)) +
          ggtitle(g.title) +
          xlab("Location") +
          ylab("Pup count")
      })


      #----------------------------------------------------
      ### Table and plot for fs_mult_total summaries
      ## Table
      census_df_fs_total <- reactive({
        census.df <- census_df_filter_location()

        if (input$summary_location %in% c("by_beach")) {
          cwp_total_by_loc(census.df, loc.agg = loc_agg()) %>%
            rename(count_mean = count_loc_mean,
                   count_var = count_loc_var,
                   count_sd = count_loc_sd) %>%
            mutate(count_mean = round_logical(count_mean, 0),
                   count_var = round_logical(count_var, 2),
                   count_sd = round_logical(count_sd, 2))

        } else {
          cwp_total(census.df, loc.agg = loc_agg()) %>%
            mutate(count_mean = round_logical(count_mean, 0),
                   count_sd = round_logical(count_sd, 2))
        }
      })

      ## Plot
      plot_fs_total <- reactive({
        x <- census_df_fs_total() %>% mutate(group = 1)
        g.title <- "Cape Shirreff AFS Capewide Pup Census"

        # If aggregating across locations, update title
        # Also update location name in df in case many locations were aggregated
        if (loc_agg()) {
          g.title <- paste(g.title, unique(x$location), sep = " - ")
          x <- x %>%
            mutate(location = "Aggregated locations")
        } else if (input$summary_location == "by_amlr") {
          g.title <- glue("{g.title} - AMLR Study Beaches")
        }

        g.out <- if (input$summary_location == "by_beach") {
          ggplot(x, aes(season_name, count_mean,
                        group = location, color = location,
                        text = paste("count_sd:", count_sd))) +
            guides(color = guide_legend(title = "Location"))
        } else {
          ggplot(x, aes(season_name, count_mean, group = group,
                        text = paste("count_sd:", count_sd)))
        }

        g.out +
          geom_point() +
          geom_line() +
          geom_errorbar(aes(ymin = count_mean-count_sd,
                            ymax = count_mean+count_sd),
                        width = 0.5) +
          scale_x_discrete(drop = FALSE) +
          theme(axis.text.x = element_text(angle = 90)) +
          ggtitle(g.title) +
          xlab("Season") +
          ylab("Pup count (mean)") +
          expand_limits(y = 0)
      })



      ##########################################################################
      # Outputs

      # output$mean_count_uiOut_text <- renderUI({
      #   census.df.summ <- census_df_fs_single()
      #   paste(
      #     "Total mean count:", round(sum(census.df.summ$count_mean), 0),
      #     paste0("(", round(sum(census.df.summ$count_live_mean), 0), " live)")
      #   )
      # })

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        census.df <- census_df_filter_location()

        if (req(input$summary_timing) == "fs_mult_raw") {
          census.df %>% select(-c(census_afs_capewide_pup_sort, pup_count))
        } else if (input$summary_timing == "fs_single") {
          census_df_fs_single()
        } else if (input$summary_timing == "fs_mult_total") {
          census_df_fs_total()
        } else {
          .validate_else("summary_timing")
        }
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        if (req(input$summary_timing) == "fs_mult_raw") {
          validate("There is no plot for raw data summary")
        } else if (input$summary_timing == "fs_single") {
          plot_fs_single()
        } else if (input$summary_timing == "fs_mult_total") {
          plot_fs_total()
        } else {
          .validate_else("summary_timing")
        }
      })

      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))
    }
  )
}
