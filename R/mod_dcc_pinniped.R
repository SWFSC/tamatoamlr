#' @name shiny_modules
#' @export
mod_dcc_pinniped_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Load Files", status = "warning", width = 4,
        solidHeader = FALSE, collapsible = TRUE,
        fileInput(ns("dcc_files_cabo"),
                  tags$h5("Load raw DCC files from CABO station"),
                  multiple = TRUE, accept = .tamatoa.csv.accept),
        fileInput(ns("dcc_files_mad"),
                  tags$h5("Load raw DCC files from MAD station"),
                  multiple = TRUE, accept = .tamatoa.csv.accept),
        helpText("Tamatoa will pull the DCC key from the database"),
        downloadButton(ns("key_download"), "Download database key"),
      ),
      box(
        title = "Summary and Filter Options", status = "warning",
        solidHeader = FALSE, width = 8, collapsible = TRUE,
        fluidRow(
          column(4, radioButtons(ns("summary_type"), tags$h5("Summary type"),
                                 choices = c("Trips" = "trips",
                                             "Pings" = "pings",
                                             "All processed data" = "all"),
                                 selected = "trips")),
          column(4, uiOutput(ns("season"))),
          # uiOutput(ns("date_range")),
          column(4, numericInput(ns("trip_hours"), tags$h5("Trip time gap (hours)"),
                                 value = 24, min = 1, step = 1))
        ),
        # checkboxInput(ns("include_resights"), "Include resights as tx pings")
        helpText("Resights cannot currently be included as pings"),
        box(
          title = NULL, solidHeader = FALSE, width = 12, collapsible = FALSE,
          conditionalPanel(
            condition = "input.summary_type == 'all'", ns = ns,
            helpText("The below data are all raw DCC data for pings",
                     "that can be joined with the MicroVHF database key, and",
                     "filtered by deployment/recovery dates")
          ),
          conditionalPanel(
            condition = "input.summary_type == 'pings'", ns = ns,
            helpText("The below data are all processed DCC data for the",
                     "selected Tag|Freq|Code(s)"),
            checkboxInput(ns("ping_plot_trip"),
                          "Include the trip time gaps in the plot",
                          value = FALSE)
          ),
          conditionalPanel(
            condition = "input.summary_type == 'trips'", ns = ns,
            fluidRow(
              column(
                width = 6,
                selectInput(ns("trips_summary_type"),
                            tags$h5("Trips summary type"),
                            choices = c("All trips for each seal" = "by_each",
                                        "For each seal, average across trips" = "by_pinniped",
                                        "For each trip number, average across seals" = "by_trip",
                                        "Average across all seals and trips" = "by_all"))
              ),
              column(
                width = 5, offset = 1,
                conditionalPanel(
                  condition = "input.trips_summary_type == 'trips'", ns = ns,
                  checkboxInput(ns("trips_max"), "Show only maximum completed trips")
                ),
                conditionalPanel(
                  condition = "input.trips_summary_type != 'trips'", ns = ns,
                  numericInput(ns("trip_num_max"),
                               tags$h5("Maximum trip number to include"),
                               value = 6, min = 1, step = 1)
                )
              )
            )
          )
        ),
        uiOutput(ns("tag_freq_code_uiOut_select")),
        actionButton(ns("tag_freq_code_all"), "Select all seals"),
        actionButton(ns("tag_freq_code_pupped"), "Select all seals with pups"),
        actionButton(ns("tag_freq_code_clear"), "Clear all selections")
      ),
      mod_output_ui(
        ns("out"),
        tags$br(),
        uiOutput(ns("warning_na_records")),
        tags$strong("Summary text for sit rep:"),
        textOutput(ns("trip_txt"))
      )
    )
  )
}



#' @name shiny_modules
#' @export
mod_dcc_pinniped_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  # How to structure this to pull key (tx and other info) from db. Thoughts:
  #   Will need to make user select single season.
  #   When they do that, then the code pulls capture data and checks devices
  #   for all 'micro-vhf tx' devices that were still deployed at the start of the selected season.
  #   Then for those pinnipeds, pull in pinniped_season data to get pup_mortality_date.
  #   The end date used in the (individual) filter is pup_mortality_date or device recovered date


  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General
      vals <- reactiveValues(
        warning_na_records = NULL,
        tag_freq_code_single = NULL,
        tag_freq_code_multiple = NULL
      )

      observeEvent(input$tag_freq_code, {
        if (input$summary_type != "pings") {
          vals$tag_freq_code_multiple <- input$tag_freq_code
        } else {
          vals$tag_freq_code_single <- input$tag_freq_code
        }
      })

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Season
      output$season <- renderUI({
        selectInput(session$ns("season"), tags$h5("Season"),
                    choices = req(season.df())$season_name)
      })

      # ### Date range
      # output$date_range <- renderUI({
      #   req(input$tx_key_type == "Database")
      #
      #   dateRangeInput(session$ns("date_range"), tags$h5("Date range"),
      #                  start = max(req(season.df())$season_open_date))
      # })


      #-------------------------------------------------------------------------
      # Tag-freq-code selection

      ### Prep functions, used by multiple below
      tag_freq_code_pre <- reactive({
        dcc_processed() %>%
          distinct(tag_freq_code, parturition) %>%
          arrange(tag_freq_code)
      })

      tag_freq_code_choices_all <- reactive({
        tag_freq_code_pre() %>%
          select(tag_freq_code) %>%
          unlist() %>% unname()
      })

      tag_freq_code_choices_pupped <- reactive({
        tag_freq_code_pre() %>%
          filter(parturition) %>%
          select(tag_freq_code) %>%
          unlist() %>% unname()
      })

      ### Generate seletInput
      output$tag_freq_code_uiOut_select <- renderUI({
        validate(
          need(isTruthy(input$dcc_files_cabo$datapath) | isTruthy(input$dcc_files_mad$datapath),
               "Load CABO and MAD DCC files to be able to select a 'tag | frequency | code'")
        )

        selected <- isolate({
          if (input$summary_type != "pings") {
            if (is.null(vals$tag_freq_code_multiple)) {
              tag_freq_code_choices_pupped()}
            else {
              vals$tag_freq_code_multiple
            }
          } else {
            if (is.null(vals$tag_freq_code_single)) NULL else vals$tag_freq_code_single
          }
        })

        # Return widget
        selectInput(session$ns("tag_freq_code"),
                    tags$h5("Select at least one 'tag | frequency | code'"),
                    choices = tag_freq_code_choices_all(),
                    selected = selected,
                    multiple = TRUE)
      })

      ### Action buttons to set pre-specified tag/freq/codes
      observeEvent(input$tag_freq_code_all, {
        updateSelectInput(session, "tag_freq_code",
                          selected = tag_freq_code_choices_all())
      })

      observeEvent(input$tag_freq_code_pupped, {
        updateSelectInput(session, "tag_freq_code",
                          selected = tag_freq_code_choices_pupped())
      })

      observeEvent(input$tag_freq_code_clear, {
        updateSelectInput(session, "tag_freq_code",
                          selected = character(0))
      })


      ##########################################################################
      # Load and do initial formatting of DCC data

      ### Shiny-specific validate function
      dcc_validate <- function(dcc.df) {
        dcc.columns <- c("Yr", "Day", "Hr", "Mn", "Fr", "Sig", "Code")

        validate(
          need(all(dcc.columns %in% names(dcc.df)),
               paste("The loaded DCC file(s) do not have all of the expected columns:",
                     paste(dcc.columns, collapse = ", ")))
        )

        dcc.df
      }

      ### DCC data - load CABO files
      dcc_files_cabo <- reactive({
        dcc_read_files(req(input$dcc_files_cabo$datapath), "CABO") %>%
          dcc_validate()
      })

      ### DCC data - load MAD files
      dcc_files_mad <- reactive({
        dcc_read_files(req(input$dcc_files_mad$datapath), "MAD") %>%
          dcc_validate()
      })

      ### Join files, and format data
      dcc_formatted <- reactive({
        bind_rows(dcc_files_cabo(), dcc_files_mad()) %>%
          dcc_format() %>%
          arrange(freq, code, datetime)
      })


      ##########################################################################
      # Collect relevant table from the database

      ### Pinniped season
      ps_df_collect <- reactive({
        req(src(), tab() == .id.list$dcc, input$season)
        ps.sql <- try(tbl_vPinniped_Season(src()), silent = TRUE)

        validate(
          need(ps.sql,
               "Unable to collect vPinniped_Season from the specified database")
        )

        ps.sql %>%
          filter(season_name == !!req(input$season)) %>%
          select(pinniped_id, parturition, pup_mortality, pup_mortality_date,
                 attendance_study) %>%
          collect()
      })

      ### Micro-VHF key, joined with pinniped season
      microvhf_key <- reactive({
        req(src(), tab() == .id.list$dcc)
        microvhf <- try(tbl_vMicroVHF_Deployed(src()), silent = TRUE)

        validate(
          need(microvhf,
               "Unable to collect vPinniped_Season from the specified database")
        )

        microvhf %>%
          left_join(ps_df_collect(), by = join_by(pinniped_id)) %>%
          mutate(parturition = replace_na(parturition, FALSE)) %>%
          group_by(freq, code) %>%
          mutate(end_date = min(recovery_date, today()+days(1), #pup_mortality_date
                                na.rm = TRUE),
                 .after = recovery_date) %>%
          ungroup()
      })


      ##########################################################################
      # # TODO: Incorporate resights as 'pings', if specified by user
      # dcc_resights <- reactive({
      #   if (input$include_resights) validate()
      # })

      ##########################################################################
      # Join key and dcc data, and finish DCC data processing

      ### All processed DCC data
      dcc_processed <- reactive({
        # if (input$include_resights) validate("resights are not yet incorporated")
        inner_join(microvhf_key(), dcc_formatted(),
                   by = join_by(freq, code), relationship = "many-to-many") %>%
          # group_by(freq, code) %>%
          filter(between(datetime, deployment_date, end_date)) %>%
          # ungroup() %>%
          rename(tag = tag_primary) %>%
          relocate(tag, .before = freq) %>%
          dcc_calc_trips(trip.hours = input$trip_hours) %>%
          mutate_tag_freq_code()
      })

      ### Processed DCC data, filtered for parturition and selected tag/freq/code
      dcc_df <- reactive({
        validate(
          need(input$tag_freq_code, "Please select a 'tag | freq | code'")
        )

        dcc_processed() %>%
          filter(tag_freq_code %in% input$tag_freq_code,
                 parturition) %>%
          mutate(pup_alive = (
            parturition & (is.na(pup_mortality_date) | datetime < pup_mortality_date))) %>%
          select(tag, freq, code, sig, datetime, datetime_prev, time_diff_hr,
                 trip_num_completed, pup_alive, station, tag_freq_code,
                 attendance_study, parturition, pup_mortality, pup_mortality_date,
                 deployment_date, deployment_season, device_type, device_num,
                 pinniped_id, species, sex, cohort, tag_unique_primary)
      })


      ##########################################################################
      # Make output data frames

      ### Pings
      pings <- reactive({
        pings.out <- dcc_df() %>%
          mutate(date = as.Date(datetime),
                 time = format(datetime, "%H:%M:%S")) %>%
          relocate(date, time, .after = datetime) %>%
          select(-datetime_prev)

        validate(
          need(nrow(pings.out) > 0,
               "There are no pings for the selected seal(s)")
        )

        pings.out
      })

      ### Customized output for trips
      trips <- reactive({
        trips.out <- dcc_df() %>%
          filter(time_diff_hr > input$trip_hours) %>%
          filter(pup_alive) %>%
          select(tag, freq, code, trip_num = trip_num_completed, pup_alive,
                 trip_length_hr = time_diff_hr,
                 departure_dt = datetime_prev,
                 arrival_dt = datetime,
                 # capture_location = location,
                 pinniped_id)

        validate(
          need(nrow(trips.out) > 0,
               "There are no trips for the selected filters")
        )

        trips.out
      })

      ### Number of completed trips for seals
      trips_max <- reactive({
        trips() %>%
          group_by(freq, code) %>%
          filter(trip_num == max(trip_num)) %>%
          ungroup() %>%
          select(tag, freq, code, trip_num_completed_max = trip_num)
        # capture_location)
      })

      ### Function to summarise trip means
      # TODO: move to internal functions, or amlrian DCC block (also todo..)
      .summarise_trips_means <- function(.data, ...) {
        .data %>%
          group_by(...) %>%
          summarise(n_trips = n(),
                    trip_length_hr_mean = round(mean(trip_length_hr), 2),
                    trip_length_day_mean = round(trip_length_hr_mean/24, 2),
                    .groups = "drop")
      }

      ### Mean trip lengths
      trip_mean_by_pinniped <- reactive({
        trips() %>%
          filter(trip_num <= input$trip_num_max) %>%
          .summarise_trips_means(tag, freq, code)
      })
      trip_mean_by_trip <- reactive({
        trips() %>%
          filter(trip_num <= input$trip_num_max) %>%
          .summarise_trips_means(trip_num)
      })
      trip_mean_by_all <- reactive({
        trips() %>%
          filter(trip_num <= input$trip_num_max) %>%
          .summarise_trips_means()
      })

      trips_means <- reactive({
        # dcc.trips <- trips() %>%
        #   filter(trip_num <= input$trip_num_max)

        if (input$trips_summary_type == "by_pinniped") {
          trip_mean_by_pinniped()
        } else if (input$trips_summary_type == "by_trip") {
          trip_mean_by_trip()
        } else if (input$trips_summary_type == "by_all") {
          trip_mean_by_all()
        } else {
          .validate_else("trip_lengths_summary_type")
        }
      })


      ##########################################################################
      # Make plots
      ### Plot for trips summary type
      trips_plot <- reactive({
        dcc.trip.max <- max(trips()$trip_num)
        dcc.triplen.max <- max(trips()$trip_length_hr)

        # Make plot depending on trips_completed checkbox
        if (input$trips_max) {
          trips_max() %>%
            # mutate_tag_freq_code() %>%
            ggplot(aes(tag, trip_num_completed_max)) +
            geom_col() +
            scale_y_continuous(breaks = seq(0, dcc.trip.max, by = 1),
                               minor_breaks = NULL) +
            theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
            ggtitle(paste("Number of completed trips:", input$season)) +
            # xlab("Tag | Frequency | Code") +
            xlab("Tag") +
            ylab(NULL)


        } else {
          ggplot(trips(), aes(trip_num, trip_length_hr)) +
            geom_point(aes(color = tag), size = 3) +
            geom_line(aes(group = tag, color = tag)) +
            guides(color = guide_legend(title = "Tag")) +
            scale_x_continuous(breaks = seq_len(dcc.trip.max),
                               minor_breaks = NULL) +
            scale_y_continuous(breaks = seq(0, dcc.triplen.max, by = 20),
                               minor_breaks = seq(0, dcc.triplen.max, by = 10)) +
            expand_limits(y = 0) +
            ggtitle(paste("Number of completed trips:", input$season)) +
            # ggtitle(paste("AFS female trips:",
            #               paste(format(input$dcc_date_range, "%d %b %Y"),
            #                     collapse = " to "))) +
            xlab("Trip number") +
            ylab("Trip length (hours)")
        }
      })


      ### Plot for trip length summary type
      trips_mean_plot <- reactive({
        req(input$trips_summary_type != "by_each")

        # Helper plot function
        trips_mean_plot_help <- function(df, x, y, fill, xlab) {
          ggplot.out <-  df %>%
            ggplot(aes({{x}}, {{y}}, fill = {{fill}})) +
            geom_col() +
            scale_y_continuous(breaks = seq(0,  max(df$trip_length_hr_mean),
                                            by = 20),
                               minor_breaks = NULL) +
            ggtitle(paste("Average trip length through up to",
                          input$trip_num_max, "trips:", input$season)) +
            xlab(xlab) +
            ylab("Trip length (hours)")

          if (xlab == "Tag | Frequency | Code") {
            ggplot.out +
              theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
          } else {
            ggplot.out
          }
        }

        # Make plot depending on trip_lengths_summary_type
        if (input$trips_summary_type == "by_pinniped") {
          trips_mean_plot_help(
            trips_means() %>% mutate_tag_freq_code(),
            tag_freq_code, trip_length_hr_mean, n_trips,
            "Tag | Frequency | Code"
          )
        } else if (input$trips_summary_type == "by_trip") {
          trips_mean_plot_help(
            trips_means(), trip_num, trip_length_hr_mean, n_trips,
            "Trip number"
          )
        } else if (input$trips_summary_type == "by_all") {
          validate("No plot for all trips trip length summary")
        } else {
          .validate_else("trip_lengths_summary_type")
        }
      })


      ### Plot for pings summary type
      pings_plot <- reactive({
        pings.toplot <- if (input$ping_plot_trip) {
          pings()
        } else {
          pings() %>% filter(time_diff_hr < input$trip_hours)
        }

        pings.toplot %>%
          ggplot(aes(datetime)) +
          geom_point(aes(y = time_diff_hr, size = sig, color = pup_alive)) +
          geom_line(aes(y = time_diff_hr, color = pup_alive,
                        group = trip_num_completed )) +
          guides(color = guide_legend(title = "Pup alive?"),
                 size = guide_legend(title = "Signal")) +
          scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "6 hours",
                           date_labels = "%d %b %Y") +
          theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
          ggtitle(paste0(pings()$tag[1],
                         if_else(input$ping_plot_trip,
                                 ": time gap and signal strength (of all)",
                                 ": time gap and signal strength (of non-trips)"))) +
          xlab("Datetime") +
          ylab("Time gap (hours)")
      })


      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Sit rep text
      output$trip_txt <- renderText({
        req(input$trips_summary_type == "by_all")

        t.mean.all <- trip_mean_by_all()
        t.mean.trips <- trip_mean_by_trip()

        dcc_text_format <- function(df) {
          h <- df$trip_length_hr_mean
          n <- df$n_trips
          d <- df$trip_length_day_mean
          glue("{round_logical(h, 1)} hours (n={n}, {round_logical(d, 1)} days)")
        }

        trips.list <- lapply(t.mean.trips$trip_num,  function(i, df) {
          df.curr <- df %>% filter(trip_num == i)
          trip.num.txt <- case_when(
            i == 1 ~ "first",
            i == 2 ~ "second",
            i == 3 ~ "third",
            i == 4 ~ "fourth",
            i == 5 ~ "fifth",
            i == 6 ~ "sixth",
          )
          trip.txt <- dcc_text_format(df.curr)

          glue("{trip.txt} for the {trip.num.txt} trip")
        }, df = t.mean.trips)

        glue(
          "As of ####, the average trip length across all completed trips is ",
          dcc_text_format(t.mean.all), ". ",
          "All seals in the study have now completed their #### trips. ",
          "The {updated} average trip lengths are ",
          paste(trips.list, collapse = ", "), "."
        )
      })


      #-------------------------------------------------------------------------
      ### Table
      tbl_output <- reactive({
        if (input$summary_type == "trips") {
          if (input$trips_summary_type == "by_each") {
            if (input$trips_max) trips_max() else trips()
          } else {
            trips_means()
          }
        } else if (input$summary_type == "pings") {
          pings()
        } else if (input$summary_type == "all") {
          dcc_processed() %>%
            mutate(date = as.Date(datetime),
                   time = format(datetime, "%H:%M:%S")) %>%
            relocate(date, time, .after = datetime)
        } else {
          validate("invalid summary_type - please contact the database manager")
        }
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        if (input$summary_type == "trips") {
          if (input$trips_summary_type == "by_each") {
            trips_plot()
          } else {
            trips_mean_plot()
          }
        } else if (input$summary_type == "pings") {
          pings_plot()
        } else if (input$summary_type == "all") {
          validate("No summary plot for all processed data")
        } else {
          validate("invalid summary_type - please contact the database manager")
        }
        # validate("plot in the works..")
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output))


      ##########################################################################
      ### Download database key
      output$key_download <- downloadHandler(
        filename = function() paste0("database_key", ".csv"),
        content = function(file) write.csv(microvhf_key(), file)
      )
    }
  )
}
