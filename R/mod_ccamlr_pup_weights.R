#' @name shiny_modules
#' @export
mod_ccamlr_pup_weights_ui <- function(id) {
  ns <- NS(id)

  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        mod_filter_season_ui(ns("filter_season")),
        fluidRow(
          column(6, uiOutput(ns("round_num_uiOut"))),
          column(6, uiOutput(ns("sex_uiOut")))
        )
      ),
      box(
        title = "Summary options", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
        helpText("This tab allows you to summarize and visualize CCAMLR Pup Weights data. ",
                 "Select how you wish to summarize this data, ",
                 "and then specify any filters you would like to apply"),
        fluidRow(
          column(6, .summaryTimingUI(ns, c("fs_mult_total", "fs_single", "fs_mult_raw"))), #"fs_facet",
          conditionalPanel(
            condition = "input.summary_timing != 'fs_mult_raw'", ns = ns,
            column(6, radioButtons(ns("summary_type"), .lbl("Summarize by"),
                                   choices = c("Mean weight" = "weight",
                                               "Growth rate" = "metric"),
                                   selected = "weight")),
          )
        ),
        conditionalPanel(
          condition = "input.summary_timing != 'fs_mult_raw'", ns = ns,
          checkboxInput(ns("sex_grp"), "Separate weights by sex", value = TRUE)
        )
      )
    ),
    mod_output_ui(
      ns("out"),
      tags$br(), uiOutput(ns("warning_na_records"))
    )
  )
}



#' @name shiny_modules
#' @export
mod_ccamlr_pup_weights_server <- function(id, src, season.df, tab) {
  .mod_check(src, season.df, tab)

  moduleServer(
    id,
    function(input, output, session) {
      ##########################################################################
      # General

      vals <- reactiveValues(
        warning_na_records = NULL
      )

      ### Get filter_season values
      filter_season <- reactive({
        mod_filter_season_server(
          "filter_season",  reactive(input$summary_timing), season.df
        )
      })


      ##########################################################################
      # RenderUIs

      ### Warning messages
      output$warning_na_records <- renderUI({
        span(req(vals$warning_na_records), style = "color:red;")
      })

      ### Round number
      output$round_num_uiOut<- renderUI({
        selectInput(session$ns("round_num"), .lbl("Pup weight rounds"),
                    choices = sort(unique(req(cpw_df_collect())$round_num)),
                    selected = c(1:4), multiple = TRUE)
      })

      ### Sex
      output$sex_uiOut <- renderUI({
        req(input$sex_grp)
        checkboxGroupInput(session$ns("sex"), .lbl("Sex"),
                           choices = c("F", "M"), selected = c("F", "M"),
                           inline = TRUE)
      })


      ##########################################################################
      ##########################################################################
      ##########################################################################
      # Collect all cpw data - one time run, then all data is collected
      cpw_df_collect <- reactive({
        req(src(), tab() == .id.list$cpw)

        validate(
          need(try(tbl_vCCAMLR_Pup_Weights(src()), silent = TRUE),
               "Unable to find vCCAMLR_Pup_Weights on specified database")
        )

        tbl_vCCAMLR_Pup_Weights(src()) %>%
          arrange(round_date, round_num, pup_num)
      })


      ##########################################################################
      # Filter collected data

      #-------------------------------------------------------------------------
      ### Filter data by species, season/date, and remove NA values
      cpw_df_filter_season <- reactive({
        filter_timing(
          cpw_df_collect(), round_date, filter_season(), input$summary_timing
        )

        # cpw.df.orig <- cpw_df_collect()
        # # Filter by season/date/week num
        # fs <- filter_season()
        #
        # cpw.df <- if (input$summary_timing %in% .summary.timing.multiple) {
        #   cpw.df.orig %>%
        #     filter(season_name %in% !!req(fs$season()))
        # } else if (input$summary_timing %in% .summary.timing.single) {
        #   cpw.df.orig %>%
        #     filter(season_name == !!req(fs$season()),
        #            between(round_date,
        #                    !!req(fs$date_range())[1], !!req(fs$date_range())[2]))
        # } else {
        #   validate("invalid input$summary_timing value")
        # }
        #
        # validate(
        #   need(nrow(cpw.df) > 0,
        #        "There are no data for the given season filter(s)")
        # )
        #
        # cpw.df
      })

      #-------------------------------------------------------------------------
      ### Filter by round and sex
      cpw_df_round_sex <- reactive({
        validate(
          need(input$round_num, "Please select at least one round number"),
          need(input$sex, "Please select at least one sex")
        )
        cpw_df_filter_season() %>%
          filter(round_num %in% input$round_num,
                 sex %in% input$sex)
      })

      #-------------------------------------------------------------------------
      ### Filtered df
      cpw_df_filtered <- reactive({
        vals$warning_na_records <- NULL
        df.orig <- cpw_df_round_sex()

        #----------------------------------------------
        # Filter records for non-NA values, verbosely, as appropriate
        df.nona <- df.orig %>%
          filter(!is.na(mass_kg))

        nrow.diff <- nrow(df.orig) - nrow(df.nona)
        vals$warning_na_records <- if (nrow.diff != 0) {
          paste(
            nrow.diff,
            ifelse(nrow.diff == 1, "row was", "rows were"),
            "removed because of a NULL mass_kg value.",
            "If this is not expected, please tell the database manager."
          )
        } else {
          NULL
        }

        validate(
          need(nrow(df.nona) > 0,
               "No data to process after removing rows with NA mass values")
        )

        df.nona
      })


      ##########################################################################
      # Summarize
      cpw_df <- reactive({
        cpw.grp <- cpw_df_filtered() %>%
          group_by(season_name, round_num, round_date, time_start, time_end)

        if (input$sex_grp) cpw.grp <- cpw.grp %>% group_by(sex, .add = TRUE)

        if (input$summary_type == "weight") {
          cpw.grp %>%
            summarise(mean_mass_kg = round(mean(mass_kg), 2),
                      n_weights = n(),
                      min_mass_kg = min(mass_kg),
                      max_mass_kg = max(mass_kg),
                      mass_std_deviation = round(sd(mass_kg), 2),
                      mass_std_error = round(mass_std_deviation / sqrt(n_weights), 2),
                      locations = paste(unique(location_group), collapse = ", "),
                      .groups = "drop")

        } else if (input$summary_type == "metric") {
          validate("Metric value is not ready yet")

        } else {
          validate("Invlaid input$summary_type value")
        }
      })

      ##########################################################################
      # Outputs

      #-------------------------------------------------------------------------
      ### Sit rep text
      txt_output <- reactive({
        req(input$summary_timing == "fs_single", input$sex_grp)
        df <- tbl_output()

        rounds.txt.list <- lapply(df$round_num,  function(i, df) {
          df.curr <- df %>% filter(round_num == i)
          i.txt <- case_when(
            i == 1 ~ "first",
            i == 2 ~ "second",
            i == 3 ~ "third",
            i == 4 ~ "fourth",
            i == 5 ~ "fifth",
            i == 6 ~ "sixth",
          )

          round.date <- unique(df.curr$round_date)
          validate(
            need(length(round.date) == 1, "Round date is not unique")
          )

          m.abb <- month.abb[month(round.date)]
          d <- day(round.date)

          f.mass <- round_logical(df.curr$mean_mass_kg_female, 1)
          f.n <- df.curr$n_weights_female
          f.min <- round_logical(df.curr$min_mass_kg_female, 1)
          f.max <- round_logical(df.curr$max_mass_kg_female, 1)
          f.sd <- round_logical(df.curr$mass_std_deviation_female, 1)

          m.mass <- round_logical(df.curr$mean_mass_kg_male, 1)
          m.n <- df.curr$n_weights_male
          m.min <- round_logical(df.curr$min_mass_kg_male, 1)
          m.max <- round_logical(df.curr$max_mass_kg_male, 1)
          m.sd <- round_logical(df.curr$mass_std_deviation_male, 1)

          tags$h5(glue(
            "During the {i.txt} round of CCAMLR pup weights, on {m.abb} {d}, ",
            "the average weights were {f.mass} kg ",
            "(n={f.n}, range={f.min}-{f.max}, sd={f.sd}) for females, and ",
            "{m.mass} kg (n={m.n}, range={m.min}-{m.max}, sd={m.sd}) for males."
          ))

          # During the second round of CCAMLR pup weights on 21 January, the
          # average weights were 10.3 kg for females (n=20, range = 7.4 to 12,
          # sd=1.2) and 12.4 kg for males (n=25, range = 9.6 to 16.4, sd=1.7).
        }, df = df)

        tagList(
          tags$strong("Summary text for sit rep:"),
          rounds.txt.list
          # tags$h5(paste(rounds.txt.list, collapse = "\n"))
        )
      })

      output$out_txt <- renderUI({
        txt_output()
      })

      #-------------------------------------------------------------------------
      ### Output table
      tbl_output <- reactive({
        si.dmp <- season.df() %>% select(season_name, date_median_pupping)

        df.out <- if (input$summary_timing == "fs_mult_raw") {
          cpw_df_filtered()

        } else {
          cpw.df <- cpw_df() %>%
            left_join(si.dmp, by = "season_name") %>%
            mutate(days_since_date_median_pupping = as.numeric(
              difftime(round_date, date_median_pupping, units = "days"))) %>%
            select(-date_median_pupping)

          if (input$sex_grp) {
            cpw.df %>%
              mutate(sex = case_when(
                sex == "M" ~ "male",
                sex == "F" ~ "female"
              )) %>%
              pivot_wider(id_cols = season_name:time_end, names_from = sex,
                          values_from = mean_mass_kg:mass_std_error,
                          names_glue = "{.value}_{sex}")
          } else {
            cpw.df
          }
        }
      })


      #-------------------------------------------------------------------------
      ### Output plot
      plot_output <- reactive({
        validate(
          need(input$summary_type != "fs_mult_raw",
               "There is no plot for raw data summary")
        )
        cpw.df <- cpw_df() %>%
          mutate(round_num = factor(round_num, levels = input$round_num))

        ggplot.title <- case_when(
          input$summary_timing == "fs_single" ~
            glue("CCAMLR Pup Weights - {filter_season()$season()}"),
          input$summary_timing == "fs_mult_total" ~ "CCAMLR Pup Weights"
        )

        if (input$sex_grp) {
          shape_guide_legend <- guide_legend(title = "Sex")
        } else {
          cpw.df <- cpw.df %>% mutate(sex = "1")
          shape_guide_legend <- "none"
          ggplot.title <- paste(ggplot.title, "- across sexes")
        }

        ggplot(cpw.df, aes(round_num, mean_mass_kg)) +
          geom_point(aes(color = season_name, shape = sex)) +
          # geom_smooth(method = lm, se = FALSE) +
          geom_line(aes(group = interaction(season_name, sex),
                        color = season_name)) +
          ggtitle(ggplot.title) +
          guides(color = guide_legend(title = "Season name", order = 1),
                 shape = shape_guide_legend) +
          xlab("Pup weight round") +
          ylab("Mass (kg)") +
          ylim(0, NA) +
          expand_limits(x = input$round_num)
      })


      #-------------------------------------------------------------------------
      ### Send off
      observe(mod_output_server("out", tbl_output, plot_output, txt_output))
    }
  )
}
