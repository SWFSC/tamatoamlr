#' tamatoamlr: Analyze and Visualize US AMLR Pinniped Data
#'
#' This package contains \code{\link{tamatoa}}, a Shiny app that connects to the
#' US AMLR Pinnipeds database, and allows users to analyze and visualize data
#' from said database. There are also several stand-alone functions, such as for
#' calculating krill lengths from carapace measurements.
#'
#' @name tamatoamlr-package
#' @aliases tamatoamlr
#' @title US AMLR Program Pinniped data processing and analysis
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @author Karen Snyder \email{ksnyder@@g.hmc.edu}
#'
#' @import amlrian
#' @import dplyr
#' @import ggplot2
#' @import shiny
#'
#' @importFrom DT DTOutput renderDT
#' @importFrom forcats fct
#' @importFrom glue glue
#' @importFrom lubridate day days days_in_month is.Date month today week year
#'   mdy mdy_hms ymd ymd_hms
#' @importFrom odbc odbc dbConnect
#' @importFrom pool dbIsValid dbGetQuery poolClose
#' @importFrom purrr pmap_dbl pmap_lgl set_names
#' @importFrom rlang .data enquo is_bool is_character
#' @importFrom scales hue_pal
#' @importFrom shinybusy add_busy_spinner
#' @importFrom shinydashboard box dashboardBody tabItems tabItem dashboardHeader
#'   dashboardPage dashboardSidebar sidebarMenu menuItem
#' @importFrom shinyjs useShinyjs extendShinyjs js
#' @importFrom stats na.omit var
#' @importFrom stringi stri_escape_unicode
#' @importFrom stringr str_count str_detect str_length str_match str_pad
#'   str_remove_all str_replace_all str_replace_na str_split_i str_sub
#'   str_to_lower str_to_sentence
#' @importFrom tidyselect eval_select
#' @importFrom tidyr complete nest nesting unnest pivot_longer pivot_wider
#'   replace_na
#' @importFrom utils globalVariables read.csv write.csv
#'
#' @keywords package
"_PACKAGE"



# https://github.com/hadley/r-pkgs/issues/828
ignore_unused_imports <- function() {
  dbplyr::sql
}



# https://github.com/r-lib/tidyselect/issues/248
# https://r-pkgs.org/package-within.html#echo-a-working-package
utils::globalVariables(c(
  "where", ".",

  # Commonly used variables
  "observer", "location", "location_group", "beach_id", "species",
  "Beach", "Beaches", "loc_lower", "loc_clean", "week_num", "header_id",
  "capture_id",

  # Plotting
  "count_class", "count_value", "species_lty",

  # season_info
  "season_name", "season_open_date", "season_close_date", "season_days",
  "diet_scat_date", "date_median_pupping", "ts",

  # Phocid census
  "census_phocid_header_id", "census_id", "census_type",
  "census_date_start", "census_date_end",
  "census_date", "time_start", "time_end",
  "ad_female_count", "ad_male_count", "ad_unk_count",
  "juv_female_count", "juv_male_count", "juv_unk_count",
  "pup_live_count", "pup_dead_count",
  "unk_female_count", "unk_male_count", "unk_unk_count",
  "census_notes", "census_created_dt",


  # AFS Cape-wide Pup Census
  "pup_count", "pup_live_count", "pup_dead_count", "date_min",
  "count_loc_mean", "count_loc_var", "count_loc_sd", "study_beach_count",
  "count_mean", "count_var", "count_sd", "count_range", "group",
  "census_date_min", "count_range_perc_diff",

  # AFS Study Beach Census
  "ad_male_count_sum", "adult_male_non_terr_count", "adult_male_terr_count",
  "adult_male_terr_noFem_count", "adult_male_terr_wFem_count",
  "adult_male_unk_count",


  # Misc
  "Code", "Day", "Hr", "ID", "Mass", "Mn", "Sig", "Yr", "action",
  "attendance_study", "body_condition", "box_time",
  "capt_to_release", "capt_to_reunion", "capture_date", "capture_date_rm",
  "capture_datetime", "capture_location", "capture_time", "capture_time_rm",
  "ccamlr_pup_weights_notes", "census_afs_capewide_pup_sort", "cohort",
  "combined_number", "d", "datetime", "datetime_prev", "days_diff",
  "device_inventory_id", "device_num", "device_type", "exclude_count", "freq",
  "frequency", "gas_off", "gas_on", "gas_time", "head", "header_notes", "in_box",
  "location_fctr", "location_lty", "m", "mass_kg", "mass_std_deviation",
  "mass_std_error", "mass_total_kg", "max_dt", "mean_mass_kg", "min_dt", "n_records",
  "n_resights", "n_trips", "n_weights", "name", "notes_tmp", "number_of_captures",
  "parturition", "parturition_date", "pinniped_id", "pinniped_season_id",
  "primary_tag", "pup_alive", "pup_mortality", "pup_mortality_date", "pup_num",
  "pup_total_count", "release_time", "research_program", "resight_date",
  "resight_time", "reunion_time", "round_date", "round_num", "sd", "season_date",
  "season_info_id", "sex", "sig", "station", "std_length_cm", "tag_freq_code", "tag_letter",
  "tag_numeric", "tag_sort", "tag_type", "tagging_date", "tare_check", "tare_kg", "time",
  "time_diff_hr", "trip_length_hr", "trip_length_hr_mean", "trip_num",
  "trip_num_completed", "trip_num_completed_max", "notes",

  "freq_chr", "recovery_date", "deployment_date", "end_date", "tag_primary",
  "tag_type_primary", "tag_unique_primary", "non_amlr_tag_primary",
  "tag_sort_primary", "tag_primary_sort", "col_sort",
  "amlr_tag_primary", "apw_collect", "unk_group_id",
  "deployment_season", "pinniped_sex", "arrival_date", 'twins', "status", "age",
  "on_the_fly_id", "sample_type", "sample_type_group", "age_class",
  "n_individual_animals", "n_adults_juveniles", "n_pups", "ad_male_sum",
  "tag_resights_leopards_id", "id_unique",

  "n_packages", "carapace_save", "collection_date", "collector", "diets_id",
  "fish_type", "krill_type", "squid_type",
  "process_date", "processor", "sample_num", "sample_source", "tag_id",

  "freq_code",

  # vTakes
  "take_date", "individual_id", "individual_identifier", "individual_id_source",
  "n_takes", "table_name", "take_notes", "sample_types", "record_id",
  "created_dt",

  # Pinniped season
  "ISNULL", "location_name", "n_pups_total", "percent_pups",
  "study_beach_season_start_id", "study_beach_season_start_open_date",
  "tr_first_per_season", "first_resight_date"
))

