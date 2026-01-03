#' Extract data from database
#'
#' Functions used to extract pinniped data from database
#'
#' @name extract
#'
#' @param src a data source; likely a \code{\link[pool]{pool}} object
#'   or some other connection to a database
#'
#' @details
#' These functions return the output of \code{\link[dplyr]{tbl}},
#' likely with at least some selecting or renaming.
#' Functions or modules using these functions will need to call
#' \code{link[dplyr]{collect}} to retrieve the data to R.
#'
#' These functions provide a single place in the package where functions
#' that extract data from a database are located, making it easier to update
#' said functions if some aspect of the database structure changes.
#'
#' @export
tbl_vCensus_Phocid <- function(src) {
  tbl(src, "vCensus_Phocid") %>%
    select(season_name, census_phocid_header_id, census_id,
           census_type, observer,
           census_date_start, census_date_end,
           census_date, time_start, time_end,
           location, location_group, beach_id, species,
           ad_female_count, ad_male_count, ad_unk_count,
           juv_female_count, juv_male_count, juv_unk_count,
           pup_live_count, pup_dead_count,
           unk_female_count, unk_male_count, unk_unk_count,
           census_notes, census_created_dt) %>%
    collect() %>%
    mutate(species = str_to_sentence(species))
}

#' @name extract
#' @export
tbl_vCensus_AFS_Study_Beach <- function(src) {
  tbl(src, "vCensus_AFS_Study_Beach") %>%
    select(season_name, census_id, census_type,
           observer, census_date, time_start, time_end,
           location, location_group, beach_id, species,
           ad_female_count, pup_live_count, pup_dead_count,
           ad_male_sum,
           juv_female_count, juv_male_count, juv_unk_count,
           ad_male_count, ad_unk_count,
           adult_male_terr_count,
           adult_male_terr_wFem_count, adult_male_terr_noFem_count,
           adult_male_non_terr_count, adult_male_unk_count,
           census_notes, census_created_dt) %>%
    collect() %>%
    mutate(species = str_to_sentence(species))
}

#' @name extract
#' @export
tbl_vCensus_AFS_SAM <- function(src) {
  tbl(src, "vCensus_AFS_SAM") %>%
    select(season_name, census_id, census_type,
           observer, census_date, time_start, time_end,
           location, location_group, beach_id, species,
           ad_male_count, juv_male_count, juv_unk_count,
           census_notes, census_created_dt) %>%
    collect() %>%
    mutate(species = str_to_sentence(species))
}

#' @name extract
#' @export
tbl_vCensus_AFS_Capewide_Pup <- function(src) {
  tbl(src, "vCensus_AFS_Capewide_Pup") %>%
    select(season_name, census_id, census_type,
           observer, census_date, time_start, time_end,
           location, beach_id, census_afs_capewide_pup_sort, species,
           pup_count, pup_live_count, pup_dead_count, exclude_count,
           research_program, census_notes, census_created_dt) %>%
    collect() %>%
    mutate(species = str_to_sentence(species))
}

#' @name extract
#' @export
tbl_vCaptures <- function(src) {
  tbl(src, "vCaptures") %>%
    select(season_name, capture_id, pinniped_id, species, sex, cohort,
           everything()) %>%
    collect()
}

#' @name extract
#' @export
tbl_vCaptures_Drugs <- function(src) {
  tbl(src, "vCaptures_Drugs") %>%
    # select(season_name, capture_id, capture_date, capture_time,
    #        everything()) %>%
    arrange(capture_date, capture_time) %>%
    collect()
}

#' @name extract
#' @export
tbl_vCaptures_Samples <- function(src) {
  tbl(src, "vCaptures_Samples") %>%
    # select(season_name, capture_id, capture_date, capture_time,
    #        everything()) %>%
    arrange(capture_date, capture_time) %>%
    collect()
}

#' @name extract
#' @export
tbl_vCCAMLR_Pup_Weights <- function(src) {
  tbl(src, "vCCAMLR_Pup_Weights") %>%
    select(season_name, round_num, round_date, time_start, time_end,
           location, location_group, beach_id,
           pup_num, sex, mass_total_kg, tare_kg, mass_kg, tare_check,
           # pinniped_id, tag_unqiue_mother_primary, attendance_pup_id,
           notes, header_notes, research_program) %>%
    collect()
}

#  #' @name extract
#  #' @export
#  tbl_vBeaches_capewide <- function(src) {
#   tbl(src, "vBeaches") %>%
#     filter(!is.na(census_afs_capewide_pup_sort)) %>%
#     arrange(census_afs_capewide_pup_sort) %>%
#     select(beach_id, location = name, census_afs_capewide_pup_sort) %>%
#     collect()
# }

#' @name extract
#' @export
tbl_vTag_Resights <- function(src) {
  tbl(src, "vTag_Resights") %>%
    arrange(species)
  # collect() %>%
  # tag_sort(tag.sort = TRUE, tag.sort.primary = TRUE) %>%
  # mutate(species = str_to_sentence(species),
  #        species = factor(species, levels = sort(unique(species))))
}

#' @name extract
#' @export
tbl_vTag_Resights_First_Per_Season <- function(src) {
  tbl(src, "vTag_Resights_First_Per_Season") %>%
    distinct(season_name, season_info_id, pinniped_id,
             resight_date, location_group)
  # NOTE: This will silently remove some records.
  # 20240323: None of the removed records affect pup mortality calculations
}

#' @name extract
#' @export
tbl_vTag_Resights_Leopards <- function(src) {
  tbl(src, "vTag_Resights_Leopards") %>%
    rename(tag_resight_id = tag_resights_leopards_id) %>%
    arrange(species)
  # collect() %>%
  # tag_sort(tag.sort = TRUE, tag.sort.primary = TRUE) %>%
  # mutate(species = str_to_sentence(species),
  #        species = factor(species, levels = sort(unique(species))))
}

#' @name extract
#' @export
tbl_vMicroVHF_Deployed <- function(src) {
  tbl(src, "vMicroVHF_Deployed") %>%
    collect() %>%
    rename(freq = frequency) %>%
    mutate(code = as.integer(str_sub(device_num, 3)),
           # TODO: update freq in database???
           freq = if_else(freq == 164.513, 164.514, freq))
}

#' @name extract
#' @export
tbl_vPinniped_Season <- function(src) {
  tbl(src, "vPinniped_Season") %>%
    select(pinniped_season_id, pinniped_id, season_name, season_info_id,
           tag_primary, tag_type_primary, cohort, species, sex, attendance_study,
           arrival_date, parturition, parturition_date, twins,
           pup_mortality, pup_mortality_date, notes)
}


#' @name extract
#' @export
tbl_vTakes <- function(src) {
  tbl(src, "vTakes") %>%
    select(season_name, table_name, take_date, species, age_class, sex,
           individual_identifier, location_group, take_notes, sample_types,
           individual_id, individual_id_source,
           record_id, season_info_id, beach_id, created_dt)
}


#' @name extract
#' @export
tbl_vDiets <- function(src) {
  tbl(src, "vDiets") %>%
    select(season_name, diet_scat_date,
           sample_type, sample_num, species, sex, location,
           collection_date, collector, process_date, processor,
           krill_type, fish_type, squid_type, carapace_save,
           tag_id, tag, tag_type, pinniped_id, notes,
           diets_id, season_info_id, beach_id, created_dt)
}
