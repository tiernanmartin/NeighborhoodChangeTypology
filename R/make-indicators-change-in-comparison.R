#' @title Make The Change in Comparison Indicators
#' @description Description
#' @param indicators_by_dimension desc
#' @param change_dategroupid_long desc
#' @param indicator_dimension_template desc
#' @return a `tibble`
#' @export
make_indicators_change_in_comparison <- function(indicators_comparison,
                                                 change_dategroupid_long,
                                                 indicator_type_template){



  # NOTE --------------------------------------------------------------------


  # This applies only to housing market indicators

  # PREPARE DATA --------------------------------------------------------

  inds_housing <- indicators_comparison %>%
    dplyr::rename(DATE_GROUP_ID_JOIN = DATE_GROUP_ID) %>%
    dplyr::filter(DIMENSION %in% "HOUSING_MARKET") %>%
    dplyr::select(-SOURCE, -VARIABLE_DESC)   # these columns shouldn't be included in the CHANGE indicator


  inds_housing_long <- inds_housing %>%
    tidyr::gather(VALUE_TYPE, VALUE, c(ESTIMATE, MOE, INDICATOR_TYPE_THRESHOLD_VALUE, INDICATOR_TYPE_VALUE, INDICATOR_TYPE_VALUE_DESC))


  check_inds_housing_long <- function(){
    # look at the long data structure (value-type fields are all stored in the VALUE field, which coerces them to character)
    inds_housing_long %>% count(VALUE_TYPE)
  }


  # JOIN + SPREAD DATA ------------------------------------------------------

  inds_housing_change_dategroupid_join <- change_dategroupid_long %>%
    dplyr::left_join(inds_housing_long,
                     by = c("DIMENSION",
                            "INDICATOR",
                            "VARIABLE",
                            "DATE_GROUP_ID_JOIN")) %>%
    dplyr::filter(DIMENSION %in% "HOUSING_MARKET") # Change in comparison only applies to HOUSING_MARKET -related indicators


  inds_wide <- inds_housing_change_dategroupid_join %>%
    #drop fields that will impede spread()
    dplyr::select(-DATE_GROUP_ID_JOIN, -DATE_BEGIN, -DATE_END, -DATE_RANGE, -DATE_RANGE_TYPE, -INDICATOR_TYPE_MODEL) %>%
    dplyr::mutate(DATE_TYPE = stringr::str_extract(DATE_TYPE, "BEGIN|END")) %>%
    # GROUP_ID in preparation for spread()
    dplyr::mutate(GROUP_ID = dplyr::group_indices(.,DIMENSION, INDICATOR, VARIABLE, DATE_GROUP_ID, GEOGRAPHY_ID, MEASURE_TYPE)) %>%
    tidyr::unite("TYPE_ROLE_YEAR", c(VALUE_TYPE, DATE_TYPE)) %>%
    tidyr::spread(TYPE_ROLE_YEAR, VALUE) %>%
    dplyr::select(-GROUP_ID)


  change_dategroupid_wide_change <- inds_wide %>%
    dplyr::mutate(INDICATOR_TYPE = "RELATIVE_CHANGE",
                  INDICATOR_TYPE_VALUE_CHANGE = stringr::str_c(INDICATOR_TYPE_VALUE_DESC_BEGIN," -> ",INDICATOR_TYPE_VALUE_DESC_END),
                  INDICATOR_TYPE_MODEL = INDICATOR_TYPE_VALUE_CHANGE)


  # VISUALIZE DATA ----------------------------------------------------------

  check_change_dategroupid_wide_change_na <- function(){

    # check the NA's first
    change_dategroupid_wide_change %>%
      filter(is.na(INDICATOR_TYPE_MODEL)) %>%
      count(GEOGRAPHY_TYPE,GEOGRAPHY_ID) %>% print(n=Inf)

  }

  view_change_dategroupid_wide_change_by_dategroupid <- function(){


    # check the change types (INDICATOR_TYPE_MODEL)
    change_dategroupid_wide_change %>%
      filter(! is.na(INDICATOR_TYPE_MODEL)) %>%
      count(DATE_GROUP_ID, INDICATOR, VARIABLE, INDICATOR_TYPE_MODEL) %>% View()

  }

  view_change_dategroupid_wide_change_by_ind <- function(){

    # check the change types (INDICATOR_TYPE_MODEL)
    change_dategroupid_wide_change %>%
      filter(! is.na(INDICATOR_TYPE_MODEL)) %>%
      count(INDICATOR, VARIABLE, DATE_GROUP_ID, INDICATOR_TYPE_MODEL) %>% View()
  }




  # JOIN DATE_* FIELDS ------------------------------------------------------

  date_group_id_fields <- inds_housing %>%
    dplyr::select(-MEASURE_TYPE, -ESTIMATE, -MOE,-dplyr::matches("^INDICATOR_")) %>%
    dplyr::distinct()


  change_dategroupid_all_fields <- change_dategroupid_wide_change %>%
    dplyr::mutate(DATE_GROUP_ID_SEPARATE = DATE_GROUP_ID,
                  RNUM = dplyr::row_number()) %>%
    tidyr::separate(DATE_GROUP_ID_SEPARATE, into = c("BEGIN_DATE_GROUP_ID", "END_DATE_GROUP_ID"),sep = "_TO_") %>%
    tidyr::gather(DATE_TYPE, DATE_GROUP_ID_JOIN, c(BEGIN_DATE_GROUP_ID, END_DATE_GROUP_ID)) %>%
    dplyr::left_join(date_group_id_fields,
                     by = c("DIMENSION",
                            "INDICATOR",
                            "VARIABLE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID_JOIN")) %>%
    dplyr::mutate(DATE_TYPE = stringr::str_extract(DATE_TYPE,"^BEGIN|^END")) %>%
    dplyr::rename(DATE_ROLE = DATE_TYPE) %>%
    tidyr::gather(DATE_FIELD_TYPE, DATE_FIELD_VAL, DATE_GROUP_ID, DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE) %>%
    tidyr::unite("ROLE_DATE_FIELD_TYPE", c(DATE_ROLE, DATE_FIELD_TYPE)) %>%
    dplyr::select(-DATE_GROUP_ID_JOIN) %>% # this messess up the spread()
    tidyr::spread(ROLE_DATE_FIELD_TYPE,DATE_FIELD_VAL) %>%
    dplyr::mutate(DATE_GROUP_ID = END_DATE_GROUP_ID,
                  DATE_BEGIN = BEGIN_DATE_BEGIN,
                  DATE_END = END_DATE_END,
                  DATE_RANGE = stringr::str_remove_all(stringr::str_c(DATE_BEGIN,DATE_END),"\\-"),
                  DATE_RANGE_TYPE = stringr::str_c("change (",BEGIN_DATE_RANGE_TYPE, " to ",END_DATE_RANGE_TYPE,")")) %>%
    dplyr::select(-dplyr::starts_with("BEGIN"),
                  -dplyr::starts_with("END"),
                  -RNUM)



  # CREATE SOURCE AND VARIABLE_DESC ----------------------------------------------------
  change_dategroupid_var_desc <- change_dategroupid_all_fields %>%
    dplyr::mutate(SOURCE = "MULTIPLE",
                  VARIABLE_DESC = stringr::str_c(MEASURE_TYPE, VARIABLE, sep = "_"))

  # REFORMAT ----------------------------------------------------------------

  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicators_change_in_comparison_ready <- indicator_type_template %>%
    dplyr::full_join(change_dategroupid_var_desc,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "DIMENSION",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "MEASURE_TYPE",
                            "INDICATOR_TYPE",
                            "INDICATOR_TYPE_THRESHOLD",
                            "INDICATOR_TYPE_DESC",
                            "INDICATOR_TYPE_MODEL")) %>%
    dplyr::select(dplyr::starts_with("SOURCE"),
                  dplyr::starts_with("GEOGRAPHY"),
                  dplyr::starts_with("DATE"),
                  DIMENSION,
                  INDICATOR,
                  dplyr::starts_with("VARIABLE"),
                  MEASURE_TYPE,
                  dplyr::starts_with("ESTIMATE"),
                  dplyr::starts_with("MOE"),
                  dplyr::starts_with("INDICATOR"),
                  dplyr::everything()) %>%
    # fields that were gather()'ed need to be coerced back to numeric
    dplyr::mutate_at(dplyr::vars(dplyr::matches("ESTIMATE|VALUE_BEGIN|VALUE_END")),as.numeric)

  indicators_change_in_comparison <- indicators_change_in_comparison_ready


  # NOTES -------------------------------------------------------------------

  # Fields like INDICATOR_TYPE_THRESHOLD_VALUE or INDICATOR_TYPE_VALUE are NA;
  # this is because those values are stored in *_BEGIN or *_END fields

  # RETURN ------------------------------------------------------------------

  return(indicators_change_in_comparison)

}

