#' @title Make The Change in Comparison Indicators
#' @description Description
#' @param indicators_by_topic desc
#' @param change_endyears desc
#' @param indicator_topic_template desc
#' @return a `tibble`
#' @export
make_indicators_change_in_comparison <- function(indicators_comparison,
                                                 change_endyears,
                                                 indicator_type_template){

  # This applies only to housing market indicators

  # PREPARE DATA --------------------------------------------------------

  inds_housing <- indicators_comparison %>%
    dplyr::filter(TOPIC %in% "HOUSING MARKET")

  inds <- inds_housing %>%
    dplyr::filter(!DATE_RANGE_TYPE %in% "one quarter") %>% # drop the quarter spans
    dplyr::mutate(DATE_GROUP_ID_JOIN = stringr::str_c("YEAR_", DATE_GROUP_ID)) %>%
    dplyr::filter(!is.na(GEOGRAPHY_ID))

  inds_drop_source_fields <- inds %>%
    dplyr::select(-SOURCE, -VARIABLE_DESC) # these columns shouldn't be included in the CHANGE indicator

  inds_long <- inds_drop_source_fields %>%
    tidyr::gather(VALUE_TYPE, VALUE, ESTIMATE, MOE, INDICATOR_TYPE_THRESHOLD_VALUE, INDICATOR_TYPE_VALUE, INDICATOR_TYPE_VALUE_DESC)

  change_endyears_long <- change_endyears %>%
    dplyr::mutate(CHANGE_RANGE = as.character(glue::glue("YEAR_{BEGIN}_TO_YEAR_{END}"))) %>%
    tidyr::gather(INDICATOR_ROLE, DATE_GROUP_ID_JOIN, BEGIN, END) %>%
    dplyr::mutate(DATE_GROUP_ID_JOIN = stringr::str_c("YEAR_", DATE_GROUP_ID_JOIN))

  # note: the left_join() below removes many unused DATE_GROUP_ID records
  # use the function below to check these

  check_unused_change_years <- function(){
    anti_join(inds_long, change_endyears_long, by = c("INDICATOR",
                                                      "VARIABLE",
                                                      "DATE_GROUP_ID_JOIN")) %>%
      count(INDICATOR, VARIABLE,DATE_RANGE_TYPE,DATE_GROUP_ID) %>% print(n=Inf)
  }


  change_endyears_wide <-  change_endyears_long %>%
    dplyr::left_join(inds_long, by = c("INDICATOR", # switched from left_ to inner_
                                       "VARIABLE",
                                       "DATE_GROUP_ID_JOIN")) %>%
    dplyr::filter(! is.na(VALUE_TYPE)) %>% # remove a few records that have NA in many of the metadata fields
    dplyr::select(-dplyr::starts_with("DATE")) %>%
    dplyr::mutate(GROUP_ID = dplyr::group_indices(.,TOPIC, INDICATOR, VARIABLE,CHANGE_RANGE,GEOGRAPHY_ID,MEASURE_TYPE)) %>%
    tidyr::unite("TYPE_ROLE_YEAR", c(VALUE_TYPE, INDICATOR_ROLE)) %>%
    tidyr::spread(TYPE_ROLE_YEAR, VALUE) %>%
    dplyr::select(-GROUP_ID)

  change_endyears_wide_change <- change_endyears_wide %>%
    dplyr::mutate(INDICATOR_TYPE = "CHANGE",
                  INDICATOR_TYPE_VALUE_CHANGE = stringr::str_c(INDICATOR_TYPE_VALUE_DESC_BEGIN," -> ",INDICATOR_TYPE_VALUE_DESC_END),
                  INDICATOR_TYPE_MODEL = INDICATOR_TYPE_VALUE_CHANGE)


  # JOIN DATE_* FIELDS ------------------------------------------------------

  date_group_id_fields <- inds_drop_source_fields %>%
    dplyr::select(-MEASURE_TYPE, -ESTIMATE, -MOE,-dplyr::matches("^INDICATOR_")) %>%
    dplyr::distinct()


  change_endyears_all_fields <- change_endyears_wide_change %>%
    dplyr::mutate(RNUM = dplyr::row_number(),
                  DATE_GROUP_ID = stringr::str_remove_all(CHANGE_RANGE, "TO_")
    ) %>%
    tidyr::separate(DATE_GROUP_ID, into = c("DATE_GROUP_ID_BEGIN", "DATE_GROUP_ID_END"),sep = "_(?=YEAR)") %>%
    tidyr::gather(DATE_GROUP_ID_ROLE, DATE_GROUP_ID_JOIN, DATE_GROUP_ID_BEGIN, DATE_GROUP_ID_END) %>%
    dplyr::left_join(date_group_id_fields, by = c("INDICATOR", "VARIABLE", "GEOGRAPHY_ID", "GEOGRAPHY_ID_TYPE", "GEOGRAPHY_NAME", "GEOGRAPHY_TYPE", "TOPIC", "DATE_GROUP_ID_JOIN")) %>%
    dplyr::mutate(DATE_GROUP_ID_ROLE = stringr::str_extract(DATE_GROUP_ID_ROLE,"BEGIN$|END$")) %>%
    tidyr::gather(DATE_FIELD_TYPE, DATE_FIELD_VAL, DATE_GROUP_ID, DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE) %>%
    tidyr::unite("ROLE_DATE_FIELD_TYPE", c(DATE_GROUP_ID_ROLE,DATE_FIELD_TYPE)) %>%
    dplyr::select(-DATE_GROUP_ID_JOIN) %>% # this messess up the spread()
    tidyr::spread(ROLE_DATE_FIELD_TYPE,DATE_FIELD_VAL) %>%
    dplyr::mutate(DATE_GROUP_ID = END_DATE_GROUP_ID,
                  DATE_BEGIN = BEGIN_DATE_BEGIN,
                  DATE_END = END_DATE_END,
                  DATE_RANGE = stringr::str_remove_all(stringr::str_c(DATE_BEGIN,DATE_END),"\\-"),
                  DATE_RANGE_TYPE = stringr::str_c("change (",BEGIN_DATE_RANGE_TYPE, " to ",END_DATE_RANGE_TYPE,")")) %>%
    dplyr::select(-dplyr::starts_with("BEGIN"),-dplyr::starts_with("END"), -CHANGE_RANGE, -RNUM)



  # CREATE SOURCE AND VARIABLE_DESC ----------------------------------------------------
  change_endyears_var_desc <- change_endyears_all_fields %>%
    dplyr::mutate(SOURCE = "MULTIPLE",
                  VARIABLE_DESC = stringr::str_c(MEASURE_TYPE, VARIABLE, sep = "_"))

  # REFORMAT ----------------------------------------------------------------

  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicators_change_in_comparison_ready <- indicator_type_template %>%
    dplyr::full_join(change_endyears_var_desc,
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
                            "TOPIC",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "MEASURE_TYPE",
                            "INDICATOR_TYPE",
                            "INDICATOR_TYPE_THRESHOLD",
                            "INDICATOR_TYPE_DESC",
                            "INDICATOR_TYPE_MODEL"))

  indicators_change_in_comparison <- indicators_change_in_comparison_ready

  # RETURN ------------------------------------------------------------------

  return(indicators_change_in_comparison)

}

