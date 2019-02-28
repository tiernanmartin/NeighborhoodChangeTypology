#' @title Make The Change in Comparison Indicators
#' @description Description
#' @param indicators_comparison desc
#' @param change_dategroupid_long desc
#' @param indicator_value_template desc
#' @return a `tibble`
#' @export
make_indicators_change_in_comparison <- function(indicators_comparison,
                                                 change_dategroupid_long,
                                                 indicator_value_template){



  # NOTE --------------------------------------------------------------------


  # This applies only to housing market indicators

  # PREPARE DATA --------------------------------------------------------

  inds_housing <- indicators_comparison %>%
    dplyr::rename(DATE_GROUP_ID_JOIN = DATE_GROUP_ID) %>%
    dplyr::filter(DIMENSION %in% "HOUSING_MARKET") %>%
    dplyr::select(-SOURCE, -VARIABLE_DESC) %>%    # these columns shouldn't be included in the CHANGE indicator
    drop_na_cols() # drop the empty columns

  inds_housing_long <- inds_housing %>%
    tidyr::gather(VALUE_TYPE, VALUE, dplyr::matches("ESTIMATE|MOE|RELATIVE"))


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
    dplyr::select(-DATE_GROUP_ID_JOIN, -DATE_BEGIN, -DATE_END, -DATE_RANGE, -DATE_RANGE_TYPE) %>%
    dplyr::mutate(DATE_TYPE = stringr::str_extract(DATE_TYPE, "BEGIN|END")) %>%
    # GROUP_ID in preparation for spread()
    dplyr::mutate(GROUP_ID = dplyr::group_indices(.,DIMENSION, INDICATOR, VARIABLE, DATE_GROUP_ID, GEOGRAPHY_ID, MEASURE_TYPE)) %>%
    tidyr::unite("TYPE_ROLE_YEAR", c(VALUE_TYPE, DATE_TYPE)) %>%
    tidyr::spread(TYPE_ROLE_YEAR, VALUE) %>%
    dplyr::select(-GROUP_ID)


  change_dategroupid_wide_change <- inds_wide %>%
    dplyr::mutate(RELATIVE_CHANGE_DESC = stringr::str_c(RELATIVE_DESC_BEGIN," -> ",RELATIVE_DESC_END),
                  RELATIVE_CHANGE_LGL = dplyr::case_when(
                    is.na(RELATIVE_CHANGE_DESC) ~ NA, # impacts counties and communities (only on some inds)
                    TRUE ~ RELATIVE_CHANGE_DESC %in% c("LOW/MED -> HIGH")
                  )
    )


  # VISUALIZE DATA ----------------------------------------------------------

  check_change_dategroupid_wide_change_na <- function(){

    # check the NA's first
    change_dategroupid_wide_change %>%
      count(is.na(RELATIVE_CHANGE_DESC), MEASURE_TYPE) %>% print(n=Inf)


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
    dplyr::select(-MEASURE_TYPE, -dplyr::matches("ESTIMATE|MOE|RELATIVE")) %>%
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



  # CONVERT COLUMNS BACK TO THEIR ORIGINAL CLASSES --------------------------

  change_dategroupid_classes <- change_dategroupid_all_fields %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("ESTIMATE|MOE")),as.double) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("DESC")),as.character) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("LGL")),as.logical) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("RELATIVE_BEGIN|RELATIVE_END|RELATIVE_THRESHOLD_BEGIN|RELATIVE_THRESHOLD_END")), as.double)

  # CREATE SOURCE AND VARIABLE_DESC ----------------------------------------------------
  change_dategroupid_var_desc <- change_dategroupid_classes %>%
    dplyr::mutate(SOURCE = "MULTIPLE",
                  VARIABLE_DESC = stringr::str_c(MEASURE_TYPE, VARIABLE, sep = "_"))


  indicators_change_in_comparison <- change_dategroupid_var_desc


  # NOTES -------------------------------------------------------------------

  # Fields like INDICATOR_TYPE_THRESHOLD_VALUE or INDICATOR_TYPE_VALUE are NA;
  # this is because those values are stored in *_BEGIN or *_END fields

  # RETURN ------------------------------------------------------------------

  return(indicators_change_in_comparison)

}

