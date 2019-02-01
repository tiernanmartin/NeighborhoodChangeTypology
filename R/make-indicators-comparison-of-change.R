#' @title Make The Change in Comparison Indicators
#' @description Description
#' @param indicators_by_topic desc
#' @param change_endyears desc
#' @param indicator_type_template desc
#' @return a `tibble`
#' @export
make_indicators_comparison_of_change <- function(indicators_by_topic,
                                       change_endyears,
                                       indicator_type_template){

  # This applies to demographic change and housing market indicators

  # PREPARE DATA --------------------------------------------------------


  ind_type_fields <- indicator_type_template %>%
    dplyr::full_join(indicators_by_topic,
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
                            "ESTIMATE",
                            "MOE"))


  inds <- ind_type_fields %>%
    dplyr::filter(TOPIC %in% c("DEMOGRAPHIC CHANGE")) %>%
    dplyr::filter(!DATE_RANGE_TYPE %in% "one quarter") %>% # drop the quarter spans
    dplyr::mutate(DATE_GROUP_ID_JOIN = stringr::str_c("YEAR_", DATE_GROUP_ID)) %>%
    dplyr::filter(!is.na(GEOGRAPHY_ID))

  inds_drop_source_fields <- inds %>%
    dplyr::select(-SOURCE, -VARIABLE_DESC) # these columns shouldn't be included in the CHANGE indicator

  inds_long <- inds_drop_source_fields %>%
    tidyr::gather(VALUE_TYPE, VALUE, ESTIMATE, MOE)

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
      count(TOPIC,INDICATOR, VARIABLE,DATE_RANGE_TYPE,DATE_GROUP_ID) %>% print(n=Inf)
  }


  change_endyears_wide <-  change_endyears_long %>%
    dplyr::left_join(inds_long, by = c("INDICATOR",
                                       "VARIABLE",
                                       "DATE_GROUP_ID_JOIN")) %>%
    dplyr::filter(! is.na(VALUE_TYPE)) %>% # remove a few records that have NA in many of the metadata fields
    dplyr::select(-dplyr::starts_with("DATE")) %>%
    dplyr::mutate(GROUP_ID = dplyr::group_indices(.,TOPIC, INDICATOR, VARIABLE,CHANGE_RANGE,GEOGRAPHY_ID,MEASURE_TYPE)) %>%
    tidyr::unite("TYPE_ROLE_YEAR", c(VALUE_TYPE, INDICATOR_ROLE)) %>%
    tidyr::spread(TYPE_ROLE_YEAR, VALUE) %>%
    dplyr::select(-GROUP_ID)

  change_demo_housing <- change_endyears_wide %>%
    dplyr::mutate(ESTIMATE_CHANGE_ABSOLUTE = ESTIMATE_END - ESTIMATE_BEGIN,
                  ESTIMATE_CHANGE_RATIO = (ESTIMATE_END/ESTIMATE_BEGIN) - 1, # change in pct
                  ESTIMATE_CHANGE_APPROPRIATE = dplyr::case_when(
                    MEASURE_TYPE %in% "PERCENT" ~ ESTIMATE_CHANGE_ABSOLUTE,
                    MEASURE_TYPE %in% c("COUNT", "MEDIAN", "TOTAL") ~ ESTIMATE_CHANGE_RATIO,
                    TRUE ~ NA_real_
                  )) %>%
    dplyr::mutate(MOE_CHANGE_ABSOLUTE = purrr::pmap_dbl(list(a = MOE_END,  # use pmap to vectorize this call (this works but it should be refactored/clarified at some point)
                                                             b = MOE_BEGIN,
                                                             y = ESTIMATE_END,
                                                             z = ESTIMATE_BEGIN),
                                                        ~ tidycensus::moe_sum(moe = c(..1, ..2),
                                                                              estimate = c(..3, ..4),
                                                                              na.rm = TRUE)),
                  MOE_CHANGE_RATIO = tidycensus::moe_ratio(num = ESTIMATE_CHANGE_ABSOLUTE,
                                                           denom = ESTIMATE_BEGIN,
                                                           moe_num = MOE_END,
                                                           moe_denom = MOE_BEGIN),
                  MOE_CHANGE_APPROPRIATE = dplyr::case_when(
                    MEASURE_TYPE %in% "PERCENT" ~ MOE_CHANGE_ABSOLUTE,
                    MEASURE_TYPE %in% c("COUNT", "MEDIAN", "TOTAL") ~ MOE_CHANGE_RATIO,
                    TRUE ~ NA_real_
                  ))


# CREATE COMPARISON FUNCTION ----------------------------------------------


  get_comparison_fields <- function(data, topic, measure_type){

    # IF MEASURE_TYPE ISN'T PERCENT ---------------------------------

    if(! measure_type %in% c("PERCENT")){

      return(data)
    }


    # COUNTY ------------------------------------------------------------------

    df_county <- data %>%
      dplyr::filter(GEOGRAPHY_TYPE %in% c("county"))


    # IF TOPIC %in% DEMOGRAPHIC CHANGE ---------------------------------------------

    if(topic %in% "DEMOGRAPHIC CHANGE"){

      if(! measure_type %in% c("PERCENT")){

        return(data)
      }


      county_median <- data %>% dplyr::filter(GEOGRAPHY_TYPE %in% "county") %>% dplyr::pull(ESTIMATE_CHANGE_APPROPRIATE)

      df_community_tract <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract", "community")) %>%
        dplyr::mutate(INDICATOR_TYPE = "RELATIVE CHANGE",
                      INDICATOR_TYPE_THRESHOLD_VALUE = county_median,
                      INDICATOR_TYPE_THRESHOLD = "MEDIAN",
                      INDICATOR_TYPE_DESC = "RELATION TO MEDIAN",
                      INDICATOR_TYPE_VALUE = plyr::round_any(ESTIMATE_CHANGE_APPROPRIATE - INDICATOR_TYPE_THRESHOLD_VALUE, accuracy = .001),
                      INDICATOR_TYPE_VALUE_DESC = dplyr::case_when(
                        INDICATOR_TYPE_VALUE>= 0 ~ "GREATER THAN / EQUAL TO MEDIAN",
                        TRUE ~ "LESS THAN MEDIAN"
                      ),
                      INDICATOR_TYPE_MODEL = INDICATOR_TYPE_VALUE_DESC
        )

      df_county_community_tract <- list(df_county, df_community_tract) %>%
        purrr::map_dfr(c)

      return(df_county_community_tract)

    }

    # IF THERE'S A PROBLEM ----------------------------------------------------


    stop("Something went wrong with the tests in this function!")

  }


# CALCULATE COMPARISON OF CHANGE ------------------------------------------

  comparison_of_change_demo_housing <- change_demo_housing %>%
    tidyr::nest(-TOPIC, -INDICATOR, -VARIABLE, -CHANGE_RANGE, -MEASURE_TYPE) %>%
    dplyr::mutate(COMP_FIELDS = purrr::pmap(list("data" = data,
                                                 "topic" = TOPIC,
                                                 "measure_type" = MEASURE_TYPE), get_comparison_fields)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()




  # JOIN DATE_* FIELDS ------------------------------------------------------

 date_group_id_fields <- inds_drop_source_fields %>%
    dplyr::select(-MEASURE_TYPE, -ESTIMATE, -MOE,-dplyr::matches("^INDICATOR_")) %>%
    dplyr::distinct()


  change_endyears_all_fields <- comparison_of_change_demo_housing %>%
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

  indicators_comparison_of_change_ready <- indicator_type_template %>%
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
                            "INDICATOR_TYPE_THRESHOLD_VALUE",
                            "INDICATOR_TYPE_DESC",
                            "INDICATOR_TYPE_VALUE",
                            "INDICATOR_TYPE_VALUE_DESC",
                            "INDICATOR_TYPE_MODEL"))

  indicators_comparison_of_change <- indicators_comparison_of_change_ready

  # RETURN ------------------------------------------------------------------

  return(indicators_comparison_of_change)

}

