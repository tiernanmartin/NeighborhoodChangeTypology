#' @title Make The Change in Comparison Indicators
#' @description Description
#' @param indicators_by_dimension desc
#' @param model_table_production desc
#' @param change_dategroupid_long desc
#' @param indicator_type_template desc
#' @return a `tibble`
#' @export
make_indicators_comparison_of_change <- function(indicators_by_dimension,
                                                 model_table_production,
                                                 change_dategroupid_long,
                                                 indicator_type_template){


  # NOTE --------------------------------------------------------------------

  # This applies to demographic change and housing market indicators

  # CREATE THE FILTER-JOIN OBJECT -------------------------------------------

  inds_table_filter_join <- model_table_production %>%
    dplyr::select(DIMENSION, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>%
    dplyr::distinct() %>%
    dplyr::arrange(DIMENSION, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID)



  # PREPARE DATA ------------------------------------------------------------

  ind_type_fields <- indicator_type_template %>%
    dplyr::full_join(indicators_by_dimension,
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
                            "ESTIMATE",
                            "MOE"))

  inds_in_models <-  ind_type_fields %>%
    dplyr::semi_join(inds_table_filter_join,  # only include the indicators that are used in the models
                     by = c("DIMENSION",
                            "INDICATOR",
                            "VARIABLE",
                            "MEASURE_TYPE",
                            "DATE_GROUP_ID"))

  inds_demo_housing <- inds_in_models %>%
    dplyr::rename(DATE_GROUP_ID_JOIN = DATE_GROUP_ID) %>%
    dplyr::filter(DIMENSION %in% c("DEMOGRAPHIC_CHANGE", "HOUSING_MARKET")) %>%
    dplyr::filter(! is.na(GEOGRAPHY_ID)) %>%  # for some unknown reason there are NA GEOGRAPHY_IDs in the ASSESSOR rows
    dplyr::select(-SOURCE, -VARIABLE_DESC)  # these columns shouldn't be included in the CHANGE indicator


  inds_demo_housing_long <- inds_demo_housing %>%
    tidyr::gather(VALUE_TYPE, VALUE, c(ESTIMATE, MOE, INDICATOR_TYPE_THRESHOLD_VALUE, INDICATOR_TYPE_VALUE, INDICATOR_TYPE_VALUE_DESC))

  # JOIN + SPREAD -----------------------------------------------------------

  inds_demo_housing_dategroupid_join <- change_dategroupid_long %>%
    dplyr::left_join(inds_demo_housing_long,
                     by = c("DIMENSION",
                            "INDICATOR",
                            "VARIABLE",
                            "DATE_GROUP_ID_JOIN"))


  inds_wide <- inds_demo_housing_dategroupid_join %>%
    #drop fields that will impede spread()
    dplyr::select(-DATE_GROUP_ID_JOIN, -DATE_BEGIN, -DATE_END, -DATE_RANGE, -DATE_RANGE_TYPE, -INDICATOR_TYPE_MODEL) %>%
    dplyr::mutate(DATE_TYPE = stringr::str_extract(DATE_TYPE, "BEGIN|END")) %>%
    # GROUP_ID in preparation for spread()
    dplyr::mutate(GROUP_ID = dplyr::group_indices(.,DIMENSION, INDICATOR, VARIABLE, DATE_GROUP_ID, GEOGRAPHY_ID, MEASURE_TYPE)) %>%
    tidyr::unite("TYPE_ROLE_YEAR", c(VALUE_TYPE, DATE_TYPE)) %>%
    tidyr::spread(TYPE_ROLE_YEAR, VALUE) %>%
    dplyr::select(-GROUP_ID,
                  -dplyr::matches("^INDICATOR.+BEGIN$"), # starts with INDICATOR and ends with BEGIN
                  -dplyr::matches("^INDICATOR.+END$"), # starts with INDICATOR and ends with BEGIN
                  ) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("ESTIMATE|MOE|VALUE_BEGIN|VALUE_END")),as.numeric)



  # CALCULATE CHANGE --------------------------------------------------------

  change_demo_housing <- inds_wide %>%
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


  get_comparison_fields <- function(data, dimension, measure_type){

    # IF MEASURE_TYPE ISN'T PERCENT OR MEDIAN ---------------------------------

    if(! measure_type %in% c("PERCENT", "MEDIAN")){

      return(data)
    }


    # COUNTY ------------------------------------------------------------------

    df_county <- data %>%
      dplyr::filter(GEOGRAPHY_TYPE %in% c("county"))


    # IF DIMENSION %in% DEMOGRAPHIC CHANGE ---------------------------------------------

    if(dimension %in% "DEMOGRAPHIC_CHANGE"){

      # Note: DEMOGRAPHIC_CHANGE contains mostly PERCENT indicators but there is
      # one MEDIAN indicator (INCOME)

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

    # IF DIMENSION %in% HOUSING MARKET --------------------------------------------

    if(dimension %in% "HOUSING_MARKET"){

      get_q4_lower <- function(x) {

        q4_lower <- stats::quantile(x,probs = c(0,.6,1), na.rm = TRUE)[[2]]

        q4_lower_rounded <- plyr::round_any(q4_lower, accuracy = .0001)

        return(q4_lower_rounded)
      }


      county_q4_lower <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% "tract") %>%
        dplyr::pull("ESTIMATE_CHANGE_APPROPRIATE") %>% get_q4_lower()

      df_tract <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract")) %>%
        dplyr::mutate(INDICATOR_TYPE_THRESHOLD_VALUE = county_q4_lower) %>% # create the threshold value
        dplyr::mutate(
          INDICATOR_TYPE = "RELATIVE CHANGE",
          INDICATOR_TYPE_DESC = "QUINTILE",
          INDICATOR_TYPE_VALUE = as.double(dplyr::ntile(ESTIMATE_CHANGE_APPROPRIATE, n = 5)), # should be double not integer
          INDICATOR_TYPE_VALUE_DESC = dplyr::case_when(
            INDICATOR_TYPE_VALUE <= 3 ~ "LOW/MED",
            TRUE ~ "HIGH"
          ),
          INDICATOR_TYPE_THRESHOLD_VALUE,
          INDICATOR_TYPE_THRESHOLD = "Q4 LOWER BOUND",
          INDICATOR_TYPE_MODEL = INDICATOR_TYPE_VALUE_DESC
        )

      df_community <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("community")) %>%
        dplyr::mutate(INDICATOR_TYPE_THRESHOLD_VALUE = county_q4_lower) %>% # create the threshold value
        dplyr::mutate(
          INDICATOR_TYPE = "RELATIVE CHANGE",
          INDICATOR_TYPE_DESC = "QUINTILE",
          INDICATOR_TYPE_VALUE = NA_real_, # should be double not integer # also, don't calculate the quintile because the data only contain the "community" aggregations (not the tracts)
          INDICATOR_TYPE_VALUE_DESC = dplyr::case_when(
            ESTIMATE_CHANGE_APPROPRIATE <= county_q4_lower ~ "LOW/MED",
            TRUE ~ "HIGH"
          ),
          INDICATOR_TYPE_THRESHOLD_VALUE,
          INDICATOR_TYPE_THRESHOLD = "Q4 LOWER BOUND",
          INDICATOR_TYPE_MODEL = INDICATOR_TYPE_VALUE_DESC
        )

      df_county_community_tract <- list(df_county, df_community, df_tract) %>%
        purrr::map_dfr(c)

      return(df_county_community_tract)

    }

    # IF THERE'S A PROBLEM ----------------------------------------------------


    stop("Something went wrong with the tests in this function!")

  }


  # CALCULATE COMPARISON OF CHANGE ------------------------------------------

  comparison_of_change_demo_housing <- change_demo_housing %>%
    tidyr::nest(-DIMENSION, -INDICATOR, -VARIABLE, -DATE_GROUP_ID, -MEASURE_TYPE) %>%
    dplyr::mutate(COMP_FIELDS = purrr::pmap(list("data" = data,
                                                 "dimension" = DIMENSION,
                                                 "measure_type" = MEASURE_TYPE), get_comparison_fields)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()





  # VISUALIZE DATA ----------------------------------------------------------

  check_comparison_of_change_demo_housing_na <- function(){

    # check the NA's first

    # note: the only records with NA in INDICATOR_TYPE_MODEL should be 'county'
    comparison_of_change_demo_housing %>%
      dplyr::filter(is.na(INDICATOR_TYPE_MODEL)) %>%
      dplyr::count(GEOGRAPHY_TYPE,GEOGRAPHY_ID,DIMENSION, INDICATOR, VARIABLE, MEASURE_TYPE) %>% print(n=Inf)

  }

  view_comparison_of_change_demo_housing_by_dategroupid <- function(){


    # check the change types (INDICATOR_TYPE_MODEL)
    comparison_of_change_demo_housing %>%
      dplyr::filter(! is.na(INDICATOR_TYPE_MODEL)) %>%
      dplyr::count(DATE_GROUP_ID, INDICATOR, VARIABLE, INDICATOR_TYPE_MODEL) %>% View()

  }

  view_comparison_of_change_demo_housing_by_ind <- function(){

    # check the change types (INDICATOR_TYPE_MODEL)
    comparison_of_change_demo_housing %>%
      dplyr::filter(! is.na(INDICATOR_TYPE_MODEL)) %>%
      dplyr::count(INDICATOR, VARIABLE, DATE_GROUP_ID, INDICATOR_TYPE_MODEL) %>% View()
  }


  # JOIN DATE_* FIELDS ------------------------------------------------------

  date_group_id_fields <- inds_demo_housing %>%
    dplyr::select(-MEASURE_TYPE, -ESTIMATE, -MOE,-dplyr::matches("^INDICATOR_")) %>%
    dplyr::distinct()

  change_dategroupid_all_fields <- comparison_of_change_demo_housing %>%
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

  indicators_comparison_of_change_ready <- indicator_type_template %>%
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
                            "INDICATOR_TYPE_THRESHOLD_VALUE",
                            "INDICATOR_TYPE_DESC",
                            "INDICATOR_TYPE_VALUE",
                            "INDICATOR_TYPE_VALUE_DESC",
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

  indicators_comparison_of_change <- indicators_comparison_of_change_ready

  # RETURN ------------------------------------------------------------------

  return(indicators_comparison_of_change)

}

