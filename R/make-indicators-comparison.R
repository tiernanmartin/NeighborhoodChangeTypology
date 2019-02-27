#' @title Make The Comparison Indicators
#' @description Description
#' @param indicators_in_models desc
#' @param model_table_production desc
#' @param indicator_value_template desc
#' @return a `tibble`
#' @export
make_indicators_comparison <- function(indicators_in_models,
                                       model_table_production,
                                       indicator_value_template){

  # NOTE --------------------------------------------------------------------

  # This applies to vulnerability and housing market indicators

  # CREATE COMPARISON FUNCTION ---------------------------------------------


  get_comparison_fields <- function(data, dimension, measure_type){


    # IF DIMENSION ISN'T DIMENSION VULNERABILITY OR HOUSING_MARKET ------------

    if(! dimension %in% c("VULNERABILITY", "HOUSING_MARKET")){

      return(data)
    }


    # IF MEASURE_TYPE ISN'T PERCENT OR MEDIAN ---------------------------------

    if(! measure_type %in% c("PERCENT", "MEDIAN")){

      return(data)
    }


    # COUNTY ------------------------------------------------------------------

    df_county <- data %>%
      dplyr::filter(GEOGRAPHY_TYPE %in% c("county"))


    # IF DIMENSION %in% VULNERABILITY ---------------------------------------------

    if(dimension %in% "VULNERABILITY"){

      if(! measure_type %in% c("PERCENT")){

        return(data)
      }


      county_median <- data %>% dplyr::filter(GEOGRAPHY_TYPE %in% "county") %>% dplyr::pull(ESTIMATE)

      df_community_tract <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract", "community")) %>%
        dplyr::mutate(RELATIVE_THRESHOLD = county_median) %>% # create the threshold value
        dplyr::mutate(RELATIVE =  plyr::round_any(ESTIMATE - RELATIVE_THRESHOLD, accuracy = .001),
                      RELATIVE_DESC = dplyr::case_when(
                        RELATIVE>= 0 ~ "GREATER THAN / EQUAL TO MEDIAN",
                        TRUE ~ "LESS THAN MEDIAN"
                      ),
                      RELATIVE_THRESHOLD,
                      RELATIVE_LGL = RELATIVE_DESC %in% c("GREATER THAN / EQUAL TO MEDIAN")
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
        dplyr::pull("ESTIMATE") %>% get_q4_lower()

      df_tract <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract")) %>%
        dplyr::mutate(RELATIVE_THRESHOLD = county_q4_lower) %>% # create the threshold value
        dplyr::mutate(RELATIVE = as.double(dplyr::ntile(ESTIMATE, n = 5)), # should be double not integer
                      RELATIVE_DESC = dplyr::case_when(
                        RELATIVE <= 3 ~ "LOW/MED",
                        TRUE ~ "HIGH"
                      ),
                      RELATIVE_THRESHOLD,
                      RELATIVE_LGL = RELATIVE_DESC %in% "HIGH"
        )

      df_community <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("community")) %>%
        dplyr::mutate(RELATIVE_THRESHOLD = county_q4_lower) %>% # create the threshold value
        dplyr::mutate(RELATIVE = NA_real_, # should be double not integer # also, don't calculate the quintile because the data only contain the "community" aggregations (not the tracts)
                      RELATIVE_DESC = dplyr::case_when(
                        ESTIMATE <= county_q4_lower ~ "LOW/MED",
                        TRUE ~ "HIGH"
                      ),
                      RELATIVE_THRESHOLD,
                      RELATIVE_LGL = RELATIVE_DESC %in% "HIGH"
        )

      df_county_community_tract <- list(df_county, df_community, df_tract) %>%
        purrr::map_dfr(c)

      return(df_county_community_tract)

    }


    # IF THERE'S A PROBLEM ----------------------------------------------------


    stop("Something went wrong with the tests in this function!")

  }



  # CREATE COMPARISON -------------------------------------------------------

  ind_value_fields <- indicator_value_template %>%
    dplyr::full_join(indicators_in_models,
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


  inds_no_na_geogs <- ind_value_fields %>%
    dplyr::filter(! is.na(GEOGRAPHY_ID)) # for some unknown reason there are NA GEOGRAPHY_IDs in the ASSESSOR rows

  inds_vuln_housing_comparison <- inds_no_na_geogs %>%
    tidyr::nest(-DIMENSION, -INDICATOR, -VARIABLE, -DATE_GROUP_ID,-MEASURE_TYPE) %>%
    dplyr::mutate(COMP_FIELDS = purrr::pmap(list("data" = data,
                                                 "dimension" = DIMENSION,
                                                 "measure_type" = MEASURE_TYPE),
                                            get_comparison_fields)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()

  # REFORMAT ----------------------------------------------------------------

  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicators_comparison_ready <- indicator_value_template %>%
    dplyr::full_join(inds_vuln_housing_comparison,
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
                            "ESTIMATE_BEGIN",
                            "ESTIMATE_END",
                            "MOE",
                            "MOE_BEGIN",
                            "MOE_END",
                            "DIFFERENCE",
                            "DIFFERENCE_MOE",
                            "RELATIVE",
                            "RELATIVE_DESC",
                            "RELATIVE_THRESHOLD",
                            "RELATIVE_LGL",
                            "RELATIVE_BEGIN" ,
                            "RELATIVE_DESC_BEGIN",
                            "RELATIVE_THRESHOLD_BEGIN" ,
                            "RELATIVE_LGL_BEGIN",
                            "RELATIVE_END",
                            "RELATIVE_DESC_END" ,
                            "RELATIVE_THRESHOLD_END",
                            "RELATIVE_LGL_END",
                            "CHANGE",
                            "CHANGE_DESC",
                            "CHANGE_THRESHOLD",
                            "CHANGE_LGL",
                            "RELATIVE_CHANGE_DESC",
                            "RELATIVE_CHANGE_LGL",
                            "PROXIMITY_DESC"))




  indicators_comparison <- indicators_comparison_ready


  # RETURN ------------------------------------------------------------------

  return(indicators_comparison)

}

