#' @title Make The Comparison Indicators
#' @description Description
#' @param indicators_in_models desc
#' @param indicator_value_template desc
#' @return a `tibble`
#' @export
make_indicators_comparison <- function(indicators_in_models,
                                       indicator_value_template){

  # NOTE --------------------------------------------------------------------

  # This applies to vulnerability and housing market indicators

  # CREATE COMPARISON FUNCTION ---------------------------------------------


  get_comparison_fields <- function(data, dimension, indicator, measure_type){


    # IF DIMENSION IS DEMOGRAPHIC_CHANGE ------------

    if(dimension %in% c("DEMOGRAPHIC_CHANGE")){

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

    if(dimension %in% c("VULNERABILITY", "MISCELLANEOUS")){

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


    # IF DIMENSION %in% HOUSING MARKET & INDICATOR %IN% MFUNITS ----------------

     if(dimension %in% "HOUSING_MARKET" & indicator %in% "MULTIFAMILY"){

      pct_multifamily_threshold <- 0.90

      df_community_tract <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract", "community")) %>%
        dplyr::mutate(RELATIVE_THRESHOLD = pct_multifamily_threshold) %>%
        dplyr::mutate(RELATIVE = plyr::round_any(ESTIMATE - RELATIVE_THRESHOLD, accuracy = .001),
                      RELATIVE_MOE = MOE,
                      RELATIVE_DESC = dplyr::case_when(
                        RELATIVE>= 0 ~ "GREATER THAN / EQUAL TO 90%",
                        TRUE ~ "LESS THAN 90%"
                      ),
                      RELATIVE_THRESHOLD,
                      RELATIVE_LGL = RELATIVE_DESC %in% "GREATER THAN / EQUAL TO 90%"
        )

      df_county_community_tract <- list(df_county, df_community_tract) %>%
        purrr::map_dfr(c)

      return(df_county_community_tract)

    }

    # IF DIMENSION %in% HOUSING MARKET & INDICATOR %!IN% MFUNITS ---------------



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

  ind_dimensions <- ind_value_fields %>%
    # remove DEMOGRAPHIC_CHANGE fields (they will be empty)
    dplyr::filter(DIMENSION %in% c("VULNERABILITY", "HOUSING_MARKET", "MISCELLANEOUS"))

  inds_no_na_geogs <- ind_dimensions %>%
    dplyr::filter(! is.na(GEOGRAPHY_ID)) # for some unknown reason there are NA GEOGRAPHY_IDs in the ASSESSOR rows

  inds_vuln_housing_comparison <- inds_no_na_geogs %>%
    tidyr::nest(-DIMENSION, -INDICATOR, -VARIABLE, -DATE_GROUP_ID,-MEASURE_TYPE) %>%
    dplyr::mutate(COMP_FIELDS = purrr::pmap(list("data" = data,
                                                 "dimension" = DIMENSION,
                                                 "indicator" = INDICATOR,
                                                 "measure_type" = MEASURE_TYPE),
                                            get_comparison_fields)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()

  indicators_comparison <- inds_vuln_housing_comparison


  # RETURN ------------------------------------------------------------------

  return(indicators_comparison)

}

