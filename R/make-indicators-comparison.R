#' @title Make The Comparison Indicators
#' @description Description
#' @param indicators_by_dimension desc
#' @param model_table_production desc
#' @param indicator_type_template desc
#' @return a `tibble`
#' @export
make_indicators_comparison <- function(indicators_by_dimension,
                                       model_table_production,
                                       indicator_type_template){

  # NOTE --------------------------------------------------------------------

  # This applies to vulnerability and housing market indicators


  # CREATE THE FILTER-JOIN OBJECT -------------------------------------------

  inds_table_filter_join <- model_table_production %>%
    dplyr::select(DIMENSION, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>%
    dplyr::distinct() %>%
    dplyr::arrange(DIMENSION, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID)


  # REFORMAT ----------------------------------------------------------------


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

  # CREATE COMPARISON FUNCTION ---------------------------------------------


  get_comparison_fields <- function(data, dimension, measure_type){


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
        dplyr::mutate(INDICATOR_TYPE_THRESHOLD_VALUE = county_median) %>% # create the threshold value
        dplyr::mutate(INDICATOR_TYPE = "RELATIVE",
                      INDICATOR_TYPE_DESC = "RELATIVE TO MEDIAN",
                      INDICATOR_TYPE_VALUE =  plyr::round_any(ESTIMATE - INDICATOR_TYPE_THRESHOLD_VALUE, accuracy = .001),
                      INDICATOR_TYPE_VALUE_DESC = dplyr::case_when(
                        INDICATOR_TYPE_VALUE>= 0 ~ "GREATER THAN / EQUAL TO MEDIAN",
                        TRUE ~ "LESS THAN MEDIAN"
                      ),
                      INDICATOR_TYPE_THRESHOLD = "MEDIAN",
                      INDICATOR_TYPE_THRESHOLD_VALUE,
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
        dplyr::pull("ESTIMATE") %>% get_q4_lower()

      df_tract <- data %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract")) %>%
        dplyr::mutate(INDICATOR_TYPE_THRESHOLD_VALUE = county_q4_lower) %>% # create the threshold value
        dplyr::mutate(
          INDICATOR_TYPE = "RELATIVE",
          INDICATOR_TYPE_DESC = "QUINTILE",
          INDICATOR_TYPE_VALUE = as.double(dplyr::ntile(ESTIMATE, n = 5)), # should be double not integer
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
          INDICATOR_TYPE = "RELATIVE",
          INDICATOR_TYPE_DESC = "QUINTILE",
          INDICATOR_TYPE_VALUE = NA_real_, # should be double not integer # also, don't calculate the quintile because the data only contain the "community" aggregations (not the tracts)
          INDICATOR_TYPE_VALUE_DESC = dplyr::case_when(
            ESTIMATE <= county_q4_lower ~ "LOW/MED",
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



  # CREATE COMPARISON -------------------------------------------------------

  inds_in_models <-  ind_type_fields %>%
    dplyr::semi_join(inds_table_filter_join,  # only include the indicators that are used in the models
                     by = c("DIMENSION",
                            "INDICATOR",
                            "VARIABLE",
                            "MEASURE_TYPE",
                            "DATE_GROUP_ID"))

  inds_vuln_housing <- inds_in_models %>%
    dplyr::filter(DIMENSION %in% c("VULNERABILITY", "HOUSING_MARKET")) %>%
    dplyr::filter(! is.na(GEOGRAPHY_ID)) # for some unknown reason there are NA GEOGRAPHY_IDs in the ASSESSOR rows

  inds_vuln_housing_comparison <- inds_vuln_housing %>%
    tidyr::nest(-DIMENSION, -INDICATOR, -VARIABLE, -DATE_GROUP_ID,-MEASURE_TYPE) %>%
    dplyr::mutate(COMP_FIELDS = purrr::pmap(list("data" = data,
                                                 "dimension" = DIMENSION,
                                                 "measure_type" = MEASURE_TYPE),
                                            get_comparison_fields)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest()

  # REFORMAT ----------------------------------------------------------------

  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicators_comparison_ready <- indicator_type_template %>%
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
                            "MOE",
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
                  dplyr::everything())

  indicators_comparison <- indicators_comparison_ready



  # VISUALIZE: COUNT --------------------------------------------------------

  vis_count <- function(){
    indicators_comparison %>%
    filter(! GEOGRAPHY_TYPE %in% "county") %>% # INDICATOR_TYPE_MODEL values for the county are all NA
    count(GEOGRAPHY_TYPE,DIMENSION,INDICATOR,VARIABLE,DATE_GROUP_ID, INDICATOR_TYPE_MODEL) %>% print(n=Inf)
  }

  # RETURN ------------------------------------------------------------------

  return(indicators_comparison)

}

