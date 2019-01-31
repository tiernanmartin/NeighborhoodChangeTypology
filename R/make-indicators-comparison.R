#' @title Make The Comparison Indicators
#' @description Description
#' @param indicators_cnt_pct desc
#' @param indicators_median desc
#' @param indicator_template desc
#' @return a `tibble`
#' @export
make_indicators_comparison <- function(indicators_cnt_pct,
                                       indicators_median,
                                       indicator_template){

  # CREATE COMPARISON FUNCTION ----------------------------------------------


  get_comp_fields <- function(data, measure_type){

    # create a df with the new fields and NA values

    df_na <-  data %>%
      dplyr::mutate(COMP_THRESHOLD_TYPE = NA_character_,
                    COMP_THRESHOLD_VALUE = NA_real_,
                    COMP_TYPE = NA_character_,
                    COMP_VALUE = NA_real_,
                    COMP_VALUE_DESC = NA_character_
      )

    df_county <- df_na %>%
      dplyr::filter(GEOGRAPHY_TYPE %in% c("county"))

    if(! measure_type %in% c("MEDIAN","PERCENT")){

      return(df_na)
    }

    if(measure_type %in% "MEDIAN"){

      county_median <- df_na %>% dplyr::filter(GEOGRAPHY_TYPE %in% "county") %>% dplyr::pull(ESTIMATE)

      df_community_tract <- df_na %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract", "community")) %>%
        dplyr::mutate(COMP_THRESHOLD_VALUE = county_median,
                      COMP_THRESHOLD_TYPE = "MEDIAN",
                      COMP_TYPE = "RATIO TO MEDIAN",
                      COMP_VALUE = plyr::round_any(ESTIMATE/COMP_THRESHOLD_VALUE, accuracy = .01),
                      COMP_VALUE_DESC = dplyr::case_when(
                        COMP_VALUE>= 1 ~ "GREATER THAN / EQUAL TO MEDIAN",
                        TRUE ~ "LESS THAN MEDIAN"
                      )
        )

      df_county_community_tract <- dplyr::bind_rows(df_county, df_community_tract)

      return(df_county_community_tract)

    }

    if(measure_type %in% "PERCENT"){

      get_q4_lower <- function(x) {

        q4_lower <- stats::quantile(x,probs = c(0,.6,1), na.rm = TRUE)[[2]]

        q4_lower_rounded <- plyr::round_any(q4_lower, accuracy = .0001)

        return(q4_lower_rounded)
      }


      county_q4_lower <- df_na %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% "tract") %>%
        dplyr::pull("ESTIMATE") %>% get_q4_lower()

      df_community_tract <- df_na %>%
        dplyr::filter(GEOGRAPHY_TYPE %in% c("tract", "community")) %>%
        dplyr::mutate(
          COMP_TYPE = "QUINTILE",
          COMP_VALUE = as.double(dplyr::ntile(ESTIMATE, n = 5)), # should be double not integer
          COMP_VALUE_DESC = dplyr::case_when(
            COMP_VALUE <= 3 ~ "LOW/MED",
            TRUE ~ "HIGH"
          ),
          COMP_THRESHOLD_VALUE = county_q4_lower,
          COMP_THRESHOLD_TYPE = "Q4 LOWER BOUND"
        )

      df_county_community_tract <- dplyr::bind_rows(df_county, df_community_tract)

      return(df_county_community_tract)


    }


    stop("Something went wrong with the tests in this function!")

  }


  # CREATE COMPARISON ----

  inds_all <- list(indicators_cnt_pct,
                   indicators_median) %>%
    purrr::map_dfr(c) %>%
    dplyr::filter(! is.na(GEOGRAPHY_ID)) # for some unknown reason there are NA GEOGRAPHY_IDs in the ASSESSOR rows

  inds_comparison <- inds_all %>%
    tidyr::nest(-INDICATOR,-VARIABLE, -DATE_GROUP_ID,-MEASURE_TYPE) %>%
  dplyr::mutate(COMP_FIELDS = purrr::map2(data, MEASURE_TYPE, get_comp_fields)) %>%
  dplyr::select(-data) %>%
  tidyr::unnest()


    # REFORMAT ----------------------------------------------------------------

  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicators_comparison_ready <- indicator_template %>%
    dplyr::full_join(inds_comparison,
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
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  indicators_comparison <- indicators_comparison_ready

  # RETURN ------------------------------------------------------------------

  return(indicators_comparison)

}


