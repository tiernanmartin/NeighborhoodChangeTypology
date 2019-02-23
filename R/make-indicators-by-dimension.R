#' @title Make The Indicators by DIMENSION
#' @description Description
#' @param indicators_cnt_pct desc
#' @param indicators_median desc
#' @param model_table_inputs desc
#' @param indicator_DIMENSION_template desc
#' @return a `tibble`
#' @export
make_indicators_by_DIMENSION <- function(indicators_cnt_pct,
                                     indicators_median,
                                     model_table_inputs,
                                     indicator_DIMENSION_template){


  DIMENSION_join  <- model_table_inputs %>%
    dplyr::select(DIMENSION, INDICATOR) %>%
    dplyr::distinct() %>%
    dplyr::mutate_all(to_caps_underscores) # replace spaces with "_" and make everything all caps

  DIMENSION_burden_fields <- tibble::tibble(DIMENSION = c("VULNERABILITY","VULNERABILITY"),
                                        INDICATOR = c("COST_BURDEN_OWN", "COST_BURDEN_RENT")
  )

  DIMENSION_join_all <- DIMENSION_join %>%
    dplyr::filter(! INDICATOR %in% "COST_BURDEN") %>% # drop COST_BURDEN
    dplyr::bind_rows(DIMENSION_burden_fields) %>%  # replace it with COST_BURDEN_OWN and COST_BURDEN_RENT
    dplyr::arrange(DIMENSION)

  # ADD DIMENSION FIELD ---------------------------------------------------------

  ind_DIMENSION <- list(indicators_cnt_pct,
                    indicators_median) %>%
    purrr::map_dfr(c) %>%
    dplyr::left_join(DIMENSION_join_all, by = c("INDICATOR"))



  # INVERT DEMOGRAPHIC_CHANGE INDICATORS ------------------------------------

  ind_DIMENSION_demo_change <-  ind_DIMENSION %>%
    dplyr::filter(DIMENSION %in% "DEMOGRAPHIC_CHANGE")


  indicators_demo_change_wide <- ind_DIMENSION_demo_change %>%
    tidyr::gather(VALUE_TYPE, VALUE, ESTIMATE, MOE) %>%
    tidyr::unite("VALUE_MEASURE",c(VALUE_TYPE, MEASURE_TYPE)) %>%
    dplyr::select(-VARIABLE_DESC) %>% # needs to be remove for spread() to work
    tidyr::spread(VALUE_MEASURE, VALUE) %>%
    dplyr::mutate(ESTIMATE_COUNT = dplyr::case_when(
      SOURCE %in% c("ACS", "CHAS") ~ ESTIMATE_TOTAL - ESTIMATE_COUNT,
      SOURCE %in% "ASSESSOR" ~ ESTIMATE_COUNT,
      TRUE ~ NA_real_),
      ESTIMATE_PERCENT = dplyr::case_when(
        SOURCE %in% c("ACS", "CHAS") ~ 1 - ESTIMATE_PERCENT,
        SOURCE %in% "ASSESSOR" ~ ESTIMATE_PERCENT,
        TRUE ~ NA_real_),
      ESTIMATE_TOTAL = ESTIMATE_TOTAL # this stays unchanged
    ) %>%
    dplyr::mutate(MOE_COUNT = dplyr::case_when(
      SOURCE %in% c("ACS", "CHAS") ~ MOE_COUNT,
      SOURCE %in% "ASSESSOR" ~ MOE_COUNT,
      TRUE ~ NA_real_),
      MOE_PERCENT = dplyr::case_when(
        SOURCE %in% c("ACS", "CHAS") ~ tidycensus::moe_prop(num = ESTIMATE_TOTAL - ESTIMATE_COUNT,
                                                            denom = ESTIMATE_TOTAL,
                                                            moe_num = ESTIMATE_COUNT,
                                                            moe_denom = ESTIMATE_TOTAL),
        SOURCE %in% "ASSESSOR" ~ MOE_PERCENT,
        TRUE ~ NA_real_),
      MOE_TOTAL = MOE_TOTAL # this stays unchanged
    )

  indicators_demo_change_long <- indicators_demo_change_wide %>%
    tidyr::gather(VALUE_MEASURE, VALUE, dplyr::matches("ESTIMATE|MOE")) %>%
    tidyr::separate(VALUE_MEASURE, into = c("VALUE_TYPE","MEASURE_TYPE")) %>%
    tidyr::spread(VALUE_TYPE, VALUE) %>%
    dplyr::left_join(dplyr::select(ind_DIMENSION_demo_change,-dplyr::matches("ESTIMATE|MOE")), # get VARIABLE_DESC back
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
                            "DIMENSION",
                            "MEASURE_TYPE"))


# COMBINE INDICATOR OBJECTS -----------------------------------------------

  ind_DIMENSION_vuln_hous <- ind_DIMENSION %>%
    dplyr::filter(! DIMENSION %in% "DEMOGRAPHIC_CHANGE")


  ind_DIMENSION_all <- list(ind_DIMENSION_vuln_hous,
                        indicators_demo_change_long) %>%
    purrr::map_dfr(c)

  # REFORMAT ----------------------------------------------------------------


  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicators_by_DIMENSION_ready <- indicator_DIMENSION_template %>%
    dplyr::full_join(ind_DIMENSION_all,
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

  indicators_by_DIMENSION <- indicators_by_DIMENSION_ready

  # RETURN ------------------------------------------------------------------

  return(indicators_by_DIMENSION)

}


