#' @title Make The Change Indicators
#' @description Make the ACS Inidcators related to a _value of a population.
#'   An example of the type of indicator included in this object might be
#'   the count of renter households, while the median rent price would _not_ be included.
#' @param indicators_cnt_pct desc
#' @param indicators_median desc
#' @param indicator_template desc
#' @return a `tibble`
#' @export
make_indicators_change <- function(indicators_cnt_pct,
                                   indicators_median,
                                   indicator_template){


  # CREATE CHANGE_ENDYEARS_GUIDE --------------------------------------------

  change_endyears <- tibble::tribble(
    ~INDICATOR, ~VARIABLE, ~ BEGIN, ~END,
    "COST_BURDEN_OWN", "B25106_OWN", 2010, 2015,
    "COST_BURDEN_OWN", "B25106_OWN", 2011, 2017,
    "COST_BURDEN_RENT", "B25106_RENT", 2010, 2015,
    "COST_BURDEN_RENT", "B25106_RENT", 2011, 2017,
    "EDUCATION", "B15002", 2010, 2015,
    "EDUCATION", "B15002", 2011, 2017,
    "INCOME", "T7", 2010, 2015,
    "INCOME", "B19001", 2010, 2015,
    "INCOME", "B19001", 2011, 2017,
    "RACE", "B03002", 2010, 2015,
    "RACE", "B03002", 2011, 2017,
    "TENURE", "B25033", 2010, 2015,
    "TENURE", "B25033", 2011, 2017,
    "VALUE", "B25077", 2000, 2010,
    "VALUE", "B25077", 2000, 2017,
    "VALUE", "B25077", 2010, 2017,
    "RENT", "B25058", 2010, 2015,
    "RENT", "B25058", 2011, 2017,
    "ASSESSED_VALUE", "ATV_ALL", 2005, 2010,
    "ASSESSED_VALUE", "ATV_ALL", 2005, 2018,
    "ASSESSED_VALUE", "ATV_ALL", 2010, 2018,
    "ASSESSED_VALUE", "ATV_CONDO", 2005, 2010,
    "ASSESSED_VALUE", "ATV_CONDO", 2005, 2018,
    "ASSESSED_VALUE", "ATV_CONDO", 2010, 2018,
    "ASSESSED_VALUE", "ATV_SF", 2005, 2010,
    "ASSESSED_VALUE", "ATV_SF", 2005, 2018,
    "ASSESSED_VALUE", "ATV_SF", 2010, 2018,
    "SALE_PRICE", "SP_ALL", 2005, 2010,
    "SALE_PRICE", "SP_ALL", 2005, 2018,
    "SALE_PRICE", "SP_ALL", 2010, 2018,
    "SALE_PRICE", "SP_CONDO", 2005, 2010,
    "SALE_PRICE", "SP_CONDO", 2005, 2018,
    "SALE_PRICE", "SP_CONDO", 2010, 2018,
    "SALE_PRICE", "SP_SF", 2005, 2010,
    "SALE_PRICE", "SP_SF", 2005, 2018,
    "SALE_PRICE", "SP_SF", 2010, 2018,
    "SALE_PRICE", "SP_SQFT_ALL", 2005, 2010,
    "SALE_PRICE", "SP_SQFT_ALL", 2005, 2018,
    "SALE_PRICE", "SP_SQFT_ALL", 2010, 2018,
    "SALE_PRICE", "SP_SQFT_CONDO", 2005, 2010,
    "SALE_PRICE", "SP_SQFT_CONDO", 2005, 2018,
    "SALE_PRICE", "SP_SQFT_CONDO", 2010, 2018,
    "SALE_PRICE", "SP_SQFT_SF", 2005, 2010,
    "SALE_PRICE", "SP_SQFT_SF", 2005, 2018,
    "SALE_PRICE", "SP_SQFT_SF", 2010, 2018,
    "SALE_RATE", "SR_ALL", 2005, 2010,
    "SALE_RATE", "SR_ALL", 2005, 2018,
    "SALE_RATE", "SR_ALL", 2010, 2018,
    "SALE_RATE", "SR_CONDO", 2005, 2010,
    "SALE_RATE", "SR_CONDO", 2005, 2018,
    "SALE_RATE", "SR_CONDO", 2010, 2018,
    "SALE_RATE", "SR_SF", 2005, 2010,
    "SALE_RATE", "SR_SF", 2005, 2018,
    "SALE_RATE", "SR_SF", 2010, 2018
  )

  # PREPARE DATA --------------------------------------------------------


  # the demographic change indicators are the _inverse_ of the vulnerability indicatords
  # begin by subtracting COUNT from TOTAL and 1 - PERCENT

  indicators_cnt_pct_demo_change_wide <- indicators_cnt_pct %>%
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

  indicators_cnt_pct_demo_change <- indicators_cnt_pct_demo_change_wide %>%
    tidyr::gather(VALUE_MEASURE, VALUE, dplyr::matches("ESTIMATE|MOE")) %>%
    tidyr::separate(VALUE_MEASURE, into = c("VALUE_TYPE","MEASURE_TYPE")) %>%
    tidyr::spread(VALUE_TYPE, VALUE) %>%
    dplyr::left_join(dplyr::select(indicators_cnt_pct,-dplyr::matches("ESTIMATE|MOE")), # get VARIABLE_DESC back
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_END_YEAR",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "INDICATOR",
                            "VARIABLE",
                            "MEASURE_TYPE"))



  inds <- list(indicators_cnt_pct_demo_change, indicators_median) %>%
    purrr::map_dfr(c) %>%
    dplyr::filter(!DATE_RANGE_TYPE %in% "one quarter") %>%
    dplyr::mutate(DATE_END_YEAR_JOIN = stringr::str_c("YEAR_", DATE_END_YEAR)) %>%
    dplyr::filter(!is.na(GEOGRAPHY_ID))

  inds_drop_source_fields <- inds %>%
    dplyr::select(-SOURCE, -VARIABLE_DESC) # these columns shouldn't be included in the CHANGE indicator

  inds_long <- inds_drop_source_fields %>%
    tidyr::gather(VALUE_TYPE, VALUE, ESTIMATE, MOE)

  change_endyears_long <- change_endyears %>%
    dplyr::mutate(CHANGE_RANGE = as.character(glue::glue("YEAR_{BEGIN}_TO_YEAR_{END}"))) %>%
    tidyr::gather(INDICATOR_ROLE, DATE_END_YEAR_JOIN, BEGIN, END) %>%
    dplyr::mutate(DATE_END_YEAR_JOIN = stringr::str_c("YEAR_", DATE_END_YEAR_JOIN))

  # note: the left_join() below removes many unused DATE_END_YEAR records
  # use the function below to check these

  check_unused_change_years <- function(){
    anti_join(inds_long, change_endyears_long, by = c("INDICATOR",
                                                      "VARIABLE",
                                                      "DATE_END_YEAR_JOIN")) %>%
      count(INDICATOR, VARIABLE,DATE_RANGE_TYPE,DATE_END_YEAR) %>% print(n=Inf)
  }


  change_endyears_wide <- change_endyears_long %>%
    dplyr::left_join(inds_long, by = c("INDICATOR",
                                       "VARIABLE",
                                       "DATE_END_YEAR_JOIN")) %>%
    tidyr::unite("TYPE_ROLE_YEAR", c(VALUE_TYPE, INDICATOR_ROLE)) %>%
    dplyr::select(-dplyr::starts_with("DATE")) %>%
    tidyr::spread(TYPE_ROLE_YEAR, VALUE)

  change_endyears_wide_change <- change_endyears_wide %>%
    dplyr::mutate(ESTIMATE_CHANGE_ABSOLUTE = ESTIMATE_END - ESTIMATE_BEGIN,
                  ESTIMATE_CHANGE_RATIO = (ESTIMATE_END/ESTIMATE_BEGIN) - 1, # change in pct
                  ESTIMATE_CHANGE_APPROPRIATE = dplyr::case_when(
                    MEASURE_TYPE %in% "PERCENT" ~ ESTIMATE_CHANGE_ABSOLUTE,
                    MEASURE_TYPE %in% c("COUNT", "MEDIAN", "TOTAL") ~ ESTIMATE_CHANGE_RATIO,
                    TRUE ~ NA_real_
                  )) %>%
    dplyr::mutate(MOE_CHANGE_ABSOLUTE = purrr::pmap_dbl(list(a = MOE_END,  # use pmap to vectorize this call
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

  change_endyears_long <- change_endyears_wide_change %>%
    dplyr::mutate(RNUM = dplyr::row_number()) %>%
    dplyr::select(-dplyr::matches("BEGIN|END")) %>% # only keep the CHANGE fields (BEGIN and END will be joined later)
    dplyr::select(-MEASURE_TYPE) %>% # this will be filled in with CHANGE_*
    tidyr::gather(VALUE_MEASURE, VALUE, dplyr::matches("ESTIMATE|MOE")) %>%
    tidyr::separate(VALUE_MEASURE, c("VALUE_TYPE","MEASURE_TYPE", "MEASURE_TYPE_DETAIL")) %>%
    tidyr::unite("MEASURE_TYPE", c(MEASURE_TYPE,MEASURE_TYPE_DETAIL)) %>%
    tidyr::spread(VALUE_TYPE, VALUE) %>%
    dplyr::select(-RNUM)


  # JOIN DATE_* FIELDS ------------------------------------------------------

  date_end_year_fields <- inds_drop_source_fields %>%
    dplyr::select(-MEASURE_TYPE, -ESTIMATE, -MOE) %>%
    dplyr::distinct()


  change_endyears_all_fields <- change_endyears_long %>%
    dplyr::mutate(RNUM = dplyr::row_number(),
                  DATE_END_YEAR = stringr::str_remove_all(CHANGE_RANGE, "YEAR_|TO_")
    ) %>%
    tidyr::separate(DATE_END_YEAR, into = c("DATE_END_YEAR_BEGIN", "DATE_END_YEAR_END")) %>%
    tidyr::gather(DATE_END_YEAR_ROLE, DATE_END_YEAR, DATE_END_YEAR_BEGIN, DATE_END_YEAR_END) %>%
    dplyr::left_join(date_end_year_fields, by = c("INDICATOR",
                                                  "VARIABLE",
                                                  "GEOGRAPHY_ID",
                                                  "GEOGRAPHY_ID_TYPE",
                                                  "GEOGRAPHY_NAME",
                                                  "GEOGRAPHY_TYPE",
                                                  "DATE_END_YEAR")) %>%
    dplyr::mutate(DATE_END_YEAR_ROLE = stringr::str_extract(DATE_END_YEAR_ROLE,"BEGIN$|END$")) %>%
    tidyr::gather(DATE_FIELD_TYPE, DATE_FIELD_VAL, DATE_END_YEAR, DATE_BEGIN, DATE_END, DATE_RANGE, DATE_RANGE_TYPE) %>%
    tidyr::unite("ROLE_DATE_FIELD_TYPE", c(DATE_END_YEAR_ROLE,DATE_FIELD_TYPE)) %>%
    dplyr::select(-DATE_END_YEAR_JOIN) %>% # this messess up the spread()
    tidyr::spread(ROLE_DATE_FIELD_TYPE,DATE_FIELD_VAL) %>%
    dplyr::mutate(DATE_END_YEAR = END_DATE_END_YEAR,
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

                  indicators_change_ready <- indicator_template %>%
                    dplyr::full_join(change_endyears_var_desc,
                                     by = c("SOURCE",
                                            "GEOGRAPHY_ID",
                                            "GEOGRAPHY_ID_TYPE",
                                            "GEOGRAPHY_NAME",
                                            "GEOGRAPHY_TYPE",
                                            "DATE_BEGIN",
                                            "DATE_END",
                                            "DATE_END_YEAR",
                                            "DATE_RANGE",
                                            "DATE_RANGE_TYPE",
                                            "INDICATOR",
                                            "VARIABLE",
                                            "VARIABLE_DESC",
                                            "MEASURE_TYPE",
                                            "ESTIMATE",
                                            "MOE"))

                  indicators_change <- indicators_change_ready

                  # RETURN ------------------------------------------------------------------

                  return(indicators_change)

}
