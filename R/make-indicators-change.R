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
    ~INDICATOR,       ~VARIABLE,      ~BEGIN,        ~END,
    "COST_BURDEN_OWN",    "B25106_OWN",      "2010",      "2015",
    "COST_BURDEN_OWN",    "B25106_OWN",      "2011",      "2017",
    "COST_BURDEN_RENT",   "B25106_RENT",      "2010",      "2015",
    "COST_BURDEN_RENT",   "B25106_RENT",      "2011",      "2017",
    "EDUCATION",        "B15002",      "2010",      "2015",
    "EDUCATION",        "B15002",      "2011",      "2017",
    "INCOME",            "T7",      "2010",      "2015",
    "INCOME",        "B19001",      "2010",      "2015",
    "INCOME",        "B19001",      "2011",      "2017",
    "RACE",        "B03002",      "2010",      "2015",
    "RACE",        "B03002",      "2011",      "2017",
    "TENURE",        "B25033",      "2010",      "2015",
    "TENURE",        "B25033",      "2011",      "2017",
    "VALUE",        "B25077",      "2000",      "2010",
    "VALUE",        "B25077",      "2000",      "2017",
    "VALUE",        "B25077",      "2010",      "2017",
    "RENT",        "B25058",      "2010",      "2015",
    "RENT",        "B25058",      "2011",      "2017",
    "ASSESSED_VALUE",       "ATV_ALL",      "2005",      "2010",
    "ASSESSED_VALUE",       "ATV_ALL",      "2005",      "2018",
    "ASSESSED_VALUE",       "ATV_ALL",      "2010",      "2018",
    "ASSESSED_VALUE",     "ATV_CONDO",      "2005",      "2010",
    "ASSESSED_VALUE",     "ATV_CONDO",      "2005",      "2018",
    "ASSESSED_VALUE",     "ATV_CONDO",      "2010",      "2018",
    "ASSESSED_VALUE",        "ATV_SF",      "2005",      "2010",
    "ASSESSED_VALUE",        "ATV_SF",      "2005",      "2018",
    "ASSESSED_VALUE",        "ATV_SF",      "2010",      "2018",
    "SALE_PRICE",        "SP_ALL",      "2005",      "2010",
    "SALE_PRICE",        "SP_ALL",      "2005",      "2018",
    "SALE_PRICE",        "SP_ALL",      "2010",      "2018",
    "SALE_PRICE",        "SP_ALL", "2013_2015", "2016_2018",
    "SALE_PRICE",      "SP_CONDO",      "2005",      "2010",
    "SALE_PRICE",      "SP_CONDO",      "2005",      "2018",
    "SALE_PRICE",      "SP_CONDO",      "2010",      "2018",
    "SALE_PRICE",      "SP_CONDO", "2013_2015", "2016_2018",
    "SALE_PRICE",         "SP_SF",      "2005",      "2010",
    "SALE_PRICE",         "SP_SF",      "2005",      "2018",
    "SALE_PRICE",         "SP_SF",      "2010",      "2018",
    "SALE_PRICE",         "SP_SF", "2013_2015", "2016_2018",
    "SALE_PRICE",   "SP_SQFT_ALL",      "2005",      "2010",
    "SALE_PRICE",   "SP_SQFT_ALL",      "2005",      "2018",
    "SALE_PRICE",   "SP_SQFT_ALL",      "2010",      "2018",
    "SALE_PRICE",   "SP_SQFT_ALL", "2013_2015", "2016_2018",
    "SALE_PRICE", "SP_SQFT_CONDO",      "2005",      "2010",
    "SALE_PRICE", "SP_SQFT_CONDO",      "2005",      "2018",
    "SALE_PRICE", "SP_SQFT_CONDO",      "2010",      "2018",
    "SALE_PRICE", "SP_SQFT_CONDO", "2013_2015", "2016_2018",
    "SALE_PRICE",    "SP_SQFT_SF",      "2005",      "2010",
    "SALE_PRICE",    "SP_SQFT_SF",      "2005",      "2018",
    "SALE_PRICE",    "SP_SQFT_SF",      "2010",      "2018",
    "SALE_PRICE",    "SP_SQFT_SF", "2013_2015", "2016_2018",
    "SALE_RATE",        "SR_ALL",      "2005",      "2010",
    "SALE_RATE",        "SR_ALL",      "2005",      "2018",
    "SALE_RATE",        "SR_ALL",      "2010",      "2018",
    "SALE_RATE",        "SR_ALL", "2013_2015", "2016_2018",
    "SALE_RATE",      "SR_CONDO",      "2005",      "2010",
    "SALE_RATE",      "SR_CONDO",      "2005",      "2018",
    "SALE_RATE",      "SR_CONDO",      "2010",      "2018",
    "SALE_RATE",      "SR_CONDO", "2013_2015", "2016_2018",
    "SALE_RATE",         "SR_SF",      "2005",      "2010",
    "SALE_RATE",         "SR_SF",      "2005",      "2018",
    "SALE_RATE",         "SR_SF",      "2010",      "2018",
    "SALE_RATE",         "SR_SF", "2013_2015", "2016_2018"
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
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "INDICATOR",
                            "VARIABLE",
                            "MEASURE_TYPE"))



  inds <- list(indicators_cnt_pct_demo_change, indicators_median) %>%
    purrr::map_dfr(c) %>%
    dplyr::filter(!DATE_RANGE_TYPE %in% "one quarter") %>%
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
      count(INDICATOR, VARIABLE,DATE_RANGE_TYPE,DATE_GROUP_ID) %>% print(n=Inf)
  }


  change_endyears_wide <-  change_endyears_long %>%
    dplyr::left_join(inds_long, by = c("INDICATOR", # switched from left_ to inner_
                                       "VARIABLE",
                                       "DATE_GROUP_ID_JOIN")) %>%
    dplyr::filter(! is.na(VALUE_TYPE)) %>% # remove a few records that have NA in many of the metadata fields
    dplyr::select(-dplyr::starts_with("DATE")) %>%
    dplyr::mutate(GROUP_ID = dplyr::group_indices(.,INDICATOR, VARIABLE,CHANGE_RANGE,GEOGRAPHY_ID,MEASURE_TYPE)) %>%
    tidyr::unite("TYPE_ROLE_YEAR", c(VALUE_TYPE, INDICATOR_ROLE)) %>%
    tidyr::spread(TYPE_ROLE_YEAR, VALUE) %>%
    dplyr::select(-GROUP_ID)

  change_endyears_wide_change <- change_endyears_wide %>%
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

  change_endyears_long <- change_endyears_wide_change %>%
    dplyr::mutate(RNUM = dplyr::row_number()) %>%
    dplyr::select(-dplyr::matches("BEGIN|END")) %>% # only keep the CHANGE fields (BEGIN and END will be joined later)
    tidyr::gather(VALUE_MEASURE, VALUE, dplyr::matches("ESTIMATE|MOE")) %>%
    tidyr::separate(VALUE_MEASURE, c("VALUE_TYPE","IND_MEASURE_TYPE", "MEASURE_TYPE_DETAIL")) %>%
    tidyr::unite("MEASURE_TYPE", c(IND_MEASURE_TYPE, MEASURE_TYPE, MEASURE_TYPE_DETAIL)) %>%
    tidyr::spread(VALUE_TYPE, VALUE) %>%
    dplyr::select(-RNUM)


  # JOIN DATE_* FIELDS ------------------------------------------------------

  date_group_id_fields <- inds_drop_source_fields %>%
    dplyr::select(-MEASURE_TYPE, -ESTIMATE, -MOE) %>%
    dplyr::distinct()


  change_endyears_all_fields <- change_endyears_long %>%
    dplyr::mutate(RNUM = dplyr::row_number(),
                  DATE_GROUP_ID = stringr::str_remove_all(CHANGE_RANGE, "TO_")
    ) %>%
    tidyr::separate(DATE_GROUP_ID, into = c("DATE_GROUP_ID_BEGIN", "DATE_GROUP_ID_END"),sep = "_(?=YEAR)") %>%
    tidyr::gather(DATE_GROUP_ID_ROLE, DATE_GROUP_ID_JOIN, DATE_GROUP_ID_BEGIN, DATE_GROUP_ID_END) %>%
    dplyr::left_join(date_group_id_fields, by = c("INDICATOR",
                                                  "VARIABLE",
                                                  "GEOGRAPHY_ID",
                                                  "GEOGRAPHY_ID_TYPE",
                                                  "GEOGRAPHY_NAME",
                                                  "GEOGRAPHY_TYPE",
                                                  "DATE_GROUP_ID_JOIN")) %>%
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

  indicators_change_ready <- indicator_template %>%
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

check_change_pct <- function(){

  smooth_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.3 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- round(qnt[1] - H)
    y[x > (qnt[2] + H)] <- round(qnt[2] + H)
    y
  }


  dat <- indicators_change %>%
    dplyr::filter(GEOGRAPHY_TYPE %in% "tract") %>%
    dplyr::filter(!is.na(DATE_RANGE_TYPE)) %>%
    dplyr::filter(stringr::str_detect(MEASURE_TYPE, "PERCENT")) %>%
    dplyr::filter(stringr::str_detect(MEASURE_TYPE, "APPROPRIATE")) %>%
    dplyr::group_by(VARIABLE, DATE_RANGE) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE),
                  ESTIMATE_NO_OUTLIERS = smooth_outliers(ESTIMATE)) %>%
    dplyr::ungroup()

  dat %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE_NO_OUTLIERS)) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::scale_x_continuous(labels = scale_pct_points) +
    ggplot2::facet_grid(DATE_RANGE ~ VARIABLE, scales = "free")
}

check_change_median <- function(){

  smooth_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.3 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- round(qnt[1] - H)
    y[x > (qnt[2] + H)] <- round(qnt[2] + H)
    y
  }

  dat <- indicators_change %>%
    dplyr::filter(GEOGRAPHY_TYPE %in% "tract") %>%
    dplyr::filter(!is.na(DATE_RANGE_TYPE)) %>%
    dplyr::filter(stringr::str_detect(MEASURE_TYPE, "MEDIAN")) %>%
    dplyr::filter(stringr::str_detect(MEASURE_TYPE, "APPROPRIATE")) %>%
    dplyr::group_by(VARIABLE, DATE_RANGE) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE),
                  ESTIMATE_NO_OUTLIERS = smooth_outliers(ESTIMATE)) %>%
    dplyr::ungroup()

  dat %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::facet_grid(DATE_RANGE ~ VARIABLE, scales = "free")
}
