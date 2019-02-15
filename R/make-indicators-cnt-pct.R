#' @title Make The Count Indicators
#' @description Make the ACS Inidcators related to a _count_ of a population.
#'   An example of the type of indicator included in this object might be
#'   the count of renter households, while the median rent price would _not_ be included.
#' @param acs_variables desc
#' @param hud_chas_variables desc
#' @param parcel_value_variables desc
#' @param parcel_sales_variables desc
#' @param indicators_cnt_pct_acs_chas desc
#' @param indicators_cnt_pct_value desc
#' @param indicators_cnt_pct_sales desc
#' @param parcel_tract_overlay desc
#' @param county_community_tract_all_metadata desc
#' @param community_metadata desc
#' @param indicator_template desc
#' @return a `tibble`

#' @rdname indicators_cnt_pct
#' @export
make_indicators_cnt_pct <- function(indicators_cnt_pct_acs_chas,
                       indicators_cnt_pct_value,
                       indicators_cnt_pct_sales,
                                    indicator_template){

  # JOIN DATA ---------------------------------------------------------------


  all_cnt_vars <- list(indicators_cnt_pct_acs_chas,
                       indicators_cnt_pct_value,
                       indicators_cnt_pct_sales) %>%
    purrr::map_dfr(c)


  # CALCULATE COUNT AND PERCENT ---------------------------------------------


  indicator_values <- all_cnt_vars %>%
    dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE)) %>%
    dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL, -VARIABLE_SUBTOTAL_DESC, -ESTIMATE, -MOE)) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!VARIABLE_ROLE %in% "OMIT") %>%
    dplyr::filter(!is.na(GEOGRAPHY_ID)) %>% # there are missing GEOIDS in the assessor data
    tidyr::gather(TYPE, VALUE, ESTIMATE, MOE) %>%
    tidyr::unite(PROP_TYPE, VARIABLE_ROLE, TYPE) %>%
    tidyr::spread(PROP_TYPE, VALUE) %>%
    dplyr::group_by_at(dplyr::vars(-COUNT_ESTIMATE,-COUNT_MOE,-TOTAL_ESTIMATE,-TOTAL_MOE)) %>%
    dplyr::summarise(COUNT_ESTIMATE,
                     COUNT_MOE,
                     TOTAL_ESTIMATE ,
                     TOTAL_MOE,
                     PERCENT_ESTIMATE = dplyr::case_when(
                       COUNT_ESTIMATE <= 0 ~ 0,
                       TOTAL_ESTIMATE <= 0 ~ NA_real_,
                       COUNT_ESTIMATE/TOTAL_ESTIMATE > 1 ~ 1,
                       TRUE ~ COUNT_ESTIMATE/TOTAL_ESTIMATE
                     ),
                     PERCENT_MOE = tidycensus::moe_prop(
                       num = COUNT_ESTIMATE,
                       denom = TOTAL_ESTIMATE,
                       moe_num = COUNT_MOE,
                       moe_denom = TOTAL_MOE)
    ) %>%
    dplyr::ungroup()

  # CONVERT TO LONG FORMAT --------------------------------------------------

  indicator_values_long <- indicator_values %>%
    tidyr::gather(MEASURE_TYPE, VALUE, dplyr::matches("ESTIMATE|MOE")) %>%
    tidyr::separate(MEASURE_TYPE, into = c("MEASURE_TYPE","EST_OR_MOE"), sep = "_") %>%
    tidyr::spread(EST_OR_MOE, VALUE)

  skim_inds_long <- function(){
    indicator_values_long %>% dplyr::group_by(GEOGRAPHY_TYPE, MEASURE_TYPE, VARIABLE_DESC) %>% dplyr::select(ESTIMATE) %>% skimr::skim()
  }

  # REDEFINE VARIABLE DESC COLUMN ------------------------------------------------


  # create unique, human-readable variable names

  indicator_variable_desc <- indicator_values_long %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_c(MEASURE_TYPE, VARIABLE_DESC, sep = "_")
    )

  # REFORMAT ----------------------------------------------------------------



  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicator_values_ready <- indicator_template %>%
    dplyr::full_join(indicator_variable_desc,
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

  indicators_cnt_pct <- indicator_values_ready

  return(indicators_cnt_pct)

}

show_hist_facet_indicators_cnt_pct_year <- function(){

  if(!exists("indicators_cnt_pct")){stop("'indicators_cnt_pct' doesn't exist\nTry loading it with 'loadd(indicators_cnt_pct)'.")}


  dat_no_outliers <- indicators_cnt_pct %>%
    dplyr::mutate(ESTIMATE = dplyr::case_when(
      INDICATOR %in% "SALE_RATE" & ESTIMATE > 0.4 ~ NA_real_, # remove upper outliers
      TRUE ~ ESTIMATE
    ))


  dat_no_outliers %>%
    dplyr::filter(MEASURE_TYPE %in% "PERCENT") %>%
    dplyr::filter(DATE_RANGE_TYPE %in% c("five years","three years","one year")) %>%
    dplyr::mutate(LABEL = stringr::str_replace(VARIABLE_DESC,"PERCENT","%")) %>%
    dplyr::group_by(DATE_GROUP_ID, LABEL) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(DATE_GROUP_ID ~ LABEL, scales = "free_x")

}

show_hist_facet_indicators_cnt_pct_qtr <- function(){

  if(!exists("indicators_cnt_pct")){stop("'indicators_cnt_pct' doesn't exist\nTry loading it with 'loadd(indicators_cnt_pct)'.")}

  ind_cnt_pct_no_outliers <- indicators_cnt_pct %>%
    dplyr::filter(ESTIMATE <= .15)


  ind_cnt_pct_no_outliers %>%
    dplyr::filter(MEASURE_TYPE %in% "PERCENT") %>%
    dplyr::filter(DATE_RANGE_TYPE %in% c("one quarter")) %>%
    dplyr::mutate(LABEL = VARIABLE_DESC) %>%
    dplyr::group_by(DATE_GROUP_ID, LABEL) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(DATE_GROUP_ID ~ LABEL, scales = "free_x")

}


#' @rdname indicators_cnt_pct
#' @export
make_indicators_cnt_pct_acs_chas <- function(acs_variables,
                                             hud_chas_variables,
                                             county_community_tract_all_metadata,
                                             community_metadata){



  # PREPARE DATA: ACS --------------------------------------------------------

  acs_cnt <- acs_variables %>%
    dplyr::filter(MEASURE_TYPE %in% "COUNT")

  acs_community_cnt <- acs_cnt %>%
    dplyr::left_join(community_metadata, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(GEOGRAPHY_ID = GEOGRAPHY_COMMUNITY_ID,
                  GEOGRAPHY_ID_TYPE = GEOGRAPHY_COMMUNITY_ID_TYPE,
                  GEOGRAPHY_NAME = GEOGRAPHY_COMMUNITY_NAME,
                  GEOGRAPHY_TYPE = GEOGRAPHY_COMMUNITY_TYPE) %>%
    dplyr::group_by(SOURCE,
                    GEOGRAPHY_ID,
                    GEOGRAPHY_ID_TYPE,
                    GEOGRAPHY_TYPE,
                    GEOGRAPHY_NAME,
                    VARIABLE,
                    VARIABLE_DESC,
                    VARIABLE_ROLE,
                    INDICATOR,
                    DATE_BEGIN,
                    DATE_END,
                    DATE_GROUP_ID,
                    DATE_RANGE,
                    DATE_RANGE_TYPE) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup()


  # PREPARE DATA: CHAS --------------------------------------------------------

  chas_cnt <- hud_chas_variables %>%
    dplyr::filter(MEASURE_TYPE %in% "COUNT") # unnecessary step because they are all COUNT but I'm leaving it for clarity's sake

  chas_community_cnt <- chas_cnt %>%
    dplyr::left_join(community_metadata, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(GEOGRAPHY_ID = GEOGRAPHY_COMMUNITY_ID,
                  GEOGRAPHY_ID_TYPE = GEOGRAPHY_COMMUNITY_ID_TYPE,
                  GEOGRAPHY_NAME = GEOGRAPHY_COMMUNITY_NAME,
                  GEOGRAPHY_TYPE = GEOGRAPHY_COMMUNITY_TYPE) %>%
    dplyr::group_by(SOURCE,
                    GEOGRAPHY_ID,
                    GEOGRAPHY_ID_TYPE,
                    GEOGRAPHY_TYPE,
                    GEOGRAPHY_NAME,
                    VARIABLE,
                    VARIABLE_DESC,
                    VARIABLE_ROLE,
                    INDICATOR,
                    DATE_BEGIN,
                    DATE_END,
                    DATE_GROUP_ID,
                    DATE_RANGE,
                    DATE_RANGE_TYPE) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup()



  # JOIN --------------------------------------------------------------------

  indicators_cnt_pct_acs_chas_ready <- list(acs_cnt,
                                            acs_community_cnt,
                                            chas_cnt,
                                            chas_community_cnt) %>%
    purrr::map_dfr(c)

  # REFORMAT ----------------------------------------------------------------

  # note: there is no need to reformat this object - that will happen in make_indicators_cnt_pct()

  # RETURN ------------------------------------------------------------------

  indicators_cnt_pct_acs_chas <- indicators_cnt_pct_acs_chas_ready

  return(indicators_cnt_pct_acs_chas)

}

#' @rdname indicators_cnt_pct
#' @export
make_indicators_cnt_pct_value <- function(parcel_value_variables,
                                          parcel_tract_overlay,
                                          county_community_tract_all_metadata,
                                          community_metadata){


  # PREPARE DATA: ASSESSED VALUES --------------------------------------------------



  # Note: there is an issue with the condo record PINs.
  #  In order to successfully join the parcels to census tracts,
  #  the 2005 condo PINs need to be converted from their condo unit PIN
  #  to the condo complex PIN. This is done by replacing the last four
  #  digits of the unit PIN with "0000".

  convert_to_complex_pin <- function(x){stringr::str_replace(x,".{4}$","0000")}

  parcel_value_cnt <- parcel_value_variables %>%
    dplyr::mutate(GEOGRAPHY_ID_JOIN = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "condo" ~ convert_to_complex_pin(GEOGRAPHY_ID),
      TRUE ~ GEOGRAPHY_ID
    )) %>%
    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID_JOIN = "PIN")) %>%
    dplyr::select(-GEOGRAPHY_ID_JOIN) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = GEOID,
                  VARIABLE = stringr::str_c("SR_",stringr::str_extract(VARIABLE,"ALL|SF|CONDO")),
                  VARIABLE_DESC = stringr::str_c("SALE_RATE_",stringr::str_extract(VARIABLE,"ALL|SF|CONDO")),
                  INDICATOR = "SALE_RATE",
                  MOE = 0L,
                  ESTIMATE = dplyr::if_else(VARIABLE_ROLE %in% c("include"),1L,0L),
                  MEASURE_TYPE = "COUNT") %>%
    dplyr::select(-GEOID,-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE,  -dplyr::matches("^META")) %>%
    dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

  parcel_value_community_cnt <- parcel_value_cnt %>%
    dplyr::left_join(community_metadata, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(GEOGRAPHY_ID = GEOGRAPHY_COMMUNITY_ID,
                  GEOGRAPHY_ID_TYPE = GEOGRAPHY_COMMUNITY_ID_TYPE,
                  GEOGRAPHY_NAME = GEOGRAPHY_COMMUNITY_NAME,
                  GEOGRAPHY_TYPE = GEOGRAPHY_COMMUNITY_TYPE)

  parcel_value_county_cnt <- parcel_value_cnt %>%
    dplyr::mutate(GEOGRAPHY_ID = "53033") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE) %>%
    dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")



  # SUMMARIZE BY 3-YEAR SPAN ------------------------------------------------

  three_year_fields <- tibble::tribble(
    ~DATE_GROUP_ID, ~DATE_BEGIN, ~DATE_END, ~DATE_RANGE, ~DATE_RANGE_TYPE,
    "2013_2015", "2013-01-01", "2015-12-31", "20130101_20151231", "three years",
    "2016_2018", "2016-01-01", "2018-12-31", "20160101_20181231", "three years"
  )

  is_between_dates <- function(x, begin, end){

    dplyr::between(lubridate::ymd(x), lubridate::ymd(begin), lubridate::ymd(end))

  }

  summarize_by_3year <- function(x, variable_role){

    p_3year_only <- x %>%
      dplyr::mutate(DATE_GROUP_ID = dplyr::case_when(
        is_between_dates(DATE_BEGIN, "2013-01-01", "2015-12-31") ~ "2013_2015",
        is_between_dates(DATE_BEGIN, "2016-01-01", "2018-12-31") ~ "2016_2018",
        TRUE ~ NA_character_
      )) %>%
      dplyr::filter(! is.na(DATE_GROUP_ID)) %>% # drop sales outside of the two 3-year spans
      dplyr::select(-DATE_BEGIN, -DATE_END, -DATE_RANGE, -DATE_RANGE_TYPE) %>% # drop the original date fields
      dplyr::left_join(three_year_fields, by = "DATE_GROUP_ID")  # add the new replacement 3-year span date fields

    p_summary_by_3year <- p_3year_only %>%
      dplyr::group_by(SOURCE,
                      GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      VARIABLE,
                      VARIABLE_DESC,
                      INDICATOR,
                      DATE_BEGIN,
                      DATE_END,
                      DATE_GROUP_ID,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }


  parcel_value_cnt_3year <- summarize_by_3year(parcel_value_cnt,
                                               variable_role = "TOTAL")

  parcel_value_community_cnt_3year <- summarize_by_3year(parcel_value_community_cnt,
                                                         variable_role = "TOTAL")

  parcel_value_county_cnt_3year <- summarize_by_3year(parcel_value_county_cnt,
                                                      variable_role = "TOTAL")



  # SUMMARIZE BY YEAR --------------------------------------------

  summarize_by_year <- function(x, variable_role){

    x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "year"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "year") - 1,
                    DATE_GROUP_ID = DATE_GROUP_ID,
                    DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                    DATE_RANGE_TYPE = "one year") %>%
      dplyr::mutate_if(lubridate::is.Date,as.character) %>%
      dplyr::group_by(SOURCE,
                      GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      VARIABLE,
                      VARIABLE_DESC,
                      INDICATOR,
                      DATE_BEGIN,
                      DATE_END,
                      DATE_GROUP_ID,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }


  parcel_value_cnt_year <- summarize_by_year(parcel_value_cnt,
                                             variable_role = "TOTAL")

  parcel_value_community_cnt_year <- summarize_by_year(parcel_value_community_cnt,
                                                       variable_role = "TOTAL")

  parcel_value_county_cnt_year <- summarize_by_year(parcel_value_county_cnt,
                                                    variable_role = "TOTAL")

  # SUMMARIZE BY QUARTER ----------------------------------------------------

  get_qtr_sequence <- function(date_x, date_y){
    seq(from = lubridate::floor_date(lubridate::ymd(date_x), unit = "year"),
        to = lubridate::ceiling_date(lubridate::ymd(date_y), unit = "year")-1,
        by = "quarter")
  }


  date_cols_qtr_full <- parcel_value_cnt %>%
    dplyr::select(DATE_BEGIN, DATE_END) %>%
    dplyr::distinct() %>%
    dplyr::transmute(QTR_DATE = purrr::map2(DATE_BEGIN, DATE_END,get_qtr_sequence)) %>%
    tidyr::unnest() %>%
    dplyr::transmute(DATE_BEGIN = lubridate::floor_date(lubridate::date(QTR_DATE), unit = "quarter"),
                     DATE_END = lubridate::ceiling_date(lubridate::date(QTR_DATE), unit = "quarter") - 1,
                     DATE_GROUP_ID = create_range_quarter(DATE_BEGIN, DATE_END),
                     DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                     DATE_RANGE_TYPE = "one quarter") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)


  summarize_by_quarter <- function(x, variable_role){

    summary_by_qtr_Q4_only <- x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "quarter"),
                     DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "quarter") - 1,
                     DATE_GROUP_ID = create_range_quarter(DATE_BEGIN, DATE_END),
                     DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                    DATE_RANGE_TYPE = "one quarter") %>%
      dplyr::mutate_if(lubridate::is.Date,as.character) %>%
      dplyr::group_by(SOURCE,
                      GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      VARIABLE,
                      VARIABLE_DESC,
                      INDICATOR,
                      DATE_GROUP_ID) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    replace_q4 <- function(x, q){
      x %>% dplyr::mutate( DATE_GROUP_ID = stringr::str_replace_all(DATE_GROUP_ID, "Q4",q))
    }

    summary_by_qtr_all <- list("Q1", "Q2", "Q3") %>%
      purrr::map_dfr(~replace_q4(summary_by_qtr_Q4_only, q = .x)) %>%
      dplyr::bind_rows(summary_by_qtr_Q4_only) %>%
      dplyr::left_join(date_cols_qtr_full, by = "DATE_GROUP_ID")

    return(summary_by_qtr_all)
  }


  parcel_value_cnt_qtr <- summarize_by_quarter(parcel_value_cnt,
                                               variable_role = "TOTAL")

  parcel_value_community_cnt_qtr <- summarize_by_quarter(parcel_value_community_cnt,
                                                         variable_role = "TOTAL")

  parcel_value_county_cnt_qtr <- summarize_by_quarter(parcel_value_county_cnt,
                                                      variable_role = "TOTAL")



  # JOIN --------------------------------------------------------------------

  indicators_cnt_pct_value_ready <- list(parcel_value_cnt_3year,
                                       parcel_value_community_cnt_3year,
                                       parcel_value_county_cnt_3year,
                                       parcel_value_cnt_year,
                                       parcel_value_community_cnt_year,
                                       parcel_value_county_cnt_year,
                                       parcel_value_cnt_qtr,
                                       parcel_value_community_cnt_qtr,
                                       parcel_value_county_cnt_qtr) %>%
    purrr::map_dfr(c)


  # REFORMAT ----------------------------------------------------------------

  # note: there is no need to reformat this object - that will happen in make_indicators_cnt_pct()

  # RETURN ------------------------------------------------------------------

  indicators_cnt_pct_value <- indicators_cnt_pct_value_ready

  return(indicators_cnt_pct_value)

}

#' @rdname indicators_cnt_pct
#' @export
make_indicators_cnt_pct_sales <- function(parcel_sales_variables,
                                          parcel_tract_overlay,
                                          county_community_tract_all_metadata,
                                          community_metadata){
  # PREPARE DATA: SALES --------------------------------------------------

  # Note: there is an issue with the condo record PINs.
  #  In order to successfully join the parcels to census tracts,
  #  the 2005 condo PINs need to be converted from their condo unit PIN
  #  to the condo complex PIN. This is done by replacing the last four
  #  digits of the unit PIN with "0000".

  convert_to_complex_pin <- function(x){stringr::str_replace(x,".{4}$","0000")}

  parcel_sales_cnt <- parcel_sales_variables %>%
    dplyr::mutate(GEOGRAPHY_ID_JOIN = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "condo" ~ convert_to_complex_pin(GEOGRAPHY_ID),
      TRUE ~ GEOGRAPHY_ID
    )) %>%
    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID_JOIN = "PIN")) %>%
    dplyr::select(-GEOGRAPHY_ID_JOIN) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = GEOID,
                  VARIABLE = stringr::str_c("SR_",stringr::str_extract(VARIABLE,"ALL|SF|CONDO")),
                  VARIABLE_DESC = stringr::str_c("SALE_RATE_",stringr::str_extract(VARIABLE,"ALL|SF|CONDO")),
                  INDICATOR = "SALE_RATE",
                  MOE = 0L,
                  ESTIMATE = dplyr::if_else(VARIABLE_ROLE %in% c("include"),1L,0L),
                  MEASURE_TYPE = "COUNT") %>%
    dplyr::select(-GEOID,-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE,  -dplyr::matches("^META")) %>%
    dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

  parcel_sales_community_cnt <- parcel_sales_cnt %>%
    dplyr::left_join(community_metadata, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(GEOGRAPHY_ID = GEOGRAPHY_COMMUNITY_ID,
                  GEOGRAPHY_ID_TYPE = GEOGRAPHY_COMMUNITY_ID_TYPE,
                  GEOGRAPHY_NAME = GEOGRAPHY_COMMUNITY_NAME,
                  GEOGRAPHY_TYPE = GEOGRAPHY_COMMUNITY_TYPE)

  parcel_sales_county_cnt <- parcel_sales_cnt %>%
    dplyr::mutate(GEOGRAPHY_ID = "53033") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE) %>%
    dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")




  # SUMMARIZE BY 3-YEAR SPAN ------------------------------------------------


  three_year_fields <- tibble::tribble(
    ~DATE_GROUP_ID, ~DATE_BEGIN, ~DATE_END, ~DATE_RANGE, ~DATE_RANGE_TYPE,
    "2013_2015", "2013-01-01", "2015-12-31", "20130101_20151231", "three years",
    "2016_2018", "2016-01-01", "2018-12-31", "20160101_20181231", "three years"
  )

  is_between_dates <- function(x, begin, end){

    dplyr::between(lubridate::ymd(x), lubridate::ymd(begin), lubridate::ymd(end))

  }

  summarize_by_3year <- function(x, variable_role){

    p_3year_only <- x %>%
      dplyr::mutate(DATE_GROUP_ID = dplyr::case_when(
        is_between_dates(DATE_BEGIN, "2013-01-01", "2015-12-31") ~ "2013_2015",
        is_between_dates(DATE_BEGIN, "2016-01-01", "2018-12-31") ~ "2016_2018",
        TRUE ~ NA_character_
      )) %>%
      dplyr::filter(! is.na(DATE_GROUP_ID)) %>% # drop sales outside of the two 3-year spans
      dplyr::select(-DATE_BEGIN, -DATE_END, -DATE_RANGE, -DATE_RANGE_TYPE) %>% # drop the original date fields
      dplyr::left_join(three_year_fields, by = "DATE_GROUP_ID")  # add the new replacement 3-year span date fields

    p_summary_by_3year <- p_3year_only %>%
      dplyr::group_by(SOURCE,
                      GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      VARIABLE,
                      VARIABLE_DESC,
                      INDICATOR,
                      DATE_BEGIN,
                      DATE_END,
                      DATE_GROUP_ID,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    return(p_summary_by_3year)
  }

  parcel_sales_cnt_3year <- summarize_by_3year(parcel_sales_cnt,
                                               variable_role = "COUNT")

  parcel_sales_community_cnt_3year <- summarize_by_3year(parcel_sales_community_cnt,
                                                         variable_role = "COUNT")

  parcel_sales_county_cnt_3year <- summarize_by_3year(parcel_sales_county_cnt,
                                                      variable_role = "COUNT")


  # SUMMARIZE BY YEAR --------------------------------------------


  summarize_by_year <- function(x, variable_role){

    x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "year"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "year") - 1,
                    DATE_GROUP_ID = DATE_GROUP_ID,
                    DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                    DATE_RANGE_TYPE = "one year") %>%
      dplyr::mutate_if(lubridate::is.Date,as.character) %>%
      dplyr::group_by(SOURCE,
                      GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      VARIABLE,
                      VARIABLE_DESC,
                      INDICATOR,
                      DATE_BEGIN,
                      DATE_END,
                      DATE_GROUP_ID,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }


  parcel_sales_cnt_year <- summarize_by_year(parcel_sales_cnt,
                                             variable_role = "COUNT")

  parcel_sales_community_cnt_year <- summarize_by_year(parcel_sales_community_cnt,
                                                       variable_role = "COUNT")

  parcel_sales_county_cnt_year <- summarize_by_year(parcel_sales_county_cnt,
                                                    variable_role = "COUNT")


  # SUMMARIZE BY QUARTER ----------------------------------------------------


  get_qtr_sequence <- function(date_x, date_y){
    seq(from = lubridate::floor_date(lubridate::ymd(date_x), unit = "year"),
        to = lubridate::ceiling_date(lubridate::ymd(date_y), unit = "year")-1,
        by = "quarter")
  }

  date_cols_qtr_full <- parcel_sales_cnt %>%
    dplyr::select(DATE_BEGIN, DATE_END) %>%
    dplyr::distinct() %>%
    dplyr::transmute(QTR_DATE = purrr::map2(DATE_BEGIN, DATE_END,get_qtr_sequence)) %>%
    tidyr::unnest() %>%
    dplyr::transmute(DATE_BEGIN = lubridate::floor_date(lubridate::date(QTR_DATE), unit = "quarter"),
                     DATE_END = lubridate::ceiling_date(lubridate::date(QTR_DATE), unit = "quarter") - 1,
                     DATE_GROUP_ID = create_range_quarter(DATE_BEGIN, DATE_END),
                     DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                     DATE_RANGE_TYPE = "one quarter") %>%
    dplyr::distinct() %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)



  summarize_by_quarter <- function(x, date_cols_qtr_full, variable_role){

    summary_by_qtr <- x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "quarter"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "quarter") - 1,
                    DATE_GROUP_ID = create_range_quarter(DATE_BEGIN, DATE_END),
                    DATE_RANGE = create_range_date(DATE_BEGIN, DATE_END),
                    DATE_RANGE_TYPE = "one quarter") %>%
      dplyr::mutate_if(lubridate::is.Date,as.character) %>%
      dplyr::group_by(SOURCE,
                      GEOGRAPHY_ID,
                      GEOGRAPHY_ID_TYPE,
                      VARIABLE,
                      VARIABLE_DESC,
                      INDICATOR,
                      DATE_BEGIN,
                      DATE_END,
                      DATE_GROUP_ID,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    # it is possible that not every combination of DATE_GROUP_ID and quarter will be present
    #  if that is the case, use dplyr::expand() to add the missing combinations

    summary_by_qtr_complete <- summary_by_qtr %>%  # expand the data to include all DATE_GROUP_ID values (e.g.,"2005Q2_2005Q2") in the data
      tidyr::expand(tidyr::nesting(SOURCE,
                                   GEOGRAPHY_ID,
                                   VARIABLE,
                                   VARIABLE_DESC,
                                   INDICATOR,
                                   VARIABLE_ROLE,
                                   GEOGRAPHY_ID_TYPE,
                                   GEOGRAPHY_NAME,
                                   GEOGRAPHY_TYPE),
                    DATE_GROUP_ID) %>%
      dplyr::left_join(date_cols_qtr_full, by = "DATE_GROUP_ID") %>%
      dplyr::left_join(summary_by_qtr, by = c("SOURCE",
                                              "GEOGRAPHY_ID",
                                              "VARIABLE",
                                              "VARIABLE_DESC",
                                              "INDICATOR",
                                              "VARIABLE_ROLE",
                                              "GEOGRAPHY_ID_TYPE",
                                              "GEOGRAPHY_NAME",
                                              "GEOGRAPHY_TYPE",
                                              "DATE_GROUP_ID",
                                              "DATE_BEGIN",
                                              "DATE_END",
                                              "DATE_RANGE",
                                              "DATE_RANGE_TYPE")) %>%
      tidyr::replace_na(list(ESTIMATE = 0,
                             MOE = 0))


    return(summary_by_qtr_complete)
  }


  parcel_sales_cnt_qtr <- summarize_by_quarter(parcel_sales_cnt,
                                               date_cols_qtr_full,
                                               variable_role = "COUNT")

  parcel_sales_community_cnt_qtr <- summarize_by_quarter(parcel_sales_community_cnt,
                                                         date_cols_qtr_full,
                                                         variable_role = "COUNT")

  parcel_sales_county_cnt_qtr <- summarize_by_quarter(parcel_sales_county_cnt,
                                                      date_cols_qtr_full,
                                                      variable_role = "COUNT")



  # JOIN --------------------------------------------------------------------

  indicators_cnt_pct_sales_ready <- list(parcel_sales_cnt_3year,
                                  parcel_sales_community_cnt_3year,
                                  parcel_sales_county_cnt_3year,
                                  parcel_sales_cnt_year,
                                  parcel_sales_community_cnt_year,
                                  parcel_sales_county_cnt_year,
                                  parcel_sales_cnt_qtr,
                                  parcel_sales_community_cnt_qtr,
                                  parcel_sales_county_cnt_qtr) %>%
    purrr::map_dfr(c)



# REFORMAT ----------------------------------------------------------------

  # note: there is no need to reformat this object - that will happen in make_indicators_cnt_pct()

  # RETURN ------------------------------------------------------------------

  indicators_cnt_pct_sales <- indicators_cnt_pct_sales_ready

  return(indicators_cnt_pct_sales)

}

