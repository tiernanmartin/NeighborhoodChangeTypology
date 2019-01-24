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
make_indicators_cnt_pct <- function(acs_variables,
                                    hud_chas_variables,
                                    parcel_value_variables,
                                    parcel_sales_variables,
                                    parcel_tract_overlay,
                                    county_community_tract_all_metadata,
                                    community_metadata,
                                    indicator_template){
  stop("This command is temporarily disabled")



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
                    DATE_END_YEAR,
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
                    DATE_END_YEAR,
                    DATE_RANGE,
                    DATE_RANGE_TYPE) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup()


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



  # SUMMARIZE DATA: ASSESSED VALUES --------------------------------------------

  # all_geog_value_cnt_day <- list(parcel_value_cnt, parcel_value_community_cnt, parcel_value_county_cnt) %>%
  #   purrr::map_dfr(c) %>%
  #   dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE))

  summarize_by_year <- function(x, variable_role){
    x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "year"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "year") - 1,
                    DATE_END_YEAR = DATE_END_YEAR,
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }


  # summarize by year

  parcel_value_cnt_year <- summarize_by_year(parcel_value_cnt,
                                             variable_role = "TOTAL")

  parcel_value_community_cnt_year <- summarize_by_year(parcel_value_community_cnt,
                                                       variable_role = "TOTAL")

  parcel_value_county_cnt_year <- summarize_by_year(parcel_value_county_cnt,
                                                    variable_role = "TOTAL")

  # summarize by quarter

  get_year_quarter <- function(x){
    stringr::str_c(lubridate::year(x),"_","Q",lubridate::quarter(x))
  }

  summarize_by_quarter <- function(x, variable_role){

    q4 <- x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "quarter"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "quarter") - 1,
                    DATE_END_YEAR = get_year_quarter(DATE_BEGIN),
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    replace_q4 <- function(x, q){
      q4 %>% dplyr::mutate( DATE_END_YEAR = stringr::str_replace(DATE_END_YEAR, "Q4",q))
    }

    q1_to_q4 <- list("Q1", "Q2", "Q3") %>%
      purrr::map_dfr(~replace_q4(q4, .x)) %>%
      dplyr::bind_rows(q4)

    return(q1_to_q4)
  }


  parcel_value_cnt_qtr <- summarize_by_quarter(parcel_value_cnt,
                                               variable_role = "TOTAL")

  parcel_value_community_cnt_qtr <- summarize_by_quarter(parcel_value_community_cnt,
                                                         variable_role = "TOTAL")

  parcel_value_county_cnt_qtr <- summarize_by_quarter(parcel_value_county_cnt,
                                                      variable_role = "TOTAL")

  # join them together

  all_geog_value_year_qtr <- list(parcel_value_cnt_year,
                                  parcel_value_community_cnt_year,
                                  parcel_value_county_cnt_year,
                                  parcel_value_cnt_qtr,
                                  parcel_value_community_cnt_qtr,
                                  parcel_value_county_cnt_qtr) %>%
    purrr::map_dfr(c)


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



  # SUMMARIZE DATA: SALES --------------------------------------------


  summarize_by_year <- function(x, variable_role){
    x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "year"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "year") - 1,
                    DATE_END_YEAR = DATE_END_YEAR,
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }


  # summarize by year

  parcel_sales_cnt_year <- summarize_by_year(parcel_sales_cnt,
                                             variable_role = "COUNT")

  parcel_sales_community_cnt_year <- summarize_by_year(parcel_sales_community_cnt,
                                                       variable_role = "COUNT")

  parcel_sales_county_cnt_year <- summarize_by_year(parcel_sales_county_cnt,
                                                    variable_role = "COUNT")

  # summarize by quarter

  get_year_quarter <- function(x){
    stringr::str_c(lubridate::year(x),"_","Q",lubridate::quarter(x))
  }

  get_qtr_sequence <- function(x){
    seq(from = lubridate::ymd(stringr::str_c(x,"01-01")),
        to = lubridate::ymd(stringr::str_c(x,"12-31")),
        by = "quarter")
  }

  date_cols_qtr_full <- parcel_sales_cnt %>%
    dplyr::select(DATE_END_YEAR) %>%
    dplyr::distinct() %>%
    dplyr::transmute(QTR_DATE = purrr::map(DATE_END_YEAR, get_qtr_sequence)
    )%>%
    tidyr::unnest() %>%
    dplyr::transmute(DATE_BEGIN = lubridate::floor_date(lubridate::date(QTR_DATE), unit = "quarter"),
                     DATE_END = lubridate::ceiling_date(lubridate::date(QTR_DATE), unit = "quarter") - 1,
                     DATE_END_YEAR = get_year_quarter(DATE_BEGIN),
                     DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                     DATE_RANGE_TYPE = "one quarter") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)



  summarize_by_quarter <- function(x, date_cols_qtr_full, variable_role){

    summary_by_qtr <- x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "quarter"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "quarter") - 1,
                    DATE_END_YEAR = get_year_quarter(DATE_BEGIN),
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    # it is possible that not every combination of DATE_END_YEAR and quarter will be present
    #  if that is the case, use dplyr::expand() to add the missing combinations

    summary_by_qtr_complete <- summary_by_qtr %>%
      tidyr::expand(tidyr::nesting(SOURCE,
                            GEOGRAPHY_ID,
                            VARIABLE,
                            VARIABLE_DESC,
                            INDICATOR,
                            VARIABLE_ROLE,
                            GEOGRAPHY_ID_TYPE,
                            GEOGRAPHY_NAME,
                            GEOGRAPHY_TYPE),
                    DATE_END_YEAR) %>%
      dplyr::left_join(date_cols_qtr_full, by = "DATE_END_YEAR") %>%
      dplyr::left_join(summary_by_qtr, by = c("SOURCE", "GEOGRAPHY_ID", "VARIABLE", "VARIABLE_DESC", "INDICATOR", "VARIABLE_ROLE", "GEOGRAPHY_ID_TYPE", "GEOGRAPHY_NAME", "GEOGRAPHY_TYPE", "DATE_END_YEAR", "DATE_BEGIN", "DATE_END", "DATE_RANGE", "DATE_RANGE_TYPE")) %>%
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

  # join them together

  all_geog_sales_year_qtr <- list(parcel_sales_cnt_year,
                                  parcel_sales_community_cnt_year,
                                  parcel_sales_county_cnt_year,
                                  parcel_sales_cnt_qtr,
                                  parcel_sales_community_cnt_qtr,
                                  parcel_sales_county_cnt_qtr) %>%
    purrr::map_dfr(c)


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
                            "DATE_END_YEAR",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  indicators_cnt_pct <- indicator_values_ready

  return(indicators_cnt_pct)

}

show_hist_facet_indicators_cnt_pct <- function(){

  if(!exists("indicators_cnt_pct")){stop("'indicators_cnt_pct' doesn't exist\nTry loading it with 'loadd(indicators_cnt_pct)'.")}


  indicators_cnt_pct %>%
    dplyr::filter(MEASURE_TYPE %in% "PERCENT") %>%
    dplyr::mutate(LABEL = VARIABLE_DESC) %>%
    dplyr::group_by(DATE_END_YEAR, LABEL) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(DATE_END_YEAR ~ LABEL, scales = "free_x")

}


#' @rdname indicators_cnt_pct
#' @export
make_indicators_cnt_pct_acs_chas <- function(acs_variables,
                                             hud_chas_variables,
                                             county_community_tract_all_metadata,
                                             community_metadata,
                                             indicator_template){



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
                    DATE_END_YEAR,
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
                    DATE_END_YEAR,
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

  # RETURN ------------------------------------------------------------------

  indicators_cnt_pct_acs_chas <- indicators_cnt_pct_acs_chas_ready

  return(indicators_cnt_pct_acs_chas)

}

#' @rdname indicators_cnt_pct
#' @export
make_indicators_cnt_pct_value <- function(parcel_value_variables,
                                          parcel_tract_overlay,
                                          county_community_tract_all_metadata,
                                          community_metadata,
                                          indicator_template){


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



  # SUMMARIZE DATA: ASSESSED VALUES --------------------------------------------

  # all_geog_value_cnt_day <- list(parcel_value_cnt, parcel_value_community_cnt, parcel_value_county_cnt) %>%
  #   purrr::map_dfr(c) %>%
  #   dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE))

  summarize_by_year <- function(x, variable_role){
    x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "year"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "year") - 1,
                    DATE_END_YEAR = DATE_END_YEAR,
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }


  # summarize by year

  parcel_value_cnt_year <- summarize_by_year(parcel_value_cnt,
                                             variable_role = "TOTAL")

  parcel_value_community_cnt_year <- summarize_by_year(parcel_value_community_cnt,
                                                       variable_role = "TOTAL")

  parcel_value_county_cnt_year <- summarize_by_year(parcel_value_county_cnt,
                                                    variable_role = "TOTAL")

  # summarize by quarter

  get_year_quarter <- function(x){
    stringr::str_c(lubridate::year(x),"_","Q",lubridate::quarter(x))
  }

  summarize_by_quarter <- function(x, variable_role){

    q4 <- x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "quarter"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "quarter") - 1,
                    DATE_END_YEAR = get_year_quarter(DATE_BEGIN),
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    replace_q4 <- function(x, q){
      q4 %>% dplyr::mutate( DATE_END_YEAR = stringr::str_replace(DATE_END_YEAR, "Q4",q))
    }

    q1_to_q4 <- list("Q1", "Q2", "Q3") %>%
      purrr::map_dfr(~replace_q4(q4, .x)) %>%
      dplyr::bind_rows(q4)

    return(q1_to_q4)
  }


  parcel_value_cnt_qtr <- summarize_by_quarter(parcel_value_cnt,
                                               variable_role = "TOTAL")

  parcel_value_community_cnt_qtr <- summarize_by_quarter(parcel_value_community_cnt,
                                                         variable_role = "TOTAL")

  parcel_value_county_cnt_qtr <- summarize_by_quarter(parcel_value_county_cnt,
                                                      variable_role = "TOTAL")



  # JOIN --------------------------------------------------------------------

  indicators_cnt_pct_value_ready <- list(parcel_value_cnt_year,
                                         parcel_value_community_cnt_year,
                                         parcel_value_county_cnt_year,
                                         parcel_value_cnt_qtr,
                                         parcel_value_community_cnt_qtr,
                                         parcel_value_county_cnt_qtr) %>%
    purrr::map_dfr(c)

  # RETURN ------------------------------------------------------------------

  indicators_cnt_pct_value <- indicators_cnt_pct_value_ready

  return(indicators_cnt_pct_value)

}

#' @rdname indicators_cnt_pct
#' @export
make_indicators_cnt_pct_sales <- function(parcel_sales_variables,
                                          parcel_tract_overlay,
                                          county_community_tract_all_metadata,
                                          community_metadata,
                                          indicator_template){
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



  # SUMMARIZE DATA: SALES --------------------------------------------


  summarize_by_year <- function(x, variable_role){
    x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "year"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "year") - 1,
                    DATE_END_YEAR = DATE_END_YEAR,
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }


  # summarize by year

  parcel_sales_cnt_year <- summarize_by_year(parcel_sales_cnt,
                                             variable_role = "COUNT")

  parcel_sales_community_cnt_year <- summarize_by_year(parcel_sales_community_cnt,
                                                       variable_role = "COUNT")

  parcel_sales_county_cnt_year <- summarize_by_year(parcel_sales_county_cnt,
                                                    variable_role = "COUNT")

  # summarize by quarter

  get_year_quarter <- function(x){
    stringr::str_c(lubridate::year(x),"_","Q",lubridate::quarter(x))
  }

  get_qtr_sequence <- function(x){
    seq(from = lubridate::ymd(stringr::str_c(x,"01-01")),
        to = lubridate::ymd(stringr::str_c(x,"12-31")),
        by = "quarter")
  }

  date_cols_qtr_full <- parcel_sales_cnt %>%
    dplyr::select(DATE_END_YEAR) %>%
    dplyr::distinct() %>%
    dplyr::transmute(QTR_DATE = purrr::map(DATE_END_YEAR, get_qtr_sequence)
    )%>%
    tidyr::unnest() %>%
    dplyr::transmute(DATE_BEGIN = lubridate::floor_date(lubridate::date(QTR_DATE), unit = "quarter"),
                     DATE_END = lubridate::ceiling_date(lubridate::date(QTR_DATE), unit = "quarter") - 1,
                     DATE_END_YEAR = get_year_quarter(DATE_BEGIN),
                     DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                     DATE_RANGE_TYPE = "one quarter") %>%
    dplyr::mutate_if(lubridate::is.Date, as.character)



  summarize_by_quarter <- function(x, date_cols_qtr_full, variable_role){

    summary_by_qtr <- x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "quarter"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "quarter") - 1,
                    DATE_END_YEAR = get_year_quarter(DATE_BEGIN),
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
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
                      DATE_END_YEAR,
                      DATE_RANGE,
                      DATE_RANGE_TYPE) %>%
      dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                       MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VARIABLE_ROLE = variable_role) %>%
      dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    # it is possible that not every combination of DATE_END_YEAR and quarter will be present
    #  if that is the case, use dplyr::expand() to add the missing combinations

    summary_by_qtr_complete <- summary_by_qtr %>%
      tidyr::expand(tidyr::nesting(SOURCE,
                            GEOGRAPHY_ID,
                            VARIABLE,
                            VARIABLE_DESC,
                            INDICATOR,
                            VARIABLE_ROLE,
                            GEOGRAPHY_ID_TYPE,
                            GEOGRAPHY_NAME,
                            GEOGRAPHY_TYPE),
                    DATE_END_YEAR) %>%
      dplyr::left_join(date_cols_qtr_full, by = "DATE_END_YEAR") %>%
      dplyr::left_join(summary_by_qtr, by = c("SOURCE", "GEOGRAPHY_ID", "VARIABLE", "VARIABLE_DESC", "INDICATOR", "VARIABLE_ROLE", "GEOGRAPHY_ID_TYPE", "GEOGRAPHY_NAME", "GEOGRAPHY_TYPE", "DATE_END_YEAR", "DATE_BEGIN", "DATE_END", "DATE_RANGE", "DATE_RANGE_TYPE")) %>%
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

  # join them together

  all_geog_sales_year_qtr <- list(parcel_sales_cnt_year,
                                  parcel_sales_community_cnt_year,
                                  parcel_sales_county_cnt_year,
                                  parcel_sales_cnt_qtr,
                                  parcel_sales_community_cnt_qtr,
                                  parcel_sales_county_cnt_qtr) %>%
    purrr::map_dfr(c)


  # JOIN --------------------------------------------------------------------

  indicators_cnt_pct_sales_ready <- list(parcel_sales_cnt_year,
                                         parcel_sales_community_cnt_year,
                                         parcel_sales_county_cnt_year,
                                         parcel_sales_cnt_qtr,
                                         parcel_sales_community_cnt_qtr,
                                         parcel_sales_county_cnt_qtr) %>%
    purrr::map_dfr(c)

  # RETURN ------------------------------------------------------------------

  indicators_cnt_pct_sales <- indicators_cnt_pct_sales_ready

  return(indicators_cnt_pct_sales)

}
