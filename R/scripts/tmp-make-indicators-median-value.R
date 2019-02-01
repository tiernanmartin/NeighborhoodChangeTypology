#' @rdname indicators_median
#' @export
make_indicators_median_value <- function(parcel_value_variables,
                                         parcel_tract_overlay,
                                         county_community_tract_all_metadata,
                                         community_metadata,
                                         indicator_template){


  # PREPARE DATA --------------------------------------------------------

   parcel_value_median <- parcel_value_variables %>%
    dplyr::mutate(GEOGRAPHY_ID_JOIN = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "condo" ~ convert_to_complex_pin(GEOGRAPHY_ID),
      TRUE ~ GEOGRAPHY_ID
    )) %>%
    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID_JOIN = "PIN")) %>% # filter out parcels whose PINs don't match any tract GEOID
    dplyr::select(-GEOGRAPHY_ID_JOIN)  %>%
    dplyr::mutate(GEOGRAPHY_ID = GEOID,
                  GEOGRAPHY_ID_TYPE = "GEOID",
                  GEOGRAPHY_TYPE = "tract",
                  MEASURE_TYPE = "MEDIAN") %>%
    dplyr::select(-GEOID, -dplyr::matches("^META"))

  community_value_median <- parcel_value_median %>%
    dplyr::left_join(community_metadata, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(GEOGRAPHY_ID = GEOGRAPHY_COMMUNITY_ID,
                  GEOGRAPHY_ID_TYPE = GEOGRAPHY_COMMUNITY_ID_TYPE,
                  GEOGRAPHY_NAME = GEOGRAPHY_COMMUNITY_NAME,
                  GEOGRAPHY_TYPE = GEOGRAPHY_COMMUNITY_TYPE) %>%
    dplyr::select(-dplyr::matches("_COMMUNITY"))

 # create duplicates of the parcel variables that will be summarized
  # at the county level (instead of the tract level)

  county_value_median <- parcel_value_median %>%
    dplyr::mutate(GEOGRAPHY_ID = "53033") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE) %>%
    dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")



# SUMMARIZE BY YEAR -------------------------------------------------------

 summarize_by_year <- function(x){
    x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "year"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "year") - 1,
                    DATE_END_YEAR = DATE_END_YEAR,
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                    DATE_RANGE_TYPE = "one year") %>%
      dplyr::mutate_if(lubridate::is.Date,as.character) %>%
      dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL,-VARIABLE_SUBTOTAL_DESC,-ESTIMATE,-MOE)) %>%
      dplyr::summarise(ESTIMATE = as.integer(round(median(ESTIMATE, na.rm = TRUE),0)),
                       N = n(),
                       NAS = sum(is.na(ESTIMATE)),
                       MOE = NA_real_) %>%
      dplyr::ungroup() %>%
      dplyr::select(-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE, -GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")
  }

  parcel_value_median_year <- summarize_by_year(parcel_value_median)

  parcel_value_community_median_year <- summarize_by_year(community_value_median)

  parcel_value_county_median_year <- summarize_by_year(county_value_median)


# SUMMARIZE BY QUARTER ----------------------------------------------------

get_year_quarter <- function(x){
    stringr::str_c(lubridate::year(x),"_","Q",lubridate::quarter(x))
  }

  get_qtr_sequence <- function(x){
    seq(from = lubridate::ymd(stringr::str_c(x,"01-01")),
        to = lubridate::ymd(stringr::str_c(x,"12-31")),
        by = "quarter")
  }

  date_cols_qtr_full <- parcel_value_median %>%
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



  summarize_by_quarter <- function(x, date_cols_qtr_full){

     summary_by_qtr_Q4_only <- x %>%
      dplyr::mutate(DATE_BEGIN = lubridate::floor_date(lubridate::date(DATE_BEGIN), unit = "quarter"),
                    DATE_END = lubridate::ceiling_date(lubridate::date(DATE_BEGIN), unit = "quarter") - 1,
                    DATE_END_YEAR = get_year_quarter(DATE_BEGIN),
                    DATE_RANGE = create_daterange(DATE_BEGIN, DATE_END),
                    DATE_RANGE_TYPE = "one quarter") %>%
      dplyr::mutate_if(lubridate::is.Date,as.character) %>%
      dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL,-VARIABLE_SUBTOTAL_DESC,-ESTIMATE,-MOE)) %>%
      dplyr::summarise(ESTIMATE = as.integer(round(median(ESTIMATE, na.rm = TRUE),0)),
                       N = n(),
                       NAS = sum(is.na(ESTIMATE)),
                       MOE = NA_real_) %>%
      dplyr::ungroup() %>%
      dplyr::select(-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE, -GEOGRAPHY_ID_TYPE) %>%
      dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")

    replace_q4 <- function(x, q){
      x %>% dplyr::mutate( DATE_END_YEAR = stringr::str_replace(DATE_END_YEAR, "Q4",q))
    }

    summary_by_qtr_all <- list("Q1", "Q2", "Q3") %>%
      purrr::map_dfr(~replace_q4(summary_by_qtr_Q4_only, q = .x)) %>%
      dplyr::bind_rows(summary_by_qtr_Q4_only) %>%
      dplyr::left_join(date_cols_qtr_full, by = "DATE_END_YEAR")

    return(summary_by_qtr_all)
  }


  parcel_value_median_qtr <- summarize_by_quarter(parcel_value_median,
                                               date_cols_qtr_full)

  parcel_value_community_median_qtr <- summarize_by_quarter(community_value_median,
                                                         date_cols_qtr_full)

  parcel_value_county_median_qtr <- summarize_by_quarter(county_value_median,
                                                      date_cols_qtr_full)


  # JOIN --------------------------------------------------------

  median_value_year_qtr <- list(parcel_value_median_year,
                                 parcel_value_community_median_year,
                                 parcel_value_county_median_year,
                                 parcel_value_median_qtr,
                                 parcel_value_community_median_qtr,
                                 parcel_value_county_median_qtr) %>%
    purrr::map_dfr(c) %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_replace(VARIABLE_DESC,"^VALUE","MEDIAN")) %>%
    dplyr::filter(VARIABLE_ROLE %in% "include") %>%
    dplyr::select(-dplyr::matches("VARIABLE_SUBTOTAL|VARIABLE_ROLE"))

  # RETURN ------------------------------------------------------------------

  indicators_median_value <- median_value_year_qtr

  return(indicators_median_value)



}

