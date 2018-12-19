#' @title Make The Count Indicators
#' @description Make the ACS Inidcators related to a _count_ of a population.
#'   An example of the type of indicator included in this object might be
#'   the count of renter households, while the median rent price would _not_ be included.
#' @param acs_variables desc
#' @param hud_chas_variables desc
#' @param parcel_value_variables desc
#' @param parcel_sales_variables desc
#' @param parcel_tract_overlay desc
#' @param county_tract_all_metadata desc
#' @param indicator_template desc
#' @return a `tibble`
#' @export
make_indicators_cnt_pct <- function(acs_variables,
                                    hud_chas_variables,
                                    parcel_value_variables,
                                    parcel_sales_variables,
                                    parcel_tract_overlay,
                                    county_tract_all_metadata,
                                    indicator_template){



  # PREPARE DATA --------------------------------------------------------

  acs_cnt <- acs_variables %>%
    dplyr::filter(MEASURE_TYPE %in% "COUNT")

  chas_cnt <- hud_chas_variables %>%
    dplyr::filter(MEASURE_TYPE %in% "COUNT") # unnecessary step because they are all COUNT but I'm leaving it for clarity's sake

  parcel_value_cnt <- parcel_value_variables %>%
    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID = "PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = GEOID,
                  VARIABLE = stringr::str_c("SALE_RATE_",stringr::str_extract(VARIABLE,"ALL|SF_ONLY|CONDO_ONLY")),
                  INDICATOR = "SALE_RATE",
                  MOE = 0L,
                  ESTIMATE = dplyr::if_else(VARIABLE_ROLE %in% c("include"),1L,0L), # count of included parcels (single-family criteria)
                  MEASURE_TYPE = "COUNT") %>%
    dplyr::select(-GEOID,-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE,  -dplyr::matches("^META")) %>%
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")

  parcel_value_county_cnt <- parcel_value_cnt %>%
    dplyr::mutate(GEOGRAPHY_ID = "53033") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE) %>%
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")

  all_geog_value_cnt <- list(parcel_value_cnt, parcel_value_county_cnt) %>%
    purrr::map_dfr(c) %>%
    dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE)) %>%
    dplyr::group_by(SOURCE, GEOGRAPHY_ID, GEOGRAPHY_ID_TYPE, VARIABLE, INDICATOR, ENDYEAR) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(VARIABLE_ROLE = "TOTAL") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")


  # pick up here
  parcel_sales_cnt <- parcel_sales_variables %>%
    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID = "PIN")) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = GEOID,
                  GEOGRAPHY_ID_TYPE = "tract",
                  VARIABLE = stringr::str_c("SALE_RATE_",stringr::str_extract(VARIABLE,"ALL|SF_ONLY|CONDO_ONLY")),
                  INDICATOR = "SALE_RATE",
                  MOE = 0L,
                  ESTIMATE = dplyr::if_else(VARIABLE_ROLE %in% c("include"),1L,0L), # count of included sales (single-family criteria)
                  MEASURE_TYPE = "COUNT") %>%
    dplyr::select(-GEOID,-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE,  -dplyr::matches("^META")) %>%
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")


  parcel_sales_county_cnt <- parcel_sales_cnt %>%
    dplyr::mutate(GEOGRAPHY_ID = "53033") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE) %>%
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")

  all_geog_sales_cnt <- list(parcel_sales_cnt, parcel_sales_county_cnt) %>%
    purrr::map_dfr(c) %>%
    dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE)) %>%
    dplyr::group_by(SOURCE, GEOGRAPHY_ID, GEOGRAPHY_ID_TYPE, GEOGRAPHY_TYPE, GEOGRAPHY_NAME, VARIABLE, INDICATOR, ENDYEAR) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(VARIABLE_ROLE = "COUNT")

  all_geog_value_sales_cnt <- list(all_geog_value_cnt,
                                 all_geog_sales_cnt) %>%
    purrr::map_dfr(c)


  # JOIN DATA ---------------------------------------------------------------


  all_cnt_vars <- list(acs_cnt, chas_cnt, all_geog_value_sales_cnt) %>%
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
                     PROPORTION_ESTIMATE = dplyr::case_when(
                       COUNT_ESTIMATE <= 0 ~ 0,
                       TOTAL_ESTIMATE <= 0 ~ 0,
                       COUNT_ESTIMATE/TOTAL_ESTIMATE > 1 ~ 1,
                       TRUE ~ COUNT_ESTIMATE/TOTAL_ESTIMATE
                     ),
                     PROPORTION_MOE = tidycensus::moe_prop(
                       num = COUNT_ESTIMATE,
                       denom = TOTAL_ESTIMATE,
                       moe_num = COUNT_MOE,
                       moe_denom = TOTAL_MOE)
    ) %>%
    dplyr::ungroup()

  indicator_values_long <- indicator_values %>%
    tidyr::gather(MEASURE_TYPE, VALUE, dplyr::matches("ESTIMATE|MOE")) %>%
    tidyr::separate(MEASURE_TYPE, into = c("MEASURE_TYPE","EST_OR_MOE"), sep = "_") %>%
    tidyr::spread(EST_OR_MOE, VALUE)

  skim_inds_long <- function(){
    indicator_values_long %>% dplyr::group_by(MEASURE_TYPE, INDICATOR, VARIABLE) %>% dplyr::select(ESTIMATE) %>% skimr::skim()
  }


  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicator_values_ready <- indicator_template %>%
    dplyr::full_join(indicator_values_long,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "ENDYEAR",
                            "INDICATOR",
                            "VARIABLE",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

  indicators_cnt_pct <- indicator_values_ready

  return(indicators_cnt_pct)

}

show_hist_facet_indicators_cnt_pct <- function(){

  if(!exists("indicators_cnt_pct")){stop("'indicators_cnt_pct' doesn't exist\nTry loading it with 'loadd(indicators_cnt_pct)'.")}


  indicators_cnt_pct %>%
    dplyr::filter(MEASURE_TYPE %in% "PROPORTION") %>%
    dplyr::mutate(LABEL = glue::glue("{VARIABLE} ({SOURCE})")) %>%
    dplyr::group_by(ENDYEAR, LABEL) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(ENDYEAR ~ LABEL, scales = "free_x")

}
