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
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")

  parcel_value_county_cnt <- parcel_value_cnt %>%
    dplyr::mutate(GEOGRAPHY_ID = "53033") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE) %>%
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")

  all_geog_value_cnt <- list(parcel_value_cnt, parcel_value_county_cnt) %>%
    purrr::map_dfr(c) %>%
    dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE)) %>%
    dplyr::group_by(SOURCE, GEOGRAPHY_ID, GEOGRAPHY_ID_TYPE, VARIABLE,VARIABLE_DESC, INDICATOR, ENDYEAR) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(VARIABLE_ROLE = "TOTAL") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE) %>%
    dplyr::left_join(county_tract_all_metadata, by = "GEOGRAPHY_ID")


  parcel_sales_cnt <- parcel_sales_variables %>%
    dplyr::mutate(GEOGRAPHY_ID_JOIN = dplyr::case_when(
      META_PROPERTY_CATEGORY %in% "condo" ~ convert_to_complex_pin(GEOGRAPHY_ID),
      TRUE ~ GEOGRAPHY_ID
    )) %>%
    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID_JOIN = "PIN")) %>%
    dplyr::select(-GEOGRAPHY_ID_JOIN) %>%
    dplyr::mutate(SOURCE = "ASSESSOR",
                  GEOGRAPHY_ID = GEOID,
                  GEOGRAPHY_ID_TYPE = "tract",
                  VARIABLE = stringr::str_c("SR_",stringr::str_extract(VARIABLE,"ALL|SF|CONDO")),
                  VARIABLE_DESC = stringr::str_c("SALE_RATE_",stringr::str_extract(VARIABLE,"ALL|SF|CONDO")),
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
    dplyr::group_by(SOURCE, GEOGRAPHY_ID, GEOGRAPHY_ID_TYPE, GEOGRAPHY_TYPE, GEOGRAPHY_NAME, VARIABLE, VARIABLE_DESC, INDICATOR, ENDYEAR) %>%
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
    indicator_values_long %>% dplyr::group_by(MEASURE_TYPE, INDICATOR, VARIABLE) %>% dplyr::select(ESTIMATE) %>% skimr::skim()
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
                            "ENDYEAR",
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
    dplyr::group_by(ENDYEAR, LABEL) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(ENDYEAR ~ LABEL, scales = "free_x")

}
