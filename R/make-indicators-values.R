#' @title Make The Value Indicators
#' @description Make the ACS Inidcators related to a _value of a population.
#'   An example of the type of indicator included in this object might be
#'   the count of renter households, while the median rent price would _not_ be included.
#' @param acs_variables desc
#' @param ltdb_variables desc
#' @param factfinder_variables desc
#' @param parcel_value_variables desc
#' @param parcel_sales_variables desc
#' @param parcel_tract_overlay desc
#' @return a `tibble`
#' @export
make_indicators_values <- function(acs_variables,
                                   ltdb_variables,
                                   factfinder_variables,
                                   parcel_value_variables,
                                   parcel_sales_variables,
                                   parcel_tract_overlay,
                                   indicator_template){



  # PREPARE DATA --------------------------------------------------------


  acs_median <-acs_variables %>%
    dplyr::filter(MEASURE_TYPE %in% "MEDIAN") %>%
    dplyr::filter(VARIABLE_ROLE %in% "include") %>%
    dplyr::select(-dplyr::matches("VARIABLE_"))

  parcel_median_with_n <- list(parcel_value_variables,
                               parcel_sales_variables) %>%
    purrr::map_dfr(c) %>%
    dplyr::inner_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID = "PIN")) %>% # filter out parcels whose PINs don't match any tract GEOID
    dplyr::mutate(GEOGRAPHY_ID = GEOID,
                  GEOGRAPHY_ID_TYPE = "tract",
                  VARIABLE = stringr::str_c("MEDIAN_", VARIABLE),
                  INDICATOR = "VALUE",
                  MEASURE_TYPE = "MEDIAN") %>%
    dplyr::select(-GEOID, -dplyr::matches("^META")) %>%
    dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL,-VARIABLE_SUBTOTAL_DESC,-ESTIMATE,-MOE)) %>%
    dplyr::summarise(ESTIMATE = as.integer(round(median(ESTIMATE, na.rm = TRUE),0)),
                     N = n(),
                     NAS = sum(is.na(ESTIMATE)),
                     MOE = NA_real_) %>%
    dplyr::ungroup() %>%
    dplyr::filter(VARIABLE_ROLE %in% "include") %>%
    dplyr::select(-dplyr::matches("VARIABLE_"))

  check_parcel_median_with_n <- function(){

    list(parcel_value_variables,
         parcel_sales_variables) %>%
      purrr::map_dfr(c) %>%
      dplyr::filter(VARIABLE_ROLE %in% "include") %>%
      select(GEOGRAPHY_ID, ENDYEAR, VARIABLE, ESTIMATE) %>%
      group_by(VARIABLE,ENDYEAR) %>%
      skimr::skim()

  }


  # check the data distribution by ENDYEAR and VARIABLE (histogram)
  check_parcel_median_distrobution <- function(){
    p_no_outliers <- parcel_median_with_n %>%
      filter((VARIABLE %in% "MEDIAN_SALE_PRICE_2018" & ESTIMATE < 2e6) |
               (VARIABLE %in% "MEDIAN_VALUE_TOTAL_2018" & ESTIMATE < 2e6) |
               (VARIABLE %in% "MEDIAN_SALE_PRICE_2018_SQFT" & ESTIMATE < 750))

    label_data <- p_no_outliers %>%
      dplyr::mutate(X_VAR = if_else(VARIABLE %in% "MEDIAN_SALE_PRICE_2018_SQFT",600,1.5e6),
                    Y_VAR = 75) %>%
      dplyr::group_by(ENDYEAR, VARIABLE) %>%
      dplyr::summarise(MEDIAN = median(ESTIMATE, na.rm = TRUE),
                       X_VAR = first(X_VAR),
                       Y_VAR = first(Y_VAR),
                       N = paste0("n = ",scales::comma(sum(!is.na(ESTIMATE))))) %>%
      dplyr::ungroup()

    p_no_outliers %>%
      ggplot(aes(x = ESTIMATE)) +
      geom_histogram() +
      scale_x_continuous(labels = scales::comma) +
      facet_grid(ENDYEAR ~ VARIABLE, scales = "free_x") +
      geom_text(data = label_data, aes(x = X_VAR, y = Y_VAR, label = N), inherit.aes = FALSE) +
      ggplot2::geom_vline(data = label_data, aes(xintercept=MEDIAN), size=0.5, color = "red", inherit.aes = FALSE)

  }


  parcel_median <- parcel_median_with_n %>%
    dplyr::select(-N, -NAS)


  #  parcel_sales_cnt <- parcel_sales_variables %>%
  #    dplyr::left_join(parcel_tract_overlay, by = c(GEOGRAPHY_ID = "PIN")) %>%
  #    dplyr::mutate(GEOGRAPHY_ID = GEOID,
  #                  GEOGRAPHY_ID_TYPE = "tract",
  #                  VARIABLE = "SALE_RATE",
  #                  INDICATOR = "SALE_RATE",
  #                  VARIABLE_ROLE = "count",
  #                  MOE = 0L,
  #                  ESTIMATE = 1L, # count of sales
  #                  MEASURE_TYPE = "COUNT") %>%
  #    dplyr::select(-GEOID, -RNUM, -dplyr::matches("^META")) %>%
  #    dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE)) %>%
  #    dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL,-VARIABLE_SUBTOTAL_DESC,-ESTIMATE,-MOE)) %>%
  #    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
  #                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
  #    dplyr::ungroup() %>%
  #    dplyr::mutate(VARIABLE_ROLE = "COUNT")
  #
  # parcel_value_sales_cnt <- list(parcel_value_cnt, parcel_sales_cnt) %>%
  #   purrr::map_dfr(c)


  # JOIN DATA ---------------------------------------------------------------


  all_cnt_vars <- list(acs_cnt, chas_cnt, parcel_value_sales_cnt) %>%  # add the other count data variables
    purrr::map_dfr(c)


  # CALCULATE COUNT AND PERCENT ---------------------------------------------


  indicator_values <- all_cnt_vars %>%
    dplyr::mutate(VARIABLE_ROLE = toupper(VARIABLE_ROLE)) %>%
    dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL,-VARIABLE_SUBTOTAL_DESC,-ESTIMATE,-MOE)) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!VARIABLE_ROLE %in% "OMIT") %>%
    tidyr::gather(TYPE, VALUE, ESTIMATE, MOE) %>%
    tidyr::unite(PROP_TYPE, VARIABLE_ROLE, TYPE) %>%
    tidyr::spread(PROP_TYPE, VALUE) %>%
    dplyr::group_by_at(dplyr::vars(-COUNT_ESTIMATE,-COUNT_MOE,-TOTAL_ESTIMATE,-TOTAL_MOE)) %>%
    dplyr::summarise(COUNT_ESTIMATE,
                     COUNT_MOE,
                     TOTAL_ESTIMATE,
                     TOTAL_MOE,
                     PROPORTION_ESTIMATE = COUNT_ESTIMATE/TOTAL_ESTIMATE,
                     PROPORTION_MOE = tidycensus::moe_prop(
                       num = COUNT_ESTIMATE,
                       denom = TOTAL_ESTIMATE,
                       moe_num = COUNT_MOE,
                       moe_denom = TOTAL_MOE)
    ) %>%
    dplyr::ungroup()

  indicators_cnt_pct <- indicator_values

  return(indicators_cnt_pct)

}

show_hist_facet_indicators_cnt_pct <- function(){

  if(!exists("indicators_cnt_pct")){stop("'indicators_cnt_pct' doesn't exist\nTry loading it with 'loadd(indicators_cnt_pct)'.")}

  indicators_cnt_pct %>%
    dplyr::mutate(INDICATOR = glue::glue("{INDICATOR} ({SOURCE})")) %>%
    dplyr::group_by(ENDYEAR, INDICATOR) %>%
    dplyr::mutate(MEDIAN = median(PROPORTION_ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = PROPORTION_ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(ENDYEAR ~ INDICATOR)

}
