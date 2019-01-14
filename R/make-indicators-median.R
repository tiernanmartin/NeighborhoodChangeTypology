#' @title Make The Median Indicators
#' @description Make the ACS Inidcators related to a _value of a population.
#'   An example of the type of indicator included in this object might be
#'   the count of renter households, while the median rent price would _not_ be included.
#' @param acs_variables desc
#' @param ltdb_variables desc
#' @param factfinder_variables desc
#' @param parcel_value_variables desc
#' @param parcel_sales_variables desc
#' @param parcel_tract_overlay desc
#' @param county_community_tract_all_metadata desc
#' @param community_metadata desc
#' @param indicator_template desc
#' @return a `tibble`
#' @export
make_indicators_median <- function(acs_variables,
                                   ltdb_variables,
                                   factfinder_variables,
                                   parcel_value_variables,
                                   parcel_sales_variables,
                                   parcel_tract_overlay,
                                   county_community_tract_all_metadata,
                                   community_metadata,
                                   indicator_template){

  # PREPARE DATA --------------------------------------------------------

  # start here: add community-level geographies to medians

  acs_ltdb_ff_median <- list(acs_variables,
                             ltdb_variables,
                             factfinder_variables) %>%
    purrr::map_dfr(c) %>%
    dplyr::filter(MEASURE_TYPE %in% "MEDIAN") %>%
    dplyr::filter(VARIABLE_ROLE %in% "include") %>%
    dplyr::select(-dplyr::matches("VARIABLE_SUBTOTAL|VARIABLE_ROLE"))

  # Note: there are no median values for community-level geographies of the
  # ACS data because a "median of medians" is not an accurate summary statistic.
  # However, community-level median values are calculated for assessor data because
  # this data source is disaggregated.


  # Note: there is an issue with the condo record PINs.
  #  In order to successfully join the parcels to census tracts,
  #  the 2005 condo PINs need to be converted from their condo unit PIN
  #  to the condo complex PIN. This is done by replacing the last four
  #  digits of the unit PIN with "0000".

  convert_to_complex_pin <- function(x){stringr::str_replace(x,".{4}$","0000")}

  parcel_value_sales_median <- list(parcel_value_variables,
                                    parcel_sales_variables) %>%
    purrr::map_dfr(c) %>%
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

  community_value_sales_median <- parcel_value_sales_median %>%
    dplyr::left_join(community_metadata, by = "GEOGRAPHY_ID") %>%
    dplyr::mutate(GEOGRAPHY_ID = GEOGRAPHY_COMMUNITY_ID,
                  GEOGRAPHY_ID_TYPE = GEOGRAPHY_COMMUNITY_ID_TYPE,
                  GEOGRAPHY_NAME = GEOGRAPHY_COMMUNITY_NAME,
                  GEOGRAPHY_TYPE = GEOGRAPHY_COMMUNITY_TYPE) %>%
    dplyr::select(-dplyr::matches("_COMMUNITY"))


  # create duplicates of the parcel variables that will be summarized
  # at the county level (instead of the tract level)

  county_value_sales_median <- parcel_value_sales_median %>%
    dplyr::mutate(GEOGRAPHY_ID = "53033") %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE,-GEOGRAPHY_NAME,-GEOGRAPHY_TYPE) %>%
    dplyr::left_join(county_community_tract_all_metadata, by = "GEOGRAPHY_ID")


  # CALCULATE MEDIAN --------------------------------------------------------

  all_geog_median_with_n <- list(parcel_value_sales_median,
                                 community_value_sales_median,
                                 county_value_sales_median) %>%
    purrr::map_dfr(c) %>%
    dplyr::group_by_at(dplyr::vars(-VARIABLE_SUBTOTAL,-VARIABLE_SUBTOTAL_DESC,-ESTIMATE,-MOE)) %>%
    dplyr::summarise(ESTIMATE = as.integer(round(median(ESTIMATE, na.rm = TRUE),0)),
                     N = n(),
                     NAS = sum(is.na(ESTIMATE)),
                     MOE = NA_real_) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_replace(VARIABLE_DESC,"^VALUE","MEDIAN")) %>%
    dplyr::filter(VARIABLE_ROLE %in% "include") %>%
    dplyr::select(-dplyr::matches("VARIABLE_SUBTOTAL|VARIABLE_ROLE"))

  # CHECK RESULTS -----------------------------------------------------------

  # check the number of parcels in each variable
  check_parcel_median_with_n <- function(){

    list(parcel_value_variables,
         parcel_sales_variables) %>%
      purrr::map_dfr(c) %>%
      dplyr::filter(VARIABLE_ROLE %in% "include") %>%
      select(GEOGRAPHY_ID, ENDYEAR, VARIABLE, ESTIMATE) %>%
      group_by(VARIABLE,ENDYEAR) %>%
      skimr::skim()

  }

  # check the median values for all geographies

  check_all_geog_median_with_n <- function(){

    all_geog_median_with_n %>%
      select(GEOGRAPHY_TYPE, GEOGRAPHY_ID, ENDYEAR, VARIABLE_DESC, ESTIMATE) %>%
      group_by(GEOGRAPHY_TYPE, ENDYEAR, VARIABLE_DESC) %>%
      skimr::skim()

  }

  # check the data distribution by ENDYEAR and VARIABLE (histogram)
  check_parcel_median_distribution <- function(){

    dat <- all_geog_median_with_n %>% dplyr::filter(GEOGRAPHY_TYPE %in% "tract") # tracts only

    p_no_outliers <- dat %>%
      filter((VARIABLE_DESC %in% "SALE_PRICE_ALL" & ESTIMATE < 2e6) |
               (VARIABLE_DESC %in% "ASSESSED_TOTAL_VALUE_ALL" & ESTIMATE < 2e6) |
               (VARIABLE_DESC %in% "SALE_PRICE_SQFT_ALL" & ESTIMATE < 750))

    label_data <- p_no_outliers %>%
      dplyr::mutate(X_VAR = if_else(VARIABLE_DESC %in% "SALE_PRICE_SQFT_ALL",600,1.5e6),
                    Y_VAR = 75) %>%
      dplyr::group_by(ENDYEAR, VARIABLE_DESC) %>%
      dplyr::summarise(MEDIAN = median(ESTIMATE, na.rm = TRUE),
                       X_VAR = first(X_VAR),
                       Y_VAR = first(Y_VAR),
                       N = paste0("n = ",scales::comma(sum(!is.na(ESTIMATE))))) %>%
      dplyr::ungroup()

    p_no_outliers %>%
      ggplot(aes(x = ESTIMATE)) +
      geom_histogram() +
      scale_x_continuous(labels = scales::comma) +
      facet_grid(ENDYEAR ~ VARIABLE_DESC, scales = "free_x") +
      geom_text(data = label_data, aes(x = X_VAR, y = Y_VAR, label = N), inherit.aes = FALSE) +
      ggplot2::geom_vline(data = label_data, aes(xintercept=MEDIAN), size=0.5, color = "red", inherit.aes = FALSE)

  }


  parcel_median <- all_geog_median_with_n %>%
    dplyr::select(-N, -NAS)


  # JOIN --------------------------------------------------------------------

  indicators_median_all <- list(acs_ltdb_ff_median,
                                parcel_median) %>%
    purrr::map_dfr(c)


  # REDEFINE VARIABLE DESC COLUMN ------------------------------------------------


  # create unique, human-readable variable names

  indicator_median_desc <- indicators_median_all %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_c(MEASURE_TYPE, VARIABLE_DESC, sep = "_")
    )



  # REFORMAT ----------------------------------------------------------------

  # Note: this just makes sure that the columns have the same order as the indicator_template

  indicators_median_ready <- indicator_template %>%
    dplyr::full_join(indicator_median_desc,
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

  indicators_median <- indicators_median_ready

  # RETURN ------------------------------------------------------------------

  return(indicators_median)

}

check_parcel_median <- function(){

  smooth_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.3 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- round(qnt[1] - H)
    y[x > (qnt[2] + H)] <- round(qnt[2] + H)
    y
  }

  dat <- indicators_median %>%
    dplyr::group_by(VARIABLE_DESC, ENDYEAR) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE),
                  ESTIMATE_NO_OUTLIERS = smooth_outliers(ESTIMATE)) %>%
    dplyr::ungroup()

  dat %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE_NO_OUTLIERS)) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::facet_grid(ENDYEAR ~ VARIABLE_DESC, scales = "free")
}
