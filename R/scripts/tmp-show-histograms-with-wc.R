wc_tr <- c("53033026600",
           "53033026700",
           "53033026500",
           "53033026801",
           "53033026802",
           "53033027000")

remove_outlier <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  lwr <- qnt[1] - H
  upr <- qnt[2] + H
  res <- if_else(between(x, lwr, upr),x,NA_real_)
  return(res)
}


# dat <- indicators_cnt_pct %>%
#   dplyr::group_by(ENDYEAR, INDICATOR) %>%
#   mutate(PROPORTION_ESTIMATE = map_dbl(PROPORTION_ESTIMATE,remove_outlier),
#          PROPORTION_ESTIMATE = case_when(
#            INDICATOR %in% "SALE_RATE" & PROPORTION_ESTIMATE > .50 ~ NA_real_,
#            TRUE ~ PROPORTION_ESTIMATE
#          )) %>%
#   ungroup




dat %>%
  ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggplot2::geom_histogram() +
  ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
  ggplot2::facet_grid(DATE_END_YEAR ~ LABEL, scales = "free_x")


show_hist_facet_indicators_cnt_pct_year <- function(){

  dat <-
    indicators_cnt_pct %>%
    dplyr::filter(MEASURE_TYPE %in% "PERCENT") %>%
    dplyr::filter(DATE_RANGE_TYPE %in% c("five years","one year")) %>%
    dplyr::filter(! INDICATOR %in% "POPULATION") %>%
    dplyr::mutate(LABEL = stringr::str_replace(VARIABLE_DESC,"PERCENT","%")) %>%
    dplyr::group_by(DATE_END_YEAR, LABEL) %>%
    dplyr::mutate(ESTIMATE = map_dbl(ESTIMATE,remove_outlier),
                  ESTIMATE = case_when(
                    INDICATOR %in% "SALE_RATE" & ESTIMATE > .50 ~ NA_real_,
                    TRUE ~ ESTIMATE
                  ),
                  MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup()

  dat %>%
    dplyr::mutate(WC_ESTIMATE = if_else(GEOGRAPHY_ID %in% wc_tr,ESTIMATE,NA_real_)) %>%
    dplyr::group_by(DATE_END_YEAR, LABEL) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=WC_ESTIMATE), size=0.5, color = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(DATE_END_YEAR ~ LABEL, scales = "free_x")
}


show_hist_facet_indicators_cnt_pct_qtr <- function(){
 dat <-
    indicators_cnt_pct %>%
    dplyr::filter(MEASURE_TYPE %in% "PERCENT") %>%
    dplyr::filter(DATE_RANGE_TYPE %in% c("one quarter")) %>%
    dplyr::filter(! INDICATOR %in% "POPULATION") %>%
    dplyr::mutate(LABEL = stringr::str_replace(VARIABLE_DESC,"PERCENT","%")) %>%
    dplyr::group_by(DATE_END_YEAR, LABEL) %>%
    dplyr::mutate(ESTIMATE = map_dbl(ESTIMATE,remove_outlier),
                  ESTIMATE = case_when(
                    INDICATOR %in% "SALE_RATE" & ESTIMATE > .50 ~ NA_real_,
                    TRUE ~ ESTIMATE
                  ),
                  MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup()

  dat %>%
    dplyr::mutate(WC_ESTIMATE = if_else(GEOGRAPHY_ID %in% wc_tr,ESTIMATE,NA_real_)) %>%
    dplyr::group_by(DATE_END_YEAR, LABEL) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=WC_ESTIMATE), size=0.5, color = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(DATE_END_YEAR ~ LABEL, scales = "free_x")
}

show_hist_facet_indicators_median_year <- function(){
  dat_median <- indicators_median %>%
  dplyr::filter(DATE_RANGE_TYPE %in% c("five years","one year")) %>%
  filter(ESTIMATE > 10) %>%
  dplyr::group_by(DATE_END_YEAR, VARIABLE_DESC) %>%
  dplyr::mutate(ESTIMATE = map_dbl(ESTIMATE,remove_outlier),
                ESTIMATE = case_when(
                  VARIABLE_DESC %in% "MEDIAN_RENT_ACS" & ESTIMATE > 2500 ~ NA_real_,
                  VARIABLE_DESC %in% c("MEDIAN_ASSESSED_TOTAL_VALUE_ALL",
                                  "MEDIAN_SALE_PRICE_ALL") & ESTIMATE > 1e6 ~ NA_real_,
                  VARIABLE_DESC %in% "MEDIAN_SALE_PRICE_SQFT_ALL" & ESTIMATE <= 1 ~ NA_real_,
                  VARIABLE_DESC %in% "MEDIAN_SALE_PRICE_SQFT_ALL" & ESTIMATE > 1000 ~ NA_real_,
                  TRUE ~ ESTIMATE
                ),
                WC_ESTIMATE = if_else(GEOGRAPHY_ID %in% wc_tr,ESTIMATE,NA_real_)
  ) %>%
  ungroup() %>%
  dplyr::filter(VARIABLE_DESC %in% c("MEDIAN_RENT_ACS",
                                     "MEDIAN_VALUE_ACS",
                                     "MEDIAN_VALUE_LTDB",
                                     "MEDIAN_ASSESSED_TOTAL_VALUE_ALL",
                                     "MEDIAN_SALE_PRICE_ALL",
                                     "MEDIAN_SALE_PRICE_SQFT_ALL"))

dat_median %>%
  dplyr::group_by(DATE_END_YEAR, VARIABLE_DESC) %>%
  dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
  ggplot2::scale_x_continuous(labels = scales::dollar) +
  ggplot2::geom_histogram() +
  ggplot2::geom_vline(ggplot2::aes(xintercept=WC_ESTIMATE), size=0.5, color = "blue") +
  ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
  ggplot2::facet_grid(DATE_END_YEAR ~ VARIABLE_DESC, scales = "free_x")

}

ind_labels <- indicators_cnt_pct %>%
  select(VARIABLE,VARIABLE_DESC) %>%
  mutate(VARIABLE_DESC = str_remove(VARIABLE_DESC,"COUNT_|TOTAL_|PERCENT_")) %>%
  distinct()


dat_change <- indicators_change %>%
  dplyr::filter(MEASURE_TYPE %in% "CHANGE_APPROPRIATE") %>%
  dplyr::select(-VARIABLE_DESC) %>%
  left_join(ind_labels) %>%
  dplyr::mutate(DATE_END_YEAR = as.character(glue::glue("{lubridate::year(DATE_BEGIN)}_TO_{DATE_END_YEAR}")),
                WC_ESTIMATE = if_else(GEOGRAPHY_ID %in% wc_tr,ESTIMATE,NA_real_)
  )

dat_change %>%
  dplyr::group_by(DATE_END_YEAR, VARIABLE_DESC) %>%
  dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
  ggplot2::scale_x_continuous(labels = scales::dollar) +
  ggplot2::geom_histogram() +
  ggplot2::geom_vline(ggplot2::aes(xintercept=WC_ESTIMATE), size=0.5, color = "blue") +
  ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
  ggplot2::facet_grid(DATE_END_YEAR ~ VARIABLE_DESC, scales = "free_x")


