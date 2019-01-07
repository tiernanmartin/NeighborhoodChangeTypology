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


dat <- indicators_cnt_pct %>%
  dplyr::group_by(ENDYEAR, INDICATOR) %>%
  mutate(PROPORTION_ESTIMATE = map_dbl(PROPORTION_ESTIMATE,remove_outlier),
         PROPORTION_ESTIMATE = case_when(
           INDICATOR %in% "SALE_RATE" & PROPORTION_ESTIMATE > .50 ~ NA_real_,
           TRUE ~ PROPORTION_ESTIMATE
         )) %>%
  ungroup

dat %>%
    dplyr::mutate(INDICATOR = glue::glue("{INDICATOR} ({SOURCE})"),
                  WC_PROPORTION_ESTIMATE = if_else(GEOGRAPHY_ID %in% wc_tr,PROPORTION_ESTIMATE,NA_real_)) %>%
    dplyr::group_by(ENDYEAR, INDICATOR) %>%
    dplyr::mutate(MEDIAN = median(PROPORTION_ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = PROPORTION_ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=WC_PROPORTION_ESTIMATE), size=0.5, color = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(ENDYEAR ~ INDICATOR, scales = "free_x")


dat_median <- indicators_median %>%
  filter(ESTIMATE > 10) %>%
   dplyr::group_by(ENDYEAR, VARIABLE) %>%
    dplyr::mutate(ESTIMATE = map_dbl(ESTIMATE,remove_outlier),
                  ESTIMATE = case_when(
                    INDICATOR %in% "RENT" & ESTIMATE > 2500 ~ NA_real_,
                    VARIABLE %in% c("MEDIAN_SALE_PRICE_2018",
                                    "MEDIAN_VALUE_TOTAL_2018") & ESTIMATE > 1e6 ~ NA_real_,
                    VARIABLE %in% "MEDIAN_SALE_PRICE_2018_SQFT" & ESTIMATE <= 1 ~ NA_real_,
                    TRUE ~ ESTIMATE
                  ),
                  WC_ESTIMATE = if_else(GEOGRAPHY_ID %in% wc_tr,ESTIMATE,NA_real_)
    ) %>%
  ungroup()

dat_median %>%
  mutate(VARIABLE = case_when(
    VARIABLE %in% c("B25058", "B25077") ~ INDICATOR,
    TRUE ~ VARIABLE
  )) %>%
    dplyr::mutate(VARIABLE = glue::glue("{VARIABLE} ({SOURCE})")) %>%
    dplyr::group_by(ENDYEAR, VARIABLE) %>%
    dplyr::mutate(MEDIAN = median(ESTIMATE,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = ESTIMATE)) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=WC_ESTIMATE), size=0.5, color = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
    ggplot2::facet_grid(ENDYEAR ~ VARIABLE, scales = "free")
