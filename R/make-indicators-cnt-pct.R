#' @title Make The Count Indicators
#' @description Make the ACS Inidcators related to a _count_ of a population.
#'   An example of the type of indicator included in this object might be
#'   the count of renter households, while the median rent price would _not_ be included.
#' @param acs_variables desc
#' @param hud_chas_variables desc
#' @return a `tibble`
#' @export
make_indicators_cnt_pct <- function(acs_variables, hud_chas_variables){



# PREPARE DATA --------------------------------------------------------

  acs_cnt <- acs_variables %>%
    dplyr::filter(MEASURE_TYPE %in% "COUNT")

  chas_cnt <- hud_chas_variables %>%
    dplyr::filter(MEASURE_TYPE %in% "COUNT") # unnecessary step because they are all COUNT but I'm leaving it for clarity's sake


# JOIN DATA ---------------------------------------------------------------


  all_cnt_vars <- list(acs_cnt, chas_cnt) %>%  # add the other count data variables
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
