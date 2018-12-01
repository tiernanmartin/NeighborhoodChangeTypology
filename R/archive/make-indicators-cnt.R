#' @title Make The Count Indicators
#' @description Make the ACS Inidcators related to a _count_ of a population.
#'   An example of the type of indicator included in this object might be
#'   the count of renter households, while the median rent price would _not_ be included.
#' @param acs_data desc
#' @param hud_chas_data desc
#' @param acs_tables desc
#'
#' @return a `tibble`
#' @export
make_indicators_cnt <- function(acs_data, hud_chas_data, acs_tables){



# PREPARE ACS DATA --------------------------------------------------------

  # Join each acs variable its respective indicator

  cnt_indicators <- acs_tables %>%
    dplyr::filter(MEASURE_TYPE %in% "COUNT")


  all_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(NAME = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     VARIABLE = name,
                     LABEL = label,
                     CONCEPT = concept)

  indicator_cnt_vars <- cnt_indicators %>%
    dplyr::inner_join(all_vars, by = "NAME")


  # FIGURE OUT THE MEASURE_TYPE INDICATORS

  race_vars_join <- indicator_cnt_vars %>%
    dplyr::filter(INDICATOR %in% "RACE") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     MEASURE_TYPE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "total",
                       stringr::str_detect(LABEL, "Not Hispanic or Latino!!White alone") ~ "omit",
                       stringr::str_detect(LABEL, "Latino$") ~ "omit",
                       TRUE ~ "count"

                     )
    )

  ed_vars_join <- indicator_cnt_vars %>%
    dplyr::filter(INDICATOR %in% "EDUCATION") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     MEASURE_TYPE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "total",
                       stringr::str_detect(LABEL, "No schooling") ~ "count",
                       stringr::str_detect(LABEL, "grade") ~ "count",
                       stringr::str_detect(LABEL, "High school") ~ "count",
                       stringr::str_detect(LABEL, "Some college") ~ "count",
                       stringr::str_detect(LABEL, "Associate's degree") ~ "count",
                       TRUE ~ "omit"

                     ))

  inc_vars_join <- indicator_cnt_vars %>%
    dplyr::filter(INDICATOR %in% "INCOME") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     MEASURE_TYPE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "total",
                       stringr::str_detect(LABEL, "Less") ~ "count",
                       stringr::str_detect(LABEL, "\\$14,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$19,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$24,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$29,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$34,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$39,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$44,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$49,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$59,999") ~ "count",
                       stringr::str_detect(LABEL, "\\$74,999") ~ "count",
                       TRUE ~ "omit"

                     ))

  tenure_vars_join <- indicator_cnt_vars %>%
    dplyr::filter(INDICATOR %in% "TENURE") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     MEASURE_TYPE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "total",
                       stringr::str_detect(LABEL, "Renter occupied$") ~ "count",
                       TRUE ~ "omit"

                     ))


  burden_own_vars_join <- indicator_cnt_vars %>%
    dplyr::filter(INDICATOR %in% "COST BURDEN") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR = "COST BURDEN OWN",
                     TOPIC,
                     VARIABLE,
                     MEASURE_TYPE = dplyr::case_when(
                       LABEL %in% "Estimate!!Total!!Owner-occupied housing units" ~ "total",
                       stringr::str_detect(LABEL, "Owner") & stringr::str_detect(LABEL, "30 percent") ~ "count",
                       stringr::str_detect(LABEL, "Owner") & stringr::str_detect(LABEL, "Zero or negative income") ~ "count",
                       TRUE ~ "omit"

                     ))

  burden_rent_vars_join <- indicator_cnt_vars %>%
    dplyr::filter(INDICATOR %in% "COST BURDEN") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR = "COST BURDEN RENT",
                     TOPIC,
                     VARIABLE,
                     MEASURE_TYPE = dplyr::case_when(
                       LABEL %in% "Estimate!!Total!!Renter-occupied housing units" ~ "total",
                       stringr::str_detect(LABEL, "Renter") & stringr::str_detect(LABEL, "30 percent") ~ "count",
                       stringr::str_detect(LABEL, "Renter") & stringr::str_detect(LABEL, "Zero or negative income") ~ "count",
                       TRUE ~ "omit"

                     ))

  acs_vars_join <- list(race_vars_join,
                    ed_vars_join,
                    inc_vars_join,
                    tenure_vars_join,
                    burden_own_vars_join,
                    burden_rent_vars_join) %>%
    purrr::reduce(dplyr::bind_rows)


# JOIN DATA ---------------------------------------------------------------


  pct_vars_join_all <- acs_data %>%
    dplyr::select(-SOURCE, -MEASURE_TYPE) %>%
    dplyr::inner_join(acs_vars_join,  by = "VARIABLE") %>%   # this will filter out non-pct variables like RENT
    dplyr::bind_rows(hud_chas_data) %>%
    dplyr::mutate(MEASURE_TYPE = toupper(MEASURE_TYPE))


# CALCULATE % -------------------------------------------------------------

  indicator_values <- pct_vars_join_all %>%
    dplyr::group_by(GEOID, ENDYEAR, INDICATOR, SOURCE, MEASURE_TYPE) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!MEASURE_TYPE %in% "omit") %>%
    tidyr::gather(TYPE, VALUE, ESTIMATE, MOE) %>%
    tidyr::unite(PROP_TYPE, MEASURE_TYPE:TYPE) %>%
    tidyr::spread(PROP_TYPE, VALUE) %>%
    dplyr::group_by(GEOID, ENDYEAR, INDICATOR, SOURCE) %>%
    dplyr::summarise(PROPORTION = COUNT_ESTIMATE/TOTAL_ESTIMATE,
                     PROPORTION_MOE = tidycensus::moe_prop(
                       num = COUNT_ESTIMATE,
                       denom = TOTAL_ESTIMATE,
                       moe_num = COUNT_MOE,
                       moe_denom = TOTAL_MOE)
    ) %>%
    dplyr::ungroup()




  # indicator_values_comparison <-
  #   indicator_values %>%
  #   dplyr::mutate(TYPE = dplyr::case_when(
  #     GEOID %in% "53033" ~ "COUNTY",
  #     TRUE ~ "TRACT"
  #   )) %>%
  #   tidyr::gather(VAL_TYPE, VALUE, tidyselect::matches("MEASURE_TYPE")) %>%
  #   tidyr::unite(GEOG_VAL, TYPE:VAL_TYPE) %>%
  #   tidyr::spread(GEOG_VAL, VALUE) %>%
  #   dplyr::group_by(INDICATOR, ENDYEAR) %>%
  #   dplyr::arrange(COUNTY_MEASURE_TYPE) %>%
  #   tidyr::fill(matches("COUNTY")) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(GREATER_THAN_COUNTY = TRACT_MEASURE_TYPE >= COUNTY_MEASURE_TYPE)
  #
  # acs_indicators <- indicator_values_comparison

  indicators_pct <- indicator_values

  return(indicators_pct)

}

show_hist_facet_indicators_pct <- function(){

  if(!exists("indicators_pct")){stop("'indicators_pct' doesn't exist\nTry loading it with 'loadd(indicators_pct)'.")}

  indicators_pct %>%
      dplyr::mutate(INDICATOR = glue::glue("{INDICATOR} ({SOURCE})")) %>%
      dplyr::group_by(ENDYEAR, INDICATOR) %>%
      dplyr::mutate(MEDIAN = median(PROPORTION,na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = PROPORTION)) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      ggplot2::geom_histogram() +
      ggplot2::geom_vline(ggplot2::aes(xintercept=MEDIAN), size=0.5, color = "red") +
      ggplot2::facet_grid(ENDYEAR ~ INDICATOR)

}
