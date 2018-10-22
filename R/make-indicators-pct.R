#' @title Make The Census Data (ACS) Indicators
#' @description Make the ACS Inidcators related to a _proportion_ of a population.
#'   An example of the type of indicator included in this object might be
#'   the percentage of renter households, while the median rent price would _not_ be included.
#' @param acs_data desc
#' @param acs_tables desc
#'
#' @return a `tibble`
#' @export
make_acs_indicators_pct <- function(acs_data, acs_tables){

  # Join each acs variable its respective indicator

  pct_indicators <- acs_tables %>%
    dplyr::filter(MEASURE_TYPE %in% "PERCENT")


  all_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(NAME = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     VARIABLE = name,
                     LABEL = label,
                     CONCEPT = concept)

  indicator_pct_vars <- pct_indicators %>%
    dplyr::inner_join(all_vars, by = "NAME")


  # FIGURE OUT THE ROLE INDICATORS

  race_vars_join <- indicator_pct_vars %>%
    dplyr::filter(INDICATOR %in% "RACE") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     ROLE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "DENOMINATOR",
                       stringr::str_detect(LABEL, "Not Hispanic or Latino!!White alone") ~ "OMIT",
                       stringr::str_detect(LABEL, "Latino$") ~ "OMIT",
                       TRUE ~ "NUMERATOR"

                     )
    )

  ed_vars_join <- indicator_pct_vars %>%
    dplyr::filter(INDICATOR %in% "EDUCATION") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     ROLE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "DENOMINATOR",
                       stringr::str_detect(LABEL, "No schooling") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "grade") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "High school") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "Some college") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "Associate's degree") ~ "NUMERATOR",
                       TRUE ~ "OMIT"

                     ))

  inc_vars_join <- indicator_pct_vars %>%
    dplyr::filter(INDICATOR %in% "INCOME") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     ROLE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "DENOMINATOR",
                       stringr::str_detect(LABEL, "Less") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$14,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$19,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$24,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$29,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$34,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$39,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$44,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$49,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$59,999") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "\\$74,999") ~ "NUMERATOR",
                       TRUE ~ "OMIT"

                     ))

  tenure_vars_join <- indicator_pct_vars %>%
    dplyr::filter(INDICATOR %in% "TENURE") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR,
                     TOPIC,
                     VARIABLE,
                     ROLE = dplyr::case_when(
                       stringr::str_detect(LABEL, "Total$") ~ "DENOMINATOR",
                       stringr::str_detect(LABEL, "Renter occupied$") ~ "NUMERATOR",
                       TRUE ~ "OMIT"

                     ))


  burden_own_vars_join <- indicator_pct_vars %>%
    dplyr::filter(INDICATOR %in% "COST BURDEN") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR = "COST BURDEN OWN",
                     TOPIC,
                     VARIABLE,
                     ROLE = dplyr::case_when(
                       LABEL %in% "Estimate!!Total!!Owner-occupied housing units" ~ "DENOMINATOR",
                       stringr::str_detect(LABEL, "Owner") & stringr::str_detect(LABEL, "30 percent") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "Owner") & stringr::str_detect(LABEL, "Zero or negative income") ~ "NUMERATOR",
                       TRUE ~ "OMIT"

                     ))

  burden_rent_vars_join <- indicator_pct_vars %>%
    dplyr::filter(INDICATOR %in% "COST BURDEN") %>%
    dplyr::transmute(SOURCE = "ACS",
                     INDICATOR = "COST BURDEN RENT",
                     TOPIC,
                     VARIABLE,
                     ROLE = dplyr::case_when(
                       LABEL %in% "Estimate!!Total!!Renter-occupied housing units" ~ "DENOMINATOR",
                       stringr::str_detect(LABEL, "Renter") & stringr::str_detect(LABEL, "30 percent") ~ "NUMERATOR",
                       stringr::str_detect(LABEL, "Renter") & stringr::str_detect(LABEL, "Zero or negative income") ~ "NUMERATOR",
                       TRUE ~ "OMIT"

                     ))

  vars_join <- list(race_vars_join,
                    ed_vars_join,
                    inc_vars_join,
                    tenure_vars_join,
                    burden_own_vars_join,
                    burden_rent_vars_join) %>%
    purrr::reduce(dplyr::bind_rows)


  indicator_values <- acs_data %>%
    dplyr::inner_join(vars_join,  by = "VARIABLE") %>%   # this will filter out non-pct variables like RENT
    dplyr::group_by(GEOID, ENDYEAR, INDICATOR, SOURCE, ROLE) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
                     MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!ROLE %in% "OMIT") %>%
    tidyr::gather(TYPE, VALUE, ESTIMATE, MOE) %>%
    tidyr::unite(PROP_TYPE, ROLE:TYPE) %>%
    tidyr::spread(PROP_TYPE, VALUE) %>%
    dplyr::group_by(GEOID, ENDYEAR, INDICATOR, SOURCE) %>%
    dplyr::summarise(PROPORTION = NUMERATOR_ESTIMATE/DENOMINATOR_ESTIMATE,
                     PROPORTION_MOE = tidycensus::moe_prop(
                       num = NUMERATOR_ESTIMATE,
                       denom = DENOMINATOR_ESTIMATE,
                       moe_num = NUMERATOR_MOE,
                       moe_denom = DENOMINATOR_MOE)
    ) %>%
    dplyr::ungroup()




  # indicator_values_comparison <-
  #   indicator_values %>%
  #   dplyr::mutate(TYPE = dplyr::case_when(
  #     GEOID %in% "53033" ~ "COUNTY",
  #     TRUE ~ "TRACT"
  #   )) %>%
  #   tidyr::gather(VAL_TYPE, VALUE, tidyselect::matches("ROLE")) %>%
  #   tidyr::unite(GEOG_VAL, TYPE:VAL_TYPE) %>%
  #   tidyr::spread(GEOG_VAL, VALUE) %>%
  #   dplyr::group_by(INDICATOR, ENDYEAR) %>%
  #   dplyr::arrange(COUNTY_ROLE) %>%
  #   tidyr::fill(matches("COUNTY")) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(GREATER_THAN_COUNTY = TRACT_ROLE >= COUNTY_ROLE)
  #
  # acs_indicators <- indicator_values_comparison

  acs_indicators <- indicator_values

  return(acs_indicators)

}

#' @export
show_hist_facet_acs_indicators_pct <- function(){

  if(!exists("acs_indicators_pct")){stop("'acs_indicators_pct' doesn't exist\nTry loading it with 'loadd(acs_indicators_pct)'.")}

  acs_indicators_pct %>%
      dplyr::group_by(ENDYEAR, INDICATOR) %>%
      dplyr::mutate(MEDIAN = median(ROLE,na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = ROLE)) +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      ggplot2::geom_histogram() +
      ggplot2::geom_vline(aes(xintercept=MEDIAN), size=0.5, color = "red") +
      ggplot2::facet_grid(ENDYEAR ~ INDICATOR)

}
