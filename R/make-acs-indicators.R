#' Make The Census Data (ACS) Indicators
#'
#' @param acs_data desc
#' @param acs_tables desc
#'
#' @return a `tibble`
#' @export
make_acs_indicators <- function(acs_data, acs_tables){

  # Separate the tables

  race_table <- acs_tables %>%
    dplyr::filter(stringr::str_detect(TOPIC, "RACE"))

  ed_table <- acs_tables %>%
    dplyr::filter(stringr::str_detect(TOPIC, "EDUCATION"))

  inc_table <- acs_tables %>%
    dplyr::filter(stringr::str_detect(TOPIC, "INCOME"))

  tenure_table <- acs_tables %>%
    dplyr::filter(stringr::str_detect(TOPIC, "TENURE"))


  # Separate the variables

  all_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(NAME = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     VARIABLE = name,
                     LABEL = label,
                     TOPIC = concept)

  all_race_vars <- all_vars %>%
    dplyr::semi_join(race_table, by = "NAME")

  all_ed_vars <- all_vars %>%
    dplyr::semi_join(ed_table, by = "NAME")

  all_inc_vars <- all_vars %>%
    dplyr::semi_join(inc_table, by = "NAME")

  all_tenure_vars <- all_vars %>%
    dplyr::semi_join(tenure_table, by = "NAME")


  # FIGURE OUT THE PROPORTION INDICATORS

  race_vars_join <- all_race_vars %>%
    dplyr::transmute(TOPIC = "RACE",
              VARIABLE,
              PROPORTION = dplyr::case_when(
                stringr::str_detect(LABEL, "Total$") ~ "DENOMINATOR",
                stringr::str_detect(LABEL, "Not Hispanic or Latino!!White alone") ~ "OMIT",
                stringr::str_detect(LABEL, "Latino$") ~ "OMIT",
                TRUE ~ "NUMERATOR"

              ))

  ed_vars_join <- all_ed_vars %>%
    dplyr::transmute(TOPIC = "EDUCATION",
              VARIABLE,
              PROPORTION = dplyr::case_when(
                stringr::str_detect(LABEL, "Total$") ~ "DENOMINATOR",
                stringr::str_detect(LABEL, "No schooling") ~ "NUMERATOR",
                stringr::str_detect(LABEL, "grade") ~ "NUMERATOR",
                stringr::str_detect(LABEL, "High school") ~ "NUMERATOR",
                stringr::str_detect(LABEL, "Some college") ~ "NUMERATOR",
                stringr::str_detect(LABEL, "Associate's degree") ~ "NUMERATOR",
                TRUE ~ "OMIT"

              ))


  inc_vars_join <- all_inc_vars %>%
    dplyr::transmute(TOPIC = "INCOME",
              VARIABLE,
              PROPORTION = dplyr::case_when(
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

  tenure_vars_join <- all_tenure_vars %>%
    dplyr::transmute(TOPIC = "TENURE",
              VARIABLE,
              PROPORTION = dplyr::case_when(
                stringr::str_detect(LABEL, "Total$") ~ "DENOMINATOR",
                stringr::str_detect(LABEL, "Renter occupied$") ~ "NUMERATOR",
                TRUE ~ "OMIT"

              ))

  vars_join <- list(race_vars_join,
                    ed_vars_join,
                    inc_vars_join,
                    tenure_vars_join) %>%
    purrr::reduce(dplyr::bind_rows)

  indicator_values <- acs_data %>%
    dplyr::left_join(vars_join,  by = "VARIABLE") %>%
    dplyr::group_by(GEOID, ENDYEAR, TOPIC, PROPORTION) %>%
    dplyr::summarise(ESTIMATE = sum(ESTIMATE, na.rm = TRUE),
              MOE = tidycensus::moe_sum(moe = MOE, estimate = ESTIMATE, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!PROPORTION %in% "OMIT") %>%
    tidyr::gather(TYPE, VALUE, ESTIMATE, MOE) %>%
    tidyr::unite(PROP_TYPE, PROPORTION:TYPE) %>%
    tidyr::spread(PROP_TYPE, VALUE) %>%
    dplyr::group_by(GEOID, ENDYEAR, TOPIC) %>%
    dplyr::summarise(PROPORTION = NUMERATOR_ESTIMATE/DENOMINATOR_ESTIMATE,
              PROPORTION_MOE = tidycensus::moe_prop(
                num = NUMERATOR_ESTIMATE,
                denom = DENOMINATOR_ESTIMATE,
                moe_num = NUMERATOR_MOE,
                moe_denom = DENOMINATOR_MOE)
    ) %>%
    dplyr::ungroup()

  indicator_values_comparison <-
    indicator_values %>%
    dplyr::mutate(TYPE = dplyr::case_when(
      GEOID %in% "53033" ~ "COUNTY",
      TRUE ~ "TRACT"
    )) %>%
    tidyr::gather(VAL_TYPE, VALUE, tidyselect::matches("PROPORTION")) %>%
    tidyr::unite(GEOG_VAL, TYPE:VAL_TYPE) %>%
    tidyr::spread(GEOG_VAL, VALUE) %>%
    dplyr::group_by(TOPIC, ENDYEAR) %>%
    dplyr::arrange(COUNTY_PROPORTION) %>%
    tidyr::fill(matches("COUNTY")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(GREATER_THAN_COUNTY = TRACT_PROPORTION >= COUNTY_PROPORTION)

acs_indicators <- indicator_values_comparison

return(acs_indicators)



}
