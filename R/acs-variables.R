#' @title Prepare ACS Data Variables
#' @description Return a `tibble` of all of the American Community Survey data variables.
#' @param acs_data Tibble, description.
#' @param acs_tables desc
#' @param variable_template desc
#' @return a `tibble`

#' @rdname acs-variables
#' @export
make_acs_variables <- function(acs_data, acs_tables, variable_template){

  # PREPARE ACS DATA ROLES --------------------------------------------------------

  # Join each acs variable its respective indicator

  # cnt_indicators <- acs_tables %>%
  #   dplyr::filter(VARIABLE_ROLE %in% "COUNT")


  all_vars <- tidycensus::load_variables(2016, "acs5", cache = TRUE) %>%
    dplyr::transmute(SOURCE = "ACS",
                     VARIABLE = stringr::str_extract(name,".*(?=_\\d{3})"), # regex lookahead for '_001'
                     VARIABLE_SUBTOTAL = name,
                     VARIABLE_SUBTOTAL_DESC = label,
                     CONCEPT = concept) %>%
    dplyr::inner_join(acs_tables, by = "VARIABLE")

  # indicator_cnt_vars <- acs_tables %>%
  #   dplyr::inner_join(all_vars, by = c(VARIABLE = "NAME"))


  # FIGURE OUT THE VARIABLE_ROLE INDICATORS

  race_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "RACE") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR,
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = dplyr::case_when(
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Total$") ~ "total",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Not Hispanic or Latino!!White alone") ~ "omit",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Latino$") ~ "omit",
                       TRUE ~ "count"

                     )
    )

  ed_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "EDUCATION") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR,
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = dplyr::case_when(
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Total$") ~ "total",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "No schooling") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "grade") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "High school") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Some college") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Associate's degree") ~ "count",
                       TRUE ~ "omit"

                     ))

  inc_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "INCOME") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR,
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = dplyr::case_when(
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Total$") ~ "total",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Less") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$14,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$19,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$24,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$29,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$34,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$39,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$44,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$49,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$59,999") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "\\$74,999") ~ "count",
                       TRUE ~ "omit"

                     ))

  tenure_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "TENURE") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR,
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = dplyr::case_when(
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Total$") ~ "total",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Renter occupied$") ~ "count",
                       TRUE ~ "omit"

                     ))


  burden_own_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "COST BURDEN") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR = "COST_BURDEN_OWN",
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = dplyr::case_when(
                       VARIABLE_SUBTOTAL_DESC %in% "Estimate!!Total!!Owner-occupied housing units" ~ "total",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Owner") & stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "30 percent") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Owner") & stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Zero or negative income") ~ "count",
                       TRUE ~ "omit"

                     ))

  burden_rent_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "COST BURDEN") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR = "COST_BURDEN_RENT",
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = dplyr::case_when(
                       VARIABLE_SUBTOTAL_DESC %in% "Estimate!!Total!!Renter-occupied housing units" ~ "total",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Renter") & stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "30 percent") ~ "count",
                       stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Renter") & stringr::str_detect(VARIABLE_SUBTOTAL_DESC, "Zero or negative income") ~ "count",
                       TRUE ~ "omit"

                     ))

  rent_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "RENT") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR = "RENT",
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = "include"
    )

  value_vars_join <- all_vars %>%
    dplyr::filter(INDICATOR %in% "VALUE") %>%
    dplyr::transmute(SOURCE,
                     INDICATOR = "VALUE",
                     VARIABLE_SUBTOTAL,
                     VARIABLE_ROLE = "include"
    )

  acs_vars_join <- list(race_vars_join,
                        ed_vars_join,
                        inc_vars_join,
                        tenure_vars_join,
                        burden_own_vars_join,
                        burden_rent_vars_join,
                        rent_vars_join,
                        value_vars_join) %>%
    purrr::reduce(dplyr::bind_rows)


  # JOIN ROLES TO ACS DATA --------------------------------------------------

  acs_vars_data <- acs_data %>%
    dplyr::full_join(acs_vars_join, by = c("SOURCE", "VARIABLE_SUBTOTAL"))

  # REFINE VARIABLE AND CREATE VARIABLE_DESC --------------------------------

  acs_vars_data <- acs_vars_data %>%
    dplyr::mutate(VARIABLE = dplyr::case_when(
      INDICATOR %in% "COST_BURDEN_OWN" ~ stringr::str_c(VARIABLE,"_OWN"),
      INDICATOR %in% "COST_BURDEN_RENT" ~ stringr::str_c(VARIABLE,"_RENT"),
      TRUE ~ VARIABLE
    )) %>%
    dplyr::mutate(VARIABLE_DESC = stringr::str_c(MEASURE_TYPE, INDICATOR, SOURCE, sep = "_"))

  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------

  acs_vars_ready <- variable_template %>%
    dplyr::full_join(acs_vars_data,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "ENDYEAR",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "VARIABLE_ROLE",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))

 acs_variables <- acs_vars_ready

# CHECK DATA --------------------------------------------------------------


  check_acs_vars_ready <- function(){

    # This function shows all of the INDICATOR values and their INDICATOR_ROLEs.
    # If any NA's are showing up then something needs to be fixed

     acs_variables %>% dplyr::count(ENDYEAR,INDICATOR, VARIABLE, VARIABLE_ROLE)
  }



  # RETURN ------------------------------------------------------------------

  return(acs_variables)

}
