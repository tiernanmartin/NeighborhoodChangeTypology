#' @title Make A Tibble of The Project's US Census Variables from American Factfinder
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the American Factfinder user interface: \link{https://factfinder.census.gov/}
#' @param factfinder_data Tibble, desc
#' @param variable_template Tibble, desc
#' @param census_geography_metadata desc
#' @return a `tibble`

#' @rdname factfinder-variables
#' @export
make_factfinder_variables <- function(factfinder_data, variable_template, census_geography_metadata){

  # PREPARE LTDB DATA ROLES --------------------------------------------------------

  factfinder_variables_roles <- factfinder_data %>%
    dplyr::mutate(INDICATOR = "VALUE",
                  VARIABLE_DESC = stringr::str_c(INDICATOR, SOURCE, sep = "_"),
                  VARIABLE_ROLE = "include") # there's only one variable and it is a value variable so its ROLE is "include"



  # STANDARDIZE CENSUS GEOGRAPHY FIELDS -------------------------------------

  factfinder_variable_geography <- factfinder_variables_roles %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE, -GEOGRAPHY_NAME, -GEOGRAPHY_TYPE) %>%  #drop all geography fields accept the join field (GEOGRAPHY_ID)
  dplyr::left_join(census_geography_metadata, by = c("GEOGRAPHY_ID"))



  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------

  factfinder_variables_ready <- variable_template %>%
    dplyr::full_join(factfinder_variable_geography,
                     by = c("SOURCE",
                            "GEOGRAPHY_ID",
                            "GEOGRAPHY_ID_TYPE",
                            "GEOGRAPHY_NAME",
                            "GEOGRAPHY_TYPE",
                            "DATE_GROUP_ID",
                            "DATE_BEGIN",
                            "DATE_END",
                            "DATE_RANGE",
                            "DATE_RANGE_TYPE",
                            "INDICATOR",
                            "VARIABLE",
                            "VARIABLE_DESC",
                            "VARIABLE_SUBTOTAL",
                            "VARIABLE_SUBTOTAL_DESC",
                            "VARIABLE_ROLE",
                            "MEASURE_TYPE",
                            "ESTIMATE",
                            "MOE"))


  factfinder_variables <- factfinder_variables_ready

  # CHECK DATA --------------------------------------------------------------


  check_factfinder_vars_ready <- function(){

    # This function shows all of the INDICATOR values and their INDICATOR_ROLEs.
    # If any NA's are showing up then something needs to be fixed

    factfinder_variables %>% dplyr::count(DATE_GROUP_ID,INDICATOR, VARIABLE, VARIABLE_DESC, VARIABLE_ROLE) %>% print(n=Inf)
  }

  # RETURN ------------------------------------------------------------------

  return(factfinder_variables)


}
