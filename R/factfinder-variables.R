#' @title Make A Tibble of The Project's US Census Variables from American Factfinder
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the American Factfinder user interface: \link{https://factfinder.census.gov/}
#' @param factfinder_data Tibble, desc
#' @param variable_template Tibble, desc
#' @return a `tibble`

#' @rdname factfinder-variables
#' @export
make_factfinder_variables <- function(factfinder_data, variable_template){

  # PREPARE LTDB DATA ROLES --------------------------------------------------------

  factfinder_variables_roles <- factfinder_data %>%
    dplyr::mutate(INDICATOR = "VALUE",
                  VARIABLE_ROLE = "include") # there's only one variable and it is a value variable so its ROLE is "include"

  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------

  factfinder_variables_ready <- variable_template %>%
    dplyr::full_join(factfinder_variables_roles,
                     by = c("SOURCE", "GEOGRAPHY_ID", "GEOGRAPHY_ID_TYPE", "GEOGRAPHY_NAME", "GEOGRAPHY_TYPE", "ENDYEAR", "INDICATOR", "VARIABLE", "VARIABLE_SUBTOTAL", "VARIABLE_SUBTOTAL_DESC", "VARIABLE_ROLE", "MEASURE_TYPE", "ESTIMATE", "MOE"))


  factfinder_variables <- factfinder_variables_ready

  # RETURN ------------------------------------------------------------------

  return(factfinder_variables)


}
