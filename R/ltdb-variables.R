
#' @title Make A Tibble of The Project's US Census Variables from LTDB
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the Brown University Longitudinal Tract Database (LTDB).
#' @param ltdb_data Tibble, desc
#' @param variable_template Tibble, desc
#' @return a `tibble`

#' @rdname ltdb-variables
#' @export
make_ltdb_variables <- function(ltdb_data, variable_template){

  # PREPARE LTDB DATA ROLES --------------------------------------------------------

  ltdb_variables_roles <- ltdb_data %>%
    dplyr::mutate(INDICATOR = "VALUE",
                  VARIABLE_ROLE = "include") # there's only one variable and it is a value variable so its ROLE is "include"

  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------

  ltdb_variables_ready <- variable_template %>%
    dplyr::full_join(ltdb_variables_roles,
                     by = c("SOURCE", "GEOGRAPHY_ID", "GEOGRAPHY_ID_TYPE", "GEOGRAPHY_NAME", "GEOGRAPHY_TYPE", "ENDYEAR", "INDICATOR", "VARIABLE", "VARIABLE_SUBTOTAL", "VARIABLE_SUBTOTAL_DESC", "VARIABLE_ROLE", "MEASURE_TYPE", "ESTIMATE", "MOE"))


  ltdb_variables <- ltdb_variables_ready

  # RETURN ------------------------------------------------------------------

  return(ltdb_variables)


}
