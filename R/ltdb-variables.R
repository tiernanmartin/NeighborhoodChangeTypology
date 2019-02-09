
#' @title Make A Tibble of The Project's US Census Variables from LTDB
#' @description Return a `tibble` of all of the US Census data variables
#'   that are obtained from the Brown University Longitudinal Tract Database (LTDB).
#' @param ltdb_data Tibble, desc
#' @param census_geography_metadata desc
#' @param cpi desc
#' @param variable_template Tibble, desc
#' @return a `tibble`

#' @rdname ltdb-variables
#' @export
make_ltdb_variables <- function(ltdb_data, census_geography_metadata, cpi, variable_template){


# PREPARE LTDB DATA ROLES & ADJUST FOR INFLATION --------------------------


  ltdb_variables_roles_2018_dollars <- ltdb_data %>%
    dplyr::mutate(INDICATOR = "VALUE",
                  VARIABLE_DESC = stringr::str_c(INDICATOR, SOURCE, sep = "_"),
                  VARIABLE_ROLE = "include", # there's only one variable and it is a value variable so its ROLE is "include"
                  ESTIMATE = purrr::map2_dbl(ESTIMATE, DATE_END, convert_to_2018_dollars))


  # STANDARDIZE CENSUS GEOGRAPHY FIELDS -------------------------------------

  ltdb_variables_geography <- ltdb_variables_roles_2018_dollars %>%
    dplyr::select(-GEOGRAPHY_ID_TYPE, -GEOGRAPHY_NAME, -GEOGRAPHY_TYPE) %>%  #drop all geography fields accept the join field (GEOGRAPHY_ID)
  dplyr::left_join(census_geography_metadata, by = c("GEOGRAPHY_ID"))

  # ARRANGE COLUMNS WITH TEMPLATE -------------------------------------------

  ltdb_variables_ready <- variable_template %>%
    dplyr::full_join(ltdb_variables_geography,
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


  ltdb_variables <- ltdb_variables_ready

  # CHECK DATA --------------------------------------------------------------


  check_ltdb_vars_ready <- function(){

    # This function shows all of the INDICATOR values and their INDICATOR_ROLEs.
    # If any NA's are showing up then something needs to be fixed

     ltdb_variables %>% dplyr::count(DATE_GROUP_ID,INDICATOR, VARIABLE, VARIABLE_DESC, VARIABLE_ROLE) %>% print(n=Inf)
  }


  # RETURN ------------------------------------------------------------------

  return(ltdb_variables)


}
