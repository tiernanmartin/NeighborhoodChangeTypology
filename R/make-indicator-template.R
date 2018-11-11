#' @title Make A Template for Indicator Objects
#' @description Return a `tibble` containing the unified set of columns and
#'   column types used for all indicator objects in this project.
#' @return a `tibble`
#' @export
make_indicator_template <- function(){

indicator_template <- tibble::tibble(GEOGRAPHY_ID = NA_character_,
                         GEOGRAPHY_ID_TYPE = NA_character_,
                            GEOGRAPHY_NAME = NA_character_,
                            GEOGRAPHY_TYPE = NA_character_,
                                   ENDYEAR = NA_integer_,
                                  VARIABLE = NA_character_,
                              MEASURE_TYPE = NA_character_,
                                  ESTIMATE = NA_real_,
                                       MOE = NA_real_
                      )

return(indicator_template)

}
