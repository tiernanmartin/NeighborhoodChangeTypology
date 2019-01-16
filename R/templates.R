#' @title Make Template Objects
#' @description Return a `tibble` containing the common set of columns and
#'   column types.
#'   The following template objects are available:
#'   \itemize{
#'     \item \code{data_template}
#'     \item \code{variable_template}
#'     \item \code{indicator_template}
#'   }
#' @return a `tibble`

#' @rdname templates
#' @export
make_data_template <- function(){

data_template <- tibble::tibble(SOURCE = NA_character_,
                          GEOGRAPHY_ID = NA_character_,
                     GEOGRAPHY_ID_TYPE = NA_character_,
                        GEOGRAPHY_NAME = NA_character_,
                        GEOGRAPHY_TYPE = NA_character_,
                            DATE_BEGIN = NA_character_,
                              DATE_END = NA_character_,
                            DATE_RANGE = NA_character_,
                       DATE_RANGE_TYPE = NA_character_,
                              VARIABLE = NA_character_,
                     VARIABLE_SUBTOTAL = NA_character_,
                VARIABLE_SUBTOTAL_DESC = NA_character_,
                          MEASURE_TYPE = NA_character_,
                              ESTIMATE = NA_real_,
                                   MOE = NA_real_
                      ) %>% dplyr::slice(0)

return(data_template)

}

#' @rdname templates
#' @export
make_metadata_template <- function(){

metadata_template <- tibble::tibble(SOURCE = NA_character_,
                          GEOGRAPHY_ID = NA_character_,
                     GEOGRAPHY_ID_TYPE = NA_character_,
                        GEOGRAPHY_NAME = NA_character_,
                        GEOGRAPHY_TYPE = NA_character_,
                            DATE_BEGIN = NA_character_,
                              DATE_END = NA_character_,
                            DATE_RANGE = NA_character_,
                       DATE_RANGE_TYPE = NA_character_
                      ) %>% dplyr::slice(0)

return(metadata_template)

}

#' @rdname templates
#' @export
make_variable_template <- function(){

variable_template <- tibble::tibble(SOURCE = NA_character_,
                                     GEOGRAPHY_ID = NA_character_,
                         GEOGRAPHY_ID_TYPE = NA_character_,
                            GEOGRAPHY_NAME = NA_character_,
                            GEOGRAPHY_TYPE = NA_character_,
                                DATE_BEGIN = NA_character_,
                                  DATE_END = NA_character_,
                                DATE_RANGE = NA_character_,
                           DATE_RANGE_TYPE = NA_character_,
                                 INDICATOR = NA_character_,
                                  VARIABLE = NA_character_,
                             VARIABLE_DESC = NA_character_,
                         VARIABLE_SUBTOTAL = NA_character_,
                    VARIABLE_SUBTOTAL_DESC = NA_character_,
                             VARIABLE_ROLE = NA_character_,
                              MEASURE_TYPE = NA_character_,
                                  ESTIMATE = NA_real_,
                                       MOE = NA_real_
                      ) %>% dplyr::slice(0)

return(variable_template)

}

#' @rdname templates
#' @export
make_indicator_template <- function(){

indicator_template <- tibble::tibble(SOURCE = NA_character_,
                                     GEOGRAPHY_ID = NA_character_,
                         GEOGRAPHY_ID_TYPE = NA_character_,
                            GEOGRAPHY_NAME = NA_character_,
                            GEOGRAPHY_TYPE = NA_character_,
                                DATE_BEGIN = NA_character_,
                                  DATE_END = NA_character_,
                                DATE_RANGE = NA_character_,
                           DATE_RANGE_TYPE = NA_character_,
                                 INDICATOR = NA_character_,
                                  VARIABLE = NA_character_,
                             VARIABLE_DESC = NA_character_,
                              MEASURE_TYPE = NA_character_,
                                  ESTIMATE = NA_real_,
                                       MOE = NA_real_
                      ) %>% dplyr::slice(0)

return(indicator_template)

}
