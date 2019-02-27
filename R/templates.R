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
                         DATE_GROUP_ID = NA_character_,
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
                         DATE_GROUP_ID = NA_character_,
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
                             DATE_GROUP_ID = NA_character_,
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
                             DATE_GROUP_ID = NA_character_,
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

#' @rdname templates
#' @export
make_indicator_dimension_template <- function(){

indicator_dimension_template <- tibble::tibble(SOURCE = NA_character_,
                                     GEOGRAPHY_ID = NA_character_,
                         GEOGRAPHY_ID_TYPE = NA_character_,
                            GEOGRAPHY_NAME = NA_character_,
                            GEOGRAPHY_TYPE = NA_character_,
                             DATE_GROUP_ID = NA_character_,
                                DATE_BEGIN = NA_character_,
                                  DATE_END = NA_character_,
                                DATE_RANGE = NA_character_,
                           DATE_RANGE_TYPE = NA_character_,
                                 DIMENSION = NA_character_,
                                 INDICATOR = NA_character_,
                                  VARIABLE = NA_character_,
                             VARIABLE_DESC = NA_character_,
                              MEASURE_TYPE = NA_character_,
                                  ESTIMATE = NA_real_,
                                       MOE = NA_real_
                      ) %>% dplyr::slice(0)

return(indicator_dimension_template)

}

#' @rdname templates
#' @export
make_indicator_type_template <- function(){



  indicator_dimension_template %>%




indicator_type_template <- tibble::tibble(SOURCE = NA_character_,
                                     GEOGRAPHY_ID = NA_character_,
                         GEOGRAPHY_ID_TYPE = NA_character_,
                            GEOGRAPHY_NAME = NA_character_,
                            GEOGRAPHY_TYPE = NA_character_,
                             DATE_GROUP_ID = NA_character_,
                                DATE_BEGIN = NA_character_,
                                  DATE_END = NA_character_,
                                DATE_RANGE = NA_character_,
                           DATE_RANGE_TYPE = NA_character_,
                                 DIMENSION = NA_character_,
                                 INDICATOR = NA_character_,
                                  VARIABLE = NA_character_,
                             VARIABLE_DESC = NA_character_,
                              MEASURE_TYPE = NA_character_,
                                  ESTIMATE = NA_real_,
                                       MOE = NA_real_,
                         INDICATOR_TYPE = NA_character_,
                         INDICATOR_TYPE_THRESHOLD = NA_character_,
                         INDICATOR_TYPE_THRESHOLD_VALUE = NA_real_,
                         INDICATOR_TYPE_DESC = NA_character_,
                         INDICATOR_TYPE_VALUE = NA_real_,
                         INDICATOR_TYPE_VALUE_DESC = NA_character_,
                         INDICATOR_TYPE_MODEL = NA_character_
                      ) %>% dplyr::slice(0)

return(indicator_type_template)

}

#' @rdname templates
#' @export
make_indicator_value_template <- function(){

indicator_value_template <- tibble::tibble(SOURCE = NA_character_,
                                     GEOGRAPHY_ID = NA_character_,
                         GEOGRAPHY_ID_TYPE = NA_character_,
                            GEOGRAPHY_NAME = NA_character_,
                            GEOGRAPHY_TYPE = NA_character_,
                             DATE_GROUP_ID = NA_character_,
                                DATE_BEGIN = NA_character_,
                                  DATE_END = NA_character_,
                                DATE_RANGE = NA_character_,
                           DATE_RANGE_TYPE = NA_character_,
                                 DIMENSION = NA_character_,
                                 INDICATOR = NA_character_,
                                  VARIABLE = NA_character_,
                             VARIABLE_DESC = NA_character_,
                              MEASURE_TYPE = NA_character_,
                                  ESTIMATE = NA_real_,
                         ESTIMATE_BEGIN = NA_real_,
                         ESTIMATE_END = NA_real_,
                         MOE = NA_real_,
                         MOE_BEGIN = NA_real_,
                         MOE_END = NA_real_,
                         DIFFERENCE = NA_real_ ,
                         DIFFERENCE_MOE = NA_real_ ,
                         RELATIVE = NA_real_,
                         RELATIVE_DESC = NA_character_,
                         RELATIVE_THRESHOLD = NA_real_,
                         RELATIVE_LGL = NA,
                         RELATIVE_BEGIN = NA_real_,
                         RELATIVE_DESC_BEGIN = NA_character_,
                         RELATIVE_THRESHOLD_BEGIN = NA_real_,
                         RELATIVE_LGL_BEGIN = NA,
                         RELATIVE_END = NA_real_,
                         RELATIVE_DESC_END = NA_character_,
                         RELATIVE_THRESHOLD_END = NA_real_,
                         RELATIVE_LGL_END = NA,
                         CHANGE = NA_real_,
                         CHANGE_MOE = NA_real_,
                         CHANGE_DESC = NA_character_,
                         CHANGE_THRESHOLD = NA_real_,
                         CHANGE_LGL = NA,
                         RELATIVE_CHANGE_DESC = NA_character_,
                         RELATIVE_CHANGE_LGL = NA,
                         PROXIMITY_DESC = NA_character_
                      ) %>% dplyr::slice(0)

return(indicator_value_template)

}
