#' @title Make The Indicators in Models
#' @description Description
#' @param indicators_by_dimension desc
#' @param model_table_production desc
#' @return a `tibble`
#' @export
make_indicators_in_models <- function(indicators_by_dimension,
                                       model_table_production){

  # FILTER IND_BY_DIMENSION -------------------------------------------------

  inds_table_filter_join <- model_table_production %>%
    dplyr::select(DIMENSION, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID) %>%
    dplyr::distinct() %>%
    dplyr::arrange(DIMENSION, INDICATOR, VARIABLE, MEASURE_TYPE, DATE_GROUP_ID)

  inds_in_models_ready <-  indicators_by_dimension %>%
    dplyr::semi_join(inds_table_filter_join,  # only include the indicators that are used in the models
                     by = c("DIMENSION",
                            "INDICATOR",
                            "VARIABLE",
                            "MEASURE_TYPE",
                            "DATE_GROUP_ID"))

  inds_in_models <- inds_in_models_ready


  # CHECK RESULTS -----------------------------------------------------------

  view_count <- function(){
    inds_in_models %>% dplyr::count(DIMENSION,INDICATOR,VARIABLE,MEASURE_TYPE,DATE_GROUP_ID) %>% View()
  }

  # RETURN ------------------------------------------------------------------

  return(inds_in_models)

}
