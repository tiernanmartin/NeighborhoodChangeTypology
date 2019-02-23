#' @title Make A Tibble of the Project Models
#' @description Return a `tibble` containing metadata for each of the
#'   models used to describe neighborhood change.
#' @return a `tibble`

#' @rdname project-tables
#' @export
make_acs_tables <- function(){

  acs_tables <- tibble::tribble(
    ~VARIABLE,    ~INDICATOR, ~MEASURE_TYPE,                                                                               ~TOPIC,                                        ~UNIVERSE,
    "B01003",  "POPULATION",       "COUNT",                                                                   "TOTAL POPULATION",                               "Total population",
    "B03002",        "RACE",       "COUNT",                                                  "HISPANIC OR LATINO ORIGIN BY RACE",                               "Total population",
    "B15002",   "EDUCATION",       "COUNT",                 "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",                   "Population 25 years and over",
    "B19001",      "INCOME",       "COUNT",        "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)",                                     "Households",
    "B19013",      "INCOME",      "MEDIAN", "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2011 INFLATION-ADJUSTED DOLLARS)",                                     "Households",
    "B25033",      "TENURE",       "COUNT",         "TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY UNITS IN STRUCTURE",     "Total population in occupied housing units",
    "B25106", "COST BURDEN",       "COUNT",    "TENURE BY HOUSING COSTS AS A COUNTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS",                         "Occupied housing units",
    "B25058",        "RENT",      "MEDIAN",                                                     "MEDIAN CONTRACT RENT (DOLLARS)", "Renter-occupied housing units paying cash rent",
    "B25077",       "VALUE",      "MEDIAN",                                                             "MEDIAN VALUE (DOLLARS)",                   "Owner-occupied housing units",
    "B25032", "MULTIFAMILY",       "COUNT",                                                       "TENURE BY UNITS IN STRUCTURE",                         "Occupied housing units"
  )

  return(acs_tables)
}


#' @rdname project-tables
#' @export
make_model_table_inputs <- function(path){


  # READ DATA ------------------------------------------------------------

  # this data object is created in a Google Sheets document and doesn't require any adjustments
  model_table_inputs <- readr::read_csv(path, col_types = "cccccccccc")

  # RETURN ------------------------------------------------------------------

  return(model_table_inputs)

}

#' @rdname project-tables
#' @export
make_model_table_production <- function(path){

  # this script should be edited once the final version of `model_table_production` is ready

  # PREPARE DATA ------------------------------------------------------------

  model_table_production_raw <- readr::read_csv(path, col_types = "cccccccc")

  model_table_production_ready <- model_table_production_raw %>%
    dplyr::mutate(MODEL = stringr::str_remove(MODEL,"^\\d{2}\\s-\\s")) %>% # drop the leading numbering ("01 - PORTLAND")
    dplyr::mutate_all(to_caps_underscores) %>%
    dplyr::select(-VARIABLE_DESC) # once this field is filled in it should no longer be dropped

  # RETURN ------------------------------------------------------------------
  model_table_production <- model_table_production_ready

  return(model_table_production)


}


