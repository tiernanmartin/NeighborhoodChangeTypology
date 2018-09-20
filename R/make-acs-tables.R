#' @title Make A Tibble of ACS Tables
#' @description Return a `tibble` of the American Community Survey tables that
#'   are used in the Neighborhood Change Typology model.
#' @return a `tibble`
#' @importFrom tibble tribble
#' @export
make_acs_tables <- function(){

  acs_tables <- tibble::tribble(
      ~NAME,    ~INDICATOR,                                                                              ~TOPIC,                                    ~UNIVERSE,
   "B03002",        "RACE",                                                 "HISPANIC OR LATINO ORIGIN BY RACE",                           "Total population",
   "B15002",   "EDUCATION",                "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",               "Population 25 years and over",
   "B19001",      "INCOME",       "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)",                                 "Households",
   "B25033",      "TENURE",        "TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY UNITS IN STRUCTURE", "Total population in occupied housing units",
   "B25106", "COST BURDEN", "TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS",                     "Occupied Housing Units"
   )

  return(acs_tables)
}
