#' @title Make A Tibble of ACS Tables
#' @description Return a `tibble` of the American Community Survey tables that
#'   are used in the Neighborhood Change Typology model.
#' @return a `tibble`
#' @importFrom tibble tribble
#' @export
make_acs_tables <- function(){

  acs_tables <- tibble::tribble(
                       ~VARIABLE,    ~INDICATOR, ~MEASURE_TYPE,                                                                            ~TOPIC,                                        ~UNIVERSE,
                       "B01003",        "POPULATION",       "COUNT",                                                                "TOTAL POPULATION",                               "Total population",
                    "B03002",        "RACE",       "COUNT",                                               "HISPANIC OR LATINO ORIGIN BY RACE",                               "Total population",
                    "B15002",   "EDUCATION",       "COUNT",              "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",                   "Population 25 years and over",
                    "B19001",      "INCOME",       "COUNT",     "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)",                                     "Households",
                    "B25033",      "TENURE",       "COUNT",      "TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY UNITS IN STRUCTURE",     "Total population in occupied housing units",
                    "B25106", "COST BURDEN",       "COUNT", "TENURE BY HOUSING COSTS AS A COUNTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS",                         "Occupied Housing Units",
                    "B25058",        "RENT",      "MEDIAN",                                                  "MEDIAN CONTRACT RENT (DOLLARS)", "Renter-occupied housing units paying cash rent",
                    "B25077",       "VALUE",      "MEDIAN",                                                          "MEDIAN VALUE (DOLLARS)",                   "Owner-occupied housing units"
                    )
return(acs_tables)
}
