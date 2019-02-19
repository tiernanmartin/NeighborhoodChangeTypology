#' @title Make A Tibble of the Project Models
#' @description Return a `tibble` containing metadata for each of the
#'   models used to describe neighborhood change.
#' @return a `tibble`

#' @rdname project-tables
#' @export
make_acs_tables <- function(){

  acs_tables <- tibble::tribble(
    ~VARIABLE,    ~INDICATOR, ~MEASURE_TYPE,                                                                            ~TOPIC,                                        ~UNIVERSE,
    "B01003",        "POPULATION",       "COUNT",                                                                "TOTAL POPULATION",                               "Total population",
    "B03002",        "RACE",       "COUNT",                                               "HISPANIC OR LATINO ORIGIN BY RACE",                               "Total population",
    "B15002",   "EDUCATION",       "COUNT",              "SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",                   "Population 25 years and over",
    "B19001",      "INCOME",       "COUNT",     "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS)",                                     "Households",
    "B25033",      "TENURE",       "COUNT",      "TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY UNITS IN STRUCTURE",     "Total population in occupied housing units",
    "B25106", "COST BURDEN",       "COUNT", "TENURE BY HOUSING COSTS AS A COUNTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS",                         "Occupied housing units",
    "B25058",        "RENT",      "MEDIAN",                                                  "MEDIAN CONTRACT RENT (DOLLARS)", "Renter-occupied housing units paying cash rent",
    "B25077",       "VALUE",      "MEDIAN",                                                          "MEDIAN VALUE (DOLLARS)",                   "Owner-occupied housing units",
    "B25032", "MULTIFAMILY",       "COUNT",                                                    "TENURE BY UNITS IN STRUCTURE",                         "Occupied housing units"
  )
  return(acs_tables)
}


#' @rdname project-tables
#' @export
make_model_table_inputs <- function(){

  model_table_inputs <- tibble::tribble(
    ~MODEL,               ~TOPIC,       ~INDICATOR, ~MEASURE_TYPE,    ~SOURCE, ~DATE_GROUP_ID, ~DATE_BEGIN, ~DATE_END,
    "PORTLAND",      "VULNERABILITY",           "RACE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",      "VULNERABILITY",      "EDUCATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",      "VULNERABILITY",         "TENURE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",      "VULNERABILITY",         "INCOME",       "COUNT",     "CHAS",    "2011_2015",      "2011",    "2015",
    "PORTLAND", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "PORTLAND", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",     "CHAS",    "2006_2010",      "2006",    "2010",
    "PORTLAND", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",     "HOUSING MARKET",          "VALUE",      "MEDIAN",     "LTDB",    "2000_2000",      "2000",    "2000",
    "PORTLAND",     "HOUSING MARKET",          "VALUE",      "MEDIAN",      "ACS",    "2006_2010",      "2006",    "2010",
    "PORTLAND",     "HOUSING MARKET",          "VALUE",      "MEDIAN",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL",      "VULNERABILITY",           "RACE",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",      "VULNERABILITY",      "EDUCATION",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",      "VULNERABILITY",         "TENURE",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",      "VULNERABILITY",         "INCOME",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_ORIGINAL",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_ORIGINAL",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2015_2015",      "2015",    "2015",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",           "RACE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",      "EDUCATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",         "TENURE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",         "INCOME",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2018_2018",      "2018",    "2018",
    "COO_REVISED",      "VULNERABILITY",           "RACE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",      "VULNERABILITY",      "EDUCATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",      "VULNERABILITY",         "TENURE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",      "VULNERABILITY",         "INCOME",       "COUNT",     "CHAS",    "2011_2015",      "2011",    "2015",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",     "CHAS",    "2006_2010",      "2006",    "2010",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",           "RACE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",      "EDUCATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "TENURE",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "INCOME",       "COUNT",     "CHAS",    "2011_2015",      "2011",    "2015",
    "COO_REVISED",     "HOUSING MARKET",           "RENT",      "MEDIAN",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_REVISED",     "HOUSING MARKET",           "RENT",      "MEDIAN",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",     "HOUSING MARKET",    "MULTIFAMILY",       "COUNT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_REVISED",     "HOUSING MARKET",    "MULTIFAMILY",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",     "HOUSING MARKET",     "SALE PRICE",      "MEDIAN", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_REVISED",     "HOUSING MARKET",     "SALE PRICE",      "MEDIAN", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_REVISED",     "HOUSING MARKET",     "SALE PRICE",      "MEDIAN", "ASSESSOR",    "2018_2018",      "2018",    "2018",
    "COO_REVISED",     "HOUSING MARKET",      "SALE RATE",       "COUNT", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_REVISED",     "HOUSING MARKET",      "SALE RATE",       "COUNT", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_REVISED",     "HOUSING MARKET",      "SALE RATE",       "COUNT", "ASSESSOR",    "2018_2018",      "2018",    "2018",
    "OTHER",     "VULENERABILITY",    "COST BURDEN",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017",
    "OTHER",     "VULENERABILITY",    "COST BURDEN",       "COUNT",      "ACS",    "2007_2011",      "2007",    "2011",
    "OTHER",      "MISCELLANEOUS",     "POPULATION",       "COUNT",      "ACS",    "2006_2010",      "2006",    "2010",
    "OTHER",      "MISCELLANEOUS",     "POPULATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017"
  )


  return(model_table_inputs)

}

#' @rdname project-tables
#' @export
make_model_table_production <- function(){

  # this script should be edited once the final version of `model_table_production` is ready

  # PREPARE DATA ------------------------------------------------------------

  model_table_production_raw <- readr::read_csv("tmp/model_table_production_ref_only_20190216.csv",col_types = "cccccccc")

  model_table_production_ready <- model_table_production_raw %>%
    dplyr::mutate(MODEL = stringr::str_remove(MODEL,"^\\d{2}\\s-\\s")) %>% # drop the leading numbering ("01 - PORTLAND")
    dplyr::mutate_all(to_caps_underscores) %>%
    dplyr::select(-VARIABLE_DESC) # once this field is filled in it should no longer be dropped

  # RETURN ------------------------------------------------------------------
  model_table_production <- model_table_production_ready

  return(model_table_production)


}

#' @rdname project-tables
#' @export
make_model_table_archive <- function(){

  model_table <- tibble::tribble(
    ~MODEL,               ~TOPIC,       ~INDICATOR, ~MEASURE_TYPE,    ~SOURCE, ~DATE_GROUP_ID, ~DATE_BEGIN, ~DATE_END,
    "PORTLAND",      "VULNERABILITY",           "RACE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",      "VULNERABILITY",      "EDUCATION",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",      "VULNERABILITY",         "TENURE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",      "VULNERABILITY",         "INCOME",     "PERCENT",     "CHAS",    "2011_2015",      "2011",    "2015",
    "PORTLAND", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "PORTLAND", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",     "CHAS",    "2006_2010",      "2006",    "2010",
    "PORTLAND", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "PORTLAND",     "HOUSING MARKET",          "VALUE",      "MEDIAN",     "LTDB",    "2000_2000",      "2000",    "2000",
    "PORTLAND",     "HOUSING MARKET",          "VALUE",      "MEDIAN",      "ACS",    "2006_2010",      "2006",    "2010",
    "PORTLAND",     "HOUSING MARKET",          "VALUE",      "MEDIAN",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL",      "VULNERABILITY",           "RACE",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",      "VULNERABILITY",      "EDUCATION",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",      "VULNERABILITY",         "TENURE",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",      "VULNERABILITY",         "INCOME",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",      "ACS",    "2011_2015",      "2011",    "2015",
    "COO_ORIGINAL",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_ORIGINAL",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_ORIGINAL",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2015_2015",      "2015",    "2015",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",           "RACE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",      "EDUCATION",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",         "TENURE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",         "INCOME",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET", "ASSESSED VALUE",      "MEDIAN", "ASSESSOR",    "2018_2018",      "2018",    "2018",
    "COO_REVISED",      "VULNERABILITY",           "RACE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",      "VULNERABILITY",      "EDUCATION",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",      "VULNERABILITY",         "TENURE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",      "VULNERABILITY",         "INCOME",     "PERCENT",     "CHAS",    "2011_2015",      "2011",    "2015",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",     "CHAS",    "2006_2010",      "2006",    "2010",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",           "RACE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",      "EDUCATION",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "TENURE",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED", "DEMOGRAPHIC CHANGE",         "INCOME",     "PERCENT",     "CHAS",    "2011_2015",      "2011",    "2015",
    "COO_REVISED",     "HOUSING MARKET",           "RENT",      "MEDIAN",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_REVISED",     "HOUSING MARKET",           "RENT",      "MEDIAN",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",     "HOUSING MARKET",   "MULTIFAMILY",      "PERCENT",      "ACS",    "2006_2010",      "2006",    "2010",
    "COO_REVISED",     "HOUSING MARKET",   "MULTIFAMILY",      "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "COO_REVISED",     "HOUSING MARKET",     "SALE PRICE",      "MEDIAN", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_REVISED",     "HOUSING MARKET",     "SALE PRICE",      "MEDIAN", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_REVISED",     "HOUSING MARKET",     "SALE PRICE",      "MEDIAN", "ASSESSOR",    "2018_2018",      "2018",    "2018",
    "COO_REVISED",     "HOUSING MARKET",      "SALE RATE",     "PERCENT", "ASSESSOR",    "2005_2005",      "2005",    "2005",
    "COO_REVISED",     "HOUSING MARKET",      "SALE RATE",     "PERCENT", "ASSESSOR",    "2010_2010",      "2010",    "2010",
    "COO_REVISED",     "HOUSING MARKET",      "SALE RATE",     "PERCENT", "ASSESSOR",    "2018_2018",      "2018",    "2018",
    "OTHER",     "VULENERABILITY",    "COST BURDEN",     "PERCENT",      "ACS",    "2013_2017",      "2013",    "2017",
    "OTHER",     "VULENERABILITY",    "COST BURDEN",     "PERCENT",      "ACS",    "2007_2011",      "2007",    "2011",
    "OTHER",      "MISCELLANEOUS",     "POPULATION",       "COUNT",      "ACS",    "2006_2010",      "2006",    "2010",
    "OTHER",      "MISCELLANEOUS",     "POPULATION",       "COUNT",      "ACS",    "2013_2017",      "2013",    "2017"
  )


  return(model_table)

}


