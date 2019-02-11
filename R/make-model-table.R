#' @title Make A Tibble of the Project Models
#' @description Return a `tibble` containing metadata for each of the
#'   models used to describe neighborhood change.
#' @return a `tibble`

#' @rdname model-table
#' @export
make_model_table <- function(){

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


