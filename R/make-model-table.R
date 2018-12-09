#' @title Make A Tibble of the Project Models
#' @description Return a `tibble` containing metadata for each of the
#'   models used to describe neighborhood change.
#' @return a `tibble`
#' @export
make_model_table <- function(){

  model_table <- tibble::tribble(
   ~MODEL,               ~TOPIC, ~MEASURE_TYPE,       ~INDICATOR,    ~SOURCE, ~ENDYEAR,
  "PORTLAND",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",    2017L,
  "PORTLAND",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",    2017L,
  "PORTLAND",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",    2017L,
  "PORTLAND",      "VULNERABILITY",     "PERCENT",         "INCOME",     "CHAS",    2015L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2011L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2011L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2011L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",     "CHAS",    2010L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2017L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2017L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2017L,
  "PORTLAND", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2017L,
  "PORTLAND",     "HOUSING MARKET",       "MEDIAN",          "VALUE",     "LTDB",    2000L,
  "PORTLAND",     "HOUSING MARKET",       "MEDIAN",          "VALUE",      "ACS",    2010L,
  "PORTLAND",     "HOUSING MARKET",       "MEDIAN",          "VALUE",      "ACS",    2017L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",    2015L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",    2015L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",    2015L,
    "COO_ORIGINAL",      "VULNERABILITY",     "PERCENT",         "INCOME",      "ACS",    2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2010L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2015L,
    "COO_ORIGINAL", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2015L,
    "COO_ORIGINAL",     "HOUSING MARKET",       "MEDIAN", "ASSESSED VALUE", "ASSESSOR",    2005L,
    "COO_ORIGINAL",     "HOUSING MARKET",       "MEDIAN", "ASSESSED VALUE", "ASSESSOR",    2010L,
    "COO_ORIGINAL",     "HOUSING MARKET",       "MEDIAN", "ASSESSED VALUE", "ASSESSOR",    2015L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED",      "VULNERABILITY",     "PERCENT",         "INCOME",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2011L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "ACS",    2017L,
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET",       "MEDIAN", "ASSESSED VALUE", "ASSESSOR",    2005L,
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET",       "MEDIAN", "ASSESSED VALUE", "ASSESSOR",    2010L,
    "COO_ORIGINAL_UPDATED",     "HOUSING MARKET",       "MEDIAN", "ASSESSED VALUE", "ASSESSOR",    2018L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",           "RACE",      "ACS",    2017L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",      "EDUCATION",      "ACS",    2017L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",         "TENURE",      "ACS",    2017L,
    "COO_REVISED",      "VULNERABILITY",     "PERCENT",         "INCOME",      "CHAS",   2015L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2011L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2011L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2011L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "CHAS",   2010L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",           "RACE",      "ACS",    2017L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",      "EDUCATION",      "ACS",    2017L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "TENURE",      "ACS",    2017L,
    "COO_REVISED", "DEMOGRAPHIC CHANGE",     "PERCENT",         "INCOME",      "CHAS",   2015L,
    "COO_REVISED",     "HOUSING MARKET",       "MEDIAN",           "RENT",      "ACS",    2010L,
    "COO_REVISED",     "HOUSING MARKET",       "MEDIAN",           "RENT",      "ACS",    2017L,
    "COO_REVISED",     "HOUSING MARKET",       "MEDIAN",     "SALE PRICE", "ASSESSOR",    2005L,
    "COO_REVISED",     "HOUSING MARKET",       "MEDIAN",     "SALE PRICE", "ASSESSOR",    2010L,
    "COO_REVISED",     "HOUSING MARKET",       "MEDIAN",     "SALE PRICE", "ASSESSOR",    2018L,
    "COO_REVISED",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",    2005L,
    "COO_REVISED",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",    2010L,
    "COO_REVISED",     "HOUSING MARKET",     "PERCENT",      "SALE RATE", "ASSESSOR",    2018L,
  "OTHER",     "VULENERABILITY",     "PERCENT",    "COST BURDEN",      "ACS",    2017L,
  "OTHER",     "VULENERABILITY",     "PERCENT",    "COST BURDEN",      "ACS",    2011L
  )






return(model_table)

}
